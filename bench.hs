{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}

import           Control.Applicative
import           Control.Monad
import           Criterion.Main
import qualified Data.ByteString.Char8 as S
import           Data.Hashable
import qualified Data.HashMap          as HM
import qualified Data.HashMap.Strict   as HMS
import qualified Data.HashTable        as HT
import qualified Data.HashTable.Class  as HTC
import qualified Data.HashTable.IO     as HTIO
import           Data.List
import qualified Data.Map.Strict       as MS
import           Data.Maybe
import qualified Data.Judy as J

import           Foreign               hiding (void)
import           Foreign.C

main :: IO ()
main = do
  ws <- S.words <$> S.readFile "bible"

  let !n = length ws
  ppw <- newArray =<< mapM (newCString . S.unpack) ws

  putStrLn $ show n ++ " words"

  defaultMain
    [ bgroup "pure"
      [ bench "containers"           $ whnf wcMS ws
      , bench "unordered-containers" $ whnf wcHMS ws
      , bench "hashmap"              $ whnf wcHM ws
      ]

    , bgroup "impure"
      [ bench "base (deprecated)" $ whnfIO $ wcHT ws

      , bench "hashtables/BasicHashTable" $ whnfIO
        $ wcHTs (undefined :: HTIO.BasicHashTable S.ByteString Int) ws
      , bench "hashtables/CuckooHashTable" $ whnfIO
        $ wcHTs (undefined :: HTIO.CuckooHashTable S.ByteString Int) ws
      , bench "hashtables/LinearHashTable" $ whnfIO
        $ wcHTs (undefined :: HTIO.LinearHashTable S.ByteString Int) ws

      , bench "judy" $ whnfIO $ wcJudy ws
      ]

    , bgroup "C++"
      [ bench "std::map"            $ whnfIO $ wc_std_map n ppw
      , bench "std::uomap"          $ whnfIO $ wc_std_uomap n ppw
      ]
    ]

--

wcMS :: [S.ByteString] -> Int
wcMS ws =
  let !mm = -- MS.fromListWith (+) $ map (, 1) ws
            -- this is faster
            foldl' (\m w -> MS.insertWith (+) w 1 m) MS.empty ws
  in MS.foldl' (+) 0 mm
{-# INLINE wcMS #-}

wcHMS :: [S.ByteString] -> Int
wcHMS ws =
  let !mm = foldl' (\m w -> HMS.insertWith (+) w 1 m) HMS.empty ws
  in HMS.foldl' (+) 0 mm
{-# INLINE wcHMS #-}

wcHM :: [S.ByteString] -> Int
wcHM ws =
  let !mm = foldl' ins HM.empty ws
  in sum . map snd $! HM.toList mm
  where
    -- hashmap's Data.HashMap has no strict operator,
    -- so I implemented explicit strict insertion operator
    ins m w = let !v = fromMaybe 0 (HM.lookup w m) + 1
              in HM.insert w v m
{-# INLINE wcHM #-}

wcHT :: [S.ByteString] -> IO Int
wcHT ws = do
  !ht <- HT.new (==) (fromIntegral . hash)
  forM_ ws $ \w -> do
    !mb <- HT.lookup ht w
    case mb of
      -- HT.insert does not overwrite, instead of other's implementation.
      Nothing -> HT.insert ht w 1
      Just v  -> void $ HT.update ht w $! (v + 1)
  sum . map snd <$> HT.toList ht
{-# INLINE wcHT #-}

wcHTs :: forall h htt. (HTC.HashTable h, htt ~ HTIO.IOHashTable h S.ByteString Int)
         => htt -> [S.ByteString] -> IO Int
wcHTs _ ws = do
  !ht <- HTIO.new :: IO htt
  forM_ ws $ \w -> do
    !mb <- HTIO.lookup ht w
    HTIO.insert ht w $! fromMaybe 0 mb + 1
  HTIO.foldM (\s (_, v) -> return $! s + v) 0 ht
{-# INLINE wcHTs #-}

-- XXX: This is not correct implementation
wcJudy :: [S.ByteString] -> IO Int
wcJudy ws = do
  !ht <- J.new
  forM_ ws $ \w -> do
    let !h = fromIntegral $ hash w
    !mb <- J.lookup h ht
    J.insert h (fromMaybe 0 mb + 1) ht
  sum <$> J.elems ht
{-# INLINE wcJudy #-}

foreign import ccall wc_std_map   :: Int -> Ptr CString -> IO Int
foreign import ccall wc_std_uomap :: Int -> Ptr CString -> IO Int
