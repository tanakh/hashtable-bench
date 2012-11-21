{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}

import           Control.Applicative
import           Control.Monad
import           Criterion.Main
import qualified Data.ByteString.Char8 as S
import           Data.Hashable
import qualified Data.HashMap.Strict   as HMS
import qualified Data.HashTable        as HT
import qualified Data.HashTable.Class  as HTC
import qualified Data.HashTable.IO     as HTIO
import           Data.List
import qualified Data.Map.Strict       as MS
import           Data.Maybe

import           Foreign               hiding (void)
import           Foreign.C

main :: IO ()
main = do
  ws <- S.words <$> S.readFile "bible"

  let n = length ws
  ppw <- newArray =<< mapM (newCString . S.unpack) ws

  putStrLn $ show n ++ " words"

  defaultMain
    [ bgroup "pure"
      [ bench "Data.Map.Strict"     $ whnf wcMS ws
      , bench "Data.HashMap.Strict" $ whnf wcHMS ws
      ]

    , bgroup "impure"
      [ bench "Data.HashTable"      $ whnfIO $ wcHT ws

      , bench "Data.HashTable.IO.BasicHashTable" $ whnfIO
        $ wcHTs (undefined :: HTIO.BasicHashTable S.ByteString Int) ws
      , bench "Data.HashTable.IO.CuckooHashTable" $ whnfIO
        $ wcHTs (undefined :: HTIO.CuckooHashTable S.ByteString Int) ws
      , bench "Data.HashTable.IO.LinearHashTable" $ whnfIO
        $ wcHTs (undefined :: HTIO.LinearHashTable S.ByteString Int) ws
      ]

    , bgroup "C++"
      [ bench "std::map"            $ whnfIO $ wc_std_map n ppw
      , bench "std::uomap"          $ whnfIO $ wc_std_uomap n ppw
      ]
    ]

--

wcMS :: [S.ByteString] -> Int
wcMS ws =
  let mm = -- MS.fromListWith (+) $ map (, 1) ws
           -- this is faster
           foldl' (\m w -> MS.insertWith (+) w 1 m) MS.empty ws
  in MS.foldl' (+) 0 mm

wcHMS :: [S.ByteString] -> Int
wcHMS ws =
  let mm = foldl' (\m w -> HMS.insertWith (+) w 1 m) HMS.empty ws
  in HMS.foldl' (+) 0 mm

wcHT :: [S.ByteString] -> IO Int
wcHT ws = do
  ht <- HT.new (==) (fromIntegral . hash)
  forM_ ws $ \w -> do
    mb <- HT.lookup ht w
    case mb of
      Nothing -> HT.insert ht w 1
      Just v  -> void $ HT.update ht w $! (v + 1)
  sum . map snd <$> HT.toList ht

wcHTs :: forall h htt. (HTC.HashTable h, htt ~ HTIO.IOHashTable h S.ByteString Int)
         => htt -> [S.ByteString] -> IO Int
wcHTs _ ws = do
  ht <- HTIO.new :: IO htt
  forM_ ws $ \w -> do
    mb <- HTIO.lookup ht w
    HTIO.insert ht w $! fromMaybe 0 mb + 1
  HTIO.foldM (\s (_, v) -> return $! s + v) 0 ht
{-# INLINE wcHTs #-}

foreign import ccall wc_std_map   :: Int -> Ptr CString -> IO Int
foreign import ccall wc_std_uomap :: Int -> Ptr CString -> IO Int
