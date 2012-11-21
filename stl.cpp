#include <iostream>
#include <string>
#include <map>
#include <tr1/unordered_map>
using namespace std;
using namespace std::tr1;

extern "C"
int wc_std_map(int n, char **words)
{
    map<string, int> mm;
    for (int i = 0; i < n; ++i) {
	// ++mm[words[i]];

	// adjust Haskell implementation
	// (Haskell does not handle pointer references)
	int t = mm[words[i]];
	mm[words[i]] = t + 1;
    }

    int ret = 0;
    for (map<string, int>::const_iterator p = mm.begin();
	 p != mm.end(); ++p)
	ret += p-> second;
    return ret;
}

extern "C"
int wc_std_uomap(int n, char **words)
{
    unordered_map<string, int> mm;
    for (int i = 0; i < n; ++i) {
	//++mm[words[i]];

	// same as above
	int t = mm[words[i]];
	mm[words[i]] = t + 1;
    }

    int ret = 0;
    for (unordered_map<string, int>::const_iterator p = mm.begin();
	 p != mm.end(); ++p)
	ret += p-> second;
    return ret;
}
