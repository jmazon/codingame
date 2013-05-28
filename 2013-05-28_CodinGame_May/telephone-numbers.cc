#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

static vector<string> v;

static int count(int s, int e, int p)
{
  if (s >= e) return 0;
  if (p >= v[s].length()) return count(s+1, e, p);

  char c = v[s][p];
  int i;
  for (i = s; i < e; i++) {
    if (v[i][p] != c) break;
  }
  return 1 + count(s, i, p+1) + count(i, e, p);
}

int main()
{
  string buf;
  getline(cin, buf);
  int n = stoi(buf);

  while (n--) {
    getline(cin, buf);
    v.push_back(buf);
  }

  sort(v.begin(), v.end());

  cout << count(0, v.size(), 0);
}
