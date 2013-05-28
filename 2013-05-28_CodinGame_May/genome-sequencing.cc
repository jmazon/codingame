#include <iostream>
#include <list>

using namespace std;

static list<string> superpose(string s1, string s2)
{
  int l1 = s1.length(), l2 = s2.length();
  list<string> r;

  if (l1 == 0) {
    r.push_back(s2);
    return r;
  }
  
  for (int offset = -l2; offset <= l1; offset++) {
    string s;
    for (int i = min(0,offset); i < max(l1,l2+offset); i++) {
      bool v1 = (i >= 0 && i < l1);
      bool v2 = (i >= offset && i < l2+offset);
      if (v1 && v2) {
        if (s1[i] == s2[i-offset]) s.push_back(s1[i]);
        else goto fail;
      }
      else if (v1) s.push_back(s1[i]);
      else if (v2) s.push_back(s2[i-offset]);
    }
    r.push_back(s);
  fail: ;
  }

  return r;
}

static int solve(string s, list<string> v)
{
  if (v.empty()) return s.length();
  int m = 60;
  for (auto i : v) {
    list<string> v2(v); v2.remove(i);
    list<string> s2 = superpose(s, i);
    for (auto j : s2) {
      int n = solve(j, v2);
      m = min(m, n);
    }
  }
  return m;
}

int main()
{
  int n; cin >> n;
  list<string> v;
  while (n--) { string s; cin >> s; v.push_back(s); }
  cout << solve("", v);
}
  
