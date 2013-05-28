#include <iostream>
#include <queue>

using namespace std;

int main()
{
  int r, l;
  cin >> r >> l;

  queue<int> a;
  a.push(r);
  
  while (--l) {
    queue<int> b;
    while (! a.empty() ) {
      int h = a.front();
      int n = 0;
      while (a.front() == h) {
        n++;
        a.pop();
      }
      b.push(n);
      b.push(h);
    }
    a = b;
  }
  cout << a.front();
  a.pop();
  while (! a.empty()) {
    cout << ' ' << a.front();
    a.pop();
  }
  cout << endl;
}
