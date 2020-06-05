#!/usr/bin/env bash
assert() {
  expected="$1"
  input="$2"
  cargo run "$input" > tmp.s
  cc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert_link() {
  expected="$1"
  input="$2"
  link_src="$3"
  cargo run "$input" > tmp.s
  echo "$link_src" > tmp.c
  cc -o tmp tmp.s tmp.c
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 41 "int main(){return 12 + 34 - 5;}"
assert 47 'int main(){return 5+6*7;}'
assert 15 'int main(){return 5*(9-6);}'
assert 4 'int main(){return (3 + 5)/2;}'
assert 8 'int main(){return(1 + 2) / 3 - - 3 + + 4;}'
assert 1 'int main(){return 1 < 3;}'
assert 0 'int main(){return 1 + 4 >= 1 + 3 * 4;}'
assert 1 'int main(){return 1 <= 1 * 1;}'
assert 1 'int main(){return 3 / 3 + 5 > (5 / 5);}'
assert 1 'int main(){int a; a = 1; return a;}'
assert 1 'int main(){int a=1;return a;}'
assert 1 'int main(){int a=1; int b=1;return b;}'
assert 4 'int main(){int a = 1 + 1;int b = a + 2; return b;}'
assert 6 'int main(){int a=2;int b=a+2;int c=b+2;return c;}'
assert 10 'int main(){int a=2;int b=a+2;int c=b+2;int d=c+2;int e=d+2;return e;}'
assert 5 'int main(){int four = 4; return four + 1;}'
assert 10 'int main(){if(1) int a = 13; return 10;}'
assert 3 'int main(){if(0) int a = 2; return a + 3;}'
assert 3 'int main(){if(1 > 3) int a = 4; else int b = 3;return b;}'
assert 4 'int main(){if(1 < 3) int a = 4; else int b = 3;return a;}'
assert 3 'int main(){int a = 0; while(a<3) a = a + 1; return a;}'
assert 10 'int main(){int ten = 0; while(ten < 10) ten = ten + 1; return ten;}'
assert 55 'int main(){int a = 0;int i; for(i = 0; i < 10; i = i + 1) a = a + (i + 1); return a;}'
assert 1 'int main(){int i = 0; {i = i + 1;} return i;}'
assert 55 'int main(){int a = 0; int i; for(i = 0; i < 10; i = i + 1) {a = a + (i + 1);} return a;}'
assert 100 '
int main(){
  int a = 0;
  int b = 0;
  int ans = 0;
  int i;
  for(i = 0; i < 10; i = i + 1) {
    a = a + (i + 1);
    b = b + i;
  }
  ans = a + b;
  return ans;
}'

#assert_link 5 'main(){ return foo(2, 3);}' '
#int foo(int x, int y){
#  printf("%d, %d ok\n", x, y);
#  return x+y;
#}'

assert 50 '
int func(){
  return 50;
}
int main(){
  return func();
}
'
assert 50 '
int func() {
    int a = 50;
    return a;
}
int main(){
    int a = func();
    return a;
}
'
assert 3 'int fun(int x){return x;}int main(){return fun(3);}'
assert 4 'int fun(int x, int y){return x + y;}int main(){return fun(2, 2);}'


assert 100 '
int sum(int x) {
  if(x < 1) return 0;
  return 1 + sum(x - 1);
}
int main() {
  int a = sum(100);
  return a;
}
'
assert 1 '
int sum(int x) {
  if(x < 1) return 0;
  return x + sum(x - 1);
}
int main() {
  int res = sum(100);
  int cmp = 5050;
  if(res == cmp) return 1;
  else return 0;
}
'

assert 120 '
int fact(int x) {
  if(x < 2) return 1;
  return x * fact(x - 1);
}
int main() {
    return fact(5);
}
'
assert 1 '
int fib(int n) {
  if(n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}
int main() {
  int res = fib(20);
  int cmp = 6765;
  return res == cmp;
}
'
assert 3 'int main(){int x=3;int y=5;int z=&y+8;return *z;}'

assert 0 'int main(){int x = 0; {int x = 1;} return x;}'

assert 5 'int main(){int x=3;int*y=&x;x=x+2;return *y;}'

#assert 0 '
#int main() {
#    int x = 3;
#    int refx = &x;
#    *refx = *refx - 3;
#    return x;
#}
#'


echo OK
