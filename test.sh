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

assert 41 "main(){return 12 + 34 - 5;}"
assert 47 'main(){return 5+6*7;}'
assert 15 'main(){return 5*(9-6);}'
assert 4 'main(){return (3 + 5)/2;}'
assert 8 'main(){return(1 + 2) / 3 - - 3 + + 4;}'
assert 1 'main(){return 1 < 3;}'
assert 0 'main(){return 1 + 4 >= 1 + 3 * 4;}'
assert 1 'main(){return 1 <= 1 * 1;}'
assert 1 'main(){return 3 / 3 + 5 > (5 / 5);}'
assert 4 'main(){a = 1 + 1; b = a + 2; return b;}'
assert 5 'main(){four = 4; return four + 1;}'
assert 10 'main(){if(1) a = 13; return 10;}'
assert 3 'main(){if(0) a = 2; return a + 3;}'
assert 3 'main(){if(1 > 3) a = 4; else b = 3;return b;}'
assert 4 'main(){if(1 < 3) a = 4; else b = 3;return a;}'
assert 3 'main(){a = 0; while(a<3) a = a + 1; return a;}'
assert 10 'main(){ten = 0; while(ten < 10) ten = ten + 1; return ten;}'
assert 55 'main(){a = 0; for(i = 0; i < 10; i = i + 1) a = a + (i + 1); return a;}'
assert 1 'main(){i = 0; {i = i + 1;} return i;}'
assert 55 'main(){a = 0; for(i = 0; i < 10; i = i + 1) {a = a + (i + 1);} return a;}'
assert 100 '
main(){
  a = 0;
  b = 0;
  ans = 0;
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
func(){
  return 50;
}
main(){
  return func();
}
'
assert 50 '
func() {
    a = 50;
    return a;
}
main(){
    a = func();
    return a;
}
'

echo OK
