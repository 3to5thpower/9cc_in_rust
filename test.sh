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

assert 41 " 12 + 34 - 5;"
assert 47 '5+6*7;'
assert 15 '5*(9-6);'
assert 4 '(3 + 5)/2;'
assert 8 '(1 + 2) / 3 - - 3 + + 4;'
assert 1 '1 < 3;'
assert 0 '1 + 4 >= 1 + 3 * 4;'
assert 1 '1 <= 1 * 1;'
assert 1 '3 / 3 + 5 > (5 / 5);'
assert 4 'a = 1 + 1; b = a + 2; b;'
assert 5 'four = 4; four + 1;'
assert 10 'if(1) a = 13; return 10;'
assert 3 'if(0) a = 2; return a + 3;'
assert 3 'if(1 > 3) a = 4; else b = 3;return b;'
assert 4 'if(1 < 3) a = 4; else b = 3;return a;'
assert 3 'a = 0; while(a<3) a = a + 1; return a;t '
assert 10 'ten = 0; while(ten < 10) ten = ten + 1; return ten;'
assert 55 'a = 0; for(i = 0; i < 10; i = i + 1;) a = a + (i + 1); return a;'
assert 1 'i = 0; {i = i + 1;} return i;'
assert 55 'a = 0; for(i = 0; i < 10; i = i + 1;) {a = a + (i + 1);} return a;'
assert 100 'a=0;b=0;ans=0;for(i=0;i<10;i=i+1;){a=a+i+1;b=b+i;}ans=a+b;return ans;'

echo OK
