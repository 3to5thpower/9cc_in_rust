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

echo OK
