# 9cc_in_rust

### 概要
趣味で作っているx64のCコンパイラです。

[低レイヤを知りたい人のためのCコンパイラ作成入門](https://www.sigbus.info/compilerbook)を参考に、Rustで書き換えながら書いています。

コンパイラ自作は実は初です。

### テスト
プロジェクトのルートディレクトリで

`cargo make --makefile make.toml flow`

を実行するとテストできます。
