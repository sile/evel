evel
=====

An Eventual Leader Election Library for Erlang

Memo
----

結果整合性を持つリーダ選出ライブラリ。

「ノードの追加・削除がない」状態が続けば、いつかは全てのメンバが共通のリーダに合意するようになる。

ノードの追加・削除時を除けば、クラスタサイズによらずスケーラブルにリーダ選出が可能な点も特徴。
(正確には、特定にリーダ選出に興味のないメンバはコストを払わなくて済む構造になっている)

Definition
----------

- Termination: TODO
- Uniqueness: TODO
- Agreement: TODO

Build
-----

```sh
$ rebar3 compile
```

Example
-------

```erlang
$ make start
> Leader = evel:elect(foo, self()).
> {ok, Leader} = evel:find_leader(foo).
> evel:dismiss(Leader).
> error = evel:find_leader(foo).
```

TODO
----

- ノード追加・削除時のコストを下げる
  - 現在は`hash_ring_static`へのノード追加・削除が重いので`hash_ring`側に別のより構成変更に強い実装モジュールを備える形にする
