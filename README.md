evel
=====

An Eventual Leader Election Library for Erlang

Memo
----

結果整合性を持つリーダ選出ライブラリ。

「ノードの追加・削除がない」状態が続けば、いつかは全てのメンバが共通のリーダに合意するようになる。

ノードの追加・削除時を除けば、クラスタサイズによらずスケーラブルにリーダ選出が可能な点も特徴。
(正確には、特定のリーダ選出に興味のないメンバはコストを払わなくて済む構造になっている)


遅いノードに影響を受けない(受けにくい):
- globalの場合には、例えばCtrl-Zで止めたノードがいた場合には延々と(ノードが切断されるまで？)ブロックする

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
