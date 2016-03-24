evel
=====

`evel` is a distributed leader election library which has eventual consistency.

Memo
----

結果整合性を持つリーダ選出ライブラリ。

「ノードの追加・削除がない」状態が続けば、いつかは全てのメンバが共通のリーダに合意するようになる。

ノードの追加・削除時を除けば、クラスタサイズによらずスケーラブルにリーダ選出が可能な点も特徴。
(正確には、特定のリーダ選出に興味のないメンバはコストを払わなくて済む構造になっている)
(evelが起動していないノードがあっても大丈夫)

遅いノードに影響を受けない(受けにくい):
- globalの場合には、例えばCtrl-Zで止めたノードがいた場合には延々と(ノードが切断されるまで？)ブロックする

TODO: globalとの比較
- `rpc:multicall(global, register_name, [foo, self()]).`

Definition
----------

- Termination: TODO
- Uniqueness: TODO
- Agreement: TODO

Build
-----

To build the library for stand-alone usage:
```sh
$ git clone https://github.com/sile/evel.git
$ cd evel
$ ./rebar3 compile
$ ./rebar3 shell
$ > evel:module_info().
```

If you want to use from your application:
```erlang
%% In your 'rebar.config'

%% Add following lines
{deps,
 [
   evel
 ]}.
```

Example
-------

```erlang
$ ./rebar3 shell --sname master

%%
%% Starts 10 slave nodes
%%
> ok = evel_debug:slave_start_link_n(10).
> nodes().
['1@localhost','2@localhost','3@localhost','4@localhost',
'5@localhost','6@localhost','7@localhost','8@localhost',
'9@localhost','10@localhost']

%%
%% Elects the leader from among following candidates
%%
> Candidates = [spawn(timer, sleep, [infinity]) || _ <- lists:seq(1, 100)].
> rpc:parallel_eval([{evel, elect, [foo, Candidate]} || Candidate <- Candidates]).

%%
%% Finds the elected leader
%%
> {ok, Leader} = evel:find_leader(foo).
> {Results0, []} = rpc:multicall(evel, find_leader, [foo]).
> lists:foreach(fun (R) -> {ok, Leader} = R end, Results0). % All nodes agree with the single leader

%%
%% Dismisses the leader
%%
> ok = evel:dismiss(Leader).
> error = evel:find_leader(foo).
> {Results1, []} = rpc:multicall(evel, find_leader, [foo]).
> lists:foreach(fun (R) -> error = R end, Results1).
```

API
---

See [EDoc Documents](doc/README.md)

License
-------

This library is released under the MIT License.
See the [LICENSE](LICENSE) file for full license information.
