%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
%%
%% @doc An Eventual Leader Election Library
%% @end
-module(evel).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([elect/2]).
-export([dismiss/1]).
-export([find_leader/1]).
-export([known_leaders/0]).
-export([is_leader/1]).
-export([get_winner/1]).
-export([get_certificate/1]).

-export_type([election_id/0]).
-export_type([candidate/0]).
-export_type([leader/0]).
-export_type([winner/0]).
-export_type([certificate/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type election_id() :: term().
-type candidate()   :: pid().
-type winner()      :: candidate().
-type certificate() :: pid().
-type leader()      :: {winner(), certificate()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec elect(election_id(), candidate()) -> leader().
elect(ElectionId, Candidate) ->
    _ = is_pid(Candidate) orelse error(badarg, [ElectionId, Candidate]),
    evel_commission:elect(ElectionId, Candidate).

-spec dismiss(leader()) -> ok.
dismiss(Leader) ->
    _ = is_leader(Leader) orelse error(badarg, [Leader]),
    evel_commission:dismiss(Leader).

-spec find_leader(election_id()) -> {ok, leader()} | error.
find_leader(ElectionId) ->
    evel_commission:find_leader(ElectionId).

-spec known_leaders() -> [{election_id(), leader()}].
known_leaders() ->
    error(unimplemented, []).

-spec get_winner(leader()) -> winner().
get_winner(Leader) ->
    _ = is_leader(Leader) orelse error(badarg, [Leader]),
    element(1, Leader).

-spec get_certificate(leader()) -> certificate().
get_certificate(Leader) ->
    _ = is_leader(Leader) orelse error(badarg, [leader]),
    element(2, Leader).

-spec is_leader(leader()) -> boolean().
is_leader({Winner, Certificate}) -> is_pid(Winner) andalso is_pid(Certificate);
is_leader(_)                     -> false.
