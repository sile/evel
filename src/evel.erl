-module(evel).

-export([elect/2]).
-export([dismiss/1]).
-export([find_leader/1]).
%% get_pid (?)
-export([get_certificate/1]).
%% link/1, monitor/1

-export_type([election_id/0]).
-export_type([candidate/0]).
-export_type([leader/0]).
-export_type([certificate/0]).

-type election_id() :: term().
-type candidate()   :: pid().
-type leader()      :: {candidate(), certificate()}.
-type certificate() :: pid().

-spec elect(election_id(), candidate()) -> leader().
elect(ElectionId, Candidate) ->
    _ = is_pid(Candidate) orelse error(badarg, [ElectionId, Candidate]),
    evel_commission:elect(ElectionId, Candidate).

-spec dismiss(leader()) -> ok.
dismiss(Leader) ->
    evel_commission:dismiss(Leader).

-spec find_leader(election_id()) -> {ok, leader()} | error.
find_leader(ElectionId) ->
    evel_commission:find_leader(ElectionId).

-spec get_certificate(leader()) -> certificate().
get_certificate({_, Certificate}) ->
    Certificate.
