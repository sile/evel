%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
%%
%% @doc An agent process which handles daily affairs for the corresponding candidate
%%
%% The liveness of an agent process indicates the legitimacy of the leader.
%% (i.e., If an agent process exits, the corresponding candidate is no longer the leader)
%%
%% @private
%% @end
-module(evel_agent).

-hebaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).
-export([start_campaign/3]).
-export([unlink_candidate/1]).
-export([get_summary/1]).

-export_type([start_arg/0]).
-export_type([agent/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).
-record(?STATE,
        {
          election_id            :: evel:election_id(),
          candidate              :: evel:candidate(),
          max_voter_count        :: pos_integer(),
          vote                   :: evel_voter:vote(),
          voters = ordsets:new() :: ordsets:ordset(evel_voter:voter()),
          monitors = #{}         :: monitors()
        }).

-type start_arg() :: {evel:election_id(), evel:candidate(), [evel:elect_option()]}.
-type agent() :: evel:certificate().
-type monitors() :: #{evel_voter:voter() => reference()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts an agent process
-spec start_link(start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%% @doc Starts an election campaign for `Candidate'
-spec start_campaign(evel:election_id(), evel:candidate(), [evel:elect_option()]) -> ok.
start_campaign(ElectionId, Candidate, Options) ->
    _ = evel_agent_sup:start_child({ElectionId, Candidate, Options}),
    ok.

%% @doc Removes the link between `Agent' and the corresponding candidate process
-spec unlink_candidate(agent()) -> ok.
unlink_candidate(Agent) ->
    gen_server:call(Agent, unlink_candidate).

%% @doc Gets the summary of `Agent'
-spec get_summary(agent()) -> {evel:election_id(), evel_voter:vote(), [evel_voter:voter()]}.
get_summary(Agent) ->
    gen_server:call(Agent, get_summary).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init({ElectionId, Candidate, Options}) ->
    Priority = proplists:get_value(priority, Options, erlang:system_time(micro_seconds)),
    VoterCount = proplists:get_value(voter_count, Options, 5),
    _ = case proplists:get_value(link, Options, true) of
            true  -> link(Candidate);
            false -> true
        end,
    _ = monitor(process, Candidate),
    State0 =
        #?STATE{
            election_id     = ElectionId,
            candidate       = Candidate,
            vote            = {Priority, Candidate, self()},
            max_voter_count = VoterCount
           },
    State1 = do_campaign(State0),
    {ok, State1}.

%% @private
handle_call(unlink_candidate, _From, State) ->
    _ = unlink(State#?STATE.candidate),
    {reply, ok, State};
handle_call(get_summary, _From, State) ->
    Summary = {State#?STATE.election_id, State#?STATE.vote, ordsets:to_list(State#?STATE.voters)},
    {reply, Summary, State};
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info({'DOWN', _, _, Pid, Reason}, State = #?STATE{candidate = Pid}) ->
    {stop, {shutdown, {candidate_exited, Pid, Reason}}, State};
handle_info({'DOWN', _, _, Pid, _}, State) ->
    handle_voter_down(Pid, State);
handle_info({'PERSON_LEAVE', Voter}, State) ->
    case ordsets:is_element(Voter, State#?STATE.voters) of
        false -> {noreply, State};
        true  -> handle_population_change(State)
    end;
handle_info({'PERSON_JOIN', _}, State) ->
    handle_population_change(State);
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OlsVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handle_population_change(#?STATE{}) -> {noreply, #?STATE{}}.
handle_population_change(State0) ->
    State1 = do_campaign(State0),
    {noreply, State1}.

-spec handle_voter_down(evel_voter:voter(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_voter_down(Voter, State) ->
    Voters = ordsets:del_element(Voter, State#?STATE.voters),
    Monitors = maps:remove(Voter, State#?STATE.monitors),
    {noreply, State#?STATE{monitors = Monitors, voters = Voters}}.

-spec do_campaign(#?STATE{}) -> #?STATE{}.
do_campaign(State) ->
    Voters = ordsets:from_list(
               evel_people:inquire_voters(State#?STATE.election_id, State#?STATE.max_voter_count, true)),

    Joins = ordsets:subtract(Voters, State#?STATE.voters),
    Monitors0 =
        ordsets:fold(
          fun (Voter, Acc) -> solicit_vote(Voter, Acc, State) end,
          State#?STATE.monitors,
          Joins),

    Leaves = ordsets:subtract(State#?STATE.voters, Voters),
    Monitors1 = ordsets:fold(fun cancel_vote/2, Monitors0, Leaves),

    State#?STATE{voters = Voters, monitors = Monitors1}.

-spec solicit_vote(evel_voter:voter(), monitors(), #?STATE{}) -> monitors().
solicit_vote(Voter, Monitors, State) ->
    Monitor = monitor(process, Voter),
    ok = evel_voter:solicit(Voter, State#?STATE.election_id, State#?STATE.vote),
    maps:put(Voter, Monitor, Monitors).

-spec cancel_vote(evel_voter:voter(), monitors()) -> monitors().
cancel_vote(Voter, Monitors) ->
    Monitor = maps:get(Voter, Monitors),
    _ = demonitor(Monitor, [flush]),
    ok = evel_voter:cancel(Voter, self()),
    maps:remove(Voter, Monitors).
