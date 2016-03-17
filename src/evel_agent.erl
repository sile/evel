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
-export([start_campaign/2]).

-export_type([start_arg/0]).
-export_type([agent/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
%% FIXME: To be specified from the outside
-define(CHECK_VOTER_UP_INTERVAL, 1000).
-define(DECIDE_PRIORITY, erlang:system_time(micro_seconds)). % rand:uniform()).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          election_id            :: evel:election_id(),
          candidate              :: evel:candidate(),
          vote                   :: evel_voter:vote(),
          voters = ordsets:new() :: ordsets:ordset(evel_voter:voter()),
          monitors = #{}         :: #{reference() => evel_voter:voter()}
        }).

-type start_arg() :: {evel:election_id(), evel:candidate()}.
-type agent() :: evel:certificate().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts an agent process
-spec start_link(start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%% @doc Starts an election campaign for `Candidate'
-spec start_campaign(evel:election_id(), evel:candidate()) -> ok.
start_campaign(ElectionId, Candidate) ->
    {ok, _} = evel_agent_sup:start_child({ElectionId, Candidate}),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init({ElectionId, Candidate}) ->
    _ = monitor(process, Candidate),
    State0 =
        #?STATE{
            election_id = ElectionId,
            candidate   = Candidate,
            vote        = {?DECIDE_PRIORITY, Candidate, self()}
           },
    State1 = do_campaign(State0),
    {ok, State1}.

%% @private
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info({'DOWN', _, _, Pid, Reason}, State = #?STATE{candidate = Pid}) ->
    {stop, {shutdown, {candidate_exited, Pid, Reason}}, State};
handle_info({'DOWN', Ref, _, _, _}, State) ->
    handle_voter_down(Ref, State);
handle_info({'PERSON_LEAVE', Voter}, State) ->
    case ordsets:is_element(Voter, State#?STATE.voters) of
        false -> {noreply, State};
        true  -> handle_population_change(State)
    end;
handle_info({'PERSON_JOIN', _}, State) ->
    handle_population_change(State);
handle_info({check_voter_up, Arg}, State) ->
    handle_check_voter_up(Arg, State);
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

-spec handle_voter_down(reference(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_voter_down(Ref, State) ->
    Voter = maps:get(Ref, State#?STATE.monitors),
    Monitors = maps:remove(Ref, State#?STATE.monitors),
    ok = schedule_check_voter_up(Voter),
    {noreply, State#?STATE{monitors = Monitors}}.

-spec handle_check_voter_up(evel_voter:voter(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_check_voter_up(Voter, State) ->
    case ordsets:is_element(Voter, State#?STATE.voters) of
        false -> {noreply, State}; % `Voter' have left
        true  ->
            Monitors = solicit_vote(Voter, State#?STATE.monitors, State),
            {noreply, State#?STATE{monitors = Monitors}}
    end.

-spec do_campaign(#?STATE{}) -> #?STATE{}.
do_campaign(State) ->
    Voters = ordsets:from_list(evel_people:inquire_voters(State#?STATE.election_id, true)),

    Joins = ordsets:subtract(Voters, State#?STATE.voters),
    Monitors0 =
        ordsets:fold(
          fun (Voter, Acc) -> solicit_vote(Voter, Acc, State) end,
          State#?STATE.monitors,
          Joins),

    Leaves = ordsets:subtract(State#?STATE.voters, Voters),
    Monitors1 =
        maps:filter(
          fun (Ref, Voter) ->
                  case ordsets:is_element(Voter, Leaves) of
                      false -> true;
                      true  ->
                          _ = demonitor(Ref, [flush]),
                          false
                  end
          end,
          Monitors0),

    State#?STATE{voters = Voters, monitors = Monitors1}.

-spec schedule_check_voter_up(evel_voter:voter()) -> ok.
schedule_check_voter_up(Voter) ->
    _ = erlang:send_after(?CHECK_VOTER_UP_INTERVAL, self(), {check_voter_up, Voter}),
    ok.

-spec solicit_vote(evel_voter:voter(), Monitors, #?STATE{}) -> Monitors when
      Monitors :: #{reference() => evel_voter:voter()}.
solicit_vote(Voter, Monitors, State) ->
    Monitor = monitor(process, evel_voter:global_name(Voter)),
    ok = evel_voter:solicit(Voter, State#?STATE.election_id, State#?STATE.vote),
    maps:put(Monitor, Voter, Monitors).
