%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
%%
%% @doc A voter process
%%
%% @private
%% @end
-module(evel_voter).

-hebaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([solicit/3]).
-export([collect_votes/3]).
-export([vote_to_leader/1]).
-export([get_agent/1]).
-export([get_votes/0]).
-export([self_voter/0]).

-export_type([voter/0]).
-export_type([vote/0]).
-export_type([priority/0]).

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
          votes  = #{} :: #{evel:election_id() => vote()},
          agents = #{} :: #{evel_agent:agent() => evel:election_id()}
        }).

-type voter() :: pid().
-type vote() :: {priority(), evel:candidate(), evel_agent:agent()}.
-type priority() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a voter process
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Solicits `Vote' from `Voter'
-spec solicit(voter(), evel:election_id(), vote()) -> ok.
solicit(Voter, ElectionId, Vote) ->
    gen_server:cast(Voter, {solicit, {ElectionId, Vote}}).

%% @doc Collects the votes of `Voters'
-spec collect_votes([voter()], evel:election_id(), timeout()) -> [vote()].
collect_votes(Voters, ElectionId, Timeout) ->
    {_, Monitor} =
        spawn_monitor(
          fun () ->
                  _ = is_integer(Timeout) andalso erlang:send_after(Timeout, self(), timeout),
                  Tag = make_ref(),
                  ok = lists:foreach(
                         fun (Voter) ->
                                 _ = monitor(process, Voter),
                                 gen_server:cast(Voter, {vote, {self(), Tag, ElectionId}})
                         end,
                         Voters),
                  (fun Loop (0, Acc) -> exit({ok, Acc});
                       Loop (N, Acc) ->
                           receive
                               {'DOWN', _, _, _, _} -> Loop(N - 1, Acc);
                               {Tag, no_vote}       -> Loop(N - 1, Acc);
                               {Tag, Vote}          -> Loop(N - 1, [Vote | Acc]);
                               timeout              -> exit({ok, Acc})
                           end
                   end)(length(Voters), [])
          end),
    receive
        {'DOWN', Monitor, _, _, Result} ->
            case Result of
                {ok, Votes} -> Votes;
                _           -> []
            end
    end.

%% @doc Converts `vote()' to `evel:leader()'
-spec vote_to_leader(vote()) -> evel:leader().
vote_to_leader({_, Candidate, Agent}) ->
    {Candidate, Agent}.

%% @doc Gets the agent of `Vote'
-spec get_agent(Vote :: vote()) -> evel_agent:agent().
get_agent({_, _, Agent}) ->
    Agent.

%% @doc Gets the current votes
-spec get_votes() -> [{evel:election_id(), vote()}].
get_votes() ->
    gen_server:call(?MODULE, get_votes).

%% @doc Gets the voter on the current node
-spec self_voter() -> voter().
self_voter() ->
    Pid = whereis(?MODULE),
    true = is_pid(Pid),
    Pid.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    State = #?STATE{},
    {ok, State}.

%% @private
handle_call(get_votes, _From, State) ->
    {reply, maps:to_list(State#?STATE.votes), State};
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast({vote, Arg}, State) ->
    handle_vote(Arg, State);
handle_cast({solicit, Arg}, State) ->
    handle_solicit(Arg, State);
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info({'DOWN', _, _, Pid, _}, State) ->
    handle_down(Pid, State);
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
-spec handle_solicit({evel:election_id(), vote()}, #?STATE{}) -> {noreply, #?STATE{}}.
handle_solicit({ElectionId, SolicitedVote}, State) ->
    case maps:find(ElectionId, State#?STATE.votes) of
        error ->
            Agent = get_agent(SolicitedVote),
            _ = monitor(process, Agent),
            Votes = maps:put(ElectionId, SolicitedVote, State#?STATE.votes),
            Agents = maps:put(Agent, ElectionId, State#?STATE.agents),
            {noreply, State#?STATE{votes = Votes, agents = Agents}};
        {ok, SolicitedVote} ->
            {noreply, State};
        {ok, CurrentVote} when CurrentVote < SolicitedVote ->
            ok = evel:dismiss(vote_to_leader(SolicitedVote)),
            {noreply, State};
        {ok, CurrentVote} ->
            ok = evel:dismiss(vote_to_leader(CurrentVote)),
            handle_solicit({ElectionId, SolicitedVote}, remove_vote(get_agent(CurrentVote), State))
    end.

-spec handle_down(evel_agent:agent(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Agent, State0) ->
    State1 = remove_vote(Agent, State0),
    {noreply, State1}.

-spec handle_vote({pid(), reference(), evel:election_id()}, #?STATE{}) -> {noreply, #?STATE{}}.
handle_vote({Sender, Tag, ElectionId}, State) ->
    Vote = maps:get(ElectionId, State#?STATE.votes, no_vote),
    _ = Sender ! {Tag, Vote},
    {noreply, State}.

-spec remove_vote(evel_agent:agent(), #?STATE{}) -> #?STATE{}.
remove_vote(Agent, State) ->
    case maps:find(Agent, State#?STATE.agents) of
        error            -> State;
        {ok, ElectionId} ->
            Votes = maps:remove(ElectionId, State#?STATE.votes),
            Agents = maps:remove(Agent, State#?STATE.agents),
            State#?STATE{votes = Votes, agents = Agents}
    end.
