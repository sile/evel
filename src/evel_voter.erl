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
-export([global_name/1]).
-export([solicit/3]).
-export([request_vote/3]).

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

-type voter() :: node().
-type vote() :: {priority(), evel:candidate(), evel:agent()}.
-type priority() :: float().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a voter process
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns the globally accessible name of `Voter'
-spec global_name(voter()) -> {?MODULE, voter()}.
global_name(Voter) ->
    {?MODULE, Voter}.

%% @doc Solicits `Vote' from `Voter'
-spec solicit(voter(), evel:election_id(), vote()) -> ok.
solicit(Voter, ElectionId, Vote) ->
    gen_server:cast(global_name(Voter), {solicit, {ElectionId, Vote}}).

%% @doc Requests a vote on the election
%%
%% If `Voter' votes, the caller will receive `{Tag, vote()}' message.
%% Otherwise no messages will be delivered.
-spec request_vote(voter(), evel:election_id(), reference()) -> ok.
request_vote(Voter, ElectionId, Tag) ->
    gen_server:cast({?MODULE, Voter}, {request_vote, {self(), ElectionId, Tag}}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    State = #?STATE{},
    {ok, State}.

%% @private
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast({solicit, Arg}, State) ->
    handle_solicit(Arg, State);
handle_cast({request_vote, Arg}, State) ->
    handle_request_vote(Arg, State);
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
            ok = reject(SolicitedVote),
            {noreply, State};
        {ok, CurrentVote} ->
            ok = reject(CurrentVote),
            handle_solicit({ElectionId, SolicitedVote}, remove_vote(get_agent(CurrentVote), State))
    end.

-spec handle_down(evel_agent:agent(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Agent, State0) ->
    State1 = remove_vote(Agent, State0),
    {noreply, State1}.

-spec handle_request_vote({pid(), evel:election_id(), reference()}, #?STATE{}) -> {noreply, #?STATE{}}.
handle_request_vote({Venue, ElectionId, Tag}, State) ->
    ok = case maps:find(ElectionId, State#?STATE.votes) of
             error      -> ok;
             {ok, Vote} -> vote(Venue, Tag, Vote)
         end,
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

-spec reject(vote()) -> ok.
reject({_, Candidate, Agent}) ->
    evel:dismiss({Candidate, Agent}).

-spec get_agent(vote()) -> evel_agent:agent().
get_agent({_, _, Agent}) ->
    Agent.

-spec vote(pid(), reference(), vote()) -> ok.
vote(Venue, Tag, Vote) ->
    _ = Venue ! {Tag, Vote},
    ok.
