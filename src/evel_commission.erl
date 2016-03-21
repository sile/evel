%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
%%
%% @doc This process manages elections and records leaders in which local clients are interested
%%
%% @private
%% @end
-module(evel_commission).

-hebaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([elect/3]).
-export([dismiss/2]).
-export([find_leader/2]).
-export([known_leaders/0]).

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
          elections    :: ets:tid(),
          agents = #{} :: #{evel_agent:agent() => {evel:election_id(), evel_voter:vote()}}
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a process
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @see evel:elect/3
-spec elect(evel:election_id(), evel:candidate(), [evel:elect_option()]) -> evel:leader().
elect(ElectionId, Candidate, Options) ->
    case find_leader(ElectionId, Options) of
        {ok, Leader} -> Leader;
        error        ->
            ok = evel_agent:start_campaign(ElectionId, Candidate, Options),
            {ok, Leader} = find_leader(ElectionId, Options),
            Leader
    end.

%% @see evel:dismiss/2
-spec dismiss(evel:leader(), boolean()) -> ok.
dismiss({_, Agent}, Async) ->
    case Async of
        true ->
            _ = exit(Agent, kill),
            ok;
        false ->
            Voters =
                try
                    element(3, evel_agent:get_summary(Agent))
                catch
                    _:_ -> []
                end,
            _ = exit(Agent, kill),
            ok = lists:foreach(fun (Voter) -> evel_voter:cancel(Voter, Agent) end, Voters),
            gen_server:call(?MODULE, {cancel, Agent})
    end.

%% @see evel:find_leader/2
-spec find_leader(evel:election_id(), [evel:find_option()]) -> {ok, evel:leader()} | error.
find_leader(ElectionId, Options) ->
    case find_local(ElectionId) of
        {ok, Leader} -> {ok, Leader};
        error        ->
            ok = fetch_leader(ElectionId, Options),
            find_local(ElectionId)
    end.

%% @see evel:known_leaders/0
-spec known_leaders() -> [{evel:election_id(), evel:leader()}].
known_leaders() ->
    try
        ets:tab2list(?MODULE)
    catch
        _:_ -> []
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    Table = ets:new(?MODULE, [named_table, protected, {read_concurrency, true}]),
    State =
        #?STATE{
            elections = Table
           },
    {ok, State}.

%% @private
handle_call({record_leader, Arg}, _From, State) ->
    handle_record_leader(Arg, State);
handle_call({cancel, Arg}, _From, State) ->
    handle_cancel(Arg, State);
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
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
-spec find_local(evel:election_id()) -> {ok, evel:leader()} | error.
find_local(ElectionId) ->
    try ets:lookup(?MODULE, ElectionId) of
        []            -> error;
        [{_, Leader}] -> {ok, Leader}
    catch
        _:_ -> error
    end.

-spec collect_votes(evel:election_id(), [evel:find_option()]) -> [evel_voter:vote()].
collect_votes(ElectionId, Options) ->
    Timeout = proplists:get_value(timeout, Options, 100),
    VoterCount = proplists:get_value(voter_count, Options, 5),
    Voters = evel_people:inquire_voters(ElectionId, VoterCount, false),
    evel_voter:collect_votes(Voters, ElectionId, Timeout).

-spec fetch_leader(evel:election_id(), [evel:find_option()]) -> ok.
fetch_leader(ElectionId, Options) ->
    case lists:sort(collect_votes(ElectionId, Options)) of
        []                -> ok;
        [ElectedVote | _] -> gen_server:call(?MODULE, {record_leader, {ElectionId, ElectedVote}})
    end.

-spec handle_record_leader({evel:election_id(), evel_voter:vote()}, #?STATE{}) -> {reply, ok, #?STATE{}}.
handle_record_leader(Arg = {ElectionId, Vote}, State) ->
    case compete_with_present_vote(ElectionId, Vote, State) of
        {lose, Winner} ->
            _ = Winner =/= Vote andalso dismiss(evel_voter:vote_to_leader(Vote), true),
            {reply, ok, State};
        {win, Loser} ->
            ok = dismiss(evel_voter:vote_to_leader(Loser), true),
            handle_record_leader(Arg, remove_leader(evel_voter:get_agent(Loser), State));
        bye ->
            Agent = evel_voter:get_agent(Vote),
            Agents = maps:put(Agent, {ElectionId, Vote}, State#?STATE.agents),
            _ = monitor(process, Agent),
            _ = ets:insert(State#?STATE.elections, {ElectionId, evel_voter:vote_to_leader(Vote)}),
            {reply, ok, State#?STATE{agents = Agents}}
    end.

-spec compete_with_present_vote(evel:election_id(), evel_voter:vote(), #?STATE{}) ->
                                       bye | {win, evel_voter:vote()} | {lose, evel_voter:vote()}.
compete_with_present_vote(ElectionId, Vote, State) ->
    case ets:lookup(State#?STATE.elections, ElectionId) of
        []               -> bye;
        [{_, Contender}] ->
            {_, ContendVote} = maps:get(evel_voter:get_agent(Contender), State#?STATE.agents),
            case ContendVote =< Vote of
                true  -> {lose, ContendVote};
                false -> {win, ContendVote}
            end
    end.

-spec handle_cancel(evel_agent:agent(), #?STATE{}) -> {reply, ok, #?STATE{}}.
handle_cancel(Agent, State) ->
    {reply, ok, remove_leader(Agent, State)}.

-spec handle_down(evel_agent:agent(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Agent, State) ->
    {noreply, remove_leader(Agent, State)}.

-spec remove_leader(evel_agent:agent(), #?STATE{}) -> #?STATE{}.
remove_leader(Agent, State) ->
    case maps:find(Agent, State#?STATE.agents) of
        error                 -> State;
        {ok, {ElectionId, _}} ->
            Agents = maps:remove(Agent, State#?STATE.agents),
            _ = ets:delete(State#?STATE.elections, ElectionId),
            State#?STATE{agents = Agents}
    end.
