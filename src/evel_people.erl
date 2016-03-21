%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
%%
%% @doc This process manages the list of potential voters
%%
%% @private
%% @end
-module(evel_people).

-hebaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([inquire_voters/3]).

-export_type([notification/0]).

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
          people         :: hash_ring:ring(evel_voter:voter()),
          listeners = [] :: [pid()]
        }).

-type notification() :: {'PERSON_JOIN', PotentialVoter::evel_voter:voter()}
                      | {'PERSON_LEAVE', PotentialVoter::evel_voter:voter()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a process
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Inquires voters who are concerned with the election
%%
%% If `DoMonitor' is `true', the caller will receive `notification()' messages when the population has changed.
-spec inquire_voters(evel:election_id(), pos_integer(), boolean()) -> [evel_voter:voter()].
inquire_voters(ElectionId, VoterCount, DoMonitor) ->
    gen_server:call(?MODULE, {inquire_voters, {ElectionId, self(), VoterCount, DoMonitor}}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    ok = net_kernel:monitor_nodes(true),
    {ok, RingOptions} = application:get_env(hash_ring_options),
    Voters = nodes([this, visible]),
    People = hash_ring:make(hash_ring:list_to_nodes(Voters), RingOptions),
    State =
        #?STATE{
            people = People
           },
    {ok, State}.

%% @private
handle_call({inquire_voters, Arg}, _From, State) ->
    handle_inquire_voters(Arg, State);
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info({nodeup, Node}, State) ->
    handle_nodeup(Node, State);
handle_info({nodedown, Node}, State) ->
    handle_nodedown(Node, State);
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
-spec handle_inquire_voters({evel:election_id(), pid(), pos_integer(), boolean()}, #?STATE{}) ->
                                   {reply, [evel_voter:voter()], #?STATE{}}.
handle_inquire_voters({ElectionId, From, VoterCount, DoMonitor}, State) ->
    Voters =
        lists:map(
          fun hash_ring_node:get_key/1,
          hash_ring:collect_nodes(ElectionId, VoterCount, State#?STATE.people)),
    Listeners =
        case DoMonitor of
            false -> State#?STATE.listeners;
            true  ->
                _ = monitor(process, From),
                [From | State#?STATE.listeners]
        end,
    {reply, Voters, State#?STATE{listeners = Listeners}}.

-spec handle_down(pid(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Listener, State) ->
    Listeners = lists:delete(Listener, State#?STATE.listeners),
    {noreply, State#?STATE{listeners = Listeners}}.

-spec handle_nodeup(node(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_nodeup(Node, State) ->
    People = hash_ring:add_node(hash_ring_node:make(Node), State#?STATE.people),
    ok = notify_change({'PERSON_JOIN', Node}, State),
    {noreply, State#?STATE{people = People}}.

-spec handle_nodedown(node(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_nodedown(Node, State) ->
    People = hash_ring:remove_node(Node, State#?STATE.people),
    ok = notify_change({'PERSON_LEAVE', Node}, State),
    {noreply, State#?STATE{people = People}}.

-spec notify_change(notification(), #?STATE{}) -> ok.
notify_change(Message, State) ->
    lists:foreach(
      fun (Pid) -> Pid ! Message end,
      State#?STATE.listeners).
