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
-export([join/2]).
-export([join_ack/2]).
-export([inquire_voters/3]).
-export([get_people/0]).

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

%% @doc Gets the known people list
-spec get_people() -> [evel_voter:voter()].
get_people() ->
    gen_server:call(?MODULE, get_people).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    Voter = evel_voter:self_voter(),
    ok = lists:foreach(fun (Node) -> join(Node, Voter) end, nodes()),

    ok = net_kernel:monitor_nodes(true),
    {ok, RingOptions} = application:get_env(hash_ring_options),
    People = hash_ring:make([hash_ring_node:make(Voter)], RingOptions),
    State =
        #?STATE{
            people = People
           },
    {ok, State}.

%% @private
handle_call({inquire_voters, Arg}, _From, State) ->
    handle_inquire_voters(Arg, State);
handle_call(get_people, _From, State) ->
    People = lists:map(fun hash_ring_node:get_key/1, hash_ring:get_node_list(State#?STATE.people)),
    {reply, People, State};
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast({join, Arg}, State) ->
    handle_join(Arg, State);
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info({nodeup, Node}, State) ->
    handle_nodeup(Node, State);
handle_info({nodedown, _}, State) ->
    {noreply, State};
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

-spec handle_join({pid(), evel_voter:voter(), boolean()}, #?STATE{}) -> {noreply, #?STATE{}}.
handle_join({Sender, Voter, NeedAck}, State) ->
    case maps:is_key(Voter, hash_ring:get_nodes(State#?STATE.people)) of
        true  -> {noreply, State};
        false ->
            _ = monitor(process, Voter),
            People = hash_ring:add_node(hash_ring_node:make(Voter), State#?STATE.people),
            ok = notify_change({'PERSON_JOIN', Voter}, State),
            _ = NeedAck andalso join_ack(Sender, evel_voter:self_voter()),
            {noreply, State#?STATE{people = People}}
    end.

-spec handle_down(pid(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Pid, State) ->
    People0 = State#?STATE.people,
    People1 =
        case maps:is_key(Pid, hash_ring:get_nodes(People0)) of
            false -> People0;
            true  ->
                ok = notify_change({'PERSON_LEAVE', Pid}, State),
                hash_ring:remove_node(Pid, People0)
        end,
    Listeners = lists:delete(Pid, State#?STATE.listeners),
    {noreply, State#?STATE{listeners = Listeners, people = People1}}.

-spec handle_nodeup(node(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_nodeup(Node, State) ->
   _ = Node > node() andalso join(Node, evel_voter:self_voter()),
    {noreply, State}.

-spec notify_change(notification(), #?STATE{}) -> ok.
notify_change(Message, State) ->
    lists:foreach(
      fun (Pid) -> Pid ! Message end,
      State#?STATE.listeners).

-spec join(node(), evel_voter:voter()) -> ok.
join(Node, Voter) ->
    gen_server:cast({?MODULE, Node}, {join, {self(), Voter, true}}).

-spec join_ack(pid(), evel_voter:voter()) -> ok.
join_ack(Pid, Voter) ->
    gen_server:cast(Pid, {join, {self(), Voter, false}}).
