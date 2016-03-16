%% @private
-module(evel_people).

-hebaviour(gen_server).

-export([start_link/0]).
-export([select_voters/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          people :: hash_ring:ring()
        }).

-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec select_voters(evel:election_id()) -> [evel_voter:voter()].
select_voters(ElectionId) ->
    gen_server:call(?MODULE, {select_voters, ElectionId}).

%% @private
init([]) ->
    ok = net_kernel:monitor_nodes(true), % TODO: どの程度信頼できるか(i.e., 自前pollingが不要かどうか)は要確認
    People =
        hash_ring:make(
          lists:map(fun hash_ring_node:make/1, nodes([this, visible])),
          [{virtual_node_count, 64}]),
    State =
        #?STATE{
            people = People
           },
    {ok, State}.

%% @private
handle_call({select_voters, Arg}, _From, State) ->
    handle_select_voters(Arg, State);
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info({nodeup, Node}, State) ->
    People = hash_ring:add_nodes([hash_ring_node:make(Node)], State#?STATE.people),
    {noreply, State#?STATE{people = People}};
handle_info({nodedown, Node}, State) ->
    People = hash_ring:remove_nodes([Node], State#?STATE.people),
    {noreply, State#?STATE{people = People}};
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OlsVsn, State, _Extra) ->
    {ok, State}.

-spec handle_select_voters(evel:election_id(), #?STATE{}) -> {reply, [evel_voter:voter()], #?STATE{}}.
handle_select_voters(ElectionId, State) ->
    Count = 5, % TODO
    Voters =
        lists:map(
          fun hash_ring_node:get_key/1,
          hash_ring:collect_nodes(ElectionId, Count, State#?STATE.people)),
    {reply, Voters, State}.
