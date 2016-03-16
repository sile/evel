%% @private
-module(evel_commission).

-hebaviour(gen_server).

-export([start_link/0]).
-export([elect/2]).
-export([dismiss/1]).
-export([find_leader/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          elections          :: ets:tid(),
          cert_to_vote = #{} :: #{evel:certificate() => {evel:election_id(), evel_voter:vote()}}
        }).

-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec elect(evel:election_id(), evel:candidate()) -> evel:leader().
elect(ElectionId, Candidate) ->
    case find_leader(ElectionId) of
        {ok, Leader} -> Leader;
        error        ->
            ok = evel_voter:vote(ElectionId, Candidate),
            {ok, Leader} = find_leader(ElectionId),
            Leader
    end.

-spec dismiss(evel:leader()) -> ok.
dismiss({_, Certificate}) ->
    _ = exit(Certificate, kill),
    ok.

-spec find_leader(evel:election_id()) -> {ok, evel:leader()} | error.
find_leader(ElectionId) ->
    case find_local(ElectionId) of
        error ->
            ok = fetch_leader(ElectionId),
            find_local(ElectionId);
        Other ->
            Other
    end.

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

-spec find_local(evel:election_id()) -> {ok, evel:leader()} | error.
find_local(ElectionId) ->
    try ets:lookup(?MODULE, ElectionId) of
        []            -> error;
        [{_, Leader}] -> {ok, Leader}
    catch
        _:_ -> error
    end.

-spec fetch_leader(evel:election_id()) -> ok.
fetch_leader(ElectionId) ->
    case evel_voter:collect_majority_votes(ElectionId) of
        []    -> ok;
        Votes ->
            {{_, ElectedVote}, Others} = take_highest_priority_vote(Votes),
            {_, _, Certificate} = ElectedVote,
            ok = lists:foreach(
                   fun ({Voter, {_, _, Cert}}) ->
                           Cert =:= Certificate orelse evel_voter:recommend(Voter, ElectionId, ElectedVote)
                   end,
                   Others),
            gen_server:call(?MODULE, {record_leader, {ElectionId, ElectedVote}})
    end.

-spec take_highest_priority_vote([Vote]) -> {Vote, [Vote]} when
      Vote :: {evel_voter:voter(), evel_voter:vote()}.
take_highest_priority_vote(Votes) ->
    [Elected | Others] = lists:keysort(2, Votes),
    {Elected, Others}.

-spec vote_to_leader(evel_voter:vote()) -> evel:leader().
vote_to_leader({_, Candidate, Certificate}) ->
     {Candidate, Certificate}.

-spec handle_record_leader({evel:election_id(), evel_voter:vote()}, #?STATE{}) -> {reply, ok, #?STATE{}}.
handle_record_leader({ElectionId, Vote}, State) ->
    Leader = vote_to_leader(Vote),
    case compete_with_present_leader(ElectionId, Vote, State) of
        {lose, Winner} ->
            _ = Winner =/= Leader andalso dismiss(Leader),
            {reply, ok, State};
        {win, Loser} ->
            ok = dismiss(Loser),
            handle_record_leader({ElectionId, Vote}, remove_leader(Loser, State));
        bye ->
            _ = monitor(process, evel:get_certificate(Leader)),
            CertToVote = maps:put(evel:get_certificate(Leader), {ElectionId, Vote}, State#?STATE.cert_to_vote),
            _ = ets:insert(State#?STATE.elections, {ElectionId, Leader}),
            {reply, ok, State#?STATE{cert_to_vote = CertToVote}}
    end.

-spec compete_with_present_leader(evel:election_id(), evel_voter:vote(), #?STATE{}) ->
                                         bye | {win, evel:leader()} | {lose, evel:leader()}.
compete_with_present_leader(ElectionId, Vote, State) ->
    case ets:lookup(State#?STATE.elections, ElectionId) of
        []               -> bye;
        [{_, Contender}] ->
            {_, ContendVote} = maps:get(evel:get_certificate(Contender), State#?STATE.cert_to_vote),
            case ContendVote =< Vote of
                true  -> {lose, vote_to_leader(ContendVote)};
                false -> {win, vote_to_leader(ContendVote)}
            end
    end.

-spec handle_down(evel:certificate(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Certificate, State) ->
    Dummy = self(),
    {noreply, remove_leader({Dummy, Certificate}, State)}.

-spec remove_leader(evel:leader(), #?STATE{}) -> #?STATE{}.
remove_leader({_, Certificate}, State) ->
    case maps:find(Certificate, State#?STATE.cert_to_vote) of
        error                 -> State;
        {ok, {ElectionId, _}} ->
            CertToVote = maps:remove(Certificate, State#?STATE.cert_to_vote),
            _ = ets:delete(State#?STATE.elections, ElectionId),
            State#?STATE{cert_to_vote = CertToVote}
    end.
