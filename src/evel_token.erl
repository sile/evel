%% @private
-module(evel_token).

%% NOTE: agent = 選挙事務所、的な用語でも良いのかも (agent同士がvoterを取り合う)
%%       -> その類推で、people => voter, voter => supporter, でも良いかも
-hebaviour(gen_server).

-export([start_link/1]).
-export([issue_token/2]).
-export([campaign/1]).

-export_type([start_arg/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHECK_VOTER_UP_INTERVAL, 10000).

-type start_arg() :: {evel:election_id(), evel:candidate()}.

-define(STATE, ?MODULE).
-record(?STATE,
        {
          election_id :: evel:election_id(),
          candidate :: evel:candidate(),
          vote :: evel_voter:vote(),
          monitors = #{} :: #{reference() => evel_voter:voter()}
        }).

%% TODO: ノードの追加・削除が一度に大幅に行われて、構成が大きく変わった場合にも対応する(これが一番手間)
%%       => tokenが定期的(or ノードの追加・削除のタイミング)にselect_voters/1を叩いて変化を確認するのが確実？
%%  => evel_people側でノード追加・削除に、それによって影響を受けるtokenに通知を送るようにしたい
%%   => コンシステントハッシュの実装に手を入れればそこまで難しい話ではない (各仮想ノードが自分が担当しているアイテム一覧を保持していれば良い)
-spec start_link(start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Arg) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arg, []).

-spec issue_token(evel:election_id(), evel:candidate()) -> evel:token().
issue_token(ElectionId, Candidate) ->
    {ok, Pid} = evel_token_sup:start_child({ElectionId, Candidate}),
    Pid.

-spec campaign(evel:token()) -> ok.
campaign(Token) ->
    gen_server:call(Token, campaign).

%% @private
init({ElectionId, Candidate}) ->
    _ = monitor(process, Candidate),
    State =
        #?STATE{
            election_id = ElectionId,
            candidate = Candidate,
            vote = {rand:uniform(), Candidate, self()}
           },
    {ok, State}.

%% @private
handle_call(campaign, _From, State) ->
    handle_campaign(State);
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

-spec handle_campaign(#?STATE{}) -> {reply, ok, #?STATE{}}.
handle_campaign(State) ->
    %% TODO: 重複呼び出しは禁止する
    Voters = evel_people:select_voters(State#?STATE.election_id),
    Monitors =
        maps:from_list(
          [begin
               ok = evel_voter:recommend(Voter, State#?STATE.election_id, State#?STATE.vote),
               {monitor(process, {evel_voter, Voter}), Voter}
           end || Voter <- Voters]),
    {reply, ok, State#?STATE{monitors = Monitors}}.

-spec handle_voter_down(reference(), #?STATE{}) -> {noreply, #?STATE{}} | {stop, Reason::term(), #?STATE{}}.
handle_voter_down(Ref, State) ->
    Voter = maps:get(Ref, State#?STATE.monitors),
    Monitors = maps:remove(Ref, State#?STATE.monitors),
    case maps:size(Monitors) =:= 0 of
        true ->
            {stop, {shutdown, {all_voter_down, State#?STATE.election_id}}, State#?STATE{monitors = Monitors}};
        false ->
            ok = schedule_check_voter_up(Voter),
            {noreply, State#?STATE{monitors = Monitors}}
    end.

-spec schedule_check_voter_up(evel_voter:voter()) -> ok.
schedule_check_voter_up(Voter) ->
    _ = erlang:send_after(?CHECK_VOTER_UP_INTERVAL, self(), {check_voter_up, Voter}),
    ok.

-spec handle_check_voter_up(evel_voter:voter(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_check_voter_up(Voter, State) ->
    Monitor = monitor(process, {evel_voter, Voter}),
    ok = evel_voter:recommend(Voter, State#?STATE.election_id, State#?STATE.vote),
    Monitors = maps:put(Monitor, Voter, State#?STATE.monitors),
    {noreply, State#?STATE{monitors = Monitors}}.
