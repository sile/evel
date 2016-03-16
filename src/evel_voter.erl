%% @private
-module(evel_voter).

-hebaviour(gen_server).

-export([start_link/0]).
-export([vote/2]).
-export([recommend/3]).
-export([collect_majority_votes/1]).

-export_type([voter/0]).
-export_type([vote/0]).
-export_type([priority/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          votes            = #{} :: #{evel:election_id() => vote()},
          cert_to_election = #{} :: #{evel:certificate() => evel:election_id()}
        }).

-type voter() :: node().
-type vote() :: {priority(), evel:candidate(), evel:certificate()}.
-type priority() :: float(). % A result of `rand:uniform/0'

-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec vote(evel:election_id(), evel:candidate()) -> ok.
vote(ElectionId, Candidate) ->
    Token = evel_token:issue_token(ElectionId, Candidate),
    evel_token:campaign(Token).
    %Vote = {rand:uniform(), Candidate, Token},
    %% Voters = evel_people:select_voters(ElectionId),
    %% ok = lists:foreach(fun (Voter) -> recommend(Voter, ElectionId, Vote) end, Voters).

-spec recommend(voter(), evel:election_id(), vote()) -> ok.
recommend(Voter, ElectionId, Vote) ->
    gen_server:cast({?MODULE, Voter}, {recommend, {ElectionId, Vote}}).

-spec inquire_vote(voter(), reference(), evel:election_id()) -> ok.
inquire_vote(Voter, Tag, ElectionId) ->
    gen_server:cast({?MODULE, Voter}, {inquire_vote, {self(), Tag, ElectionId}}).

-spec collect_majority_votes(evel:election_id()) -> [{voter(), vote()}].
collect_majority_votes(ElectionId) ->
    Voters = evel_people:select_voters(ElectionId),
    {Worker, Monitor} =
        spawn_monitor(
          fun () ->
                  Tag = make_ref(),
                  Majority = (length(Voters) + 2) div 2,
                  ok = lists:foreach(
                         fun (Voter) -> inquire_vote(Voter, Tag, ElectionId) end,
                         Voters),
                  (fun Loop (0, Acc) ->
                           exit({ok, Acc});
                       Loop (N, Acc) ->
                           receive
                               {Tag, {ok, Answer}} -> Loop(N - 1, [Answer | Acc]);
                               {Tag, error}        -> Loop(N - 1, Acc)
                           end
                   end)(Majority, [])
          end),
    Timeout = 5000, % TODO
    receive
        {'DOWN', Monitor, _, _, Result} ->
            case Result of
                {ok, Votes} -> Votes;
                _           -> []
            end
    after Timeout ->
            _ = demonitor(Monitor, [flush]),
            _ = exit(Worker, kill),
            []
    end.

%% @private
init([]) ->
    State = #?STATE{},
    {ok, State}.

%% @private
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast({recommend, Arg}, State) ->
    handle_recommend(Arg, State);
handle_cast({inquire_vote, Arg}, State) ->
    handle_inquire_vote(Arg, State);
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

-spec handle_recommend({evel:election_id(), vote()}, #?STATE{}) -> {noreply, #?STATE{}}.
handle_recommend({ElectionId, RecommendedVote}, State) ->
    case maps:find(ElectionId, State#?STATE.votes) of
        error ->
            Certificate = evel:get_certificate(vote_to_leader(RecommendedVote)),
            _ = monitor(process, Certificate),
            Votes = maps:put(ElectionId, RecommendedVote, State#?STATE.votes),
            CertToElection = maps:put(Certificate, ElectionId, State#?STATE.cert_to_election),
            {noreply, State#?STATE{votes = Votes, cert_to_election = CertToElection}};
        {ok, RecommendedVote} ->
            {noreply, State};
        {ok, CurrentVote} when CurrentVote < RecommendedVote ->
            ok = evel:dismiss(vote_to_leader(RecommendedVote)),
            {noreply, State};
        {ok, CurrentVote} ->
            Leader = vote_to_leader(CurrentVote), % XXX: name: Leader
            ok = evel:dismiss(Leader),
            handle_recommend({ElectionId, RecommendedVote}, remove_vote(evel:get_certificate(Leader), State))
    end.

-spec handle_inquire_vote({pid(), reference(), evel:election_id()}, #?STATE{}) -> {noreply, #?STATE{}}.
handle_inquire_vote({From, Tag, ElectionId}, State) ->
    ok = case maps:find(ElectionId, State#?STATE.votes) of
             error      -> reply(From, Tag, error);
             {ok, Vote} -> reply(From, Tag, {ok, {node(), Vote}})
         end,
    {noreply, State}.

-spec reply(pid(), reference(), term()) -> ok.
reply(From, Tag, Message) ->
    _ = From ! {Tag, Message},
    ok.

%% TODO: evel_util
-spec vote_to_leader(vote()) -> evel:leader().
vote_to_leader({_, Candidate, Certificate}) ->
     {Candidate, Certificate}.

-spec handle_down(evel:certificate(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Certificate, State) ->
    {noreply, remove_vote(Certificate, State)}.

-spec remove_vote(evel:certificate(), #?STATE{}) -> #?STATE{}.
remove_vote(Certificate, State) ->
    case maps:find(Certificate, State#?STATE.cert_to_election) of
        error            -> State;
        {ok, ElectionId} ->
            Votes = maps:remove(ElectionId, State#?STATE.votes),
            CertToElection = maps:remove(Certificate, State#?STATE.cert_to_election),
            State#?STATE{votes = Votes, cert_to_election = CertToElection}
    end.
