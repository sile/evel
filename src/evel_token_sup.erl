%% @private
-module(evel_token_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/1]).

-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(evel_token:start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_child(Arg) ->
    supervisor:start_child(?MODULE, [Arg]).

%% @private
init([]) ->
    Child = #{id => token, start => {evel_token, start_link, []}, restart => temporary},
    {ok, {#{strategy => simple_one_for_one}, [Child]}}.
