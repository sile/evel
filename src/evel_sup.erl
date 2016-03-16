%%%-------------------------------------------------------------------
%% @doc evel top level supervisor.
%% @private
%% @end
%%%-------------------------------------------------------------------

-module(evel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children =
        [
         #{id => voter, start => {evel_voter, start_link, []}},
         #{id => commission, start => {evel_commission, start_link, []}},
         #{id => people, start => {evel_people, start_link, []}},
         #{id => token_sup, start => {evel_token_sup, start_link, []}} % This depends on the evel_people process
        ],
    {ok, { #{strategy => rest_for_one}, Children} }.
