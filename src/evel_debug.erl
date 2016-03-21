%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
%%
%% @doc Miscellaneous Debugging Functions
%%
%% @private
%% @end
-module(evel_debug).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([summary/0]).
-export([slave_start_link_n/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec summary() -> Summary when
      Summary :: #{
        knowns => [{evel:election_id(), evel:leader()}],
        votes  => [{evel:election_id(), evel_voter:vote()}],
        people => [evel_voter:voter()],
        agents => [{evel:election_id(), evel_voter:vote(), [evel_voter:voter()]}]
       }.
summary() ->
    #{
       knowns => evel:known_leaders(),
       votes  => evel_voter:get_votes(),
       people => evel_people:get_people(),
       agents => lists:map(fun evel_agent:get_summary/1, evel_agent_sup:which_children())
     }.

-spec slave_start_link_n(non_neg_integer()) -> ok.
slave_start_link_n(Count) ->
    {ok, Host} = inet:gethostname(),
    ok = lists:foreach(
           fun (I) ->
                   {ok, _} = slave:start_link(Host, integer_to_list(I))
           end,
           lists:seq(1, Count)),
    _ = rpc:eval_everywhere(code, add_pathsa, [code:get_path()]),
    _ = rpc:eval_everywhere(application, ensure_all_started, [evel]),
    ok.
