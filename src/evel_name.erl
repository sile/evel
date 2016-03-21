%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
%%
%% @doc An evel based global name registration facility
%%
%% TODO: `{via, evel_name, foo}'
%%
%% @end
-module(evel_name).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).
-export([send/2]).

-export_type([name/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type name() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec register_name(name(), pid()) -> yes | no.
register_name(Name,  Pid) ->
    case evel:elect({?MODULE, Name}, Pid) of
        {Pid, _} -> yes;
        _        -> no
    end.

-spec unregister_name(name()) -> ok.
unregister_name(Name) ->
    case evel:find_leader({?MODULE, Name}) of
        error        -> ok;
        {ok, Leader} -> evel:dismiss(Leader, [{unlink, true}])
    end.

-spec whereis_name(name()) -> pid() | undefined.
whereis_name(Name) ->
    case evel:find_leader({?MODULE, Name}) of
        error          -> undefined;
        {ok, {Pid, _}} -> Pid
    end.

-spec send(name(), term()) -> pid().
send(Name, Message) ->
    case whereis_name(Name) of
        undefined -> error({badarg, {Name, Message}});
        Pid       ->
            _ = Pid ! Message,
            Pid
    end.
