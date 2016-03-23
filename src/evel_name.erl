%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
%%
%% @doc An evel based global name registration facility
%%
%% This module is available as a `via' callback module for OTP behaviours (e.g., supervisor, gen_server, etc).
%%
%% <pre lang="erlang">
%% %%
%% %% Example
%% %%
%% gen_server:start_link({via, evel_name, SomeName}, ?MODULE, [], []).
%% </pre>
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
%% A name of the process

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Globally associates the name `Name' with a pid `Pid'
%%
%% The function returns `yes' if successful, `no' if it fails.
%% For example, `no' is returned if an attempt is made to register a process with a name that is already in use.
%%
%% Unlike `global:register_name/1', this function allows a process to have multiple names.
%%
%% If conlict arises between multiple processes during a registration, only one process survive.
-spec register_name(name(), pid()) -> yes | no.
register_name(Name,  Pid) ->
    case evel:elect({?MODULE, Name}, Pid) of
        {Pid, _} -> yes;
        _        -> no
    end.

%% @doc Removes the globally registered name `Name'
-spec unregister_name(name()) -> ok.
unregister_name(Name) ->
    case evel:find_leader({?MODULE, Name}) of
        error        -> ok;
        {ok, Leader} -> evel:dismiss(Leader, [{unlink, true}])
    end.

%% @doc Returns the pid with the globally registered name `Name'
%%
%% Returns `undefined' if the name is not registered.
-spec whereis_name(name()) -> pid() | undefined.
whereis_name(Name) ->
    case evel:find_leader({?MODULE, Name}) of
        error          -> undefined;
        {ok, {Pid, _}} -> Pid
    end.

%% @doc Sends the message `Message' to the pid globally registered as `Name'
%%
%% Failure: If `Name' is not a registered name, the calling function will exit with reason `{badarg, {Name, Message}}'
-spec send(name(), term()) -> pid().
send(Name, Message) ->
    case whereis_name(Name) of
        undefined -> error({badarg, {Name, Message}});
        Pid       ->
            _ = Pid ! Message,
            Pid
    end.
