%%%-------------------------------------------------------------------
%% @doc firebase_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module('firebase_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    'firebase_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================