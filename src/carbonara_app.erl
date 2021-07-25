%%%-------------------------------------------------------------------
%% @doc carbonara public API
%% @end
%%%-------------------------------------------------------------------

-module(carbonara_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    carbonara_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
