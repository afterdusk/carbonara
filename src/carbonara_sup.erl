%%%-------------------------------------------------------------------
%% @doc carbonara top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(carbonara_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{
            strategy => one_for_one,
            intensity => 0,
            period => 1
        },
    ChildSpecs =
        [
            {server, {carbonara_serv, start_link_local, []}, permanent, 5000, worker, [
                carbonara_serv
            ]}
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
