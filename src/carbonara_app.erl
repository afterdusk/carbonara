%%%-------------------------------------------------------------------
%% @doc carbonara public API
%% @end
%%%-------------------------------------------------------------------

-module(carbonara_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Token =
        case os:getenv("DISCORD_TOKEN") of
            false ->
                throw({missing_envvar, "DISCORD_TOKEN"});
            V ->
                V
        end,
    Msgs =
        #{
            <<"cb!start">> => #{call => {carbonara, start_pom, []}},
            <<"cb!stop">> => #{call => {carbonara, stop_pom, []}},
            <<"cb!join">> => #{call => {carbonara, join_pom, []}},
            <<"cb!leave">> => #{call => {carbonara, leave_pom, []}}
        },
    discordant:connect(Token),
    discordant:set_routes(Msgs, []),
    carbonara_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
