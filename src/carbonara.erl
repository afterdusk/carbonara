-module(carbonara).

-export([start_pom/3, stop_pom/3, join_pom/3, leave_pom/3]).

start_pom(_Rest, _Api, #{<<"channel_id">> := ChannelId}) ->
    gen_server:call(carbonara_serv, {start_pom, ChannelId}).

stop_pom(_Rest, _Api, _Msg) ->
    gen_server:call(carbonara_serv, stop_pom).

join_pom(_Rest, _Api, #{<<"author">> := #{<<"id">> := UserId}}) ->
    gen_server:call(carbonara_serv, {join_pom, UserId}).

leave_pom(_Rest, _Api, #{<<"author">> := #{<<"id">> := UserId}}) ->
    gen_server:call(carbonara_serv, {leave_pom, UserId}).
