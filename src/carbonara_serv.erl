-module(carbonara_serv).

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

% API
-export([start_link_local/0, start_link_local/1, start_link_local/2]).
% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state,
    %% inactive | work | break
    {
        stage = inactive,
        work_duration_mins = 25,
        small_break_duration_mins = 5,
        big_break_duration_mins = 15,
        break_index = 1,
        channel_id,
        subscribers = sets:new([{version, 2}]),
        timer_ref
    }
).

% API
start_link_local() ->
    start_link_local(#{}).

start_link_local(Args) ->
    start_link_local(Args, []).

start_link_local(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

% Callbacks
init(_Args) ->
    {ok, #state{}}.

handle_call({start_pom, _}, _From, State = #state{stage = Stage}) when
    Stage =/= inactive
->
    {reply, {reply, <<"Pomodoro session already in progress.">>, []}, State};
handle_call(
    {start_pom, ChannelId},
    _From,
    State = #state{work_duration_mins = WorkDuration}
) ->
    TimerRef = erlang:start_timer(WorkDuration * 1000 * 60, self(), timeout, [{abs, false}]),
    {reply, {reply, <<"Pomodoro started! Your break is in 25 minutes.">>, []}, State#state{
        stage = work,
        channel_id = ChannelId,
        timer_ref = TimerRef
    }};
handle_call(stop_pom, _From, State = #state{stage = inactive}) ->
    {reply, {reply, <<"No session in progress.">>, []}, State};
handle_call(stop_pom, _From, State = #state{timer_ref = TimerRef}) ->
    erlang:cancel_timer(TimerRef),
    {reply, {reply, <<"Pomodoro stopped!">>, []}, State#state{
        stage = inactive,
        break_index = 1,
        subscribers = []
    }};
handle_call({join_pom, _UserId}, _From, State = #state{stage = inactive}) ->
    {reply, {reply, <<"No session in progress.">>, []}, State};
handle_call({join_pom, UserId}, _From, State = #state{subscribers = Subscribers}) ->
    Message =
        case sets:is_element(UserId, Subscribers) of
            false ->
                <<"You joined the pomodoro session!">>;
            true ->
                <<"You've already joined the pomodoro session :)">>
        end,
    MessageWithMention = build_mention(Message, UserId),
    {reply, {reply, MessageWithMention, []}, State#state{
        subscribers = sets:add_element(UserId, Subscribers)
    }};
handle_call({leave_pom, _UserId}, _From, State = #state{stage = inactive}) ->
    {reply, {reply, <<"No session in progress.">>, []}, State};
handle_call({leave_pom, UserId}, _From, State = #state{subscribers = Subscribers}) ->
    Message =
        case sets:is_element(UserId, Subscribers) of
            false ->
                <<"You've not joined the pomodoro session!">>;
            true ->
                <<"You've left the pomodoro session!">>
        end,
    MessageWithMention = build_mention(Message, UserId),
    {reply, {reply, MessageWithMention, []}, State#state{
        subscribers = sets:del_element(UserId, Subscribers)
    }};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    {timeout, _Ref, timeout},
    State =
        #state{
            stage = work,
            small_break_duration_mins = SmallBreakDuration,
            big_break_duration_mins = BigBreakDuration,
            break_index = BreakIndex,
            channel_id = ChannelId,
            subscribers = Subscribers
        }
) ->
    {BreakDuration, Message} =
        if
            BreakIndex =:= 4 ->
                {BigBreakDuration, <<"Time for a big break of 15 minutes!">>};
            BreakIndex =/= 4 ->
                {SmallBreakDuration, <<"Time for a small break of 5 minutes!">>}
        end,
    MessageWithMentions =
        sets:fold(fun(UserId, Acc) -> build_mention(Acc, UserId) end, Message, Subscribers),
    send_message(MessageWithMentions, ChannelId),
    TimerRef = erlang:start_timer(BreakDuration * 1000 * 60, self(), timeout, [{abs, false}]),
    {noreply, State#state{stage = break, timer_ref = TimerRef}};
handle_info(
    {timeout, _Ref, timeout},
    State =
        #state{
            stage = break,
            work_duration_mins = WorkDuration,
            break_index = BreakIndex,
            channel_id = ChannelId,
            subscribers = Subscribers
        }
) ->
    Message = <<"Your break is over, time to work!">>,
    MessageWithMentions =
        sets:fold(fun(UserId, Acc) -> build_mention(Acc, UserId) end, Message, Subscribers),
    send_message(MessageWithMentions, ChannelId),
    TimerRef = erlang:start_timer(WorkDuration * 1000 * 60, self(), timeout, [{abs, false}]),
    {noreply, State#state{
        stage = work,
        break_index = BreakIndex rem 4 + 1,
        timer_ref = TimerRef
    }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions
send_message(Message, ChannelId) ->
    ApiServer = discordant_sup:get_api_server(),
    discord_api:send_message(ApiServer, ChannelId, Message).

build_mention(Message, UserId) ->
    <<"<@!", UserId/binary, "> ", Message/binary>>.
