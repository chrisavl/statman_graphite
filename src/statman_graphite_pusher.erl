-module(statman_graphite_pusher).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {timer, prefix, graphite, socket, filtermapper}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Interval = application:get_env(statman_graphite, interval, 60000),
    Prefix = application:get_env(statman_graphite, prefix, undefined),
    {ok, Host} = application:get_env(statman_graphite, host),
    {ok, Port} = application:get_env(statman_graphite, port),
    Filtermapper = case application:get_env(statman_graphite, filtermapper) of
                       {ok, {M, F}} ->
                           fun M:F/1;
                       {ok, Fun} when is_function(Fun) ->
                           Fun;
                       undefined ->
                           fun default_filtermapper/1
                   end,
    Timer = erlang:start_timer(Interval, self(), {push, Interval}),
    {ok, #state{timer = Timer,
                prefix = Prefix,
                graphite = {Host, Port},
                socket = undefined,
                filtermapper = Filtermapper}}.


handle_call(get_timer, _From, State) ->
    {reply, {ok, State#state.timer}, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({timeout, Timer, {push, Interval}}, #state{timer = Timer} = State) ->
    NewTimer = erlang:start_timer(Interval, self(), {push, Interval}),
    {ok, Metrics} = statman_aggregator:get_window(Interval div 1000),
    Filtered = lists:filtermap(State#state.filtermapper, Metrics),
    Serialized = serialize_metrics(State#state.prefix, Filtered),
    case push(Serialized, State#state.socket, State#state.graphite, 3) of
        {ok, NewSocket} ->
            {noreply, State#state{timer = NewTimer, socket = NewSocket}};
        {error, Reason} ->
            error_logger:warning_msg(
              "statman_graphite: failed to push to graphite: ~p",
              [Reason]),
            ok = gen_tcp:close(State#state.socket),
            {noreply, State#state{timer = NewTimer}}
    end;

handle_info({tcp_closed, Socket}, State = #state{socket = Socket}) ->
    {noreply, State#state{socket = undefined}};

handle_info({tcp_closed, _OtherSocket}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    error_logger:info_msg("statman_graphite: got unexpected message: ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

push(_Message, _Socket, _Graphite, Tries) when Tries =< 0 ->
    {error, max_tries};
push([], Socket, _Graphite, _Tries) ->
    {ok, Socket};
push(Message, Socket, Graphite, Tries) ->
    case maybe_connect(Socket, Graphite) of
        {ok, NewSocket} ->
            case gen_tcp:send(NewSocket, Message) of
                ok              -> {ok, NewSocket};
                {error, closed} -> push(Message, undefined, Graphite, Tries-1);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

open_socket({Host, Port}) ->
    gen_tcp:connect(Host, Port, [{packet, 0}]).

maybe_connect(undefined, {Host, Port}) ->
    open_socket({Host, Port});
maybe_connect(Socket, _) ->
    {ok, Socket}.

default_filtermapper(Metric) ->
    case application:get_env(statman_graphite, whitelist) of
        {ok, List} ->
            lists:member(proplists:get_value(key, Metric), List);
        undefined ->
            %% Send all metrics if a whitelist is not configured
            true
    end.

serialize_metrics(Prefix, Metrics) ->
    lists:flatmap(fun (Metric) ->
                          format_metric(Prefix, Metric)
                  end, Metrics).

format_metric(Prefix, Metric) ->
    case proplists:get_value(type, Metric) of
        counter ->
            [<<(binary_prefix(Prefix))/binary, (format_counter(Metric))/binary, $\n>>];
        gauge ->
            [<<(binary_prefix(Prefix))/binary, (format_gauge(Metric))/binary, $\n>>];
        histogram ->
            [<<(binary_prefix(Prefix))/binary, Data/binary, $\n>>
             || Data <- format_histogram(Metric)];
        _ ->
            []
    end.

binary_prefix(undefined) ->
    <<>>;
binary_prefix(B) when is_binary(B) ->
    <<B/binary, $.>>.

format_counter(Metric) ->
    Name = format_key(proplists:get_value(key, Metric)),
    Value = number_to_binary(proplists:get_value(value, Metric)),
    Timestamp = number_to_binary(timestamp()),
    <<Name/binary, " ", Value/binary, " ", Timestamp/binary>>.

format_gauge(Metric) ->
    Name = format_key(proplists:get_value(key, Metric)),
    Value = number_to_binary(proplists:get_value(value, Metric)),
    Timestamp = number_to_binary(timestamp()),
    <<Name/binary, " ", Value/binary, " ", Timestamp/binary>>.

format_histogram(Metric) ->
    Summary = statman_histogram:summary(proplists:get_value(value, Metric)),
    Percentiles = [min, p25, mean, median, p75, p95, p99, p999, max],
    lists:flatmap(
      fun (Percentile) ->
              Name = format_key({proplists:get_value(key, Metric), Percentile}),
              case proplists:get_value(Percentile, Summary) of
                  N when is_number(N) ->
                      Timestamp = number_to_binary(timestamp()),
                      [<<Name/binary, " ", (number_to_binary(N))/binary, " ",
                        Timestamp/binary>>];
                  _ ->
                      []
              end
      end, Percentiles).


format_key(Key) ->
    sanitize(iolist_to_binary(format_key2(Key))).

format_key2(Key) when is_integer(Key) ->
    [integer_to_list(Key)];
format_key2(Key) when is_tuple(Key) ->
    [string:join(lists:map(fun format_key2/1, tuple_to_list(Key)), ".")];
format_key2(Key) when is_atom(Key) ->
    [atom_to_list(Key)];
format_key2(Key) when is_binary(Key) orelse is_list(Key) ->
    [Key].

sanitize(Key) ->
    binary:replace(binary:replace(Key, <<" ">>, <<"_">>, [global]),
                   <<"/">>, <<".">>, [global]).

number_to_binary(N) when is_float(N) ->
    iolist_to_binary(io_lib:format("~f", [N]));
number_to_binary(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N)).


timestamp() ->
    {MegaSeconds, Seconds, _} = os:timestamp(),
    MegaSeconds * 1000000 + Seconds.

%%
%% TEST
%%

format_key_test() ->
    Key = {some, {<<"nested metric">>, "with/special/chars "}},
    ?assertEqual(<<"some.nested_metric.with.special.chars_">>,
                 format_key(Key)).
