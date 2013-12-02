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

-record(state, {timer, prefix, graphite, socket}).

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
    {ok, Prefix} = application:get_env(statman_graphite, prefix),
    {ok, Host} = application:get_env(statman_graphite, host),
    {ok, Port} = application:get_env(statman_graphite, port),
    Timer = erlang:start_timer(Interval, self(), {push, Interval, 0}),
    {ok, Socket} = open_socket({Host, Port}),
    {ok, #state{timer = Timer,
                prefix = Prefix,
                graphite = {Host, Port},
                socket = Socket}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({push, _Interval, 3}, State) ->
    error_logger:warning_msg("statman_graphite: failed to push to graphite: "
                             "max_retries"),
    {noreply, State};

handle_info({push, Interval, Retries}, State) ->
    {ok, Metrics} = statman_aggregator:get_window(Interval div 1000),
    Serialized = serialize_metrics(State#state.prefix, filter(Metrics)),
    {ok, NewSocket} = case push(Serialized, State#state.socket) of
                          ok ->
                              {ok, State#state.socket};
                          {error, Reason} ->
                              error_logger:info_msg(
                                "statman_graphite: failed to push to graphite: ~p",
                                [Reason]),
                              self() ! {push, Interval, Retries+1},
                              open_socket(State#state.graphite)
                      end,
    {noreply, State#state{socket = NewSocket}};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    _ = erlang:cancel_timer(State#state.timer),
    ok = gen_tcp:close(State#state.socket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

open_socket({Host, Port}) ->
    gen_tcp:connect(Host, Port, [{packet, 0}]).

filter(Metrics) ->
    case application:get_env(statman_graphite, whitelist) of
        {ok, List} ->
            [Metric || Metric <- Metrics,
                       lists:member(proplists:get_value(key, Metric), List)];
        undefined ->
            %% Send all metrics if a whitelist is not configured
            Metrics
    end.

serialize_metrics(Prefix, Metrics) ->
    lists:flatmap(fun (Metric) ->
                          format_metric(Prefix, Metric)
                  end, Metrics).

format_metric(Prefix, Metric) ->
    case proplists:get_value(type, Metric) of
        counter ->
            [<<Prefix/binary, $., (format_counter(Metric))/binary, $\n>>];
        gauge ->
            [<<Prefix/binary, $., (format_gauge(Metric))/binary, $\n>>];
        histogram ->
            [<<Prefix/binary, $., Data/binary, $\n>>
             || Data <- format_histogram(Metric)];
        _ ->
            []
    end.

format_counter(Metric) ->
    Name = format_key(proplists:get_value(key, Metric)),
    Value = number_to_binary(proplists:get_value(value, Metric)),
    <<Name/binary, " ", Value/binary>>.

format_gauge(Metric) ->
    Name = format_key(proplists:get_value(key, Metric)),
    Value = number_to_binary(proplists:get_value(value, Metric)),
    <<Name/binary, " ", Value/binary>>.

format_histogram(Metric) ->
    Summary = statman_histogram:summary(proplists:get_value(value, Metric)),
    Percentiles = [p25, mean, p75, p95, p99, p999],
    lists:flatmap(
      fun (Percentile) ->
              Name = format_key({proplists:get_value(key, Metric), Percentile}),
              case proplists:get_value(Percentile, Summary) of
                  N when is_number(N) ->
                      [<<Name/binary, " ", (number_to_binary(N))/binary>>];
                  _ ->
                      []
              end
      end, Percentiles).


format_key(Key) ->
    iolist_to_binary(format_key2(Key)).

format_key2(Key) when is_tuple(Key) ->
    [string:join(lists:map(fun format_key2/1, tuple_to_list(Key)), ".")];
format_key2(Key) when is_atom(Key) ->
    [atom_to_list(Key)];
format_key2(Key) when is_binary(Key) orelse is_list(Key) ->
    [Key].


number_to_binary(N) when is_float(N) ->
    iolist_to_binary(io_lib:format("~f", [N]));
number_to_binary(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N)).


push([], _Socket) ->
    ok;
push(Message, Socket) ->
    gen_tcp:send(Socket, Message).

%%
%% TEST
%%


set_test_env() ->
    application:set_env(statman_graphite, prefix, <<"myprefix">>),
    application:set_env(statman_graphite, host, "localhost"),
    application:set_env(statman_graphite, port, 2003),
    ok.

push_test() ->
    ok = set_test_env(),
    {ok, _Pid} = statman_aggregator:start_link(),
    ok = application:start(statman_graphite),
    ok = statman_server:add_subscriber(statman_aggregator),
    link(whereis(?MODULE)),

    statman_counter:incr({test, counter}, 42),
    statman_gauge:set({test, gauge}, 4711),
    statman_histogram:record_value({test, histogram}, 7),

    statman_server:report(),
    timer:sleep(100),

    ?MODULE ! {push, 60000, 0}.
