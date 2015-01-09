-module(integration_test).
-include_lib("eunit/include/eunit.hrl").

-define(_testx(T), fun (_X, R) -> ?_test(T(element(1,R))) end).

-define(assertElementMatch(M, L),
        ?assert(lists:any(fun (E) -> case E of M -> true; _ -> false end end, L))).
-define(assertNotElementMatch(M, L),
        ?assertNot(lists:any(fun (E) -> case E of M -> true; _ -> false end end, L))).

-define(PORT, 2003).

-export([fm/1]).

integration_test_() ->
    {foreachx, local,
     fun setup/1, fun teardown/2,
     [{[{host, "localhost"}, {port, ?PORT}],
       ?_testx(simple)},

      {[{host, "localhost"}, {port, ?PORT}, {prefix, <<"myprefix">>}],
       ?_testx(prefix)},

      {[{host, "localhost"}, {port, ?PORT}, {whitelist, [{foo, bar}, baz]}],
       ?_testx(whitelist)},

      {[{host, "localhost"}, {port, ?PORT}, {filtermapper, fun fm/1}],
       ?_testx(filtermapper)},
      {[{host, "localhost"}, {port, ?PORT}, {filtermapper, {?MODULE, fm}}],
       ?_testx(filtermapper)}
     ]}.

setup(Env) ->
    {ok, Socket} = gen_tcp:listen(?PORT, [{active, once}, binary, {reuseaddr, true}]),
    lists:foreach(fun ({Key, Val}) ->
                          application:set_env(statman_graphite, Key, Val)
                  end, Env),
    ok = application:start(statman),
    ok = application:start(statman_graphite),
    {ok, Aggregator} = statman_aggregator:start_link(),
    ok = statman_server:add_subscriber(statman_aggregator),
    {ok, Timer} = gen_server:call(statman_graphite_pusher, get_timer),
    erlang:cancel_timer(Timer),
    {Socket, Aggregator}.

teardown(Env, {Socket, Aggregator}) ->
    unlink(Aggregator),
    exit(Aggregator, kill),
    lists:foreach(fun ({Key, _Val}) ->
                          ok = application:unset_env(statman_graphite, Key)
                  end, Env),
    ok = gen_tcp:close(Socket),
    ok = application:stop(statman_graphite),
    ok = application:stop(statman).

simple(Socket) ->
    statman_counter:incr({test, counter}, 42),
    statman_gauge:set({test, gauge}, 4711),
    statman_histogram:record_value({test, histogram}, 7),

    statman_server:report(),
    timer:sleep(500),

    {ok, Timer} = gen_server:call(statman_graphite_pusher, get_timer),
    pusher_pid() ! {timeout, Timer, {push, 60000}},

    Lines = recv_lines(Socket),
    ?assertEqual(1 + 1 + 10, length(Lines)),

    ?assertElementMatch(<<"test.counter 42", _/binary>>, Lines),
    ?assertElementMatch(<<"test.gauge 4711", _/binary>>, Lines),
    ?assertElementMatch(<<"test.histogram.min 7", _/binary>>, Lines),
    ?assertElementMatch(<<"test.histogram.p25 7", _/binary>>, Lines),
    ?assertElementMatch(<<"test.histogram.mean 7", _/binary>>, Lines),
    ?assertElementMatch(<<"test.histogram.median 7", _/binary>>, Lines),
    ?assertElementMatch(<<"test.histogram.p75 7", _/binary>>, Lines),
    ?assertElementMatch(<<"test.histogram.p95 7", _/binary>>, Lines),
    ?assertElementMatch(<<"test.histogram.p99 7", _/binary>>, Lines),
    ?assertElementMatch(<<"test.histogram.p999 7", _/binary>>, Lines),
    ?assertElementMatch(<<"test.histogram.max 7", _/binary>>, Lines),
    ok.

prefix(Socket) ->
    statman_counter:incr({test, counter}, 42),
    statman_gauge:set({test, gauge}, 4711),
    statman_histogram:record_value({test, histogram}, 7),

    statman_server:report(),
    timer:sleep(500),

    {ok, Timer} = gen_server:call(statman_graphite_pusher, get_timer),
    pusher_pid() ! {timeout, Timer, {push, 60000}},

    Lines = recv_lines(Socket),
    ?assertEqual(1 + 1 + 9 + 1, length(Lines)),

    ?assertElementMatch(<<"myprefix.test.counter 42", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.gauge 4711", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.histogram.min 7", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.histogram.p25 7", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.histogram.mean 7", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.histogram.median 7", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.histogram.p75 7", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.histogram.p95 7", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.histogram.p99 7", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.histogram.p999 7", _/binary>>, Lines),
    ?assertElementMatch(<<"myprefix.test.histogram.max 7", _/binary>>, Lines),
    ok.

whitelist(Socket) ->
    statman_gauge:set({foo, bar}, 4711),
    statman_gauge:set(baz, 42),
    statman_gauge:set(foo, 42),
    statman_gauge:set(bar, 42),

    statman_server:report(),
    timer:sleep(500),

    {ok, Timer} = gen_server:call(statman_graphite_pusher, get_timer),
    pusher_pid() ! {timeout, Timer, {push, 60000}},

    Lines = recv_lines(Socket),
    ?assertEqual(1 + 1 + 1, length(Lines)),

    ?assertElementMatch(<<"foo.bar 4711", _/binary>>, Lines),
    ?assertElementMatch(<<"baz 42", _/binary>>, Lines),
    ?assertNotElementMatch(<<"foo 42", _/binary>>, Lines),
    ?assertNotElementMatch(<<"bar 42", _/binary>>, Lines),
    ok.

filtermapper(Socket) ->
    statman_gauge:set({foo, bar}, 42),
    statman_gauge:set(foobar, 42),
    statman_gauge:set(foo, 42),

    statman_server:report(),
    timer:sleep(500),

    {ok, Timer} = gen_server:call(statman_graphite_pusher, get_timer),
    pusher_pid() ! {timeout, Timer, {push, 60000}},

    Lines = recv_lines(Socket),
    ?assertEqual(1 + 1 + 1, length(Lines)),

    ?assertElementMatch(<<"foo.bar.baz 42", _/binary>>, Lines),
    ?assertElementMatch(<<"foo 42", _/binary>>, Lines),
    ?assertNotElementMatch(<<"foobar 42", _/binary>>, Lines),
    ?assertNotElementMatch(<<"foo.bar 42", _/binary>>, Lines),
    ok.
fm(Metric) ->
    case proplists:get_value(key, Metric) of
        {foo, bar} ->
            {true, lists:keystore(key, 1, Metric, {key, {foo, bar, baz}})};
        foobar ->
            false;
        foo ->
            true
    end.



recv_lines(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    receive
        {tcp, _, Packet} ->
            ok = gen_tcp:close(Socket),
            binary:split(Packet, <<$\n>>, [global])
    end.

pusher_pid() ->
    Pid = whereis(statman_graphite_pusher),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    Pid.
