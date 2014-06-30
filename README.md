# Statman -> Graphite

This library pushes your metrics collected with [statman](https://github.com/knutin/statman) to a graphite host over TCP.

For statman histograms only the percentiles are pushed as data points to
graphite.

## Using it

You need to set the `host` and `port` application variables before starting
the app. After that you just need to record some metrics with statman.

```erlang
application:set_env(statman_graphite, host, "graphite.example.com").
application:set_env(statman_graphite, port, 2003).
application:start(statman_graphite).
statman_aggregator:start_link().
record_some_statman_stats().
```

You can use the `prefix` application variable to set a global prefix that will
be prepended to all metrics before they are send to graphite. This is useful
in a multi-node scenario or when using something like [hostedgraphite.com](https://www.hostedgraphite.com).

```erlang
application:set_env(statman_graphite, prefix, <<"my-api-key">>).
application:set_env(statman_graphite, prefix, <<"my-api-key.", (list_to_binary(atom_to_list(node())))/binary>>).
```

### Filtering/rewriting

There are two ways to filter what statman metrics are sent to graphite. By
default all metrics are sent. The easy way to filter is to define the
`whitelist` application variable, it should be a list of statman keys that you
want to send to graphite.

```erlang
application:set_env(statman_graphite, whitelist, [foo, {bar, baz}]).
```

If you want to do dynamic filtering and/or rewrite the metrics before sending
them you can set the `filtermapper` application variable, it should be a fun
that matches the docs for `lists:filtermap`. This option precedes the
`whitelist` option.

```erlang
application:set_env(statman_graphite, filtermapper, {mymodule, myfunction}).
%% Or as a fun
application:set_env(statman_graphite, filtermapper,
                    fun (Metric) ->
                         case proplists:get_value(key, Metric) of
                              foo ->
                                  false;
                              bar ->
                                  {true, baz};
                              baz ->
                                  true
                          end
                    end).
```
