# Statman -> Graphite

This library pushes your metrics collected with [statman](https://github.com/knutin/statman) to a graphite host over TCP.

For statman histograms only the percentiles are pushed as data points to
graphite.

## Using it

You need to set the `host` and `port` application variables before starting
the app. After that you just need to record some metrics with statman.

    > application:set_env(statman_graphite, host, "graphite.example.com").
    > application:set_env(statman_graphite, port, 2003).
    > application:start(statman_graphite).
    > statman_aggregator:start_link().
    > record_some_statman_stats().

You can use the `prefix` application variable to set a global prefix that will
be prepended to all metrics before they are send to graphite. This is useful
in a multi-node scenario or when using something like [hostedgraphite.com](https://www.hostedgraphite.com).

    > application:set_env(statman_graphite, prefix, <<"my-api-key">>).
    > application:set_env(statman_graphite, prefix, <<"my-api-key.", (list_to_binary(atom_to_list(node())))/binary>>).
