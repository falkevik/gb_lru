-module(btree_lru_tests).

-include_lib("eunit/include/eunit.hrl").

%% Benchmark helpers preserved from the previous btree_lru_test module.
%% Exported for manual timing experiments; not exercised by eunit.
-export([create/0, create/1,
         write/1, write/2,
         read/2,
         ets_read/2, ets_write/2,
         timing_ets_read/1, timing_ets_write/1,
         timing_read/1, timing_write/1,
         bench/0, bench/1]).

%% =====================================================================
%% Benchmark helpers
%% =====================================================================

create() ->
    create(1024 * 1024 * 1024 * 1000).

create(Size) ->
    {ok, _Tab} = btree_lru:create(Size).

write(Tab) ->
    Objs = [{X, X} || X <- lists:seq(1, 10000000)],
    write(Tab, Objs).

write(Tab, [Obj | Objs]) ->
    ok = btree_lru:write(Tab, Obj),
    write(Tab, Objs);
write(_Tab, []) ->
    ok.

read(Tab, [{K, D} | Objs]) ->
    ?assertEqual({ok, {K, D}}, btree_lru:read(Tab, K)),
    read(Tab, Objs);
read(_Tab, []) ->
    ok.

timing_write(Tab) ->
    Objs = [{X, X} || X <- lists:seq(1, 10000000)],
    timer:tc(?MODULE, write, [Tab, Objs]).
timing_read(Tab) ->
    Objs = [{X, X} || X <- lists:seq(1, 10000000)],
    timer:tc(?MODULE, read, [Tab, Objs]).

timing_ets_write(Tab) ->
    Objs = [{X, X} || X <- lists:seq(1, 10000000)],
    timer:tc(?MODULE, ets_write, [Tab, Objs]).
timing_ets_read(Tab) ->
    Objs = [{X, X} || X <- lists:seq(1, 10000000)],
    timer:tc(?MODULE, ets_read, [Tab, Objs]).

ets_write(Tab, [Obj | Objs]) ->
    true = ets:insert(Tab, Obj),
    ets_write(Tab, Objs);
ets_write(_Tab, []) ->
    ok.

ets_read(Tab, [{K, D} | Objs]) ->
    ?assertEqual([{K, D}], ets:lookup(Tab, K)),
    ets_read(Tab, Objs);
ets_read(_Tab, []) ->
    ok.

%% =====================================================================
%% Benchmark: ETS vs btree_lru, working set fits entirely in cache
%% =====================================================================
%%
%% Run with e.g.
%%   rebar3 as test shell
%%   1> btree_lru_tests:bench().
%%   2> btree_lru_tests:bench(100000).
%%
%% The btree_lru is sized large enough that no entry is ever evicted,
%% so this measures pure insert/lookup overhead.

bench() ->
    bench(1000000).

bench(N) ->
    Objs = [{X, X} || X <- lists:seq(1, N)],

    %% --- ETS ---
    Ets = ets:new(bench_ets, [set, public]),
    {EtsW, ok} = timer:tc(?MODULE, ets_write, [Ets, Objs]),
    {EtsR, ok} = timer:tc(?MODULE, ets_read,  [Ets, Objs]),
    true = ets:delete(Ets),

    %% --- btree_lru (sized so nothing evicts) ---
    {ok, Lru} = btree_lru:create(N * 2),
    {LruW, ok} = timer:tc(?MODULE, write, [Lru, Objs]),
    {LruR, ok} = timer:tc(?MODULE, read,  [Lru, Objs]),
    ok = btree_lru:close(Lru),

    print_bench(N, EtsW, EtsR, LruW, LruR),
    [{n, N},
     {ets_write_us, EtsW}, {ets_read_us, EtsR},
     {lru_write_us, LruW}, {lru_read_us, LruR}].

print_bench(N, EtsW, EtsR, LruW, LruR) ->
    io:format("~n  N = ~b (all entries fit, no eviction)~n~n", [N]),
    io:format("  ~-10s  ~12s  ~12s  ~10s~n",
              ["op", "ets", "btree_lru", "ratio"]),
    io:format("  ~-10s  ~12s  ~12s  ~10s~n",
              ["----------", "------------", "------------", "----------"]),
    row("write", EtsW, LruW, N),
    row("read",  EtsR, LruR, N),
    row("total", EtsW + EtsR, LruW + LruR, N),
    io:format("~n").

row(Label, EtsUs, LruUs, N) ->
    EtsPer = EtsUs / N,
    LruPer = LruUs / N,
    Ratio  = case EtsUs of
                 0 -> 0.0;
                 _ -> LruUs / EtsUs
             end,
    io:format("  ~-10s  ~9.3f us  ~9.3f us  ~8.2fx~n",
              [Label, EtsPer, LruPer, Ratio]).

%% =====================================================================
%% EUnit fixtures
%% =====================================================================

setup() ->
    setup(1024 * 1024).

setup(MaxSize) ->
    {ok, Tab} = btree_lru:create(MaxSize),
    Tab.

cleanup(Tab) ->
    ok = btree_lru:close(Tab).

with_table(Body) ->
    {setup, fun setup/0, fun cleanup/1, Body}.

with_table(MaxSize, Body) ->
    {setup, fun () -> setup(MaxSize) end, fun cleanup/1, Body}.

%% =====================================================================
%% Basic CRUD
%% =====================================================================

write_read_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ?assertEqual(ok, btree_lru:write(Tab, k1, v1)),
            ?assertEqual({ok, {k1, v1}}, btree_lru:read(Tab, k1))
        end
    end).

read_missing_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ?assertEqual({error, invalid}, btree_lru:read(Tab, missing))
        end
    end).

%% Regression: btree_map::operator[] inserted a NULL on every miss,
%% which polluted the underlying tree (memory leak, broken first/last,
%% segfault risk in iterators). After the fix, misses must not change
%% the table at all.
read_miss_does_not_pollute_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ?assertEqual({error, empty}, btree_lru:first(Tab)),
            ?assertEqual({error, empty}, btree_lru:last(Tab)),
            [?assertEqual({error, invalid}, btree_lru:read(Tab, M))
             || M <- [a, b, c, d, e, f, g, h]],
            ?assertEqual(0, btree_lru:get_size(Tab)),
            ?assertEqual({error, empty}, btree_lru:first(Tab)),
            ?assertEqual({error, empty}, btree_lru:last(Tab))
        end
    end).

overwrite_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ok = btree_lru:write(Tab, k, v1),
            ok = btree_lru:write(Tab, k, v2),
            ok = btree_lru:write(Tab, k, v3),
            ?assertEqual({ok, {k, v3}}, btree_lru:read(Tab, k)),
            ?assertEqual(1, btree_lru:get_size(Tab))
        end
    end).

remove_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ok = btree_lru:write(Tab, k1, v1),
            ?assertEqual({ok, {k1, v1}}, btree_lru:read(Tab, k1)),
            ?assertEqual(ok, btree_lru:remove(Tab, k1)),
            ?assertEqual({error, invalid}, btree_lru:read(Tab, k1)),
            ?assertEqual(0, btree_lru:get_size(Tab))
        end
    end).

remove_missing_is_noop_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ?assertEqual(ok, btree_lru:remove(Tab, never_inserted)),
            ?assertEqual(0, btree_lru:get_size(Tab)),
            ?assertEqual({error, empty}, btree_lru:first(Tab))
        end
    end).

write_tuple_api_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            %% write/2 accepts a {Key, Value} tuple.
            ?assertEqual(ok, btree_lru:write(Tab, {kt, vt})),
            ?assertEqual({ok, {kt, vt}}, btree_lru:read(Tab, kt))
        end
    end).

%% =====================================================================
%% LRU ordering: oldest / latest / next / prev
%% =====================================================================

oldest_latest_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ok = btree_lru:write(Tab, a, 1),
            ok = btree_lru:write(Tab, b, 2),
            ok = btree_lru:write(Tab, c, 3),
            ?assertEqual({c, 3}, btree_lru:latest(Tab)),
            ?assertEqual({a, 1}, btree_lru:oldest(Tab))
        end
    end).

%% Overwriting a key must move it to the head of the LRU list.
overwrite_promotes_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ok = btree_lru:write(Tab, a, 1),
            ok = btree_lru:write(Tab, b, 2),
            ok = btree_lru:write(Tab, c, 3),
            ok = btree_lru:write(Tab, a, 11),
            ?assertEqual({a, 11}, btree_lru:latest(Tab)),
            ?assertEqual({b, 2}, btree_lru:oldest(Tab))
        end
    end).

next_prev_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ok = btree_lru:write(Tab, a, 1),
            ok = btree_lru:write(Tab, b, 2),
            ok = btree_lru:write(Tab, c, 3),
            %% latest = c; next/2 walks toward older.
            ?assertEqual({b, 2}, btree_lru:next(Tab, c)),
            ?assertEqual({a, 1}, btree_lru:next(Tab, b)),
            ?assertEqual({error, invalid}, btree_lru:next(Tab, a)),
            %% prev/2 walks toward newer.
            ?assertEqual({b, 2}, btree_lru:prev(Tab, a)),
            ?assertEqual({c, 3}, btree_lru:prev(Tab, b)),
            ?assertEqual({error, invalid}, btree_lru:prev(Tab, c))
        end
    end).

next_prev_missing_key_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ok = btree_lru:write(Tab, a, 1),
            ?assertEqual({error, invalid}, btree_lru:next(Tab, no_such)),
            ?assertEqual({error, invalid}, btree_lru:prev(Tab, no_such))
        end
    end).

%% =====================================================================
%% Key-order traversal: first / last / seek / iterate_next
%% =====================================================================

first_last_in_key_order_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            %% Inserted out of key order; first/last reflect btree order.
            ok = btree_lru:write(Tab, 5, e),
            ok = btree_lru:write(Tab, 1, a),
            ok = btree_lru:write(Tab, 9, i),
            ok = btree_lru:write(Tab, 3, c),
            ?assertEqual({1, a}, btree_lru:first(Tab)),
            ?assertEqual({9, i}, btree_lru:last(Tab))
        end
    end).

empty_first_last_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ?assertEqual({error, empty}, btree_lru:first(Tab)),
            ?assertEqual({error, empty}, btree_lru:last(Tab))
        end
    end).

seek_and_iterate_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            [ok = btree_lru:write(Tab, K, K * 10) || K <- lists:seq(1, 5)],
            {{3, 30}, It} = btree_lru:seek(Tab, 3),
            ?assertEqual({ok, {4, 40}}, btree_lru:iterate_next(Tab, It)),
            ?assertEqual({ok, {5, 50}}, btree_lru:iterate_next(Tab, It)),
            ?assertEqual({error, invalid}, btree_lru:iterate_next(Tab, It))
        end
    end).

seek_lower_bound_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ok = btree_lru:write(Tab, 10, ten),
            ok = btree_lru:write(Tab, 20, twenty),
            ok = btree_lru:write(Tab, 30, thirty),
            %% lower_bound(15) lands on the next key >= 15.
            ?assertMatch({{20, twenty}, _}, btree_lru:seek(Tab, 15)),
            %% seek past the end is an error.
            ?assertEqual({error, invalid}, btree_lru:seek(Tab, 99))
        end
    end).

%% =====================================================================
%% Sizes and limits
%% =====================================================================

get_size_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ?assertEqual(0, btree_lru:get_size(Tab)),
            ok = btree_lru:write(Tab, a, 1),
            ?assertEqual(1, btree_lru:get_size(Tab)),
            ok = btree_lru:write(Tab, b, 2),
            ?assertEqual(2, btree_lru:get_size(Tab)),
            ok = btree_lru:write(Tab, a, 11),    %% overwrite
            ?assertEqual(2, btree_lru:get_size(Tab)),
            ok = btree_lru:remove(Tab, a),
            ?assertEqual(1, btree_lru:get_size(Tab))
        end
    end).

max_size_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            ?assertEqual(1024 * 1024, btree_lru:get_max_size(Tab)),
            ?assertEqual(ok, btree_lru:set_max_size(Tab, 42)),
            ?assertEqual(42, btree_lru:get_max_size(Tab))
        end
    end).

%% =====================================================================
%% Eviction
%% =====================================================================

eviction_test_() ->
    %% write/3 uses size = 1 per entry, so max_size doubles as a count.
    with_table(3, fun (Tab) ->
        fun () ->
            ok = btree_lru:write(Tab, a, 1),
            ok = btree_lru:write(Tab, b, 2),
            ok = btree_lru:write(Tab, c, 3),
            ok = btree_lru:write(Tab, d, 4),    %% evicts a
            ?assertEqual({error, invalid}, btree_lru:read(Tab, a)),
            ?assertEqual({ok, {b, 2}}, btree_lru:read(Tab, b)),
            ?assertEqual({ok, {c, 3}}, btree_lru:read(Tab, c)),
            ?assertEqual({ok, {d, 4}}, btree_lru:read(Tab, d)),
            ?assertEqual(3, btree_lru:get_size(Tab))
        end
    end).

%% Regression: check_size used to evict at most one entry per write.
%% Inserting many entries with a tight limit must settle to max_size.
eviction_drains_to_limit_test_() ->
    with_table(3, fun (Tab) ->
        fun () ->
            [ok = btree_lru:write(Tab, K, K) || K <- lists:seq(1, 10)],
            ?assertEqual(3, btree_lru:get_size(Tab)),
            %% The three most-recent writes survive.
            ?assertEqual({ok, {10, 10}}, btree_lru:read(Tab, 10)),
            ?assertEqual({ok, {9, 9}},   btree_lru:read(Tab, 9)),
            ?assertEqual({ok, {8, 8}},   btree_lru:read(Tab, 8)),
            ?assertEqual({error, invalid}, btree_lru:read(Tab, 7))
        end
    end).

eviction_sends_lru_old_test_() ->
    with_table(2, fun (Tab) ->
        fun () ->
            ok = btree_lru:register_pid(Tab, self()),
            ok = btree_lru:write(Tab, a, 1),
            ok = btree_lru:write(Tab, b, 2),
            ok = btree_lru:write(Tab, c, 3),    %% evicts a
            receive
                {lru_old, a, 1} -> ok
            after 1000 ->
                erlang:error(timeout_waiting_for_lru_old)
            end
        end
    end).

%% =====================================================================
%% Pid registration
%% =====================================================================

register_pid_test_() ->
    with_table(fun (Tab) ->
        fun () ->
            Self = self(),
            ?assertEqual({error, invalid}, btree_lru:get_registered_pid(Tab)),
            ?assertEqual(ok, btree_lru:register_pid(Tab, Self)),
            ?assertEqual(Self, btree_lru:get_registered_pid(Tab)),
            ?assertEqual(ok, btree_lru:unregister_pid(Tab)),
            ?assertEqual({error, invalid}, btree_lru:get_registered_pid(Tab))
        end
    end).
