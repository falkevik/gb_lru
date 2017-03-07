-module(btree_lru_test).

-compile(export_all).

-export([create/0,
	 create/1]).



create() ->
    create(1024*1024*1024*1000).

create(Size) ->
    {ok, _Tab} = btree_lru:create(Size).


write(Tab) ->
    Objs = [{X,X} || X <- lists:seq(1,10000000)],
    write(Tab, Objs).

write(Tab, [Obj | Objs]) ->
    ok = btree_lru:write(Tab, Obj),
    write(Tab, Objs);
write(_Tab, []) ->
    ok.

read(Tab, [{K,D} | Objs]) ->
    {K,D} = btree_lru:read(Tab, K),
    read(Tab, Objs);
read(_Tab, []) ->
    ok.
    
timing_write(Tab) ->
    Objs = [{X,X} || X <- lists:seq(1,10000000)],
    timer:tc(?MODULE, write, [Tab, Objs]).
timing_read(Tab) ->
    Objs = [{X,X} || X <- lists:seq(1,10000000)],
    timer:tc(?MODULE, read, [Tab, Objs]).
    
timing_ets_write(Tab) ->
    Objs = [{X,X} || X <- lists:seq(1,10000000)],
    timer:tc(?MODULE, ets_write, [Tab, Objs]).

timing_ets_read(Tab) ->
    Objs = [{X,X} || X <- lists:seq(1,10000000)],
    timer:tc(?MODULE, ets_read, [Tab, Objs]).

ets_write(Tab, [Obj | Objs]) ->
    true = ets:insert(Tab, Obj),
    ets_write(Tab, Objs);
ets_write(_Tab, []) ->
    ok.

ets_read(Tab, [{K,D} | Objs]) ->
    [{K,D}] = ets:lookup(Tab, K),
    ets_read(Tab, Objs);
ets_read(_Tab, []) ->
    ok.
    
