-module(btree_lru).

-export([create/1,
	 close/1,
	 register_pid/2,
	 unregister_pid/1,
	 get_registered_pid/1,
	 set_max_size/2,
	 get_max_size/1,
	 get_size/1,
	 write/2,
	 write/3,
	 read/2,
	 next/2,
	 prev/2,
	 remove/2,
	 seek/2,
	 iterate_next/2,
	 oldest/1,
	 latest/1,
	 last/1,
	 first/1]).



-on_load(init/0).

init() ->
    Dir = "../priv",
    PrivDir = 
        case code:priv_dir(?MODULE) of
            {error, _} ->
                case code:which(?MODULE) of
                    Filename when is_list(Filename) ->
                        filename:join([filename:dirname(Filename), Dir]);
                    _ ->
                        Dir
                end;
            Path -> Path
        end,
    Lib = filename:join(PrivDir, "btreelru_nif"),
    erlang:load_nif(Lib, 0).

write(Tab, {Key, Value}) ->
    write(Tab, Key, Value).

create(_Maxsize) ->
    erlang:nif_error(nif_library_not_loaded).

register_pid(_Tab, _Pid) ->
    erlang:nif_error(nif_library_not_loaded).

unregister_pid(_Tab) ->
    erlang:nif_error(nif_library_not_loaded).

get_registered_pid(_Tab) ->
    erlang:nif_error(nif_library_not_loaded).

set_max_size(_Tab, _MaxSize) ->
    erlang:nif_error(nif_library_not_loaded).

get_max_size(_Tab) ->
    erlang:nif_error(nif_library_not_loaded).

get_size(_Tab) ->
    erlang:nif_error(nif_library_not_loaded).

write(_Tab, _Key, _Value) ->
    erlang:nif_error(nif_library_not_loaded).

read(_Tab, _Key) ->
    erlang:nif_error(nif_library_not_loaded).

next(_Tab, _Key) ->
    erlang:nif_error(nif_library_not_loaded).

prev(_Tab, _Key) ->
    erlang:nif_error(nif_library_not_loaded).

remove(_Tab, _Key) ->
    erlang:nif_error(nif_library_not_loaded).

seek(_Tab, _Key) ->
    erlang:nif_error(nif_library_not_loaded).

iterate_next(_Tab, _It) ->
    erlang:nif_error(nif_library_not_loaded).

oldest(_Tab) ->
    erlang:nif_error(nif_library_not_loaded).

latest(_Tab) ->
    erlang:nif_error(nif_library_not_loaded).

close(_Tab) ->
    erlang:nif_error(nif_library_not_loaded).

last(_Tab) ->
    erlang:nif_error(nif_library_not_loaded).

first(_Tab) ->
    erlang:nif_error(nif_library_not_loaded).
