-module(lib).
-export([create_meadowfile/2]).
-nifs([create_meadowfile/2]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("ext/liblib", 0).

create_meadowfile(Path, boolean) ->
    liblib:create_meadowfile(Path, boolean).
