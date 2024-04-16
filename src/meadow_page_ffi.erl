-module(lib).
-export([read_meadow_page/2]).
-nifs([read_meadow_page/2]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("ext/meadow_pagelib", 0).

read_meadow_page(Path, boolean) ->
    meadow_pagelib:create_meadow_page(Path, boolean).
