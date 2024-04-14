-module(meadow_ffi).
-export([list_files/1, file_open/1, file_close/1, file_exists/1, dir_exists/1]).

list_files(Path) ->
    case file:list_dir(Path) of
        {ok, Files} ->
            {ok, Files};
        {error, enoent} ->
            {error, no_entry};
        {error, eacces} ->
            {error, no_access};
        _ ->
            {error, unknown_file_error}
    end.

dir_exists(Path) ->
    case filelib:is_dir(Path) of
        true ->
            true;
        false ->
            false
    end.

file_exists(Path) ->
    case filelib:is_file(Path) of
        true ->
            true;
        false ->
            false
    end.

file_open(Path) ->
  case file:open(Path, [raw]) of
    {ok, Fd} ->
      {ok, Fd};
    {error, enoent} ->
      {error, no_entry};
    {error, eacces} ->
      {error, no_access};
    {error, eisdir} ->
      {error, is_dir};
    _ ->
      {error, unknown_file_error}
  end.

file_close(File) ->
  case file:close(File) of
    ok ->
      {ok, nil};
    {error, enoent} ->
      {error, no_entry};
    {error, eacces} ->
      {error, no_access};
    {error, eisdir} ->
      {error, is_dir};
    _ ->
      {error, unknown_file_error}
  end.
