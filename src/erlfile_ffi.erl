-module(erlfile_ffi).
-export([list_files/1, file_open/1, file_close/1, file_exists/1, dir_exists/1, cwd/0, is_text_file/1]).

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
        true -> true;
        false -> false
    end.

file_exists(Path) ->
    case filelib:is_file(Path) of
        true -> true;
        false -> false
    end.

is_text_file(Path) ->
    {ok, Binary} = file:read_file(Path),
    case is_text(Binary) of
        true -> true;
        false -> false
    end.

is_text(Binary) ->
    <<FirstChunk:100/binary, _/binary>> = Binary,
    case contains_non_printable(FirstChunk) of
        true -> false;
        false -> true
    end.

contains_non_printable(Chunk) ->
    case re:run(Chunk, "[^[:print:]\t]") of
        {match, _} -> true;
        nomatch -> false
    end.

% Check if the file is in Macintosh BinHex encoding format
is_mac_binhex(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    case is_mac_binhex_binary(Binary) of
        true -> io:format("~s is in Macintosh BinHex encoding format.~n", [FileName]);
        false -> io:format("~s is not in Macintosh BinHex encoding format.~n", [FileName])
    end.

is_mac_binhex_binary(Binary) ->
    % BinHex files start with the ASCII characters ": (Colon) H (Capital H)"
    <<":H", _/binary>> = Binary,
    % Check if the file has a valid BinHex header
    case is_binhex_header(Binary) of
        true -> true;
        false -> false
    end.

is_binhex_header(Binary) ->
    % BinHex header contains information like file type, creator application, etc.
    % We can check for certain patterns to determine if it's a valid header
    <<_:4/binary, Version:4/binary, _:2/binary, Len:4/binary, _:2/binary, _/binary>> = Binary,
    % Check if the header version is valid (typically it's "1")
    case Version of
        <<1:4>> -> true;
        _ -> false
    end.

% Check if the file is in DOS binary format
is_dos_binary(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    case is_dos_binary_binary(Binary) of
        true -> io:format("~s is in DOS binary format.~n", [FileName]);
        false -> io:format("~s is not in DOS binary format.~n", [FileName])
    end.

is_dos_binary_binary(Binary) ->
    % DOS binary files typically start with certain patterns
    % For simplicity, we'll check if the first two bytes are "MZ" (an identifier for DOS executable files)
    <<77, 90, _/binary>> = Binary, % MZ in ASCII
    true.

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

cwd() ->
    case file:get_cwd() of
        {ok, Path} ->
            {ok, Path};
        {error, enoent} ->
            {error, no_entry};
        {error, eacces} ->
            {error, no_access};
        _ ->
            {error, unknown_file_error}
    end.
