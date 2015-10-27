%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc TPS Commons CSV
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(s2_csv).

-include("prelude.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BUFFER, 10240).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Public API
-export([ read_string_with/3, read_string_with/4
        , read_file_with/3, read_file_with/4
        , write_records/3, write_records_with/4
        , write_rows/2, write_rows_with/3
        , write_terms/2, write_terms_with/3
        , field/1, field/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% = read_file_with/read_string_with ===========================================

%% @doc Read a csv file and process each parsed row with the RowFun
read_file_with(IoDevice, RowFun, Opts) when is_pid(IoDevice) ->
  read_file_with(IoDevice, RowFun, [], Opts).

%% @doc Read a csv string and process each parsed row with the RowFun
read_string_with(String, RowFun, Opts) ->
  read_string_with(String, RowFun, [], Opts).

%% @doc Read a csv file and process each parsed row with the RowFun
%% and the initial state InitState
read_file_with(IoDevice, RowFun0, InitState0, Opts) when is_pid(IoDevice) ->
  POpts = get_parser_opts(Opts),
  {RowFun, InitState} = batch_fun(RowFun0, InitState0, Opts),
  PState = s2_csv_parser:init(POpts, RowFun, InitState),
  stream_from_file(IoDevice, PState);
read_file_with(File, RowFun0, InitState0, Opts) ->
  s2_fs:with_fd(
    File,
    fun(F) -> read_file_with(F, RowFun0, InitState0, Opts) end
   ).

%% @doc Read a csv string and process each parsed row with the RowFun
%% and the initial state InitState
read_string_with(String, RowFun, InitState, Opts) ->
  POpts = get_parser_opts(Opts),
  InitState = s2_csv_parser:init(POpts, RowFun, InitState),
  stream_from_string(String, InitState).

%% = write_records/write_records_with ==========================================

%% @doc Write records to file in specified format (csv or native)
write_records(IoDevice, Records, Opts)
  when is_pid(IoDevice) ->
    write_records_with(IoDevice, all_rows(Records), 0, Opts);
write_records(File, Records, Opts) ->
  s2_fs:with_fd(
    File,
    fun(F) -> write_records(F, Records, Opts) end
   ).

%% @doc Write records to file in specified format (csv or native)
%% provided by RecordFun and the initial state InitState
write_records_with(IoDevice, RecordFun, InitState, Opts)
  when is_pid(IoDevice) ->
    Record = s2_lists:assoc(Opts, record, []),
    Fields = s2_lists:assoc(Opts, fields, []),
    Format = s2_lists:assoc(Opts, format, csv),
    RowFun = fun(S) ->
                 {R, NS} = apply(RecordFun, [S]),
                 ?INFO("~p", [R]),
                 F = record_to_row(Record, Fields),
                 {lists:map(F, R), NS}
             end,
    TermFun = fun(S) -> apply(RecordFun, [S]) end,
    case Format of
        csv ->
            write_rows_with(IoDevice, RowFun, InitState);
        native ->
            write_terms_with(IoDevice, TermFun, InitState)
    end;
write_records_with(File, RecordFun, InitState, Opts) ->
  s2_fs:with_fd(
    File,
    fun(F) -> write_records_with(F, RecordFun, InitState, Opts) end
   ).

%% @doc Write rows to file using csv format
write_rows(IoDevice, Rows) when is_pid(IoDevice) ->
    write_rows_with(IoDevice, all_rows(Rows), 0);
write_rows(File, Rows) ->
  s2_fs:with_fd(
    File,
    fun(F) -> write_rows(F, Rows) end
   ).

%% @doc Write rows to file in csv format
%% provided by RowFun and the initial state InitState
write_rows_with(IoDevice, RowFun, InitState)
  when is_pid(IoDevice) andalso is_function(RowFun) ->
    {Rows, State} = apply(RowFun, [InitState]),
    case Rows of
        [] -> State;
        _ -> write_rows0(IoDevice, Rows),
             write_rows_with(IoDevice, RowFun, State)
    end;
write_rows_with(File, RowFun, InitState) ->
  s2_fs:with_fd(
    File,
    fun(F) -> write_rows_with(F, RowFun, InitState) end
   ).

%% @doc Write terms to file
write_terms(IoDevice, Terms) when is_pid(IoDevice) ->
    write_terms_with(IoDevice, all_rows(Terms), 0);
write_terms(File, Terms) ->
  s2_fs:with_fd(
    File,
    fun(F) -> write_terms(F, Terms) end
   ).

%% @doc Write terms to file provided by TermFun
%% and the initial state InitState
write_terms_with(IoDevice, TermFun, InitState)
  when is_pid(IoDevice) andalso is_function(TermFun) ->
    io:fwrite(IoDevice, "[~n", []),
    {Terms, State} = apply(TermFun, [InitState]),
    case Terms of
        [] -> io:fwrite(IoDevice, "~n].", []), State;
        _  -> write_terms0(IoDevice, Terms),
              write_terms_with(IoDevice, TermFun, State)
    end;
write_terms_with(File, TermFun, InitState) ->
  s2_fs:with_fd(
    File,
    fun(F) -> write_terms_with(F, TermFun, InitState) end
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

batch_fun(Fun, State, Opts) ->
  Batch = s2_lists:assoc(Opts, batch, 1),
  batch_fun(Batch, Fun, State, Opts).

batch_fun(1, Fun, State, _) ->
  {Fun, State};
batch_fun(B, Fun, State, _) ->
  BatchFun = 
  fun
    ({header, H}, {C, _, Rs, Acc0}) ->
      {C, H, Rs, Acc0};
    ({newline, R}, {C, H, Rs, Acc0}) when C==B ->
      Acc = apply(Fun, [H, [R | Rs], Acc0]),
      {0, H, [], Acc};
    ({newline, R}, {C, H, Rs, Acc}) ->
      {C + 1, H, [R | Rs], Acc};
    ({eof}, {_, H, Rs, Acc0}) ->
      apply(Fun, [H, Rs, Acc0])
  end,
  {BatchFun, {0, [], [], State}}.

get_parser_opts(Opts) ->
  Delimiter = s2_lists:assoc(Opts, delimiter, $,),
  Header = s2_lists:assoc(Opts, header, false),
  #{delimiter => Delimiter, header => Header}.

stream_from_string(String, InitState) ->
    StringIterator = fun(StringList) ->
        get_first_char(StringList)
    end,
    iterate_chars(StringIterator, String, InitState).

stream_from_file(IoDevice, InitState) ->
    IoDeviceIterator = fun(Io) ->
        {io:get_chars(Io, "", ?BUFFER), Io}
    end,
    iterate_chars(IoDeviceIterator, IoDevice, InitState).

iterate_chars(IteratorFun, Io, State) ->
    {FirstChars, UpdatedIo} = IteratorFun(Io),
    iterate_chars(IteratorFun, UpdatedIo, State, FirstChars).

iterate_chars(_, _, State, eof) ->
    s2_csv_parser:end_parsing(State);
iterate_chars(IteratorFun, Io, State, []) ->
    {Chars, UpdatedIo} = IteratorFun(Io),
    iterate_chars(IteratorFun, UpdatedIo, State, Chars);
iterate_chars(IteratorFun, Io, State, [Char | Chars]) ->
    UpdatedState = s2_csv_parser:parse_with_character(clean_char_argument(Char), State),
    iterate_chars(IteratorFun, Io, UpdatedState, Chars).

%% @doc make sure that an integer denoting a char is returned instead of a string
clean_char_argument([CharInt | _]) ->
    CharInt;
clean_char_argument(CharInt) when is_integer(CharInt) ->
    CharInt.

get_first_char([]) ->
    {eof, []};
get_first_char([FirstChar | Tail]) ->
    {FirstChar, Tail}.

%% @private
write_rows0(IoDevice, []) when is_pid(IoDevice) ->
    ok;
write_rows0(IoDevice, [Row | Rows]) when is_pid(IoDevice) ->
    write_row(IoDevice, Row),
    write_rows0(IoDevice, Rows);
write_rows0(IoDevice, Row) when is_pid(IoDevice) ->
    write_row(IoDevice, Row).

write_row(IoDevice, Row) ->
    file:write(IoDevice, row(Row)).

all_rows(Rows) ->
    fun(C) -> case C of
                  0 -> {Rows, length(Rows)};
                  _ -> {[], C}
              end
    end.

record_to_row([], _) ->
  fun(R) -> R end;
record_to_row(_, []) ->
    fun(R) -> tl(tuple_to_list(R)) end;
record_to_row(Record, Fields)
  when is_list(Record) andalso is_list(Fields) ->
    fun(R) ->
            L = tl(tuple_to_list(R)),
            lists:filtermap(filter_fields(Fields), L)
    end;
record_to_row(Record, Fields) ->
    record_to_row(Record:fields(), Fields).

filter_fields(Fields) ->
    fun({F,V}) ->
            case lists:member(F, Fields) of
                true -> {true, V};
                false -> false
            end
    end.

row(Row) when is_list(Row) ->
    L = lists:map(fun field/1, Row),
    {Fs, Vs} = lists:unzip(L),
    S0 = string:join(Fs, ","),
    S = string:sub_string(S0, 1, length(S0)),
    list_to_binary(io_lib:format(string:concat(S,"~n"), Vs)).

field(undefined) -> {"~s",""};
field([]) -> {"~s",""};
field(F) when is_integer(F) -> {"~B", F};
field(F) when is_float(F) -> field(F, 2);
field(F) when is_binary(F) -> {"~s", F};
field(F) when is_list(F) -> {"~s", F};
field({_,_,_} = F) -> {"~s", s2_date:format_iso8601(F)};
field({{_,_,_}, {_,_,_}} = F) -> {"~s", s2_date:format_iso8601(F)};
field(F) -> {"~w", F}.

field(F, N) when is_float(F) -> {"~." ++ integer_to_list(N) ++ "f", F};
field(F, _) when is_integer(F) -> field(F).

write_terms0(IoDevice, [H | T])
  when is_pid(IoDevice) andalso is_list(H) ->
    write_terms0(IoDevice, H),
    write_terms0(IoDevice, T);

write_terms0(IoDevice, [Term | Terms])
  when is_pid(IoDevice) ->
    write_term(IoDevice, Term),
    case Terms of
        [] -> ok;
        _ -> file:write(IoDevice, ",")
    end,
    io:fwrite(IoDevice, "~n", []),
    write_terms0(IoDevice, Terms);

write_terms0(_, []) -> 
    ok.

write_term(IoDevice, Term)
  when is_pid(IoDevice) andalso is_binary(Term) ->
    file:write(IoDevice, Term);

write_term(IoDevice, Term) 
  when is_pid(IoDevice) ->
    io:fwrite(IoDevice, "~p", [Term]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

read_file_with_test() ->
  GetRow = fun
             ({newline, Row}, {Header, Rows}) ->
               {Header, [Row | Rows]};
             ({header, Header}, {_, Rows}) ->
               {Header, Rows};
             ({eof}, HR) ->
               HR
           end,
  {ok, {Header, Rows}} = read_file_with(
        "../priv/sample.csv", 
        GetRow, 
        {[], []},
        [{header, true}]),
  19 = length(Header),
  2 = length(Rows).

read_batch_test() ->
  GetRows = fun(Header, Rows, {_, _, _}) ->
                {length(Rows), Header, Rows}
            end,
  {ok, {Counter, Header, Rows}} = read_file_with(
        "../priv/sample.csv", 
        GetRows, 
        {0, [], []},
        [{header, true}, {batch, 0}]),
  19 = length(Header),
  Counter = length(Rows).

-endif.
