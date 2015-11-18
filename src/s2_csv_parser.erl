%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Raw CSV Parser.
%%%
%%% This parser is based on the blog post written by Andy Till located
%%% here http://andrewtill.blogspot.com/2009/12/erlang-csv-parser.html.
%%%
%%% This parser supports well formed csv files which are
%%% - a set of lines ending with a \n
%%% - each line contains a set of fields separated with a comma (,)
%%% - each field value can be enclosed with double quote (") ONLY
%%% - each field value can be empty
%%%
%%% Please note:
%%% - This parser has no failsafe mechanism if the file is badly formed!
%%%   But the line a,,,,,\n is perfectly fine.
%%% - This parser doesn't allow a return (\n) in a field value!
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(s2_csv_parser).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([ init/2 
        , init/3
        , parse_with_character/2
        , end_parsing/1
        ]).

%% Types
-export_type([ opts/0
             ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type opts() :: #{
        delimiter => char(),
        header => boolean()
       }.

-type state() :: ready | in_quotes | skip_to_delimiter | eof.

-record(pstate, {
          state         = ready :: state(),
          current_line  = []    :: string(),
          current_value = []    :: string(),
          opts                  :: opts(),
          process_fun           :: function(),
          process_fun_state     :: term()
         }).

-type pstate() :: #pstate{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Opts, ProcessingFun) ->
  init(Opts, ProcessingFun, []).

init(Opts, ProcessingFun, ProcessingFunInitState) ->
  #pstate{
     opts = Opts,
     process_fun = ProcessingFun,
     process_fun_state = ProcessingFunInitState
    }.

parse_with_character(Character, State) when is_integer(Character) ->
  parse_with({char, Character}, State).

end_parsing(State) ->
  FinalState = parse_with({eof}, State),
  {ok, FinalState#pstate.process_fun_state}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_with(Input, #pstate{state = ready} = State) ->
  do_ready(Input, State);
parse_with(Input, #pstate{state = in_quotes} = State) ->
  do_in_quotes(Input, State);
parse_with(Input, #pstate{state = skip_to_delimiter} = State) ->
  do_skip_to_delimiter(Input, State);
parse_with(_, #pstate{state = eof} = State) ->
  State;
parse_with(_, _) ->
  throw({error, wrong_state}).

do_ready({eof}, State) ->
  do_eof(State);
do_ready({char, Char}, State) when (Char == $") ->
  % pass an empty string to in_quotes as we do not want the
  % preceeding characters to be included, only those in quotes
  State#pstate{state = in_quotes, current_value = []};
do_ready({char, Char},
         #pstate{opts = #{delimiter := Delimiter}} = State)
  when (Char == Delimiter) ->
  #pstate{
     current_line = CurrentLine,
     current_value = CurrentValue
    } = State,  
  State#pstate{
    current_line = [lists:reverse(CurrentValue) | CurrentLine],
    current_value = []
   };
do_ready({char, Char}, State) when (Char == $\n) ->
  % a new line has been parsed: time to send it back
  do_new_line(State);
do_ready({char, Char}, State) when (Char == $\r) ->
  % ignore line feed characters
  State;
do_ready({char, Char}, State) ->
  #pstate{current_value = CurrentValue} = State,  
  State#pstate{current_value = [Char | CurrentValue]}.

do_in_quotes({eof}, State) ->
  do_eof(State);
do_in_quotes({char, $"}, State) ->
  #pstate{
     current_line = CurrentLine,
     current_value = CurrentValue
    } = State,
  State#pstate{
    state = skip_to_delimiter,
    current_line = [lists:reverse(CurrentValue) | CurrentLine],
    current_value = []
   };
do_in_quotes({char, Char}, State) ->
  #pstate{current_value = CurrentValue} = State,
  State#pstate{current_value = [Char | CurrentValue]}.

do_skip_to_delimiter({eof}, State) ->
  do_eof(State);
do_skip_to_delimiter({char, Char},
                     #pstate{opts = #{delimiter := Delimiter}} = State)
  when (Char == Delimiter) ->
  State#pstate{state = ready, current_value = []};
do_skip_to_delimiter({char, Char}, State) when (Char == $\n) ->
  % a new line has been parsed: time to send it back
  do_new_line(State);
do_skip_to_delimiter({char, _}, State) ->
  State.

do_new_line(State) ->
  #pstate{
     opts = Opts,
     current_line = CurrentLine,
     current_value = CurrentValue,
     process_fun = ProcessingFun,
     process_fun_state = ProcessingFunState,
     state = PState
    } = State,
  NewLine = case PState of
                  skip_to_delimiter -> lists:reverse(CurrentLine);
                  _  -> lists:reverse([lists:reverse(CurrentValue) | CurrentLine])
            end,
  {UpdatedOpts, UpdatedProcessingFunState} =
      process_new_line(ProcessingFun, NewLine, ProcessingFunState, Opts),
  State#pstate{
    current_line = [],
    current_value = [],
    opts = UpdatedOpts,
    process_fun_state = UpdatedProcessingFunState
   }.

do_eof(State) ->
  UpdatedState = do_new_line(State),
  ProcessingFun = UpdatedState#pstate.process_fun,
  UpdatedProcessingFunState =
      ProcessingFun({eof}, UpdatedState#pstate.process_fun_state),
  UpdatedState#pstate{
    state = eof,
    process_fun_state = UpdatedProcessingFunState
   }.

process_new_line(_, [], State, Opts) ->
  % ignore empty lines
  {Opts, State};
process_new_line(ProcessingFun, NewLine, State, #{header := true} = Opts) ->
  UpdatedState = ProcessingFun({header, NewLine}, State),
  {Opts#{header := false}, UpdatedState};
process_new_line(ProcessingFun, NewLine, State, Opts) ->
  UpdatedState = ProcessingFun({newline, NewLine}, State),
  {Opts, UpdatedState}.
