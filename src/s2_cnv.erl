%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Convert.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(s2_cnv).

-include("prelude.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([to_integer/1,
         to_integer/2,
         to_float/1,
         to_float/2,
         to_number/1,
         to_list/1,
         to_binary/1,
         to_atom/1,
         to_boolean/1,
         is_true/1,
         is_false/1,
         is_null/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%% Automatic conversion of a term into integer type.  The conversion
%% will round a float value if nonstrict is specified otherwise badarg
-spec to_integer(string() | binary() | integer() | float()) -> integer().
to_integer(undefined) -> undefined;
to_integer(X) -> to_integer(X, nonstrict).

-spec to_integer(string() | binary() | integer() | float(),
                 strict | nonstrict) -> integer().
to_integer(X, strict) when is_float(X) -> error(badarg);
to_integer(X, nonstrict) when is_float(X) -> round(X);
to_integer(X, S) when erlang:is_binary(X) ->
  to_integer(binary_to_list(X), S);
to_integer(X, S) when is_list(X) ->
  try list_to_integer(X) of
    Result -> Result
  catch
    error:badarg when S =:= nonstrict ->
      round(list_to_float(X))
  end;
to_integer(X, _) when erlang:is_integer(X) -> X.

%% @doc
%% Automatic conversion of a term into float type. badarg if strict
%% is defined and an integer value is passed.
-spec to_float(string() | binary() | integer() | float()) -> float().
to_float(X) -> to_float(X, nonstrict).

-spec to_float(string() | binary() | integer() | float(),
               strict | nonstrict) -> float().
to_float(X, S) when is_binary(X) ->
  to_float(erlang:binary_to_list(X), S);
to_float(X, S) when is_list(X) ->
  try list_to_float(X) of
    Result -> Result
  catch
    error:badarg when S =:= nonstrict ->
      list_to_integer(X) * 1.0
  end;
to_float(X, strict) when is_integer(X) ->  error(badarg);
to_float(X, nonstrict) when is_integer(X) -> X * 1.0;
to_float(X, _) when is_float(X) -> X.

%% @doc
%% Automatic conversion of a term into number type.
-spec to_number(binary() | string() | number()) -> number().
to_number(undefined) -> undefined;
to_number(X) when erlang:is_number(X) -> X;
to_number(X) when erlang:is_binary(X) -> to_number(to_list(X));
to_number(X) when erlang:is_list(X) ->
  try list_to_integer(X) of
    Int -> Int
  catch
    error:badarg -> list_to_float(X)
  end.

-spec to_list(atom() | list() | binary() | integer() | float()) -> list().
%% @doc to_list(X) is the list-representation of X.
to_list(undefined) -> [];
to_list(<<>>) -> [];
to_list(X) when is_atom(X)    -> ?a2l(X);
to_list(X) when is_binary(X)  -> ?b2l(X);
to_list(X) when is_integer(X) -> ?i2l(X);
to_list(X) when is_list(X)    -> X;
to_list(X) when is_pid(X)     -> pid_to_list(X);
to_list(X) when is_tuple(X)   -> ?t2l(X).

%% @doc
%% Known limitations:
%%   Converting [256 | _], lists with integers > 255
-spec to_binary(atom() | string() | binary() | integer() | float()) -> binary().
to_binary(undefined) -> <<>>;
to_binary(X) when is_float(X) -> to_binary(to_list(X));
to_binary(X) when is_integer(X) -> iolist_to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) -> iolist_to_binary(X);
to_binary(X) when is_binary(X) -> X.

-spec to_boolean(binary() | string() | atom()) -> boolean().
to_boolean(<<"true">>)  -> true;
to_boolean("true")      -> true;
to_boolean(true)        -> true;
to_boolean(<<"false">>) -> false;
to_boolean("false")     -> false;
to_boolean(false)       -> false.

-spec is_true(binary() | string() | atom()) -> boolean().
is_true(<<"true">>) -> true;
is_true("true")     -> true;
is_true(true)       -> true;
is_true(_)          -> false.

-spec is_false(binary() | string() | atom()) -> boolean().
is_false(<<"false">>) -> true;
is_false("false")     -> true;
is_false(false)       -> true;
is_false(_)           -> false.

%% @doc
%% Automation conversion a term to an existing atom. badarg is
%% returned if the atom doesn't exist. the safer version, won't let
%% you leak atoms
-spec to_atom(atom() | list() | binary() | integer() | float()) -> atom().
to_atom(<<>>)              -> undefined;
to_atom([])                -> undefined;
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_atom(X);
to_atom(X)                 -> to_atom(to_list(X)).

is_null(undefined, X) -> X;
is_null(<<>>, X) -> X;
is_null(X, _) -> X.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_integer_test() ->
    ?assertError(badarg, to_integer(1.5, strict)).

to_float_test() ->
    ?assertError(badarg, to_float(10, strict)).

to_atom_test() ->
    ?assertMatch(true, to_atom("true")),
    ?assertMatch(true, to_atom(<<"true">>)),
    ?assertMatch(false, to_atom(<<"false">>)),
    ?assertMatch(false, to_atom(false)),
    ?assertError(badarg, to_atom("hello_foo_bar_baz")),

    S = erlang:list_to_atom("1"),
    ?assertMatch(S, to_atom(1)).

to_boolean_test()->
    ?assertMatch(true, to_boolean(<<"true">>)),
    ?assertMatch(true, to_boolean("true")),
    ?assertMatch(true, to_boolean(true)),
    ?assertMatch(false, to_boolean(<<"false">>)),
    ?assertMatch(false, to_boolean("false")),
    ?assertMatch(false, to_boolean(false)).

to_list_test() ->
  "atom"     = to_list(atom),
  "bin"      = to_list(<<"bin">>),
  []         = to_list(<<>>),
  "42"       = to_list(42),
  []         = to_list([]),
  "<" ++ _   = to_list(self()),
  [foo, bar] = to_list({foo, bar}).

-endif.
