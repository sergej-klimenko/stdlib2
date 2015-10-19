%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Standard Erlang Prelude.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% = Header ===================================================================

-ifndef(__PRELUDE_HRL).
-define(__PRELUDE_HRL, true).

%% = Logging ===============================================================

-include("log.hrl").

%% = Assertions ===============================================================

-define(hence(A),
        (case A of
           true -> ok;
           _    -> throw({error, {assert, {??A, '=', true}, ?FILE, ?LINE}})
         end)).

-define(given(A, B),
        (case ((not (A)) orelse (B)) of
           true -> ok;
           _    -> throw({error, {assert, {??A, '->', ??B}, ?FILE, ?LINE}})
         end)).

%% = Casts ====================================================================

-define(a2l(X), erlang:atom_to_list(X)).
-define(l2a(X), erlang:list_to_atom(X)).
-define(b2l(X), erlang:binary_to_list(X)).
-define(l2b(X), erlang:list_to_binary(X)).
-define(b2t(X), erlang:binary_to_term(X)).
-define(t2b(X), erlang:term_to_binary(X)).
-define(i2l(X), erlang:integer_to_list(X)).
-define(l2i(X), erlang:list_to_integer(X)).
-define(l2t(X), erlang:list_to_tuple(X)).
-define(t2l(X), erlang:tuple_to_list(X)).

%% = Eccentric ================================================================

-define(lift(E),   s2_maybe:lift(fun() -> E end)).
-define(unlift(E), s2_maybe:unlift(fun() -> E end)).

-define(do(F0, F1),
        s2_maybe:do([F0, F1])).
-define(do(F0, F1, F2),
        s2_maybe:do([F0, F1, F2])).
-define(do(F0, F1, F2, F3),
        s2_maybe:do([F0, F1, F2, F3])).
-define(do(F0, F1, F2, F3, F4),
        s2_maybe:do([F0, F1, F2, F3, F4])).
-define(do(F0, F1, F2, F3, F4, F5),
        s2_maybe:do([F0, F1, F2, F3, F4, F5])).
-define(do(F0, F1, F2, F3, F4, F5, F6),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6])).
-define(do(F0, F1, F2, F3, F4, F5, F6, F7),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6, F7])).
-define(do(F0, F1, F2, F3, F4, F5, F6, F7, F8),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6, F7, F8])).
-define(do(F0, F1, F2, F3, F4, F5, F6, F7, F8, F9),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6, F7, F8, F9])).

-define(thunk(E0),
        fun() -> E0 end).
-define(thunk(E0, E1),
        fun() -> E0, E1 end).
-define(thunk(E0, E1, E2),
        fun() -> E0, E1, E2 end).
-define(thunk(E0, E1, E2, E3),
        fun() -> E0, E1, E2, E3 end).
-define(thunk(E0, E1, E2, E3, E4),
        fun() -> E0, E1, E2, E3, E4 end).
-define(thunk(E0, E1, E2, E3, E4, E5),
        fun() -> E0, E1, E2, E3, E4, E5 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6),
        fun() -> E0, E1, E2, E3, E4, E5, E6 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6, E7),
        fun() -> E0, E1, E2, E3, E4, E5, E6, E7 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6, E7, E8),
        fun() -> E0, E1, E2, E3, E4, E5, E6, E7, E8 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9),
        fun() -> E0, E1, E2, E3, E4, E5, E6, E7, E8, E9 end).

%% = Guards ===================================================================

-define(is_string(X),
        (((X) =:= "") orelse (is_list(X) andalso is_integer(hd(X))))).

-define(is_thunk(X), is_function(X, 0)).

%% = Types ====================================================================

-type alist(A, B) :: [{A, B}].
-type fd()        :: file:io_device().
-type file()      :: string().
-type thunk(A)    :: fun(() -> A).

-type ok(A)       :: {ok, A}.
-type error(A)    :: {error, A}.
-type maybe(A, B) :: {ok, A} | {error, B}.
-type whynot(B)   :: ok | {error, B}.

%% = Footer ===================================================================
-endif.

