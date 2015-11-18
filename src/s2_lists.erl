%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Lists.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(s2_lists).

-include("prelude.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Quantifiers
-export([ all/1
        , exists/1
        ]).

%% Alists
-export([ assoc/2
        , assoc/3
        , dissoc/2
        , assoc_update/3
        , assoc_update/4
        , keyfilter/2
        , multikeyfilter/2
        ]).

%% Sublists
-export([ butlast/1
        , drop/2
        , take/2
        , partition/2
        , pickwith/2
        ]).

%% Constructors
-export([ cons/1
        , cons/2
        , ensure/1
        , repeatedly/2
        ]).

%% Misc
-export([ intersperse/2
        , join/1
        , numbered/1
        , dsort/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% = Quantifiers ==============================================================

-spec all(Xs::[_])    -> boolean().
%% @doc Indicate whether Xs contains only 'true'.
all([])               -> true;
all([true|Xs])        -> all(Xs);
all(_)                -> false.

-spec exists(Xs::[_]) -> boolean().
%% @doc Indicate whether Xs contains at least one 'true'.
exists([])            -> false;
exists([true|_])      -> true;
exists([_|Xs])        -> exists(Xs).

quantifier_test() ->
  false = all([true, false, true]),
  true  = exists([false, true]),
  false = exists([]).

%% = Alists ===================================================================

-spec assoc(alist(A, B), A) -> maybe(B, notfound).
%% @doc assoc(KVs, K) is the value associated with K in KVs.
assoc(L, K) ->
  case lists:keyfind(K, 1, L) of
    {K, V} -> {ok, V};
    false  -> {error, notfound}
  end.

assoc2_test() ->
  {ok, bar}         = assoc([{baz, quux}, {foo, bar}], foo),
  {error, notfound} = assoc([],                        foo).

-spec assoc(alist(A, B), A, B) -> B.
%% @doc assoc(KVs, K, Def) is the value associated with K in KVs or Def.
assoc(KVs, K, Def) ->
  case assoc(KVs, K) of
    {ok, V}           -> V;
    {error, notfound} -> Def
  end.

assoc3_test() ->
  bar = assoc([{foo, bar}], foo, bar),
  bar = assoc([],           foo, bar).

-spec dissoc(alist(A, _), A) -> alist(A, _).
%% @doc dissoc(KVs, K) is KVs with all K-entries removed.
dissoc(KVs, K) -> lists:filter(fun({Key, _Val}) -> Key =/= K end, KVs).

dissoc_test() ->
  [{bar, 1}, {baz, 3}] = dissoc([{foo, 0}, {bar, 1}, {foo, 2}, {baz, 3}], foo).

-spec assoc_update(A, fun((B) -> B), alist(A, B)) -> alist(A, B).
%% @doc Replace the newest Val associated with Key by F(Val).
assoc_update(Key, F, [{Key, Val}|Rest]) ->
  [{Key, F(Val)}|Rest];
assoc_update(Key, F, [First|Rest]) ->
  [First|assoc_update(Key, F, Rest)].

assoc_update3_test() ->
  Alist = [{foo,1},{bar,2},{baz,3},{bar,0}],
  [{foo,1},{bar,3},{baz,3},{bar,0}] =
    assoc_update(bar, fun(X) -> X + 1 end, Alist).

-spec assoc_update(A, fun((B) -> B), B, alist(A, B)) -> alist(A, B).
%% @doc Replace the newest Val associated with Key by F(Val). Associate Key
%% with F(Default) if there's no Val.
assoc_update(Key, F, _Default, [{Key,Val}|Rest]) ->
  [{Key, F(Val)}|Rest];
assoc_update(Key, F, Default, [First|Rest]) ->
  [First|assoc_update(Key, F, Default, Rest)];
assoc_update(Key, F, Default, []) ->
  [{Key, F(Default)}].

assoc_update4_test() ->
  [{foo,1}]          = assoc_update(foo, fun(X) -> X + 1 end, 0, []),
  [{foo,1}, {bar,2}] = assoc_update(bar, fun(X) -> X + 1 end, 0, [{foo,1}, {bar,1}]).

-spec keyfilter(A, alist(A, _)) -> alist(A, _).
%% @doc Return all elements of Alist with a key equal to K1.
keyfilter(K1, Alist) -> lists:filter(fun({K2, _V}) -> K1 =:= K2 end, Alist).

keyfilter_test() -> [{foo, bar}] = keyfilter(foo, [{foo, bar}]).

-spec multikeyfilter([A], alist(A, _)) -> alist(A, _).
%% @doc Return all elements of Alist with a key in Ks.
multikeyfilter(Ks, Alist) ->
  lists:filter(fun({K, _V}) -> lists:member(K, Ks) end, Alist).

multikeyfilter_test() -> [{foo, bar}] = multikeyfilter([foo], [{foo, bar}]).

%% = Sublists =================================================================

-spec butlast([A,...]) -> [A].
%% @doc butlast(Xs) is Xs with its last element removed.
butlast([_])           -> [];
butlast([X|Xs])        -> [X|butlast(Xs)].

butlast_test()         -> [foo] = butlast([foo, bar]).

-spec drop(integer(), [_]) -> [_].
%% @doc drop(N, Xs) is the Nth tail of Xs (empty if Xs has fewer than N
%% elements).
drop(N, Xs) when N =< 0    -> Xs;
drop(_, [])                -> [];
drop(N, [_|Xs])            -> drop(N - 1, Xs).

drop_test() ->
  [baz] = drop(2, [foo, bar, baz]),
  []    = drop(2, []).

-spec take(integer(), [_]) -> [_].
%% @doc take(N, Xs) is a list containing the first N elements of Xs
%% (Xs if Xs has fewer than N elements).
take(N, _) when N =< 0     -> [];
take(_, [])                -> [];
take(N, [X|Xs])            -> [X|take(N - 1, Xs)].

take_test() ->
  [1, 2] = take(2, [1, 2, 3]),
  [1]    = take(2, [1]).

-spec partition(pos_integer(), [_]) -> [[_]] | [].
%% @doc partition(N, Xs) is a list of N-partitions of Xs.
partition(N, Xs)
  when is_integer(N)
     , N > 0
     , is_list(Xs) ->
  Len = length(Xs),
  case {Len > 0, Len > N} of
    {true,  true } -> [take(N, Xs)|partition(N, drop(N, Xs))];
    {true,  false} -> [Xs];
    {false, false} -> []
  end.

partition_test() ->
  []               = partition(2, []),
  [[1]]            = partition(2, [1]),
  [[1, 2]]         = partition(2, [1, 2]),
  [[1, 2], [3]]    = partition(2, [1, 2, 3]),
  [[1, 2], [3, 4]] = partition(2, [1, 2, 3, 4]).

-spec pickwith(fun((A) -> boolean()), list(A)) -> {A, [A]} | false.
%% @doc Try to find an element satisfying Pred in List. Returns {Match, Other}
%%      where
%%      - Match is the first element in List for which Pred returned true.
%%      - Other is List with Match removed.
%%
%%      Returns false if no element in List satisfies Pred.
pickwith(_Pred, List) ->
  pickwith(_Pred, List, []).
pickwith(_Pred, [], _Acc) ->
  false;
pickwith( Pred, [H|T], Acc) ->
  case Pred(H) of
    false -> pickwith(Pred, T, [H|Acc]);
    true  -> {H, lists:reverse(Acc) ++ T}
  end.

pickwith_test_() ->
   F = fun(E) -> E =:= a end,
   [ ?_assertEqual(false,             pickwith(F, []))
   , ?_assertEqual(false,             pickwith(F, [b]))
   , ?_assertEqual({a, []},           pickwith(F, [a]))
   , ?_assertEqual({a, [b]},          pickwith(F, [a, b]))
   , ?_assertEqual({a, [b]},          pickwith(F, [b, a]))
   , ?_assertEqual({a, [b, c, d, e]}, pickwith(F, [b, c, a, d, e]))
   ].

%% = Constructors =============================================================

-spec cons(_, _) -> maybe_improper_list().
%% @doc cons(Car, Cdr) is Car consed unto Cdr.
cons(Car, Cdr)   -> [Car|Cdr].
cons(Car)        -> fun(Cdr) -> cons(Car, Cdr) end.

cons_test()      -> [foo, bar|baz] = (cons(foo))(cons(bar, baz)).

-spec ensure(_)           -> [_].
%% @doc Return a list.
ensure(X) when is_list(X) -> X;
ensure(X)                 -> [X].

ensure_test() ->
  []   = ensure([]),
  [42] = ensure(42).

-spec repeatedly(non_neg_integer(), fun(() -> A)) -> [A].
%% @doc repeatedly(N, F) is a list of the results of N calls to F.
repeatedly(N, F) when is_integer(N) , N > 0 -> [F()|repeatedly(N - 1, F)];
repeatedly(0, F) when is_function(F, 0)     -> [].

repeatedly_test() -> [foo, foo] = repeatedly(2, fun() -> foo end).

%% = Misc =====================================================================

-spec intersperse(_, [_]) -> [_].
%% @doc intersperse(X, Ys) is Ys with X interspersed.
intersperse(X, Ys) ->
  lists:reverse(tl(lists:foldl(fun(Y, Acc) -> [X, Y|Acc] end, [], Ys))).

intersperse_test() -> "f o o" = intersperse($ , "foo").

-spec join([[A]]) -> [A].
%% @doc Shallow flatten/1.
join([])         -> [];
join([X|Xs])     -> join(X, Xs).

join([],     Ys) -> join(Ys);
join([X|Xs], Ys) -> [X|join(Xs, Ys)].

join_test()      -> [1,2,[3],4,5] = join([[1,2,[3]],[4,5]]).

-spec numbered([_]) -> [{pos_integer(), _}].
%% @doc Return Xs with its elements numbered from 1.
numbered(Xs)        -> lists:zip(lists:seq(length(Xs)), Xs).

numbered_test() ->
  []                   = numbered([]),
  [{1, foo}, {2, bar}] = numbered([foo, bar]).

-spec dsort(list()) -> list().
%% @doc Sort a list and its list and proplist value elements.
dsort(L)                              -> lists:sort(dsort_i(L)).
dsort_i([])                           -> [];
dsort_i([{K, V} | T]) when is_list(V) -> [{K, lists:sort(dsort_i(V))} | dsort_i(T)];
dsort_i([H | T]) when is_list(H)      -> [lists:sort(dsort_i(H)) | dsort_i(T)];
dsort_i([H | T])                      -> [H | dsort_i(T)].

dsort_test() ->
  [] = dsort([]),
  [a, b] = dsort([b, a]),
  [c, [a, b]] = dsort([[b, a], c]),
  [{a, x}, {b, y}] = dsort([{b, y}, {a, x}]),
  [d, {c, [{a, x}, {b, y}]}] = dsort([{c, [{b, y}, {a, x}]}, d]).

-spec is_permutation([A], [A]) -> boolean().
%% @doc is_permutation(Xs, Ys) is true iff Xs is a permutation of Ys.
is_permutation(Xs, Ys) -> lists:sort(Xs) =:= lists:sort(Ys).

is_permutation_test() ->
  true  = is_permutation([foo, bar, baz], [baz, bar, foo]),
  false = is_permutation([foo, bar, baz], [baz, bar, quux]).

