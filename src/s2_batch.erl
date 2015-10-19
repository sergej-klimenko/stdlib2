%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Batches
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(s2_batch).

-include("prelude.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([ foldl/4
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foldl(Fun, Acc0, List, Opts) when is_list(List) ->
  BatchFun = batch(Fun, Opts),
  {_, Xs, Acc} = lists:foldl(BatchFun, {1, [], Acc0}, List),
  case Xs of
    [] -> Acc;
    _  -> apply(Fun, Xs, Acc)
  end;
foldl(Fun, Acc0, Table, Opts) when is_atom(Table) ->
  BatchFun = batch(Fun, Opts),
  {_, Xs, Acc} = 
    mnesia:activity(
      transaction,
      ?thunk(mnesia:foldl(BatchFun, {1, [], Acc0}, Table))
     ),
  case Xs of
    [] -> Acc;
    _  -> apply(Fun, Xs, Acc)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

batch(Fun, Opts) ->
  B = s2_lists:assoc(Opts, batch, 0),
  fun
    (X, {C, Xs, Acc0}) when C==B ->
      Acc = apply(Fun, [[X | Xs], Acc0]),
      {0, [], Acc};
    (X, {C, Xs, Acc0}) ->
      {C + 1, [X | Xs], Acc0}
  end.
