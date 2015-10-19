%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Loops.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(s2_loop).

-include("prelude.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([ for/2
        , for/3
        , retry/1
        , retry/2
        , retry/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec for(non_neg_integer(),
          fun(() -> _) | fun((non_neg_integer()) -> _)) -> ok.
%% @doc for(N, F) causes F to be called N times.
for(N, F) ->
  for(0, N, F).
for(N, N, _F) ->
  ok;
for(I, N, F) ->
  if is_function(F, 0) -> _ = F();
     is_function(F, 1) -> _ = F(I)
  end,
  for(I + 1, N, F).

-spec retry(fun()) -> maybe(_, _).
%% @doc Call F every T milliseconds until it returns ok.
%% Abort after N retries unless N is infinity.
%% Note that F is called at least once.
retry(F) ->
  retry(F, timer:seconds(1)).
retry(F, T) ->
  retry(F, T, infinity).
retry(F, T, N) ->
  retry(s2_maybe:lift(F), F, T, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retry({ok, _} = Ok, _F, _T, _N) ->
  Ok;
retry({error, _} = Err, _F, _T, 0) ->
  Err;
retry({error, _}, F, T, N) when N > 0 ->
  timer:sleep(T),
  retry(s2_maybe:lift(F), F, T, dec(N)).

dec(infinity) -> infinity;
dec(N)        -> N-1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

for_test() ->
  for(3, fun()  -> io:format(user, "foo~n",   [])  end),
  for(3, fun(I) -> io:format(user, "foo~p~n", [I]) end).

retry_test() ->
  F = ?thunk(receive foo -> self() ! bar
             after   0   -> self() ! foo, {error, foo}
             end),
  G = ?thunk(receive bar -> ok
             after   0   -> throw(exn)
             end),
  {ok, bar} = retry(F),
  G(),
  {error, foo} = retry(F, 1, 0),
  exn = (catch G()).

-endif.
