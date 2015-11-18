%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Money.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(s2_money).

-include("prelude.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([ sub/2
        , add/2
        , is_zero/1
        , mult/2
        , divide/2
        , to_money/1
        , minus/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_money(Value) ->
  Opts = #{precision => 2, rounding => round_down},
  s2_decimal:to_decimal(Value, Opts).

sub(A, B) ->
  D = s2_decimal:sub(to_money(A), to_money(B)),
  erlang:binary_to_float(s2_decimal:to_binary(D)).

add(A, B) ->
  D = s2_decimal:add(to_money(A), to_money(B)),
  erlang:binary_to_float(s2_decimal:to_binary(D)).

mult(A, B) ->
  D = s2_decimal:mult(to_money(A), to_money(B)),
  erlang:binary_to_float(s2_decimal:to_binary(D)).

divide(A, B) ->
  Opts = #{precision => 2, rounding => round_down},
  D = s2_decimal:divide(to_money(A), to_money(B), Opts),
  erlang:binary_to_float(s2_decimal:to_binary(D)).

is_zero(A) ->
  s2_decimal:is_zero(to_money(A)).

minus(A) ->
  B = s2_decimal:minus(to_money(A)),
  erlang:binary_to_float(s2_decimal:to_binary(B)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-endif.

