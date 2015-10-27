%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Logging.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% = Header ===================================================================

-ifndef(__LOG_HRL).
-define(__LOG_HRL, true).

%% = Logging ==================================================================

-compile([{parse_transform, lager_transform}]).

-define(DEBUG(Fmt), lager:log(debug, self(), Fmt)).
-define(DEBUG(Fmt, Args), lager:log(debug, self(), Fmt, Args)).

-define(INFO(Fmt), ?INFO(Fmt, [])).
-define(INFO(Fmt, Args),
        (fun() ->
             lager:log(info, self(), Fmt, Args),
             __IO = global:whereis_name(global_io),
             if 
               __IO /= undefined 
               andalso node(__IO) /= node() -> 
                 io:format(__IO, Fmt++"~n", Args);
               true -> ok
             end
         end)()).

-define(WARNING(Fmt), lager:log(warning, self(), Fmt)).
-define(WARNING(Fmt, Args), lager:log(warning, self(), Fmt, Args)).

-define(ERROR(Fmt), lager:log(error, self(), Fmt)).
-define(ERROR(Fmt, Args), lager:log(error, self(), Fmt, Args)).

%% = Metrics ==================================================================

-define(TIME(Tag, Expr),
        (fun() ->
           %% NOTE: timer:tc/4 does an annoying 'catch' so we
           %% need to wrap the result in 'ok' to be able to
           %% detect an unhandled exception.
           {__TIME, __RESULT} =
           timer:tc(erlang, apply, [fun() -> {ok, Expr} end, []]),
           ?INFO("(~s): ~999p [~B ms]",
                 [?MODULE, Tag, trunc(__TIME/1000)]),
           case __RESULT of
             {ok, _}         -> element(2, __RESULT);
             {'EXIT', Error} -> exit(Error)
           end
         end)()).

-define(TIME(Expr),
        (fun() ->
           {__TIME, __RESULT} =
           timer:tc(erlang, apply, [fun() -> {ok, Expr} end, []]),
           case __RESULT of
             {ok, [__TAG | __RES]} ->
               ?INFO(lists:flatten(["(~s): ", __TAG, " [~B ms]"]),
                     lists:append([[?MODULE], __RES, [trunc(__TIME/1000)]])), 
               __RES;
             {'EXIT', Error} ->
               exit(Error)
           end
         end)()).

%% = Footer ===================================================================
-endif.


