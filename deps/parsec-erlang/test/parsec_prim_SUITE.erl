-module(parsec_prim_SUITE).

-compile({parse_transform, import_enhancements}).

-import(parsec_prim).

-include("ct.hrl").

% ct:run("test", [_SUITE]).

-define(not_implemented, erlang:error('not_implemented')).

-compile(export_all).

-include_lib("parsec/include/record_state.hrl").

all() -> [
		label__0,
		token__0,
		token__1,
		token__2,
		token__3,
		token__4,
		token__5,
		tokens__0,
		tokens__1,
		tokens__2,
		tokens__3,
		tokens__4,
		tokens__5,
		tokens__6,
		tokens__7,
		tokens__8,
		tokens__9
	].


returnVal({_, {ok, Val, _, _}}) -> Val.
remainingInput({_, {ok, _, {state, Input, _, _}, _}}) -> Input.
error({_, {error, Err}}) -> parsec_error:show(Err).

pos({_, {ok, _, {state, _, Pos, _}, _}}) -> Pos;
pos({_, {error, {parse_error, Pos, _}}}) -> Pos.


label__0(_) ->
	Parser = label(label(pzero(), "L2"), "L1"),
	State = #state{},
	Result = runP(Parser, State),
	"(line 1, column 1):\nexpecting L1" = error(Result).


token__0(_) ->
	ShowTok = fun({_, T}) -> lists:flatten(io_lib:format("~p", [T])) end,
	PosFromTok = fun({Pos, _}) -> Pos end,
	TestTok = fun({_, X}) -> {just, X}; (_) -> nothing end,
	Parser = token(ShowTok, PosFromTok, TestTok),
	Input = [{10,$a},{20,$b}],
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	{$a, 20} = {returnVal(Result), pos(Result)}.


token__1(_) ->
	ShowTok = fun({_, T}) -> lists:flatten(io_lib:format("~p", [T])) end,
	PosFromTok = fun({Pos, _}) -> Pos end,
	TestTok = fun({_, X}) -> {just, X}; (_) -> nothing end,
	Parser = token(ShowTok, PosFromTok, TestTok),
	Input = [{10,$a}],
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	{$a, 10} = {returnVal(Result), pos(Result)}.


token__2(_) ->
	ShowTok = fun(T) -> lists:flatten(io_lib:format("~p", [T])) end,
	PosFromTok = fun(X) -> X end,
	TestTok = fun(X) -> {just, X} end,
	Parser = token(ShowTok, PosFromTok, TestTok),
	Input = lazy_list_1($a),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	{$a, $b} = {returnVal(Result), pos(Result)}.


token__3(_) ->
	ShowTok = fun(T) -> lists:flatten(io_lib:format("~p", [T])) end,
	PosFromTok = fun(X) -> X end,
	TestTok = fun(X) -> {just, X} end,
	Parser = token(ShowTok, PosFromTok, TestTok),
	Input = lazy_list_2($a),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	{$a, $b} = {returnVal(Result), pos(Result)}.


token__4(_) ->
	ShowTok = fun(T) -> lists:flatten(io_lib:format("~p", [T])) end,
	PosFromTok = fun(X) -> X end,
	TestTok = fun(X) -> {just, X} end,
	Parser = token(ShowTok, PosFromTok, TestTok),
	Input = lazy_list_3($a),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	{$a, $b} = {returnVal(Result), pos(Result)}.


token__5(_) ->
	ShowTok = fun(T) -> lists:flatten(io_lib:format("~p", [T])) end,
	PosFromTok = fun(X) -> X end,
	TestTok = fun(X) -> {just, X} end,
	Parser = token(ShowTok, PosFromTok, TestTok),
	Input = lazy_list_bin(<<"a">>),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	{$a, $a} = {returnVal(Result), pos(Result)}.


tokens__0(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABCDE"),
	Input = "ABCDE",
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"ABCDE" = returnVal(Result).


tokens__1(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABCDE"),
	Input = "ABXDE",
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"(line 1, column 1):\nunexpected \"X\"\nexpecting \"ABCDE\"" = error(Result).


tokens__2(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABCDE"),
	Input = lazy_list_1($A),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"ABCDE" = returnVal(Result).


tokens__3(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABXDE"),
	Input = lazy_list_1($A),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"(line 1, column 1):\nunexpected \"C\"\nexpecting \"ABXDE\"" = error(Result).


tokens__4(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABCDE"),
	Input = lazy_list_2($A),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"ABCDE" = returnVal(Result).


tokens__5(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABXDE"),
	Input = lazy_list_2($A),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"(line 1, column 1):\nunexpected \"C\"\nexpecting \"ABXDE\"" = error(Result).


tokens__6(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABCDE"),
	Input = lazy_list_3($A),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"ABCDE" = returnVal(Result).


tokens__7(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABXDE"),
	Input = lazy_list_3($A),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"(line 1, column 1):\nunexpected \"C\"\nexpecting \"ABXDE\"" = error(Result).


tokens__8(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABCDE"),
	Input = lazy_list_bin(<<"ABCDE">>),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"ABCDE" = returnVal(Result).


tokens__9(_) ->
	ShowS = fun(S) -> "\"" ++ S ++ "\"" end,
	NextPosS = fun parsec_pos:updatePosString/2,
	Parser = tokens(ShowS, NextPosS, "ABXDE"),
	Input = lazy_list_bin(<<"ABCDE">>),
	State = #state{stateInput = Input},
	Result = runP(Parser, State),
	"(line 1, column 1):\nunexpected \"C\"\nexpecting \"ABXDE\"" = error(Result).


lazy_list_1(Start) -> fun() -> [Start | lazy_list_1(Start + 1)] end.

lazy_list_2(Start) -> {fun(X) -> [X | {fun lazy_list_2/1, [X+1]}] end, [Start]}.

lazy_list_3(X) -> [X | {?MODULE, lazy_list_3, [X+1]}].

lazy_list_bin(<<>>) -> [];
lazy_list_bin(<<X/integer, Xs/binary>>) -> [X | {?MODULE, lazy_list_bin, [Xs]}].

