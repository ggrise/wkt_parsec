-module(parsec_error).

-compile(inline).
-compile({parse_transform, import_enhancements}).

-import(parsec_pos).
-unimport({parsec_pos, [{show,1}]}).

-export(
	[ messageString/1
	, messageCompare/2
	, messageEq/2
	, errorPos/1
	, errorMessages/1
	, errorIsUnknown/1
	, showErrorMessages/6
	, newErrorMessage/2
	, newErrorUnknown/1
	, addErrorMessage/2
	, setErrorPos/2
	, setErrorMessage/2
	, mergeError/2
	]).
-export([show/1]).


-type message() :: {'sysunexpect' | 'unexpect' | 'expect' | atom(), string()}.

-type parse_error() :: {parse_error, parsec_pos:source_pos(), [message()]}.

%% @type message() = {'sysunexpect' | 'unexpect' | 'expect' | atom(), string()}.
%% @type parse_error() = {parse_error, parsec_pos:source_pos(), [message()]}.


messageToEnum({sysunexpect, _}) -> 0;
messageToEnum({unexpect, _}) -> 1;
messageToEnum({expect, _}) -> 2;
messageToEnum({_, _}) -> 3.


%messageCompare :: Message -> Message -> Ordering
messageCompare(Msg1, Msg2) -> compare(messageToEnum(Msg1), messageToEnum(Msg2)).


compare(X, Y) when X > Y -> gt;
compare(X, Y) when X < Y -> lt;
compare(X, Y) when X == Y -> eq.


%messageString :: Message -> String
messageString({_, S}) -> S.


%messageEq :: Message -> Message -> Bool
messageEq(Msg1, Msg2) -> messageCompare(Msg1, Msg2) =:= eq.


%errorPos :: ParseError -> SourcePos
-spec errorPos(parse_error()) -> parsec_pos:source_pos().
errorPos({parse_error, Pos, _}) -> Pos.


%errorMessages :: ParseError -> [Message]
errorMessages ({parse_error, _, Msgs}) ->
	lists:sort(fun(A,B) -> messageCompare(A,B) =:= lt end, Msgs).


%errorIsUnknown :: ParseError -> Bool
errorIsUnknown ({parse_error, _, []}) -> true;
errorIsUnknown ({parse_error, _, _}) -> false.


%newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown(Pos) -> {parse_error, Pos, []}.


%newErrorMessage :: Message -> SourcePos -> ParseError
newErrorMessage(Msg, Pos) -> {parse_error, Pos, [Msg]}.


%addErrorMessage :: Message -> ParseError -> ParseError
addErrorMessage(Msg, {parse_error, Pos, Msgs}) -> {parse_error, Pos, [Msg|Msgs]}.


%setErrorPos :: SourcePos -> ParseError -> ParseError
setErrorPos(Pos, {parse_error, _, Msgs}) -> {parse_error, Pos, Msgs}.


%setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage(Msg, {parse_error, Pos, Msgs}) ->
	{parse_error, Pos, [Msg|lists:filter(fun(M) -> not(messageEq(M, Msg)) end, Msgs)]}.


%mergeError :: ParseError -> ParseError -> ParseError
mergeError({parse_error, Pos, Msgs1}, {parse_error, _, Msgs2}) ->
	{parse_error, Pos, Msgs1 ++ Msgs2}.


show(Err) ->
	parsec_pos:show(errorPos(Err)) ++ ":" ++
	showErrorMessages("or", "unknown parse error", "expecting", "unexpected", "end of input",
		errorMessages(Err)).


%showErrorMessages ::
%    String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages(_MsgOr, MsgUnknown, _MsgExpecting, _MsgUnExpected, _MsgEndOfInput, []) ->
	MsgUnknown;
showErrorMessages(MsgOr, _MsgUnknown, MsgExpecting, MsgUnExpected, MsgEndOfInput, Msgs) ->
	{SysUnExpect, Msgs1} = lists:splitwith(fun(X) -> messageEq(X, {sysunexpect, ""}) end, Msgs),
	{UnExpect, Msgs2} = lists:splitwith(fun(X) -> messageEq(X, {unexpect, ""}) end, Msgs1),
	{Expect, Messages} = lists:splitwith(fun(X) -> messageEq(X, {expect, ""}) end, Msgs2),

	Clean = fun(L) -> nub(lists:filter(fun([]) -> false; (_) -> true end, L)) end,

	Seperate = fun
		(_, [], _) -> "";
		(_, [M], _) -> M;
		(Sep, [M|Ms], Seperate) -> M ++ Sep ++ Seperate(Sep, Ms, Seperate)
		end,

	CommaSep = fun(L) -> Seperate(", ", Clean(L), Seperate) end,

	CommasOr = fun
		([]) -> "";
		([M]) -> M;
		(Ms) -> CommaSep(init(Ms)) ++ " " ++ MsgOr ++ " " ++ last(Ms)
		end,

	ShowMany = fun(Pre, Ms) ->
			case Clean(lists:map(fun messageString/1, Ms)) of
				[] -> "";
				Ms2 when Pre =:= [] -> CommasOr(Ms2);
				Ms2 -> Pre ++ " " ++ CommasOr(Ms2)
			end
		end,

	ShowExpect = ShowMany(MsgExpecting, lists:reverse(Expect)),


	ShowUnExpect =
		if UnExpect =:= [] -> "";
			true -> MsgUnExpected ++ " " ++ messageString(hd(UnExpect))
		end,

	FirstMsg = if SysUnExpect =:= [] -> ""; true -> messageString(hd(SysUnExpect)) end,
	ShowSysUnExpect = if
		UnExpect =/= []; SysUnExpect =:= [] -> "";
		FirstMsg =:= [] -> MsgUnExpected ++ " " ++ MsgEndOfInput;
		true -> MsgUnExpected ++ " " ++ FirstMsg
		end,

	ShowMessages = ShowMany("", Messages),

	lists:concat(lists:map(fun(X) -> "\n" ++ X end, Clean([ShowSysUnExpect,ShowUnExpect,ShowExpect,ShowMessages]))).


nub([]) -> [];
nub([X]) -> [X];
nub([X|Xs]) -> [X | nub(lists:filter(fun(Y)-> Y=/=X end, Xs))].


init(L) -> lists:reverse(tl(lists:reverse(L))).

last(L) -> hd(lists:reverse(L)).

%    | null msgs = msgUnknown
%    | otherwise = concat $ map ("\n"++) $ clean $
%                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
%    where
%      (sysUnExpect,msgs1)   = span (messageEq (SysUnExpect "")) msgs
%      (unExpect,msgs2)      = span (messageEq (UnExpect "")) msgs1
%      (expect,messages)     = span (messageEq (Expect "")) msgs2
%
%      showExpect        = showMany msgExpecting expect
%      showUnExpect      = showMany msgUnExpected unExpect
%      showSysUnExpect   | not (null unExpect) ||
%                          null sysUnExpect       = ""
%                        | null firstMsg          = msgUnExpected ++ " " ++ msgEndOfInput
%                        | otherwise              = msgUnExpected ++ " " ++ firstMsg
%                        where
%                          firstMsg  = messageString (head sysUnExpect)
%
%      showMessages      = showMany "" messages
%
%
%      --helpers
%      showMany pre msgs = case (clean (map messageString msgs)) of
%                            [] -> ""
%                            ms | null pre  -> commasOr ms
%                               | otherwise -> pre ++ " " ++ commasOr ms
%
%      commasOr []       = ""
%      commasOr [m]      = m
%      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms
%
%      commaSep          = seperate ", " . clean
%      semiSep           = seperate "; " . clean
%
%      seperate sep []   = ""
%      seperate sep [m]  = m
%      seperate sep (m:ms) = m ++ sep ++ seperate sep ms
%
%      clean             = nub . filter (not.null)



