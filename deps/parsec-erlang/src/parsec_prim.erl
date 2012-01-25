-module(parsec_prim).

-compile(inline).
-compile({parse_transform, parsec_do}).
-compile({parse_transform, import_enhancements}).

-import(parsec_error).
-unimport({parsec_error, [{show,1}]}).
-import(parsec_pos).
-unimport({parsec_pos, [{show,1}]}).

-export(
	% basic types
	% Parser, GenParser
	[ runP/2
	, runParser/4
	, parse/3
	, parseFromFile/2
	, parseTest/2

	% primitive parsers:
	%, fmap/2
	, return/1
	, bind/2
	, fail/1
	, fail/2
	%, mzero/0
	%, mplus/2
	, token/3
	, tokens/3
	, tokenPrim/3
	, tokenPrimEx/4
	, 'try'/1
	, label/2
	, labels/2
	, unexpected/1
	, map/2
	, pzero/0
	, pplus/2
	, pplusErrors/2

	% monad combinator
	, sequence/1

	% primitive because of space behaviour
	, many/1
	, skipMany/1

	% user state manipulation
	, getState/0
	, setState/1
	, updateState/1

	% state manipulation
	, getPosition/0
	, setPosition/1
	, getInput/0
	, setInput/1
	%, State(..)
	, getParserState/0
	, setParserState/1
	]).

%-type stream(Tok) :: maybe_improper_list(Tok, fun(() -> stream(Tok))).
%% @type stream(Tok) = [] | cons(Tok, stream(Tok)) | () -> stream(Tok).

-include_lib("include/record_state.hrl").
%-type state(Tok, St) :: #state{stateInput::stream(Tok), statePos::parsec_pos:source_pos(), stateUser::St}.
%% @type state(Tok, St) = {state, [Tok], parsec_pos:source_pos(), St}.  The ``#state{stateInput, statePos, stateUser}'' record is defined in ``parsec_prim.hrl''.

%-type consumed(A) :: {consumed, A} | {empty, A}.
%% @type consumed(A) = {consumed, A} | {empty, A}.

%-type reply(Tok, St, A) :: {ok, A, state(Tok, St), parsec_error:parse_error()} | {error, parsec_error:parse_error()}.
%% @type reply(Tok, St, A) = {ok, A, state(Tok, St), parsec_error:parse_error()} | {error, parsec_error:parse_error()}.

%-type gen_parser(Tok, St, A) :: {parser, fun((state(Tok, St)) -> consumed(reply(Tok, St, A)))}.
%% @type gen_parser(Tok, St, A) = {parser, (state(Tok, St)) -> consumed(reply(Tok, St, A))}.

%-type parser(A) :: gen_parser(char(), {}, A).
%% @type parser(A) = gen_parser(char(), {}, A).

%-type maybe(T) :: {just, T} | nothing.
%% @type maybe(T) = {just, T} | nothing.

%-type either(T1, T2) :: {left, T1} | {right, T2}.
%% @type either(T1, T2) = {left, T1} | {right, T2}.

%-type void() :: any().
%% @type void() = any().  Type means the function returns no useful value, but is called for its side-effects.


%-spec getState() -> gen_parser(_Tok, _St, _A).
%% @spec getState() -> gen_parser(Tok, St, A)
%% @doc Returns the current user state.
%% @TODO This can be implemented more efficiently without the call to getParserState/1.

getState() ->
%	{parser, fun(State) ->
%			{empty, {ok, State#state.stateUser, State, unknownError(State)}}
%		end
%	}.
	bind(getParserState(),
		fun(State) -> return(State#state.stateUser) end
		).


%-spec setState(St) -> gen_parser(_Tok, St, {}).
%% @spec setState(St) -> gen_parser(Tok, St, {})
%% @doc Sets the user state.
%% @TODO This can be implemented more efficiently without the call to updateParserState/1.

setState(St) ->
	bind(updateParserState(fun(State) -> State#state{stateUser = St} end),
		fun(_) -> return({}) end
		).


%-spec updateState(fun((St) -> St)) -> gen_parser(_Tok, St, {}).
%% @spec updateState(F) -> gen_parser(_Tok, St, {})
%% 	F = (St) -> St
%% @doc Applies a function to the user state.
%%
%% Suppose that we want
%% to count identifiers in a source, we could use the user state as:
%% ```
%%expr(St) ->
%%	case identifier(St) of
%%		X = #ok{state = St2} ->
%%			X#ok{state = (updateState(fun(X) -> X+1 end))(St2)#ok.state
%%		end.
%% '''

updateState(F) ->
	Updater = fun(State = #state{stateUser = User}) -> State#state{stateUser = F(User)} end,
	bind(updateParserState(Updater),
		fun(_) -> return({}) end
		).


%-spec getPosition() -> gen_parser(_Tok, _St, _A).
%% @spec getPosition() -> gen_parser(Tok, St, A)
%% @doc Returns the current source position.
getPosition() ->
	bind(getParserState(),
		fun(State) -> return(State#state.statePos) end
		).


%-spec getInput() -> gen_parser(_Tok, _St, _A).
%% @spec getInput() -> gen_parser(Tok, St, A)
%% @doc Returns the current input.
getInput() ->
	bind(getParserState(),
		fun(State) -> return(State#state.stateInput) end
		).


%-spec setPosition(parsec_pos:source_pos()) -> gen_parser(_Tok, _St, {}).
%% @spec setPosition(Pos) -> gen_parser(Tok, St, {})
%% 	Pos = parsec_pos:source_pos()
%% @doc Sets the current source position.

setPosition(Pos) ->
	bind(updateParserState(fun(State) -> State#state{statePos = Pos} end)
		, fun(_) -> return({}) end
		).


%-spec setInput([Tok]) -> gen_parser(Tok, _St, {}).
%% @spec setInput(Input) -> gen_parser(Tok, St, {})
%% 	Input = [Tok]
%% @doc Continues parsing with a different input.
%%
%% The ``getInput/0'' and ``setInput/1''
%% functions can, for example, be used to deal with ``#include'' files.

setInput(Input) ->
	bind(updateParserState(fun(State) -> State#state{stateInput = Input} end)
		, fun(_) -> return({}) end
		).


%-spec getParserState() -> gen_parser(Tok, St, state(Tok, St)).
%% @spec getParserState() -> gen_parser(Tok, St, state(Tok, St))
%% @doc Returns the parser state.

getParserState() -> updateParserState(fun id/1).


id(X) -> X.


%-spec setParserState(state(Tok, St)) -> gen_parser(Tok, St, state(Tok, St)).
%% @spec setParserState(St) -> gen_parser(Tok, St, state(Tok, St))
%% 	St = state(Tok, St)
%% @doc Sets the parser state.

setParserState(St) -> updateParserState(fun(_) -> St end).


%-spec runP(gen_parser(Tok, St, A), state(Tok, St)) -> consumed(reply(Tok, St, A)).
%% @spec runP(P::P, State) -> consumed(reply(Tok, St, A))
%% 	P = gen_parser(Tok, St, A)
%% 	State = state(Tok, St)
%% @equiv P(State)
%% @doc Runs a parser.

runP({parser, P}, State) -> P(State).


%-spec parseFromFile(parser(A), parsec_pos:source_name()) -> either(parsec_error:parse_error(), A).
%% @spec parseFromFile(P, FilePath::FilePath) -> either(parsec_error:parse_error(), A)
%% 	P = parser(A)
%% 	FilePath = parsec_pos:source_name()
%% @doc Run a character parser on a file.
%%
%% ``parseFromFile(P, FilePath)'' runs a character parser ``P'' on the input read
%% from ``FilePath''.
%% Returns either a {@link parsec_error:parse_error()} (``Left'') or a value of type ``A'' (``Right'').

parseFromFile(P, FName) ->
	case file:read_file(FName) of
		{ok, Bin} ->
			Input = binary_to_list(Bin),
			parse(P, FName, Input);
		{error, Err} -> erlang:error(lists:flatten(io_lib:format("~p: ~s", [Err, file:format_error(Err)])))
	end.


%-spec parseTest(gen_parser(Tok, {}, _A), [Tok]) -> void().
%% @spec parseTest(P, Input) -> void()
%% 	P = gen_parser(Tok, {}, A)
%% 	Input = [Tok]
%% @doc Runs a parser and prints the result to stdout.
%%
%% Used for testing parsers.

parseTest(P, Input) ->
	case runParser(P, {}, "", Input) of
		{left, Err} -> io:format("parse error at ~s\n", [parsec_error:show(Err)]);
		{right, X} -> io:format("~p\n", [X])
	end.


%-spec parse(gen_parser(Tok, {}, A), parsec_pos:source_name(), [Tok]) -> either(parsec_error:parse_error(), A).
%% @spec parse(P, FilePath::FilePath, Input) -> either(parsec_error:parse_error(), A)
%% 	P = gen_parser(Tok, {}, A)
%% 	FilePath = parsec_pos:source_name()
%% 	Input = [Tok]
%% @doc Run a character parser without user state.
%%
%% ``parse(P, FilePath, Input)'' runs a character parser ``P'' without user state.
%% The ``FilePath'' is only used in error messages and may be the empty string.
%%```
%%Numbers = commaSep(integer()),
%%case parse(Numbers, "", "11, 2, 43") of
%%	{left, Err} -> Err;
%%	{right, Xs} -> lists:sum(Xs)
%%end.
%%'''

parse(P, Name, Input) -> runParser(P, {}, Name, Input).


%-spec runParser(gen_parser(Tok, St, A), St, parsec_pos:source_name(), [Tok]) -> either(parsec_error:parse_error(), A).
%% @spec runParser(P, St, FilePath::FilePath, Input) -> either(parsec_error:parse_error(), A)
%% 	P= gen_parser(Tok, St, A)
%% 	FilePath= parsec_pos:source_name()
%% 	Input= [Tok]
%% @doc Run a parser.
%%
%% The most general way to run a parser. ``runParser(P, State, FilePath, Input)''
%% runs parser ``P'' on the input list of tokens ``Input'', obtained from source ``FilePath''
%% with the initial user state ``State''. The ``FilePath'' is only used in error messages
%% and may be the empty string.
%% Returns either a {@link parsec_error:parse_error()} (``Left'') or a value of type ``A'' (``Right'').

runParser(P, St, Name, Input) ->
	case parserReply(runP(P, #state{stateInput = Input, statePos = initialPos(Name), stateUser = St})) of
		{ok, X, _, _} -> {right, X};
		{error, Err} -> {left, Err}
	end.


parserReply(Result) ->
	case Result of
		{consumed, Reply} -> Reply;
		{empty, Reply} -> Reply
	end.


%-spec map(fun((A) -> B), gen_parser(Tok, St, A)) -> gen_parser(Tok, St, B).
%% @spec map((A) -> B, gen_parser(Tok, St, A)) -> gen_parser(Tok, St, B)
%% @doc Transform the output of a parser using a function.
%%
%% This function is the functor ``fmap'' function.

map(F, P) -> parsecMap(F, P).


parsecMap(F, P) ->
	MapReply = fun(Reply) ->
			case Reply of
				{ok, X, State, Err} ->
					FX = F(X),
					{ok, FX, State, Err};
				{error, Err} -> {error, Err}
			end
		end,
	{parser, fun(State) ->
			case runP(P, State) of
				{consumed, Reply} -> {consumed, MapReply(Reply)};
				{empty, Reply} -> {empty, MapReply(Reply)}
			end
		end
	}.


%-spec return(A) -> gen_parser(_Tok, _St, A).
%% @spec return(A) -> gen_parser(_Tok, _St, A)
%% @doc Parser that always succeeds without consuming any input.
%%
%% The parser ``return(X)'' always succeeds with value ``X'' without consuming any
%% input.

return(X) -> parsecReturn(X).


%-spec bind(gen_parser(Tok, St, A), fun((A) -> gen_parser(Tok, St, B))) -> gen_parser(Tok, St, B).
%% @spec bind(gen_parser(Tok, St, A), ((A) -> gen_parser(Tok, St, B))) -> gen_parser(Tok, St, B)
%% @doc Bind parser, which implements monadic sequencing.
%%
%% It is generally not possible to implement sequencing in the straightforward way you might think:
%%```
%%seqP(P1, P2) ->
%%	{parser, fun(State) ->
%%			{_, {ok, _, State1, _}} = runP(P1, State),
%%			runP(P2, State1)
%%		end
%%	}
%%'''
%% In order to properly sequence these two parsers, you would have to properly handle, for
%% example, the cases
%% where the first parser fails (the second one should not be run), and where the first parser
%% consumes input but the second does not (the return value should indicate that input was
%% consumed).
%% This logic would clutter up any parser that sequences parsers, simultaneously increasing the
%% chances of errors and decreasing readability and maintainability.
%%
%% The ``bind/2'' parser handles the required logic in sequencing parsers.
%% Using ``bind/2'', the example above could be rewritten:
%%```
%%seqP(P1, P2) ->
%%	bind(P1, fun(_) -> P2 end).
%%'''
%% Note that the second argument is not a parser, but a function that returns a parser.
%% The argument to the function is the result value from the first parse, and it may be
%% used in constructing the second parser.  For example, to return the results of the two parsers as a list, one might use the following parser:
%%```
%%seqP(P1, P2) ->
%%	bind(P1, fun(X1) -> bind(P2, fun(X2) -> return([X1, X2]) end) end).
%%'''
%% This distinction allows context-sensitive grammars to be parsed.
%% E.g., we might parse the opening and closing XML tags (which must match) something like so:
%%```
%%xml() ->
%%	bind(xml_opening_tag(),
%%		fun(TagName) -> bind(xml_fragment(),
%%			fun(_) -> xml_closing_tag(TagName) end)
%%		end).
%%'''
%% If the ``bind'''s are a bit cluttering, a ``?do/3'' macro is available to shorten it, though a full-blown parse-transform would be required to eliminate the repetitions of the macro for three or more sequenced parsers:
%%```
%%xml() ->
%%	?do(TagName, xml_opening_tag(),
%%	?do(_, xml_fragment(),
%%		xml_closing_tag(TagName)
%%	))
%%'''
%% @TODO Check whether something like this is possible (obviously would need some sort of helper function to do the same logic as ``bind/2''):
%% ``[xml_closing_tag(TagName) || TagName <- [xml_opening_tag()], _ <- [xml_fragment()]]''


bind(P, F) -> parsecBind(P, F).


%-spec sequence([gen_parser(Tok, St, A)]) -> gen_parser(Tok, St, [A]).
%% @spec sequence(Ps::Ps) -> gen_parser(Tok, St, A)
% 	Ps = [gen_parser(Tok, St, A)]
%% @doc Parse a list of parsers.

sequence([P]) ->
	[do:bind ||
		X <- P,
		return([X])
	];
sequence([P|Ps]) ->
	[do:bind ||
		X <- P,
		Xs <- sequence(Ps),
		return([X|Xs])
	].


%-spec fail(string()) -> gen_parser(_Tok, _St, _A).
%% @spec fail(string()) -> gen_parser(_Tok, _St, _A)
%% @doc Parser that always fails without consuming any input.
%%
%% The parser ``fail(Msg)'' always fails with message-error ``Msg'' without consuming
%% any input.
%%
%% @equiv fail(message, Msg)

fail(Msg) -> parsecFail(message, Msg).


%-spec fail(string()) -> gen_parser(_Tok, _St, _A).
%% @spec fail(Type, Msg) -> gen_parser(_Tok, _St, _A)
%% 	Type = atom()
%% 	Msg = string()
%% @doc Parser that always fails without consuming any input.
%%
%% The parser ``fail(Type, Msg)'' always fails with a failure type of ``Type'' and message ``Msg'' without consuming
%% any input.  Any ``Type'' other than ``sysunexpect'', ``unexpect'', and ``expect'' are output as messages, but the ``Type'' may be more succinct and easier to work with if being used with {@link withErrors/1}.

fail(Type, Msg) -> parsecFail(Type, Msg).


parsecReturn(X) -> {parser, fun(State) -> {empty, {ok, X, State, unknownError(State)}} end}.


parsecBind(P, F) ->
	{parser, fun(State) ->
			case runP(P, State) of
				{consumed, Reply1} ->
					{consumed, case Reply1 of
							{ok, X, State1, Err1} ->
								case runP(F(X), State1) of
									{empty, Reply2} -> mergeErrorReply(Err1, Reply2);
									{consumed, Reply2} -> Reply2
								end;
							{error, Err1} -> {error, Err1}
						end
					};
				{empty, Reply1} ->
					case Reply1 of
						{ok, X, State1, Err1} ->
							case runP(F(X), State1) of
								{empty, Reply2} -> {empty, mergeErrorReply(Err1, Reply2)};
								Other -> Other
							end;
						{error, Err1} -> {empty, {error, Err1}}
					end
			end
		end
	}.


mergeErrorReply(Err1, Reply) ->
	case Reply of
		{ok, X, State, Err2} -> {ok, X, State, mergeError(Err1, Err2)};
		{error, Err2} -> {error, mergeError(Err1, Err2)}
	end.


parsecFail(Type, Msg) ->
	{parser, fun(#state{statePos = Pos}) ->
			{empty, {error, newErrorMessage({Type, Msg}, Pos)}}
		end
	}.


%mzero() -> parsecZero().

%mplus(P1, P2) -> parsecPlus(P1, P2).


%-spec pzero() -> gen_parser(_Tok, _St, _A).
%% @spec pzero() -> gen_parser(Tok, St, A)
%% @doc Always fails without consuming any input.

pzero() -> parsecZero().

parsecZero() -> {parser, fun(State) -> {empty, {error, unknownError(State)}} end}.


%-spec pplus(gen_parser(Tok, St, A), gen_parser(Tok, St, A)) -> gen_parser(Tok, St, A).
%% @spec pplus(P1::gen_parser(Tok, St, A), P2::gen_parser(Tok, St, A)) -> gen_parser(Tok, St, A)
%% @doc Choice combinator.
%%
%% This combinator implements choice. The parser ``pplus(P1, P2)'' first applies ``P1''. If
%% it succeeds, the value of ``P1'' is returned. If ``P1'' fails <em>without consuming any input,</em>
%% parser ``P2'' is tried.
%%
%% The parser is called <em>predictive</em> since ``P2'' is only tried when parser ``P1'' didn't consume
%% any input (i.e., the look-ahead is 1). This non-backtracking behaviour
%% allows for both an efficient implementation of the parser combinators and the
%% generation of good error messages.
%% @TODO Check validity of last sentence with respect to this Erlang implementation.

pplus(P1, P2) -> pplusErrors(P1, fun(_) -> P2 end).


%% @spec pplusErrors(P,F) -> gen_parser(Tok, St, A)
%% 	P = gen_parser(Tok, St, A)
%% 	F = ([parsec_error:message()]) -> gen_parser(Tok, St, A)
%% @doc Choice combinator.

pplusErrors(P, F) ->
	{parser, fun(State) ->
			case runP(P, State) of
				{empty, {error, Err = {parse_error, _, Msgs}}} ->
					case runP(F(Msgs), State) of
						{empty, Reply} -> {empty, mergeErrorReply(Err, Reply)};
						Consumed -> Consumed
					end;
				Other -> Other
			end
		end
	}.


%-spec 'try'(gen_parser(Tok, St, A)) -> gen_parser(Tok, St, A).
%% @spec 'try'(gen_parser(Tok, St, A)) -> gen_parser(Tok, St, A)
%% @doc Parse without consuming input.
%%
%% The parser ``'try'(P)'' behaves like parser ``P'', except that it pretends that it
%% hasn't consumed any input when an error occurs.
%%
%% This combinator is used whenever arbitrary look ahead is needed. Since it
%% pretends that it hasn't consumed any input when ``P'' fails, the {@link pplus/2} combinator
%% will try its second alternative even when the first parser failed while consuming
%% input.
%%
%% The ``try'' combinator can for example be used to distinguish identifiers and
%% reserved words. Both reserved words and identifiers are a sequence of letters.
%% Whenever we expect a certain reserved word where we can also expect an
%% identifier we have to use the ``try'' combinator. Suppose we write:
%%```
%%expr() -> label(pplus(letExpr(), identifier()), "expression").
%%
%%letExpr() -> bind(string("let"), ...).
%%identifer() -> many1(letter()).
%%'''
%% If the user writes ``"lexical"'', the parser fails with: ``unexpected 'x', expecting
%% 't' in "let"''. Indeed, since the ``pplus'' combinator only tries alternatives
%% when the first alternative hasn't consumed input, the ``identifier'' parser is
%% never tried (because the prefix ``"le"'' of the ``string("let")'' parser is already
%% consumed). The right behaviour can be obtained by adding the ``try'' combinator:
%%```
%%expr() -> label(pplus(letExpr(), identifier()), "expression").
%%
%%letExpr() -> bind('try'(string("let")), ...).
%%identifer() -> many1(letter()).
%%'''
%% Since the use of the ``try'' combinator with lexical tokens is quite tricky, the
%% {@link parsec_token} module can be used to parse lexical tokens. This module automatically
%% uses the ``try'' combinator in the appropiate places.

'try'(P) ->
	{parser, fun(State = #state{statePos = Pos}) ->
			case runP(P, State) of
				{consumed, {error, Err}} -> {empty, {error, setErrorPos(Pos, Err)}};
				Other -> Other
			end
		end
	}.


%-spec token(fun((Tok) -> string()), fun((Tok) -> parsec_pos:source_pos()), fun((Tok) -> maybe(A))) -> gen_parser(Tok, _St, A).
%% @spec token(Show, TokPos, Test) -> gen_parser(Tok, St, A)
%% Show = (Tok) -> string()
%% TokPos = (Tok) -> parsec_pos:source_pos()
%% Test = (Tok) -> maybe(A)
%% @doc Accept a token.
%%
%% The parser ``token(ShowTok, PosFromTok, TestTok)'' accepts a token ``T'' with result
%% ``X'' when the function ``testTok(T)'' returns ``{just, X}''. The source position of
%% the ``T'' should be returned by ``posFromTok(T)'' and the token can be shown using
%% ``showTok(T)''.
%%
%% This combinator is expressed in terms of {@link tokenPrim/3}. It is used to accept user
%% defined token streams. For example, suppose that we have a stream of basic
%% tokens tupled with source positions. We can than define a parser that accepts
%% single tokens as:
%%```
%%-spec mytoken(T) -> gen_parser({{integer(), integer()}, T}, {}, T).
%%mytoken(X) ->
%%	ShowTok = fun({_, T}) -> lists:flatten(io_lib:format("~p", [T])) end,
%%	PosFromTok = fun({Pos, _}) -> Pos end,
%%	TestTok = fun({_, X}) -> {just, X}; (_) -> nothing end,
%%	token(ShowTok, PosFromTok, TestTok).
%%'''

token(Show, TokPos, Test) ->
	NextPos = fun
		(_, Tok, Toks) -> tokenNextPos(TokPos, Tok, lazy_list_expand(Toks))
		end,
	tokenPrim(Show, NextPos, Test).


tokenNextPos(TokPos, _, [Tok|_]) -> TokPos(Tok);
tokenNextPos(TokPos, Tok, []) -> TokPos(Tok).


%-spec tokenPrim(fun((Tok) -> string()), fun((parsec_pos:source_pos(), Tok, [Tok]) -> parsec_pos:source_pos()), fun((Tok) -> maybe(A))) -> gen_parser(Tok, _St, A).
%% @spec tokenPrim(Show, NextPos, Test) -> gen_parser(Tok, St, A)
%% Show = (Tok) -> string()
%% NextPos = (parsec_pos:source_pos(), Tok, [Tok]) -> parsec_pos:source_pos()
%% Test = (Tok) -> maybe(A)
%% @doc Accept a token.
%%
%% The parser ``tokenPrim(ShowTok, NextPos, TestTok)'' accepts a token ``T'' with result
%% ``X'' when the function ``TestTok(T)'' returns ``{just, X}''. The token can be shown using
%% ``ShowTok(T)''. The position of the <em>next</em> token should be returned when ``NextPos''
%% is called with the current source position ``Pos'', the current token ``T'' and the rest
%% of the tokens ``Toks'', ``NextPos(Pos, T, Toks)''.
%%
%% This is the most primitive combinator for accepting tokens. For example, the
%% {@link parsec_char:char/1} parser could be implemented as:
%%```
%%-spec char(char()) -> gen_parser(char(), _St, char()).
%%char(C) ->
%%	ShowChar = fun(X) -> lists:flatten(io_lib:format("'~c'", [X])) end,
%%	NextPos = fun(Pos, X, _) -> updatePosChar(Pos, X) end,
%%	TestChar = fun(C) -> {just, C}; (_) -> nothing end,
%%	tokenPrim(ShowChar, NextPos, TestChar).
%%'''

tokenPrim(Show, NextPos, Test) -> tokenPrimEx(Show, NextPos, nothing, Test).


%-spec tokenPrimEx(fun((Tok) -> string()), fun((parsec_pos:source_pos(), Tok, [Tok]) -> parsec_pos:source_pos()), maybe(fun((parsec_pos:source_pos(), Tok, [Tok], St) -> St)), fun((Tok) -> maybe(A))) -> gen_parser(Tok, St, A).
%% @spec tokenPrimEx(Show, NextPos, MbNextState, Test) -> gen_parser(Tok, St, A)
%% Show = (Tok) -> string()
%% NextPos = (parsec_pos:source_pos(), Tok, [Tok]) -> parsec_pos:source_pos()
%% MbNextState = maybe((parsec_pos:source_pos(), Tok, [Tok], St) -> St)
%% Test = (Tok) -> maybe(A)
%% @doc The most primitive token recogniser.
%%
%% The expression ``tokenPrimEx(Show, Nextpos, MbNextState, Test)'',
%% recognises tokens when ``Test'' returns ``{just, X}'' (and returns the value ``X''). Tokens are shown in
%% error messages using ``Show''. The position is calculated using ``NextPos'', and finally, ``MbNextState'',
%% can hold a function that updates the user state on every token recognised (nice to count tokens).
%% (The function is packed into a {@link maybe()} type for performance reasons.)

tokenPrimEx(Show, NextPos, MbNextState, Test) ->
	{parser, fun(State = #state{stateInput = Input, statePos = Pos, stateUser = User}) ->
			case lazy_list_expand(Input) of
				[C|Cs] ->
					case Test(C) of
						{just, X} ->
							NewPos = NextPos(Pos, C, Cs),
							NewState = case MbNextState of
									nothing ->
										State#state{stateInput = Cs, statePos = NewPos};
									{just, NextState} ->
										NewUser = NextState(Pos, C, Cs, User),
										State#state{stateInput = Cs, statePos = NewPos, stateUser = NewUser}
								end,
							{consumed, {ok, X, NewState, newErrorUnknown(NewPos)}};
						nothing -> {empty, sysUnExpectError(Show(C), Pos)}
					end;
				[] -> {empty, sysUnExpectError("", Pos)}
			end
		end
	}.


%-spec label(gen_parser(Tok, St, A), string()) -> gen_parser(Tok, St, A).
%% @spec label(P::gen_parser(Tok, St, A), Msg::string()) -> gen_parser(Tok, St, A)
%% @doc Gives a name to a parser (which is used in error messages).
%%
%% The parser ``label(P, Msg)'' behaves as parser ``P'', but whenever the parser ``P'' fails
%% <em>without consuming any input</em>, it replaces expect error messages with the expect
%% error message ``Msg''.
%%
%% This is normally used at the end of a set alternatives where we want to return
%% an error message in terms of a higher level construct rather than returning
%% all possible characters. For example, if the ``expr'' parser from the {@link try/1} example
%% would fail, the error message is: ``...: expecting expression''. Without
%% the ``label/2'' combinator, the message would be like ``...: expecting "let"
%% or letter'', which is less friendly.

label(P, Msg) -> labels(P, [Msg]).


%labels :: GenParser tok st a -> [String] -> GenParser tok st a
%-spec labels(gen_parser(Tok, St, A), [string()]) -> gen_parser(Tok, St, A).
%% @spec labels(gen_parser(Tok, St, A), [string()]) -> gen_parser(Tok, St, A)
%% @doc Gives multiple names to a parser.

labels(P, Msgs) ->
	{parser, fun(State) ->
			case runP(P, State) of
				{empty, Reply} ->
					{empty,
						case Reply of
							{error, Err} -> {error, setExpectErrors(Err, Msgs)};
							{ok, X, State1, Err} ->
								case errorIsUnknown(Err) of
									true -> Reply;
									_ -> {ok, X, State1, setExpectErrors(Err, Msgs)}
								end
						end
					};
				Other -> Other
			end
		end
	}.


%-spec updateParserState(fun((state(Tok, St)) -> state(Tok, St))) -> gen_parser(Tok, St, state(Tok, St)).

updateParserState(F) ->
	{parser, fun(State) ->
			NewState = F(State),
			{empty, {ok, State, NewState, unknownError(NewState)}}
		end
	}.


%-spec unexpected(string()) -> gen_parser(_Tok, _St, _A).
%% @spec unexpected(string()) -> gen_parser(Tok, St, A)
%% @doc Fail with an unexpected error message.
%%
%% The parser ``unexpected(Msg)'' always fails with an {@link parsec_error:message(). unexpected error} message
%% ``Msg'' without consuming any input.
%%
%% The parsers {@link fail/1}, {@link label/2} and ``unexpected'' are the three parsers used to generate
%% error messages. Of these, only ``label'' is commonly used. For an example
%% of the use of ``unexpected'', see the definition of {@link parsec_combinator:notFollowedBy/1}.

unexpected(Msg) ->
	{parser, fun(State) ->
			{empty, {error, newErrorMessage({unexpect, Msg}, State#state.statePos)}}
		end
	}.


setExpectErrors(Err, []) -> setErrorMessage({expect, ""}, Err);
setExpectErrors(Err, [Msg]) -> setErrorMessage({expect, Msg}, Err);
setExpectErrors(Err, [Msg|Msgs]) ->
	lists:foldr(fun(M, E) -> addErrorMessage({expect, M}, E) end,
		setErrorMessage({expect, Msg}, Err),
		Msgs).


sysUnExpectError(Msg, Pos) -> {error, newErrorMessage({sysunexpect, Msg}, Pos)}.


unknownError(State) -> newErrorUnknown(State#state.statePos).


%-spec many(gen_parser(Tok, St, A)) -> gen_parser(Tok, St, [A]).
%% @spec many(P) -> gen_parser(Tok, St, [A])
%% 	P = gen_parser(Tok, St, A)
%% @doc Apply a parser zero or more times.
%%
%% ``many(P)'' applies the parser ``P'' <em>zero</em> or more times. Returns a list of the
%% returned values of ``P''.
%%```
%%identifer() ->
%%	bind(letter(),
%%		fun(C) -> bind(many(pplus(alphaNum(), char($_))),
%%		fun(Cs) -> return([C|Cs]) end
%%		end).
%%'''
%%
%% <em>This parser is unfolded for space.  If ``many'' were not defined as a primitive,
%% it would overflow the stack on large inputs.</em>

many(P) ->
	Cons = fun(X, L) -> [X|L] end,
	bind(manyAccum(Cons, P),
		fun(Xs) -> return(lists:reverse(Xs)) end
		).


%-spec skipMany(gen_parser(Tok, St, _)) -> gen_parser(Tok, St, {}).
%% @spec skipMany(P) -> gen_parser(Tok, St, {})
%% 	P = gen_parser(Tok, St, A)
%% @doc Apply a parser zero or more times, skipping its result.
%%
%% ``skipMany(P)'' applies the parser ``P'' <em>zero</em> or more times, skipping its result.
%%```
%%spaces() -> skipMany(space()).
%%'''
%%
%% <em>This parser is unfolded for space.  If ``skipMany'' were not defined as a primitive,
%% it would overflow the stack on large inputs.</em>

skipMany(P) ->
	bind(manyAccum(fun(_,_) -> [] end, P),
		fun(_) -> return({}) end
		).


%-spec manyAccum(fun((A, [A]) -> [A]), gen_parser(Tok, St, A)) -> gen_parser(Tok, St, [A]).

manyAccum(Accum, P) ->
	{parser, fun(State) ->
			Walk = fun
				(Xs, St, {empty, {error, Err}}, _) -> {ok, Xs, St, Err};
				(_Xs, _St, {empty, _}, _) -> erlang:error("parsec_prim:many: combinator 'many' is applied to a parser that accepts an empty string.");
				(_Xs, _St, {consumed, {error, Err}}, _) -> {error, Err};
				(Xs, _St, {consumed, {ok, X, St2, _Err}}, Walk) ->
					Ys = Accum(X, Xs),
					Walk(Ys, St2, runP(P, St2), Walk)
				end,
			case runP(P, State) of
				{empty, Reply} ->
					case Reply of
						{ok, _, _, _} -> erlang:error("parsec_prim:many: combinator 'many' is applied to a parser that accepts an empty string.");
						{error, Err} -> {empty, {ok, [], State, Err}}
					end;
				Consumed -> {consumed, Walk([], State, Consumed, Walk)}
			end
		end
	}.


%-spec tokens(fun(([Tok]) -> string()), fun((parsec_pos:source_pos(), [Tok]) -> parsec_pos:source_pos()), [Tok]) -> gen_parser(Tok, _St, [Tok]).
%% @spec tokens(ShowS, NextPosS, [Tok]) -> gen_parser(Tok, _St, [Tok])
%% ShowS = ([Tok]) -> string()
%% NextPosS = (parsec_pos:source_pos(), [Tok]) -> parsec_pos:source_pos()
%% @doc Accept a specific sequence of tokens.
%%
%% ``tokens(ShowS, NextPosS, S)'' parses the specific sequence of tokens in the list ``S''.
%% If the next ``length(S)'' tokens of the input are not equivalent to the tokens in ``S'', then the parser fails.
%% The ``ShowS'' function is used to create a string representation of a sequence of token, used when an error is encountered to create the error message for what was expected (``{expect, ShowS(S)}'') and what was encountered (``{sysunexpect, ShowS([C])}'').
%% The ``NextPosS'' function is called only when the parser succeeds, to update the position.
%%
%% This function is useful, for example, when matching exact strings.  ``parsec_char:string/1'', for example, may implemented:
%%```
%%string(S) -> tokens(fun show/1, fun parsec_pos:updatePosString/2, S).
%%'''


tokens(ShowS, NextPosS, S) ->
	{parser, fun(State = #state{stateInput = Input, statePos = Pos}) ->
			OK = fun(Cs) ->
					NewPos = NextPosS(Pos, S),
					NewState = State#state{stateInput = Cs, statePos = NewPos},
					{ok, S, NewState, newErrorUnknown(NewPos)}
				end,
			ErrEOF = {error,
				setErrorMessage(
					{expect, ShowS(S)},
					newErrorMessage({sysunexpect, ""}, Pos)
				)},
			ErrExpect = fun(C) ->
					{error,
						setErrorMessage(
							{expect, ShowS(S)},
							newErrorMessage({sysunexpect, ShowS([C])}, Pos)
						)
					}
				end,
			Walk = fun
				(Xs, Cs, Walk) when not(is_list(Cs)) -> Walk(Xs, lazy_list_expand(Cs), Walk);
				([], Cs, _) -> OK(Cs);
				(_, [], _) -> ErrEOF;
				([X|Xs], [C|Cs], Walk) when X =:= C -> Walk(Xs, Cs, Walk);
				(_, [C|_], _) -> ErrExpect(C)
				end,
			case {S, lazy_list_expand(Input)} of
				{[], Cs} -> {empty, OK(Cs)};
				{_, []} -> {empty, ErrEOF};
				{[X|Xs], [C|Cs]} when X =:= C -> {consumed, Walk(Xs, Cs, Walk)};
				{_, [C|_]} -> {empty, ErrExpect(C)}
			end
		end
	}.


lazy_list_expand(L) when is_list(L) -> L;
lazy_list_expand(F) when is_function(F, 0) -> lazy_list_expand(F());
lazy_list_expand({F, A}) when is_function(F), is_list(A) -> lazy_list_expand(apply(F, A));
lazy_list_expand({M, F, A}) when is_atom(F), is_list(A) -> lazy_list_expand(apply(M, F, A)).
