-module(parsec_char).

-compile(inline).
-compile({parse_transform, import_enhancements}).

-import(parsec_prim).
-import(parsec_pos, [updatePosChar/2, updatePosString/2]).

-export(
	[ spaces/0
	, space/0
	, newline/0
	, tab/0
	, upper/0
	, lower/0
	, alphaNum/0
	, letter/0
	, digit/0
	, hexDigit/0
	, octDigit/0
	, char/1
	, string/1
	, anyChar/0
	, oneOf/1
	, noneOf/1
	, satisfy/1
	]).

-type char_parser(St, A) :: parsec_prim:gen_parser(char(), St, A).
%% @type char_parser(St, A) = gen_parser(char(), St, A).


-spec oneOf([char()]) -> char_parser(_St, _A).
%% @spec oneOf(Cs) -> char_parser(St, A)
%% 	Cs = [char()]
%% @doc Parses any character in a given list
%%
%% ``oneOf(Cs)'' succeeds if the current character is in
%% the supplied list of characters ``Cs''. Returns the parsed character.
%%```
%%vowel() -> oneOf("aeiou").
%%'''
%% @see satisfy/1

oneOf(Cs) -> satisfy(fun(C) -> lists:member(C, Cs) end).


-spec noneOf([char()]) -> char_parser(_St, _A).
%% @spec noneOf(Cs) -> char_parser(St, A)
%% 	Cs = [char()]
%% @doc Parses any character not in a given list
%%
%% As the dual of {@link oneOf/1}, ``noneOf(Cs)'' succeeds if the current character is <em>not</em> in
%% the supplied list of characters ``Cs''. Returns the parsed character.
%%```
%%consonant() -> noneOf("aeiou").
%%'''

noneOf(Cs) -> satisfy(fun(C) -> not(lists:member(C, Cs)) end).


%spaces :: CharParser st ()
-spec spaces() -> char_parser(_, {}).
spaces() -> label(skipMany(space()), "whitespace").


%space, newline, tab :: CharParser st Char


space() -> label(satisfy(fun isSpace/1), "space").

isSpace($\s) -> true;
isSpace($\t) -> true;
isSpace($\n) -> true;
isSpace($\r) -> true;
isSpace($\f) -> true;
isSpace($\v) -> true;
isSpace(_) -> false.


newline() -> label(char($\n), "newline").


tab() -> label(char($\t), "tab").


%upper, lower, alphaNum, letter, digit, hexDigit, octDigit :: CharParser st Char

upper() -> label(satisfy(fun isUpper/1), "uppercase letter").

isUpper(C) when $A =< C, C =< $Z -> true;
isUpper(_) -> false.


lower() -> label(satisfy(fun isLower/1), "lowercase letter").

isLower(C) when $a =< C, C =< $z -> true;
isLower(_) -> false.


alphaNum() -> label(satisfy(fun isAlphaNum/1), "letter or digit").

isAlphaNum(C) -> isAlpha(C) orelse isDigit(C).


letter() -> label(satisfy(fun isAlpha/1), "letter").

isAlpha(C) -> isUpper(C) orelse isLower(C).


digit() -> label(satisfy(fun isDigit/1), "digit").

isDigit(C) when $0 =< C, C =< $9 -> true;
isDigit(_) -> false.


hexDigit() -> label(satisfy(fun isHexDigit/1), "hexadecimal digit").

isHexDigit(C) when $A =< C, C =< $F -> true;
isHexDigit(C) -> isDigit(C).


octDigit() -> label(satisfy(fun isOctDigit/1), "octal digit").

isOctDigit(C) when $0 =< C, C =< $7 -> true;
isOctDigit(_) -> false.


%char :: Char -> CharParser st Char
char(C) -> label(satisfy(fun(Ch) when Ch =:= C -> true; (_) -> false end), show(C)).


%anyChar :: CharParser st Char
anyChar() -> satisfy(fun(_) -> true end).


%satisfy :: (Char -> Bool) -> CharParser st Char
satisfy(F) ->
	tokenPrim(
		fun show/1,
		fun(Pos, C, _) -> updatePosChar(Pos, C) end,
		fun(C) ->
				case F(C) of
					true -> {just, C};
					_ -> nothing
				end
			end
		).


%string :: String -> CharParser st String
string(S) -> tokens(fun show/1, fun parsec_pos:updatePosString/2, S).

-define(is_char(X), (is_integer(X) andalso X >= 0 andalso X =< 16#10FFFF andalso X =/= 16#FFFE andalso X =/= 16#FFFF andalso not(16#D800 =< X andalso X =< 16#DFFF))).


show(S) when is_list(S) -> "\"" ++ S ++ "\"";
show(C) when ?is_char(C) -> [$", C, $"];
show(P) -> lists:flatten(io_lib:format("~p", [P])).
