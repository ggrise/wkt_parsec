-module(parsec_combinator).

-compile({parse_transform, parsec_do}).
-compile({parse_transform, import_enhancements}).

-import(parsec_prim).

-export(
	[ choice/1
	, count/2
	, between/3
	, option/2
	, optionMaybe/1
	, optional/1
	, skipMany1/1
	, many1/1
	, sepBy/2
	, sepBy1/2
	, endBy/2
	, endBy1/2
	, sepEndBy/2
	, sepEndBy1/2
	, chainl/3
	, chainl1/2
	, chainr/3
	, chainr1/2
	, eof/0
	, notFollowedBy/1

	% tricky combinators
	, manyTill/2
	, lookAhead/1
	, anyToken/0
	, anyToken/1
	]).

%% -type maybe(T) :: {just, T} | nothing.
%% @type maybe(T) = {just, T} | nothing.

%% -spec choice([parsec_prim:gen_parser(Tok, St, A)]) -> parsec_prim:gen_parser(Tok, St, A).
%% @spec choice(Ps) -> parsec_prim:gen_parser(Tok, St, A)
%% 	Ps = [parsec_prim:gen_parser(Tok, St, A)]
%% @doc Choose between a list of parsers.
%%
%% ``choice(Ps)'' tries to apply the parsers in the list ``Ps'' in order, until one of
%% them succeeds. Returns the value of the succeeding parser. ``choice'' can be
%% defined as:
%%```
%%choice(Ps) -> lists:foldr(fun parsec_prim:pplus/2, pzero(), Ps).
%%'''

choice(Ps) -> lists:foldr(fun parsec_prim:pplus/2, pzero(), Ps).


%% -spec option(A, parsec_prim:gen_parser(Tok, St, A)) -> parsec_prim:gen_parser(Tok, St, A).
%% @spec option(X, P) -> parsec_prim:gen_parser(Tok, St, A)
%% 	X = A
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% @doc Optional parser, with a default value.
%%
%% ``option(X, P)'' tries to apply parser ``P''. If ``P'' fails without consuming input, it
%% returns the value ``X'', otherwise the value returned by ``P''.
%%```
%%-spec priority() -> parser(integer()).
%%priority() ->
%%	option(0,
%%		bind(parsec_char:digit(),
%%			fun(D) -> return(D - $0) end)).
%%'''

option(X, P) -> pplus(P, return(X)).


%% -spec optionMaybe(parsec_prim:gen_parser(Tok, St, A)) -> parsec_prim:gen_parser(Tok, St, maybe(A)).
%% @spec optionMaybe(P) -> parsec_prim:gen_parser(Tok, St, maybe(A))
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% @doc Optional parser.
%%
%% ``option(P)'' tries to apply parser ``P''. If ``P'' fails without consuming input, it
%% returns the value ``nothing'', otherwise ``{just, X}'', where ``X'' is the value returned by ``P''.

optionMaybe(P) -> option(nothing, [do:bind || X <- P, return({just, X})]).


%% -spec optional(parsec_prim:gen_parser(Tok, St, _)) -> parsec_prim:gen_parser(Tok, St, {}).
%% @spec optional(P) -> parsec_prim:gen_parser(Tok, St, {})
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% @doc Optional parser, result ignored.
%%
%% ``option(P)'' tries to apply parser ``P''.
%% After consuming any input consumed by ``P'', ``option(P)'' returns ``{}''.

optional(P) ->
	pplus([do:bind || P
	                , return({})
	      ]
	     , return({})
	     ).


%% -spec between(parsec_prim:gen_parser(Tok, St, _), parsec_prim:gen_parser(Tok, St, _), parsec_prim:gen_parser(Tok, St, A)) -> parsec_prim:gen_parser(Tok, St, A).
%% @spec between(Open, Close, P) -> parsec_prim:gen_parser(Tok, St, A)
%% 	Open = parsec_prim:gen_parser(Tok, St, B)
%% 	Close = parsec_prim:gen_parser(Tok, St, C)
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% @doc Parses three parsers, returning the return value of the middle one.
%%
%% ``between(Open, Close, P)''parses ``Open'', followed by ``P'' and ``Close''. Returns the
%% value returned by ``P''.
%% braces = between (symbol "{") (symbol "}")

between(Open, Close, P) ->
	[ do:bind || Open
	           , X <- P
	           , Close
	           , return(X)
	].


%% -spec skipMany1(parsec_prim:gen_parser(Tok, St, _)) -> parsec_prim:gen_parser(Tok, St, {}).
%% @spec skipMany1(P) -> parsec_prim:gen_parser(Tok, St, {})
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% @doc Apply a parser one or more times, skipping its result.
%%
%% ``skipMany1(P)'' applies the parser ``P'' <em>one</em> or more times, skipping its result.

skipMany1(P) -> bind(P, fun(_) -> skipMany(P) end).


%% -spec many1(parsec_prim:gen_parser(Tok, St, A)) -> parsec_prim:gen_parser(Tok, St, [A]).
%% @spec many1(P) -> parsec_prim:gen_parser(Tok, St, [A])
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% @doc Apply a parser one or more times.
%%
%% ``many1(P)'' applies the parser ``P'' <em>one</em> or more times. Returns a list of the
%% returned values of ``P''.

many1(P) ->
	bind(P,
		fun(X) -> bind(many(P),
			fun(Xs) -> return([X|Xs]) end
			) end
		).


%% -spec sepBy(parsec_prim:gen_parser(Tok, St, A), parsec_prim:gen_parser(Tok, St, _)) -> parsec_prim:gen_parser(Tok, St, [A]).
%% @spec sepBy(P, Sep) -> parsec_prim:gen_parser(Tok, St, [A])
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% 	Sep = parsec_prim:gen_parser(Tok, St, Sep)
%% @doc Parse zero or more occurrences of a parser, separated by another parser
%%
%% ``sepBy(P, Sep)'' parses <em>zero</em> or more occurrences of ``P'', separated by ``Sep''. Returns
%% a list of values returned by ``P''.
%%```
%%commaSep(P) -> sepBy(P, symbol(",")).
%%'''

sepBy(P, Sep) -> pplus(sepBy1(P, Sep), return([])).


%% -spec sepBy1(parsec_prim:gen_parser(Tok, St, A), parsec_prim:gen_parser(Tok, St, _)) -> parsec_prim:gen_parser(Tok, St, [A]).
%% @spec sepBy1(P, Sep) -> parsec_prim:gen_parser(Tok, St, [A])
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% 	Sep = parsec_prim:gen_parser(Tok, St, Sep)
%% @doc Parse one or more occurrences of a parser, separated by another parser
%%
%% ``sepBy1(P, Sep)'' parses <em>one</em> or more occurrences of ``P'', separated by ``Sep''. Returns
%% a list of values returned by ``P''.

sepBy1(P, Sep) ->
	[do:bind ||
		X <- P,
		Xs <- many([do:bind||Sep, P]),
		return([X|Xs])
	].


%sepEndBy1, sepEndBy :: GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepEndBy1(P, Sep) ->
	%sepEndBy1 p sep     = do{ x <- p
	%                        ; do{ sep
	%                            ; xs <- sepEndBy p sep
	%                            ; return (x:xs)
	%                            }
	%                          <|> return [x]
	%                        }
	bind(P, fun(X) -> pplus(bind(Sep,
	                             fun(_) -> bind(sepEndBy(P, Sep),
	                                            fun(Xs) -> return([X|Xs]) end
	                                           )
	                             end
	                            ),
	                        return([X])
	                       )
	        end
	    ).


sepEndBy(P, Sep) -> pplus(sepEndBy1(P, Sep), return([])).


%% -spec endBy1(parsec_prim:gen_parser(Tok, St, A), parsec_prim:gen_parser(Tok, St, _)) -> parsec_prim:gen_parser(Tok, St, [A]).
%% @spec endBy1(P, Sep) -> parsec_prim:gen_parser(Tok, St, [A])
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% 	Sep = parsec_prim:gen_parser(Tok, St, Sep)
%% @doc Parse zero or more occurrences of a parser, separated by another parser
%%
%% ``endBy1(P, Sep)'' parses <em>one</em> or more occurrences of ``P'', separated and ended by ``Sep''. Returns
%% a list of values returned by ``P''.

endBy1(P, Sep) ->
	%endBy1 p sep        = many1 (do{ x <- p; sep; return x })
	many1(bind(P,
	           fun(X) -> bind(Sep,
	                          fun(_) -> return(X) end
	                         )
	           end
	          )
	     ).


%% -spec endBy(parsec_prim:gen_parser(Tok, St, A), parsec_prim:gen_parser(Tok, St, _)) -> parsec_prim:gen_parser(Tok, St, [A]).
%% @spec endBy(P, Sep) -> parsec_prim:gen_parser(Tok, St, [A])
%% 	P = parsec_prim:gen_parser(Tok, St, A)
%% 	Sep = parsec_prim:gen_parser(Tok, St, Sep)
%% @doc Parse zero or more occurrences of a parser, separated by another parser
%%
%% ``endBy(P, Sep)'' parses <em>zero</em> or more occurrences of ``P'', separated and ended by ``Sep''. Returns
%% a list of values returned by ``P''.
%%```
%%cStatements(P) -> endBy(cStatement(), semi()).
%%'''

endBy(P, Sep) ->
	%endBy p sep        = many (do{ x <- p; sep; return x })
	many(bind(P,
	          fun(X) -> bind(Sep,
	                         fun(_) -> return(X) end
	                        )
	          end
	         )
	    ).


%count :: Int -> GenParser tok st a -> GenParser tok st [a]

count(N, _) when N =< 0 -> return([]);
count(N, P) -> sequence(lists:duplicate(N, P)).


%chainr,chainl :: GenParser tok st a -> GenParser tok st (a -> a -> a) -> a -> GenParser tok st a

chainr(P, Op, X) -> pplus(chainr1(P, Op), return(X)).
chainl(P, Op, X) -> pplus(chainl1(P, Op), return(X)).


%chainr1,chainl1 :: GenParser tok st a -> GenParser tok st (a -> a -> a) -> GenParser tok st a

chainl1(P, Op) ->
	%chainl1 p op        = do{ x <- p; rest x }
	%                    where
	%                      rest x    = do{ f <- op
	%                                    ; y <- p
	%                                    ; rest (f x y)
	%                                    }
	%                                <|> return x
	Rest = fun(X, Rest) ->
		pplus(bind(Op,
		           fun(F) -> bind(P,
		                          fun(Y) -> Rest(F(X,Y), Rest) end
		                         )
		           end
		          ),
		      return(X))
		end,
	bind(P, fun(X) -> Rest(X, Rest) end).

chainr1(P, Op) ->
	%chainr1 p op        = scan
	%                    where
	%                      scan      = do{ x <- p; rest x }
	%
	%                      rest x    = do{ f <- op
	%                                    ; y <- scan
	%                                    ; return (f x y)
	%                                    }
	%                                <|> return x
	Scan = fun(Scan, Rest) -> bind(P, fun(X) -> Rest(X, Scan, Rest) end) end,
	Rest = fun(X, Rescan, Rest) ->
		pplus(bind(Op,
		           fun(F) -> bind(Rescan(Rescan, Rest),
		                          fun(Y) -> return(F(X,Y)) end
		                         )
		           end
		          ),
		      return(X))
		end,
	Scan(Scan, Rest).


%anyToken :: Show tok => GenParser tok st tok

anyToken() -> anyToken(fun generic_show/1).

generic_show(X) when is_list(X) ->
	try lists:flatten(io_lib:format("\"~ts\"", [X])) of
		S -> S
	catch
		error:badarg -> lists:flatten(io_lib:format("~p", [X]))
	end;
generic_show(X) ->
	try lists:flatten(io_lib:format("'~tc'", [X])) of
		S -> S
	catch
		error:badarg -> lists:flatten(io_lib:format("~p", [X]))
	end.


anyToken(Show) ->
	%anyToken            = tokenPrim show (\pos tok toks -> pos) Just
	tokenPrim(Show, fun(Pos, _, _) -> Pos end, fun(Tok) -> {just, Tok} end).

%eof :: Show tok => GenParser tok st ()
%eof                 = notFollowedBy anyToken <?> "end of input"
eof() -> label(notFollowedBy(anyToken()), "end of input").


%notFollowedBy :: Show tok => GenParser tok st tok -> GenParser tok st ()
notFollowedBy(P) ->
	%notFollowedBy p     = try (do{ c <- p; unexpected (show [c]) }
	%                           <|> return ()
	%                          )
	'try'(pplus(bind(P, fun(C) -> unexpected(generic_show([C])) end),
	            return({}))).



%manyTill :: GenParser tok st a -> GenParser tok st end -> GenParser tok st [a]
manyTill(P, End) ->
	%manyTill p end      = scan
	%                    where
	%                      scan  = do{ end; return [] }
	%                            <|>
	%                              do{ x <- p; xs <- scan; return (x:xs) }
	pplus(bind(End, fun(_) -> return([]) end),
	      bind(P, fun(X) -> bind(manyTill(P, End),
	                             fun(Xs) -> return([X|Xs]) end
	                            )
	              end
	          )
	     ).


%lookAhead :: GenParser tok st a -> GenParser tok st a
lookAhead(P) ->
	%lookAhead p         = do{ state <- getParserState
	%                        ; x <- p
	%                        ; setParserState state
	%                        ; return x
	%                        }
	bind(getParserState(),
	     fun(State) -> bind(P,
	                        fun(X) -> bind(setParserState(State),
	                                       fun(_) -> return(X) end
	                                      )
	                        end
	                       )
	     end
	    ).
