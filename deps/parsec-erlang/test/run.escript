
-record(args,
       { suite = undefined
       , testcases = []
       }).


main(Args) ->
	ParsedArgs = parse_args(Args),
	%io:format("~p\n", [ParsedArgs]),
	TestDir = filename:dirname(escript:script_name()),
	file:set_cwd(TestDir),
	LogDir = "log",
	case file:make_dir(LogDir) of
		ok -> ok;
		{error, eexist} -> ok;
		Error -> exit(Error)
	end,
	RunArgs =
		[ {dir, "."}
		, {logdir, LogDir}
		, {cover, "test.coverspec"}
		| case ParsedArgs#args.suite of
				undefined -> [];
				Suite -> [{suite, Suite}]
			end
		++ [{testcase, ParsedArgs#args.testcases}]
		],
	%io:format("~p\n", [RunArgs]),
	%exit(normal),
	RetVal = (catch ct:run_test(RunArgs)),
	io:format("~p", [RetVal]).


parse_args(Args) -> parse_args(Args, #args{}).


parse_args([], Args) -> Args;

parse_args([[$-, $S | Suite] | Rest], Args) ->
	parse_args(Rest, Args#args{suite=Suite});

parse_args([[$-, $T | TestCases] | Rest], Args) ->
	parse_args(Rest,
		Args#args
			{ testcases=
				Args#args.testcases
	            	++ lists:map( fun erlang:list_to_atom/1
	                            , split($,, TestCases)
	                            )
	        });

parse_args([_|Rest], Args) -> parse_args(Rest, Args).


split(Char, String) -> split(Char, String, [""]).


split(_C, [], Accum) -> lists:reverse(Accum);

split(C, [C|Rest], Accum) -> split(C, Rest, [""|Accum]);

split(C, [C2|Rest], [Elt | Accum]) ->
	split(C, Rest, [Elt++[C2] | Accum]).
