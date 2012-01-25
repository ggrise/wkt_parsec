%%% @author David Mercer <dmercer@alum.mit.edu>
%%% @copyright 2009 David Mercer.  Use permitted in accordance with MIT license.
%%% @version {@version}, {@date} {@time}
%%% @doc Parse transform for importing all functions exported by a module and unimporting functions previously imported
%%%
%%% ==Example==
%%%```
%%%-module(example_import).
%%%
%%%-compile({parse_transform, import_enhancements}).
%%%
%%%-export([reverse/1, make_list/2]).
%%%
%%%-import(lists).
%%%-import(lists, [reverse/1]).  % This will result in an error unless unimported.
%%%-unimport({lists,
%%%	[ {last,1} % unimports lists:last/1
%%%	, {reverse,1} % unimports both imports of lists:reverse/1
%%%	, {no_such_function/0} % ignored
%%%	]}).
%%%
%%% %% Can now define our own reverse/1 function.
%%% %% This would error if both imports had not been undone.
%%%
%%%reverse(L) -> {reversed, lists:reverse(L)}.
%%%
%%%
%%% %% Can still use other lists functions that were not unimported.
%%%
%%%make_list(N, Elem) -> duplicate(N, Elem).
%%%'''

-module(import_enhancements).

-export([parse_transform/2]).

parse_transform(Forms, Options) ->
	Verbose = lists:member(verbose, Options),
	put(verbose, Verbose),
	if Verbose ->
			ModName = atom_to_list(?MODULE),
			Name = ModName,
			StarsLen = (77 - length(Name)) div 2,
			Stars = lists:duplicate(StarsLen, $*),
			io:format("~s\n", [Stars ++ " " ++ Name ++ " " ++ Stars]);
		true -> []
		end,
	% io:format("Forms: ~p\n\nOptions: ~p\n", [Forms, Options]),
    scan(Forms, [], Verbose).


%% scan: go through each form, processing -import and -unimport directives

%% Terminal case
scan([], FormAccum, _Verbose) -> lists:reverse(FormAccum);


%% -import(Module).
scan([F = {attribute, Line, import, [Module]} | Fs], FormAccum, Verbose) ->
	if Verbose ->
			io:format("Line ~B:\n\n~s\n\n", [Line, prettypr:format(prettypr:beside(prettypr:text("\t"), erl_prettypr:layout(F)))]);
		true -> []
		end,

	{ok,{Module,[{exports,Exports}]}} = beam_lib:chunks(code:which(Module), [exports]),
	Exports2 = lists:filter(fun({module_info,_}) -> false; (_) -> true end, Exports),
	F2 = {attribute, Line, import, {Module, Exports2}},

	if Verbose ->
			io:format("Transformed to:\n\n~s\n", [prettypr:format(prettypr:beside(prettypr:text("\t"), erl_prettypr:layout(F2)))]),
			io:format("\n~s\n", [lists:duplicate(79, $-)]);
		true -> []
		end,

	scan(Fs, [F2 | FormAccum], Verbose);

%% -unimport({Module, Unimports}).
scan([F = {attribute,Line,unimport,Unimport} | Fs], FormAccum, Verbose) ->
	if Verbose ->
			io:format("~s\n", [prettypr:format(prettypr:beside(prettypr:text(io_lib:format("Line ~B: ", [Line])), erl_prettypr:layout(F)))]);
		true -> []
		end,

	FormAccum2 = unimport(Unimport, FormAccum, [], Verbose),

	if Verbose ->
			io:format("\n~s\n", [lists:duplicate(79, $-)]);
		true -> []
		end,

	scan(Fs, [F|FormAccum2], Verbose);

%% Other form
scan([F|Fs], FormAccum, Verbose) -> scan(Fs, [F|FormAccum], Verbose).


%% unimport: go through previously processed forms, unimporting functions as requested.

%% Terminal case
unimport(_, [], Forms, _) -> lists:reverse(Forms);

%% Previous -import(Module, [...]).
unimport(
	{Module,Unimports},
	[F = {attribute, Line, import, {Module, Imports}} | Fs],
	Forms,
	Verbose)
->
	Unimport = fun(FunSpec) -> not(lists:member(FunSpec, Unimports)) end,
	Imports2 = lists:filter(Unimport, Imports),
	F2 = {attribute, Line, import, {Module, Imports2}},

	if Verbose, Imports =/= Imports2 ->
			io:format("\nLine ~B:\n\n~s\n\n", [Line, prettypr:format(prettypr:beside(prettypr:text("\t"), erl_prettypr:layout(F)))]),
			io:format("Transformed to:\n\n~s\n", [prettypr:format(prettypr:beside(prettypr:text("\t"), erl_prettypr:layout(F2)))]);
		true -> []
		end,

	unimport({Module, Unimports}, Fs, [F2|Forms], Verbose);

%% Non-import(Module, [...]) form
unimport(UnimportSpecs, [F|Fs], Forms, Verbose) ->
	unimport(UnimportSpecs, Fs, [F|Forms], Verbose).
