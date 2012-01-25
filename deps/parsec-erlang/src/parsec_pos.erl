-module(parsec_pos).

-compile(inline).

-export(
	[ sourceLine/1
	, sourceColumn/1
	, sourceName/1
	, incSourceLine/2
	, incSourceColumn/2
	, setSourceLine/2
	, setSourceColumn/2
	, setSourceName/2
	, newPos/3
	, initialPos/1
	, updatePosChar/2
	, updatePosString/2
	]).
-export([show/1]).

-type source_name() :: string().

-type line() :: integer().

-type column() :: integer().

-type source_pos() :: {source_pos, source_name() | undefined, line(), column()}.

%% @type source_name() = string().
%% @type line() = integer().
%% @type column() = integer().
%% @type source_pos() = {source_pos, source_name() | undefined, line(), column()}.


%newPos :: SourceName -> Line -> Column -> SourcePos
-spec newPos(source_name(), line(), column()) -> source_pos().
newPos(SourceName, Line, Column) -> {source_pos, SourceName, Line, Column}.


%initialPos :: SourceName -> SourcePos
initialPos(SourceName) -> newPos(SourceName, 1, 1).


%sourceName :: SourcePos -> SourceName
sourceName({source_pos, Name, _, _}) -> Name.


%sourceLine :: SourcePos -> Line
sourceLine({source_pos, _, Line, _}) -> Line.


%sourceColumn :: SourcePos -> Column
sourceColumn({source_pos, _, _, Column}) -> Column.


%incSourceLine :: SourcePos -> Line -> SourcePos
incSourceLine({source_pos, Name, Line, Column}, N) -> {source_pos, Name, Line + N, Column}.


%incSourceColumn :: SourcePos -> Column -> SourcePos
incSourceColumn({source_pos, Name, Line, Column}, N) -> {source_pos, Name, Line, Column + N}.


%setSourceName :: SourcePos -> SourceName -> SourcePos
setSourceName({source_pos, _, Line, Column}, N) -> {source_pos, N, Line, Column}.


%setSourceLine :: SourcePos -> Line -> SourcePos
setSourceLine({source_pos, Name, _, Column}, N) -> {source_pos, Name, N, Column}.


%setSourceColumn :: SourcePos -> Column -> SourcePos
setSourceColumn({source_pos, Name, Line, _}, N) -> {source_pos, Name, Line, N}.


%updatePosString :: SourcePos -> String -> SourcePos
updatePosString(Pos, String) -> forcePos(lists:foldl(fun(Elem, AccIn) -> updatePosChar(AccIn, Elem) end, Pos, String)).

%updatePosChar   :: SourcePos -> Char -> SourcePos
updatePosChar({source_pos, Name, Line, Column}, C) ->
	forcePos(case C of
			$\n -> {source_pos, Name, Line + 1, Column};
			$\t -> {source_pos, Name, Line, Column + 8 - ((Column - 1) rem 8)};
			_ -> {source_pos, Name, Line, Column + 1}
		end).


%forcePos :: SourcePos -> SourcePos
forcePos(Pos) -> Pos.


show({source_pos, [], Line, Column}) -> showLineColumn(Line, Column);
show({source_pos, Name, Line, Column}) -> "\"" ++ Name ++ "\" " ++ showLineColumn(Line, Column).

showLineColumn(Line, Column) -> lists:flatten(io_lib:format("(line ~p, column ~p)", [Line, Column])).
