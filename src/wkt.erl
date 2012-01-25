-module(wkt).
-export([parse_wkt/1, parse_wkt_file/1]).

between(P) ->
	A = parsec:sequence([parsec:skipMany(parsec_char:space()), 
			parsec:char($(), parsec:skipMany(parsec_char:space())]),
	B = parsec:sequence([parsec:skipMany(parsec_char:space()), 
			parsec:char($)), parsec:skipMany(parsec_char:space())]),
	parsec:between(A, B, P).
sep(P) ->
	parsec:sepBy(P,
		parsec:sequence([parsec:skipMany(parsec_char:space()), 
				parsec:char($,), parsec:skipMany(parsec_char:space())])).
parse_wkt_file(File) ->
	{_, Result} = parsec:parseFromFile(parse(), File),
	Result.


parse_wkt(Wkt) ->
	{_, Result} = parsec:parse(parse(), "Wkt", Wkt),
	Result.



parse() ->
	%number primitive
	Number = parsec:many1(parsec:digit()),
	Space = parsec_char:spaces(),
	Float = parsec:bind(Number, fun(N) ->
				parsec:pplus(
					parsec:bind(parsec:char($.), fun(_) ->
								parsec:bind(Number, fun(D) ->

									parsec:return(list_to_float(N ++ [$.|D]))
								end) 
					end), parsec:return(list_to_integer(N))) end),

	%coordinate X Y -> {X, Y}
	Coord = parsec:bind(Float, fun(X) ->
				parsec:bind(Space, fun(_) ->
					parsec:bind(Float, fun(Y) ->
								parsec:return({X, Y})
					end)
				end) 
	end),

	%coordinate sequence (X Y, X1 X2) -> [{X, Y}, {X1, X2}] 

	CoordSeq = between(parsec:bind(sep(Coord), 
			fun(V) ->
					parsec:return(V)			
	end)),

	%geometry
	Point = between(Coord),
	Polygon = between(sep(CoordSeq)),
	Multi = fun(Geom) -> between(sep(Geom)) end,
	
	PointType = parsec:bind(Point, fun(C) ->
			parsec:return({'POINT', C})
	end),
	LineStringType = parsec:bind(CoordSeq, fun(Seq) ->
			parsec:return({'LINESTRING', Seq})
	end),
	PolygonType = parsec:bind(Polygon, fun(List) ->
			[External|Holes] = List,
			parsec:return({'POLYGON', {External, Holes}})
	end),
	MultiPolygonType = parsec:bind(Multi(Polygon), fun(List) ->
			parsec:return({'MULTIPOLYGON', [{E, H} || [E|H] <- List]})
	end),
	MultiPointType = parsec:bind(Multi(Point), fun(List) ->
			parsec:return({'MULTIPOINT', List})
	end),
	MultiLinestringType = parsec:bind(Multi(CoordSeq), fun(List) ->
			parsec:return({'MULTILINESTRING', List})
	end),

	Type = parsec:bind(parsec:many1(parsec_char:letter()), fun(Type1) ->
			Type = string:to_upper(Type1),
			case Type of
				"POINT" ->
					PointType;
				"POLYGON" ->
					PolygonType;
				"LINESTRING" ->
					LineStringType;
				"MULTIPOLYGON" ->
					MultiPolygonType;
				"MULTIPOINT" ->
					MultiPointType;
				"MULTILINESTRING" ->
					MultiLinestringType;
				_ ->
					parsec:fail("Unknown type")
			end
	end),
	Collection = parsec:bind(parsec:string("GEOMETRYCOLLECTION"), fun(_) ->
			parsec:bind(between(sep(Type)), fun(V) -> parsec:return({'GEOMETRYCOLLECTION', V}) end)
	end),
	parsec:choice([Collection, Type]).



