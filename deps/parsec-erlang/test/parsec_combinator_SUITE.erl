-module(parsec_combinator_SUITE).

-compile({parse_transform, import_enhancements}).

-include("ct.hrl").

-import(parsec_prim).
-import(parsec_combinator).
-import(parsec_char).

% ct:run("test", [_SUITE]).

-compile(export_all).

-include_lib("parsec/include/record_state.hrl").

all() ->
	[ choice__0
	, choice__1
	, choice__2
	, choice__3
	, choice__4
	, optionMaybe__0
	, optionMaybe__1
	, optional__0
	, optional__1
	, between__0
	, between__1
	, between__2
	, sepBy__0
	, sepBy__1
	, sepBy__2
	, sepBy__3
	, sepBy__4
	, sepBy1__0
	, sepBy1__1
	, sepBy1__2
	, sepBy1__3
	, sepBy1__4
	].


choice__0(_) ->
	{ consumed
	, { ok, $a
	  , { state
	    , "$"
	    , {source_pos,"",1,2}
	    , {}
	    }
	  , {parse_error, {source_pos,"",1,2}, []}
	  }
	} = runP(choice([char($a), char($b), char($c)]), #state{stateInput = "a$"}).


choice__1(_) ->
	{ consumed
	, { ok, $b
	  , { state
	    , "$"
	    , {source_pos,"",1,2}
	    , {}
	    }
	  , {parse_error, {source_pos,"",1,2}, []}
	  }
	} = runP(choice([char($a), char($b), char($c)]), #state{stateInput = "b$"}).


choice__2(_) ->
	{ consumed
	, { ok, $c
	  , { state
	    , "$"
	    , {source_pos,"",1,2}
	    , {}
	    }
	  , {parse_error, {source_pos,"",1,2}, []}
	  }
	} = runP(choice([char($a), char($b), char($c)]), #state{stateInput = "c$"}).


choice__3(_) ->
	{ empty
	, { error
	  , { parse_error
	    , {source_pos,"",1,1}
	    , [ {expect,"'a'"}
	      , {sysunexpect,"'d'"}
	      , {expect,"'b'"}
	      , {sysunexpect,"'d'"}
	      , {expect,"'c'"}
	      , {sysunexpect,"'d'"}
	      ]
	    }
	  }
	} = runP(choice([char($a), char($b), char($c)]), #state{stateInput = "d$"}).


choice__4(_) ->
	{ empty
	, { error
	  , { parse_error
	    , {source_pos,"",1,1}
	    , []
	    }
	  }
	} = runP(choice([]), #state{stateInput = "$"}).


optionMaybe__0(_) ->
	{consumed, {ok,{just,$a},{state,"b",{source_pos,"",1,2},{}},{parse_error,{source_pos,"",1,2},[]}}}
		= runP(optionMaybe(char($a)), #state{stateInput = "ab"}).

optionMaybe__1(_) ->
	{empty, {ok,nothing,{state,"xb",{source_pos,"",1,1},{}},{parse_error,{source_pos,"",1,1},[{expect,"'a'"},{sysunexpect,"'x'"}]}}}
		= runP(optionMaybe(char($a)), #state{stateInput = "xb"}).


optional__0(_) ->
	{consumed, {ok,{},{state,"b",{source_pos,"",1,2},{}},{parse_error,{source_pos,"",1,2},[]}}}
		= runP(optional(char($a)), #state{stateInput = "ab"}).

optional__1(_) ->
	{empty, {ok,{},{state,"xb",{source_pos,"",1,1},{}},{parse_error,{source_pos,"",1,1},[{expect,"'a'"},{sysunexpect,"'x'"}]}}}
		= runP(optional(char($a)), #state{stateInput = "xb"}).


between__0(_) ->
	{consumed, {ok,$a,{state,"b",{source_pos,"",1,4},{}},{parse_error,{source_pos,"",1,4},[]}}}
		= runP(between(char(${), char($}), char($a)), #state{stateInput = "{a}b"}).

between__1(_) ->
	{ empty
	, {error
	  , {parse_error
	    , {source_pos,"",1,1}
	    , [ {expect,"'{'"}
	      , {sysunexpect,"'a'"}
	      ]
	    }
	  }
	} = runP(between(char(${), char($}), char($a)), #state{stateInput = "a}b"}).

between__2(_) ->
	{ consumed
	, {error
	  , {parse_error
	    , {source_pos,"",1,3}
	    , [ {expect,"'}'"}
	      , {sysunexpect,"'b'"}
	      ]
	    }
	  }
	} = runP(between(char(${), char($}), char($a)), #state{stateInput = "{ab"}).


sepBy__0(_) ->
	{consumed, {ok,"aa",{state,"c",{source_pos,"",1,4},{}},{parse_error,{source_pos,"",1,4},[{expect,"'b'"},{sysunexpect,"'c'"}]}}}
		= runP(sepBy(char($a), char($b)), #state{stateInput = "abac"}).


sepBy__1(_) ->
	{consumed, {error,{parse_error,{source_pos,"",1,5},[{expect,"'a'"},{sysunexpect,[]}]}}}
		= runP(sepBy(char($a), char($b)), #state{stateInput = "abab"}).


sepBy__2(_) ->
	{consumed, {ok,"aa",{state,[],{source_pos,"",1,4},{}},{parse_error,{source_pos,"",1,4},[{expect,"'b'"},{sysunexpect,[]}]}}}
		= runP(sepBy(char($a), char($b)), #state{stateInput = "aba"}).


sepBy__3(_) ->
	{ empty
	, { ok,""
	  , {state,"ba",{source_pos,"",1,1},{}}
	  , {parse_error,{source_pos,"",1,1},[{expect,"'a'"},{sysunexpect,"'b'"}]}
	  }
	} = runP(sepBy(char($a), char($b)), #state{stateInput = "ba"}).


sepBy__4(_) ->
	{ consumed
	, { ok,"a"
	  , {state,"c",{source_pos,"",1,2},{}}
	  , {parse_error,{source_pos,"",1,2},[{expect,"'b'"},{sysunexpect,"'c'"}]}
	  }
	} = runP(sepBy(char($a), char($b)), #state{stateInput = "ac"}).


sepBy1__0(_) ->
	{consumed, {ok,"aa",{state,"c",{source_pos,"",1,4},{}},{parse_error,{source_pos,"",1,4},[{expect,"'b'"},{sysunexpect,"'c'"}]}}}
		= runP(sepBy1(char($a), char($b)), #state{stateInput = "abac"}).


sepBy1__1(_) ->
	{ consumed
	, {error,{parse_error,{source_pos,"",1,5},[{expect,"'a'"},{sysunexpect,[]}]}}
	} = runP(sepBy1(char($a), char($b)), #state{stateInput = "abab"}).


sepBy1__2(_) ->
	{consumed, {ok,"aa",{state,[],{source_pos,"",1,4},{}},{parse_error,{source_pos,"",1,4},[{expect,"'b'"},{sysunexpect,[]}]}}}
		= runP(sepBy1(char($a), char($b)), #state{stateInput = "aba"}).


sepBy1__3(_) ->
	{ empty
	, {error,{parse_error,{source_pos,"",1,1},[{expect,"'a'"},{sysunexpect,"'b'"}]}}
	} = runP(sepBy1(char($a), char($b)), #state{stateInput = "ba"}).


sepBy1__4(_) ->
	{ consumed
	, { ok,"a"
	  , {state,"c",{source_pos,"",1,2},{}}
	  , {parse_error,{source_pos,"",1,2},[{expect,"'b'"},{sysunexpect,"'c'"}]}
	  }
	} = runP(sepBy1(char($a), char($b)), #state{stateInput = "ac"}).


