-record(state,
	{ stateInput
	, statePos = parsec_pos:initialPos("")
	, stateUser = {}
	}).
