-module(parsec).

-compile(export_all).


runP(A,B)                      -> parsec_prim:runP(A,B).
runParser(A,B,C,D)             -> parsec_prim:runParser(A,B,C,D).
parse(A,B,C)                   -> parsec_prim:parse(A,B,C).
parseFromFile(A,B)             -> parsec_prim:parseFromFile(A,B).
parseTest(A,B)                 -> parsec_prim:parseTest(A,B).
return(A)                      -> parsec_prim:return(A).
bind(A,B)                      -> parsec_prim:bind(A,B).
fail(A)                        -> parsec_prim:fail(A).
token(A,B,C)                   -> parsec_prim:token(A,B,C).
tokens(A,B,C)                  -> parsec_prim:tokens(A,B,C).
tokenPrim(A,B,C)               -> parsec_prim:tokenPrim(A,B,C).
tokenPrimEx(A,B,C,D)           -> parsec_prim:tokenPrimEx(A,B,C,D).
'try'(A)                       -> parsec_prim:'try'(A).
label(A,B)                     -> parsec_prim:label(A,B).
labels(A,B)                    -> parsec_prim:labels(A,B).
unexpected(A)                  -> parsec_prim:unexpected(A).
map(A,B)                       -> parsec_prim:map(A,B).
pzero()                        -> parsec_prim:pzero().
pplus(A,B)                     -> parsec_prim:pplus(A,B).
sequence(A)                    -> parsec_prim:sequence(A).
many(A)                        -> parsec_prim:many(A).
skipMany(A)                    -> parsec_prim:skipMany(A).
getState()                     -> parsec_prim:getState().
setState(A)                    -> parsec_prim:setState(A).
updateState(A)                 -> parsec_prim:updateState(A).
getPosition()                  -> parsec_prim:getPosition().
setPosition(A)                 -> parsec_prim:setPosition(A).
getInput()                     -> parsec_prim:getInput().
setInput(A)                    -> parsec_prim:setInput(A).
getParserState()               -> parsec_prim:getParserState().
setParserState(A)              -> parsec_prim:setParserState(A).
withErrors(A)                  -> parsec_prim:withErrors(A).

choice(A)                      -> parsec_combinator:choice(A).
count(A,B)                     -> parsec_combinator:count(A,B).
between(A,B,C)                 -> parsec_combinator:between(A,B,C).
option(A,B)                    -> parsec_combinator:option(A,B).
optionMaybe(A)                 -> parsec_combinator:optionMaybe(A).
optional(A)                    -> parsec_combinator:optional(A).
skipMany1(A)                   -> parsec_combinator:skipMany1(A).
many1(A)                       -> parsec_combinator:many1(A).
sepBy(A,B)                     -> parsec_combinator:sepBy(A,B).
sepBy1(A,B)                    -> parsec_combinator:sepBy1(A,B).
endBy(A,B)                     -> parsec_combinator:endBy(A,B).
endBy1(A,B)                    -> parsec_combinator:endBy1(A,B).
sepEndBy(A,B)                  -> parsec_combinator:sepEndBy(A,B).
sepEndBy1(A,B)                 -> parsec_combinator:sepEndBy1(A,B).
chainl(A,B,C)                  -> parsec_combinator:chainl(A,B,C).
chainl1(A,B)                   -> parsec_combinator:chainl1(A,B).
chainr(A,B,C)                  -> parsec_combinator:chainr(A,B,C).
chainr1(A,B)                   -> parsec_combinator:chainr1(A,B).
eof()                          -> parsec_combinator:eof().
notFollowedBy(A)               -> parsec_combinator:notFollowedBy(A).
manyTill(A,B)                  -> parsec_combinator:manyTill(A,B).
lookAhead(A)                   -> parsec_combinator:lookAhead(A).
anyToken()                     -> parsec_combinator:anyToken().
anyToken(A)                    -> parsec_combinator:anyToken(A).


sourceLine(A)                  -> parsec_pos:sourceLine(A).
sourceColumn(A)                -> parsec_pos:sourceColumn(A).
sourceName(A)                  -> parsec_pos:sourceName(A).
incSourceLine(A,B)             -> parsec_pos:incSourceLine(A,B).
incSourceColumn(A,B)           -> parsec_pos:incSourceColumn(A,B).
setSourceLine(A,B)             -> parsec_pos:setSourceLine(A,B).
setSourceColumn(A,B)           -> parsec_pos:setSourceColumn(A,B).
setSourceName(A,B)             -> parsec_pos:setSourceName(A,B).
newPos(A,B,C)                  -> parsec_pos:newPos(A,B,C).
initialPos(A)                  -> parsec_pos:initialPos(A).
updatePosChar(A,B)             -> parsec_pos:updatePosChar(A,B).
updatePosString(A,B)           -> parsec_pos:updatePosString(A,B).


messageString(A)               -> parsec_error:messageString(A).
messageCompare(A,B)            -> parsec_error:messageCompare(A,B).
messageEq(A,B)                 -> parsec_error:messageEq(A,B).
errorPos(A)                    -> parsec_error:errorPos(A).
errorMessages(A)               -> parsec_error:errorMessages(A).
errorIsUnknown(A)              -> parsec_error:errorIsUnknown(A).
showErrorMessages(A,B,C,D,E,F) -> parsec_error:showErrorMessages(A,B,C,D,E,F).
newErrorMessage(A,B)           -> parsec_error:newErrorMessage(A,B).
newErrorUnknown(A)             -> parsec_error:newErrorUnknown(A).
addErrorMessage(A,B)           -> parsec_error:addErrorMessage(A,B).
setErrorPos(A,B)               -> parsec_error:setErrorPos(A,B).
setErrorMessage(A,B)           -> parsec_error:setErrorMessage(A,B).
mergeError(A,B)                -> parsec_error:mergeError(A,B).


spaces()                       -> parsec_char:spaces().
space()                        -> parsec_char:space().
newline()                      -> parsec_char:newline().
tab()                          -> parsec_char:tab().
upper()                        -> parsec_char:upper().
lower()                        -> parsec_char:lower().
alphaNum()                     -> parsec_char:alphaNum().
letter()                       -> parsec_char:letter().
digit()                        -> parsec_char:digit().
hexDigit()                     -> parsec_char:hexDigit().
octDigit()                     -> parsec_char:octDigit().
char(A)                        -> parsec_char:char(A).
string(A)                      -> parsec_char:string(A).
anyChar()                      -> parsec_char:anyChar().
oneOf(A)                       -> parsec_char:oneOf(A).
noneOf(A)                      -> parsec_char:noneOf(A).
satisfy(A)                     -> parsec_char:satisfy(A).

