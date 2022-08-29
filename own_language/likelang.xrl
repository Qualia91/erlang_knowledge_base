Definitions.

U         = [A-Z]
L         = [a-z]
D         = [0-9]
C         = [\%]
WS        = [\s\t\n]
LB        = \n|\r\n|\r
LP        = \(
RP        = \)
SEP       = \,
FUNC_SEP  = :
MATH      = [\+\-\/\*]
END       = [\.]

Rules.

% Atom
{L}({U}|{L}|{D}|_)* :
    {token, {atom, TokenLine, list_to_atom(TokenChars)}}.
% Number
{D}+ :
    {token, {number, TokenLine, list_to_integer(TokenChars)}}.
% Variable
{U}({U}|{L}|{D}|_)* :
    {token, {variable, TokenLine, TokenChars}}.
    
{LP} :
    {token, {lparen, TokenLine}}.
{RP} :
    {token, {rparen, TokenLine}}.
    
{END} :
    {token, {end_func, TokenLine}}.


{MATH} : 
    {token, {math, TokenLine, list_to_atom(TokenChars)}}.

{C}* [^{C}]* : skip_token.
{WS}         : skip_token.
{LB}         : skip_token.
{SEP}        : skip_token.
{FUNC_SEP}   : skip_token.

Erlang code.
%% Purpose : Token definitions for LikeLang.
