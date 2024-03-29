% Lexes a selector

Definitions.

D    = [0-9]
L    = [A-Za-z0-9_']
WS   = ([\000-\s]|%.*)
CR   = (\r|\n|\t)
ARROW = (->)
OPEN   = [{]
CLOSE  = [}]
OPEN_LIST   = [\[]
CLOSE_LIST  = [\]]
OPEN_MSG  = (<)
CLOSE_MSG = (>)
EQUALS = [=]
SEP  = [\\]
ERLANG_TERMS = [A-Za-z0-9{}\+\-\[\]_'///\/",=()\000-\s]

Rules.
GLOBAL{OPEN}		:   {token,{global_open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}GLOBAL		:   {token,{global_close,TokenLine,list_to_atom(TokenChars)}}.

EVENTS{OPEN}		:   {token,{events_open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}EVENTS		:   {token,{events_close,TokenLine,list_to_atom(TokenChars)}}.

CALL			:   {token,{call_event,TokenLine,list_to_atom("call")}}.
RECEIVE			:   {token,{receive_event,TokenLine,list_to_atom("receive")}}.
SEND			:   {token,{send_event,TokenLine,list_to_atom("send")}}.
SPAWN			:   {token,{spawn_event,TokenLine,list_to_atom("spawn")}}.
EXIT 			:   {token,{exit_event,TokenLine,list_to_atom("exit")}}.
REGISTER		:   {token,{register_event,TokenLine,list_to_atom("register")}}.
UNREGISTER		:   {token,{unregister_event,TokenLine,list_to_atom("unregister")}}.
LINK			:   {token,{link_event,TokenLine,list_to_atom("link")}}.
UNLINK			:   {token,{unlink_event,TokenLine,list_to_atom("unlink")}}.
CHANNEL			:   {token,{channel_event,TokenLine,list_to_atom("channel")}}.

{OPEN_MSG}{WS}*{ERLANG_TERMS}*{WS}*{CLOSE_MSG} : {token,{message_pattern,TokenLine,stripMessage(TokenChars,TokenLen)}}.

%%--------------------------Foreach Rules--------------------------------
FOREACH :   {token,{foreach_open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}FOREACH		:   {token,{foreach_close,TokenLine,list_to_atom(TokenChars)}}.
%%-----------------------------------------------------------------------

%%---------------------------Property Rules------------------------------

PROPERTY{WS}+{L}+{OPEN}	:   {token,{property_open,TokenLine,list_to_atom(stripProperty(TokenChars,TokenLen))}}.
{CLOSE}PROPERTY		:   {token,{property_close,TokenLine,list_to_atom(TokenChars)}}.

VARIABLES{OPEN}		:   {token,{variables_open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}VARIABLES	:   {token,{variables_close,TokenLine,list_to_atom(TokenChars)}}.

STATES{OPEN}		:   {token,{states_open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}STATES		:   {token,{states_close,TokenLine,list_to_atom(TokenChars)}}.

TRANSITIONS{OPEN}	:   {token,{transitions_open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}TRANSITIONS	:   {token,{transitions_close,TokenLine,list_to_atom(TokenChars)}}.

BAD{OPEN}		:   {token,{bad_states_open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}BAD		:   {token,{bad_states_close,TokenLine,list_to_atom(TokenChars)}}.

NORMAL{OPEN}		:   {token,{normal_states_open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}NORMAL		:   {token,{normal_states_close,TokenLine,list_to_atom(TokenChars)}}.

STARTING{OPEN}		:   {token,{starting_state_open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}STARTING		:   {token,{starting_state_close,TokenLine,list_to_atom(TokenChars)}}.

%%-----------------------------------------------------------------------
{SEP}{OPEN_LIST}({L}+[,]?{WS}?)*{CLOSE_LIST} :   {token,{condition,TokenLine,stripCondition(TokenChars,TokenLen)}}.
{SEP}{OPEN}({L}+[,]?{WS}?)*{CLOSE} :   {token,{condition,TokenLine,stripCondition(TokenChars,TokenLen)}}.

{OPEN}	    		:   {token,{open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}	    		:   {token,{close,TokenLine,list_to_atom(TokenChars)}}.

{OPEN_LIST}    		:   {token,{open_list,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE_LIST}   		:   {token,{close_list,TokenLine,list_to_atom(TokenChars)}}.

{L}+[,]        		:   {token,{var,TokenLine,list_to_atom(stripComma(TokenChars,TokenLen))}}.
{L}+        		:   {token,{var,TokenLine,list_to_atom(TokenChars)}}.
{EQUALS}      		:   {token,{equals,TokenLine,list_to_atom(TokenChars)}}.
{ARROW}      		:   {token,{arrow,TokenLine,list_to_atom(TokenChars)}}.
{SEP}      		:   {token,{separator,TokenLine,list_to_atom(TokenChars)}}.
{D}+        		:   {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{WS}+       		:   skip_token.
{CR}+       		:   skip_token.

Erlang code.

-export([reserved_word/1]).

reserved_word('in') -> true.

stripMessage(TokenChars,TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).
stripComma(TokenChars,TokenLen) -> lists:sublist(TokenChars, TokenLen - 1).
stripProperty(TokenChars,TokenLen) -> lists:sublist(TokenChars, 10, TokenLen - 10).
stripCondition(TokenChars,TokenLen) -> lists:sublist(TokenChars, 2, TokenLen).
