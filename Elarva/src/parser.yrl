Nonterminals 
	GlobalTag
	Global
	ForeachTop
	Foreach
	ForeachTag
	EventsTag 
	Events 
	Event
	ForeachEvent
	PropertyEvent
	PropertiesTop
	Properties
	PropertyTag
	Property
	VariablesTag 
	Variables 
	Variable
	StatesTag 
	States 
	BadStatesTag 
	BadStates 
	BadState
	NormalStatesTag 
	NormalStates 
	NormalState
	StartingStateTag 
	StartingState
	TransitionsTag 
	Transitions 
	Transition.

Terminals
	var
	equals
	open
	close
	open_list
	close_list
	call_event
	receive_event
	send_event
	spawn_event
	exit_event
	register_event
	unregister_event
	link_event
	unlink_event
	channel_event
	message_pattern
	integer
	arrow
	separator
	global_open
	global_close
	foreach_open
	foreach_close
	events_open
	events_close
	property_open
	property_close
	variables_open
	variables_close
	states_open
	states_close
	bad_states_open
	bad_states_close
	normal_states_open
	normal_states_close
	starting_state_open
	starting_state_close
	transitions_open
	transitions_close.

Rootsymbol GlobalTag.

GlobalTag -> global_open Global global_close : {global, '$2'}.
Global 	-> PropertiesTop : {'$1'}.
Global 	-> ForeachTop : {'$1'}.
Global 	-> ForeachTop PropertiesTop : {'$1', '$2'}.

ForeachTop -> Foreach : {foreach, '$1'}.
Foreach -> ForeachTag : ['$1'].
Foreach -> ForeachTag Foreach : ['$1'] ++ '$2'.
ForeachTag -> foreach_open message_pattern open Properties foreach_close : {unwrap('$2'), '$4'}.

PropertiesTop -> Properties : {properties, '$1'}.
Properties -> PropertyTag : ['$1'].
Properties -> PropertyTag Properties : ['$1'] ++ '$2'.
PropertyTag -> property_open Property property_close : {unwrap('$1'), '$2'}.
Property -> EventsTag VariablesTag StatesTag TransitionsTag: {'$1', '$2', '$3', '$4'}.
Property -> EventsTag StatesTag TransitionsTag: {'$1', '$2', '$3'}.

EventsTag -> events_open Events events_close : {events, '$2'}.
Events 	-> Event : ['$1'].
Events 	-> Event Events : ['$1'] ++ '$2'.

Event   -> PropertyEvent : '$1'.
Event   -> ForeachEvent : '$1'.

PropertyEvent	-> var equals call_event message_pattern message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.
PropertyEvent	-> var equals receive_event message_pattern message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.
PropertyEvent	-> var equals send_event message_pattern message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.
PropertyEvent	-> var equals spawn_event message_pattern message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.
PropertyEvent	-> var equals exit_event message_pattern message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.
PropertyEvent	-> var equals register_event message_pattern var : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.
PropertyEvent	-> var equals unregister_event message_pattern var : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.
PropertyEvent	-> var equals link_event message_pattern message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.
PropertyEvent	-> var equals unlink_event message_pattern message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.
PropertyEvent	-> var equals channel_event message_pattern message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$5'), unwrap('$4')}}.

ForeachEvent	-> var equals call_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.
ForeachEvent	-> var equals receive_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.
ForeachEvent	-> var equals send_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.
ForeachEvent	-> var equals spawn_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.
ForeachEvent	-> var equals exit_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.
ForeachEvent	-> var equals register_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.
ForeachEvent	-> var equals unregister_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.
ForeachEvent	-> var equals link_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.
ForeachEvent	-> var equals unlink_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.
ForeachEvent	-> var equals channel_event message_pattern : {unwrap('$1'), {unwrap('$3'), unwrap('$4')}}.

VariablesTag -> variables_open open_list Variables close_list variables_close : {variables, '$3'}.
Variables -> Variable : ['$1'].
Variables -> Variable Variables : ['$1'] ++ '$2'.
Variable -> open var integer close : {unwrap('$2'), unwrap('$3')}.

StatesTag -> states_open States states_close : '$2'.
States -> BadStatesTag NormalStatesTag StartingStateTag : {states, '$1', '$2', '$3'}.
States -> BadStatesTag NormalStatesTag : {states, '$1', '$2'}.
States -> BadStatesTag : {states, '$1'}.
States -> NormalStatesTag StartingStateTag : {states, '$1', '$2'}.
States -> NormalStatesTag : {states, '$1'}.
States -> StartingStateTag : {states, '$1'}.

BadStatesTag -> bad_states_open BadStates bad_states_close : {bad, '$2'}.
BadStates -> BadState : ['$1'].
BadStates -> BadState BadStates : ['$1'] ++ '$2'.
BadState -> var open close : unwrap('$1').

NormalStatesTag -> normal_states_open NormalStates normal_states_close : {normal, '$2'}.
NormalStates -> NormalState : ['$1'].
NormalStates -> NormalState NormalStates : ['$1'] ++ '$2'.
NormalState -> var open close : unwrap('$1'). 

StartingStateTag -> starting_state_open StartingState starting_state_close : '$2'.
StartingState -> var open close : {starting, unwrap('$1')}.

TransitionsTag -> transitions_open Transitions transitions_close : {transitions, '$2'}.
Transitions -> Transition : ['$1'].
Transitions -> Transition Transitions : ['$1'] ++ '$2'.
Transition -> var arrow var open_list var separator message_pattern separator message_pattern close_list : {unwrap('$1'), to, unwrap('$3'), {event, unwrap('$5')}, {condition, unwrap('$7')}, {action, unwrap('$9')}}.
Transition -> var arrow var open_list var separator separator message_pattern close_list : {unwrap('$1'), to, unwrap('$3'), {event, unwrap('$5')}, {action, unwrap('$8')}}.
Transition -> var arrow var open_list var separator message_pattern separator close_list : {unwrap('$1'), to, unwrap('$3'), {event, unwrap('$5')}, {condition, unwrap('$7')}}.
Transition -> var arrow var open_list var separator separator close_list : {unwrap('$1'), to, unwrap('$3'), {event, unwrap('$5')}}. 

Erlang code.

unwrap({_,_,V}) -> V.
