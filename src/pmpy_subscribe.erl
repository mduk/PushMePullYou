-module( pmpy_subscribe ).

-behaviour( gen_event ).
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ] ).

init( Pid ) ->
    { ok, Pid }.

handle_event( Event, Pid ) ->
	Pid ! Event,
	{ ok, Pid }.

handle_call( _Msg, State ) ->
	{ ok, { error, invalid_call }, State }.

handle_info( _Msg, State ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Opts ) ->
	ok.
