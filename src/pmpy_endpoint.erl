-module( pmpy_endpoint ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/0, notify/2, get_latest/1, subscribe/1, unsubscribe/1 ] ).

-record( state, { event_manager, messages = [] } ).

start_link() ->
	gen_server:start_link( ?MODULE, [], [] ).

subscribe( Pid ) ->
	gen_server:cast( Pid, { subscribe, self() } ).

unsubscribe( Pid ) ->
	gen_server:cast( Pid, { unsubscribe, self() } ).

notify( Pid, Message ) ->
	gen_server:cast( Pid, { notify, Message } ).

get_latest( Pid ) ->
	gen_server:call( Pid, get_latest ).

init( _ ) -> 
	{ ok, Pid } = gen_event:start_link(),
	{ ok, #state{ event_manager = Pid } }.

handle_call( get_latest, _, S ) -> 
	[ Message | _ ] = S#state.messages,
	{ reply, Message, S };
handle_call( _, _, S ) -> 
	{ reply, { error, unknown_call }, S }.

handle_cast( { notify, Message }, S ) ->
	gen_event:notify( S#state.event_manager, S ),
	S2 = S#state{ messages = [ Message | S#state.messages ] },
	{ noreply, S2 };
handle_cast( { subscribe, Pid }, S ) ->
	gen_event:add_handler( S#state.event_manager, pmpy_subscribe, Pid ),
	{ noreply, S };
handle_cast( { unsubscribe, Pid }, S ) ->
	gen_event:remove_handler( S#state.event_manager, pmpy_subscribe, Pid ),
	{ noreply, S };
handle_cast( _, S ) -> 
	{ noreply, S }.
	
handle_info( _, S ) -> 
	{ noreply, S }.

terminate( _, _ ) -> 
	ok.

code_change( _, _, S ) -> 
	{ ok, S }.