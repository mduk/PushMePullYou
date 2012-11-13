% Copyright 2012 Daniel Kendell <daniel.kendell@gmail.com>

% This file is part of PushMePullYou.
%
% PushMePullYou is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% PushMePullYou is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with PushMePullYou.  If not, see <http://www.gnu.org/licenses/>.

-module( pmpy_endpoint ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/0, notify/2, get_latest/1, subscribe/1, unsubscribe/1 ] ).

-record( state, { event_manager, last_message = undefined } ).

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
	{ reply, S#state.last_message, S };
handle_call( _, _, S ) -> 
	{ reply, { error, unknown_call }, S }.

handle_cast( { notify, Message }, S ) ->
	gen_event:notify( S#state.event_manager, Message ),
	{ noreply, S#state{ last_message = Message } };
handle_cast( { subscribe, Pid }, S ) ->
	gen_event:add_handler( S#state.event_manager, pmpy_subscribe, Pid ),
	{ noreply, S };
handle_cast( { unsubscribe, Pid }, S ) ->
	gen_event:delete_handler( S#state.event_manager, pmpy_subscribe, Pid ),
	{ noreply, S };
handle_cast( _, S ) -> 
	{ noreply, S }.
	
handle_info( _, S ) -> 
	{ noreply, S }.

terminate( _, _ ) -> 
	ok.

code_change( _, _, S ) -> 
	{ ok, S }.