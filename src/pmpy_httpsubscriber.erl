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

-module( pmpy_httpsubscriber ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/2, subscribe/2, unsubscribe/2 ] ).

-record( state, { token, url, subscriptions = [] } ).

%===============================================================================
% Start Link
%===============================================================================
start_link( Token, Url ) ->
	gen_server:start_link( ?MODULE, { Token, Url }, [] ).
	
%===============================================================================
% Subscribe
%===============================================================================
subscribe( Pid, EndpointId ) ->
	gen_server:cast( Pid, { subscribe, EndpointId } ).

%===============================================================================
% Unsubscribe
%===============================================================================
unsubscribe( Pid, EndpointId ) ->
	gen_server:cast( Pid, { unsubscribe, EndpointId } ).
	
%===============================================================================
% Initialise
%===============================================================================
init( { Token, Url } ) ->
	process_flag( trap_exit, true ),
	
	case application:start( inets ) of
		ok ->	ok;
		{ error, { already_started, _ } } -> ok
	end,
	
	{ ok, #state{
		token = Token,
		url = Url
	} }.

%===============================================================================
% Handle Call
%===============================================================================
% Catch All
%-------------------------------------------------------------------------------
handle_call( _, _, S ) ->
	{ reply, { error, unknown_call }, S }.

%===============================================================================
% Handle Cast
%===============================================================================
% Subscribe to an endpoint
%-------------------------------------------------------------------------------
handle_cast( { subscribe, EndpointId }, S ) ->
	pmpy:subscribe( EndpointId ),
	{ noreply, S#state{ subscriptions = [ EndpointId | S#state.subscriptions ] } };
%-------------------------------------------------------------------------------
% Unsubscribe from an endpoint
%-------------------------------------------------------------------------------
handle_cast( { unsubscribe, EndpointId }, S ) ->
	pmpy:unsubscribe( EndpointId ),
	
	case lists:delete( EndpointId, S#state.subscriptions ) of
		[] ->
			{ stop, no_subscriptions, S#state{ subscriptions = [] } };
		Subscriptions ->
			{ noreply, S#state{ subscriptions = Subscriptions } }
	end;
%-------------------------------------------------------------------------------
% Stop with invalid token
%-------------------------------------------------------------------------------
handle_cast( { stop, _ }, S ) ->
	{ noreply, S };
%-------------------------------------------------------------------------------
% Catch all
%-------------------------------------------------------------------------------
handle_cast( _, S ) ->
	{ noreply, S }.

%===============================================================================
% Handle Info
%===============================================================================
% POST message to url
%-------------------------------------------------------------------------------
handle_info( { Bus, Event }, State ) when is_binary( Event ) ->
	Url = case State of
		#state{ url = Bin } when is_binary( Bin ) ->
			binary_to_list( Bin );
		#state{ url = List } when is_list( List ) ->
			List
	end,
	httpc:request( post, { Url, [ 
		{ "Server", "Push Me Pull You" },
		{ "X-PMPY-Token", binary_to_list( State#state.token ) },
		{ "X-PMPY-Bus", binary_to_list( Bus ) }
	], "text/plain", Event }, [], [] ),
	{ noreply, State }.

%===============================================================================
% Terminate
%===============================================================================
terminate( _, _ ) ->
	ok.

%===============================================================================
% Code Change
%===============================================================================
code_change( _, _, _ ) ->
	ok.
