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

-module( pmpy ).

-export( [ 
	start/0, 
	endpoint/1, 
	httpsubscriber/2,
	subscribe/1, 
	unsubscribe/1, 
	notify/2, 
	get_latest/1 
] ).

start() ->
	application:start( sasl ),
	application:start( cowboy ),
	application:start( pmpy ).

% Get the pid of an endpoint. 
% If the endpoint doesn't exist, it'll be started.
endpoint( Id ) ->
	case ets:lookup( pmpy_endpoints, Id ) of
		[] -> 
			{ ok, Pid } = pmpy_sup:start_endpoint( Id ),
			ets:insert( pmpy_endpoints, { Id, Pid } ),
			{ ok, Pid };
			
		[ { Id, Pid } ] -> 
			{ ok, Pid }
	end.

% Get the pid of a HTTP subscriber.
% If the subscriber doesn't exist, it'll be started.
httpsubscriber( Token, Url ) ->
	Id = { Token, Url },
	case ets:lookup( pmpy_httpsubscribers, Id ) of
		[] ->
			{ ok, Pid } = pmpy_sup:start_httpsubscriber( Token, Url ),
			ets:insert( pmpy_httpsubscribers, { Id, Pid } ),
			{ ok, Pid };
			
		[ { Id, Pid } ] ->
			{ ok, Pid }
	end.

% Subscribe to an endpoint
subscribe( Id ) ->
	{ ok, Pid } = endpoint( Id ),
	pmpy_endpoint:subscribe( Pid ).

% Unsubscribe from an endpoint
unsubscribe( Id ) ->
	{ ok, Pid } = endpoint( Id ),
	pmpy_endpoint:unsubscribe( Pid ).

% Send a message to an endpoint, the message gets relayed to all the subscriber processes
notify( Id, Msg ) ->
	{ ok, Pid } = endpoint( Id ),
	pmpy_endpoint:notify( Pid, Msg ).

% Get the last message sent to an endpoint
get_latest( Id ) ->
	{ ok, Pid } = endpoint( Id ),
	pmpy_endpoint:get_latest( Pid ).