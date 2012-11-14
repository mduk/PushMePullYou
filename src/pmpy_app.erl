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

-module( pmpy_app ).

-behaviour( application ).
-export( [ start/2, stop/1 ] ).

start( _StartType, _StartArgs ) ->
	start_cowboy(),
	ets:new( pmpy_endpoints, [ public, named_table ] ),
	ets:new( pmpy_httpsubscribers, [ public, named_table ] ),
	pmpy_sup:start_link().

stop( _State ) ->
	ok.

start_cowboy() ->
	HttpDispatchRules = [
		{ '_', [ % Any Host
			{ '_', pmpy_wshandler, [] } 
		] }
	],

	cowboy:start_listener( http, 10, 
		cowboy_tcp_transport, [ { port, 8000 } ],
		cowboy_http_protocol, [ { dispatch, HttpDispatchRules } ] 
	).