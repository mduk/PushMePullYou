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

-module( pmpy_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, start_endpoint/1, start_httpsubscriber/2 ] ).

%===============================================================================
% Start Link
%===============================================================================
start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

%===============================================================================
% Start Endpoint
%===============================================================================
start_endpoint( Id ) -> 
	Mfa = { pmpy_endpoint, start_link, [] },
	ChildSpec = { Id, Mfa, permanent, 5000, worker, [ pmpy_endpoint ] },
	supervisor:start_child( ?MODULE, ChildSpec ).

%===============================================================================
% Start HTTP Subscriber
%===============================================================================
start_httpsubscriber( Token, Url ) ->
	Mfa = { pmpy_httpsubscriber, start_link, [ Token, Url ] },
	ChildSpec = { { Token, Url }, Mfa, permanent, 5000, worker, [ pmpy_httpsubscriber ] },
	supervisor:start_child( ?MODULE, ChildSpec ).

%===============================================================================
% Initialise
%===============================================================================
init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [] } }.

