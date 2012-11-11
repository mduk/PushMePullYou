Push Me Pull You
================

A Websocket/HTTP message bus.

Open WebSocket connections to any path to create a bus, messages sent to it will be relayed to all other WebSocket connections that were made to the same bus.

An HTTP GET request on any path will retrieve the last message sent to that bus.

An HTTP POST request on any path will push it's request body to the bus and it's subscribers.

Instructions
------------

Currently the build is left to rebar so you'll have to have that installed.

	rebar get-deps
	rebar compile

Starting Push Me Pull You is simple. 
If you want to start it, run <code>./start.sh</code>. 
You can work out the rest of the controls for yourself.

License
-------

See COPYING.