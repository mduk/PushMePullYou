# Push Me Pull You

A Websocket/HTTP message bus.

Open WebSocket connections to any path to create a bus, messages sent to it will be relayed to all other WebSocket connections that were made to the same bus.

An HTTP GET request on any path will retrieve the last message sent to that bus.

An HTTP POST request on any path will push it's request body to the bus and it's subscribers.

## Instructions

Currently the build is left to rebar so you'll have to have that installed.

	rebar get-deps
	rebar compile

Starting Push Me Pull You is simple. If you want to start it run <code>./start.sh</code>, you can work out the rest of the controls for yourself. To paraphrase a certain [infullable computer](http://en.wikipedia.org/wiki/Red_Dwarf).

### WebSockets

	var ws = new WebSocket( "ws://localhost:8000/foo" );

	ws.onopen = function() {
		console.log( "Connected" );
	};
	
	ws.onclose = function() {
		console.log( "Closed" );
	};

	ws.onmessage = function( msg ) {
		msg = JSON.parse( msg.data );
		console.log( msg );
	};
	
	ws.send( JSON.stringify( { hello: "world" } ) );

### HTTP Get

The last message sent to a bus can be retrieved by performing a GET request to it's URL.
	
	curl http://localhost:8000/foo

### HTTP Post

You can a message to a bus over HTTP simply by POSTing it straight to the bus URL.

	curl --data "{\"hello\":\"world\"}" http://localhost:8000/foo

### HTTP Subscribers

There are two "methods" as such on every bus, <code>subscribe</code> and <code>unsubscribe</code>.

#### Request Parameters

 - <code>token</code> Any string that you want. It's just to differenciate subscribers. You'll need it to unsubscribe later.
 - <code>url</code> The URL that you want to have the messages POSTed to.

#### Subscribe
	
	curl --data "token=mytoken&url=http://localhost/" http://localhost:8000/foo.subscribe

#### Unsubscribe

	curl --data "token=mytoken&url=http://localhost/" http://localhost:8000/foo.unsubscribe

## License

See COPYING.
