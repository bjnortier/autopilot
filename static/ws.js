var ws = {};
function ws.new(port) {
    var _ws = new WebSocket("ws://localhost:" + Port + "/service");
                       
    _ws.onopen = function() {
	// websocket is connected
	addStatus(\"websocket connected!\");
	// send hello data to server.
	_ws.send(\"hello server!\");
	addStatus(\"sent message to server: 'hello server'!\");
    };
    _ws.onmessage = function (evt) {
	var receivedMsg = evt.data;
	addStatus(\"server sent the following: '\" + receivedMsg + \"'\");
    };
    _ws.onclose = function() {
	// websocket was closed
	addStatus(\"websocket was closed\");
    };
}