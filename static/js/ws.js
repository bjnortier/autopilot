
function init(port) {
    var _ws = new WebSocket("ws://localhost:" + port + "/service");
    _ws.onopen = function() {
	// websocket is connected
	addStatus("websocket connected!");
    };
    _ws.onmessage = function (evt) {
        try {
	    var receivedMsg = evt.data;
            var obj = JSON.parse(receivedMsg);
            altitude = Math.round(obj.altitude);
            speed = Math.round(obj.speed);
            roll = obj.roll/180*Math.PI;
            pitch = obj.pitch/180*Math.PI;
            draw();
        } catch(e) {
            addStatus("[error] " + e + ':' + evt.data);
        }
    };
    _ws.onclose = function() {
	// websocket was closed
	addStatus("websocket was closed");
    };
}