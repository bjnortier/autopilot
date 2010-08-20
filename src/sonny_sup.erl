
-module(sonny_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Port = 8008,
    MisultinOpts = [{port, Port}, 
                    {loop, fun(Req) -> handle_http(Req, Port) end}, 
                    {ws_loop, fun(Ws) -> handle_websocket(Ws) end}],

    MisultinChild = {misultin, {misultin, start_link, [MisultinOpts]}, permanent, 5000, worker, [misultin]},

    {ok, { {one_for_one, 5, 10}, [MisultinChild]} }.

handle_http(Req, Port) ->	
    handle(Req:get(method), Req:resource([lowercase, urldecode]), Req, Port).

handle('GET', PathElems, Req, Port) ->
    Path = string:join(PathElems, "/"),
    io:format("~p~n", [Path]),
    serve(Path, Req, Port).

serve("", Req, Port) ->
    Req:ok([{"Content-Type", "text/html"}],
           index_html(Port));
serve(Path, Req, _) ->
    FilePath = "./static/" ++ Path,
    Req:ok([{"Content-Type", "text/plain"}],
           FilePath).

index_html(Port) ->
    ["	
	<html>
		<head>
			<script type=\"text/javascript\">
				function addStatus(text){
					var date = new Date();
					document.getElementById('status').innerHTML = document.getElementById('status').innerHTML + date + \": \" + text + \"<br>\";				
				}
				function ready(){
					if (\"WebSocket\" in window) {
						// browser supports websockets
						var ws = new WebSocket(\"ws://localhost:", integer_to_list(Port) ,"/service\");
						ws.onopen = function() {
							// websocket is connected
							addStatus(\"websocket connected!\");
							// send hello data to server.
							ws.send(\"hello server!\");
							addStatus(\"sent message to server: 'hello server'!\");
						};
						ws.onmessage = function (evt) {
							var receivedMsg = evt.data;
							addStatus(\"server sent the following: '\" + receivedMsg + \"'\");
						};
						ws.onclose = function() {
							// websocket was closed
							addStatus(\"websocket was closed\");
						};
					} else {
						// browser does not support websockets
						addStatus(\"sorry, your browser does not support websockets.\");
					}
				}
			</script>
		</head>
		<body onload=\"ready();\">
			<div id=\"status\"></div>
		</body>
	</html>"].

% callback on received websockets data
handle_websocket(Ws) ->
    explane_server:remove_listener(self()),
    explane_server:add_listener(self()),
    receive
        {browser, Data} ->
            Ws:send(["received '", Data, "'"]),
            handle_websocket(Ws);
        {explane, Values} ->
            Ws:send(io_lib:format("~p", [Values])),
            handle_websocket(Ws);
        _Ignore ->
            handle_websocket(Ws)
    after 5000 ->
            Ws:send("pushing!"),
            handle_websocket(Ws)
    end.
