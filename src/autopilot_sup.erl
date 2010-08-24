-module(autopilot_sup).

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
                    {ws_loop, fun(Ws) -> init_websocket(Ws) end}],

    MisultinChild = {misultin, {misultin, start_link, [MisultinOpts]}, permanent, 5000, worker, [misultin]},

    {ok, { {one_for_one, 5, 10}, [MisultinChild]} }.

init_websocket(Ws) ->
    explane_server:add_listener(self()),
    handle_websocket(Ws).
    

% callback on received websockets data
handle_websocket(Ws) ->
    receive
        {browser, _Data} ->
            handle_websocket(Ws);
        {explane, Values} ->
            Ws:send(
              lists:concat(["{",
                            "\"altitude\":" , round(proplists:get_value(altitude, Values)*1000)/1000, ","
                            "\"speed\":" , round(proplists:get_value(speed, Values)*1000)/1000, ","
                            "\"roll\":" , round(proplists:get_value(roll, Values)*1000)/1000, ","
                            "\"pitch\":" , round(proplists:get_value(pitch, Values)*1000)/1000, 
                            "}"])),
            handle_websocket(Ws);
        _Ignore ->
            handle_websocket(Ws)
    end.


handle_http(Req, Port) ->	
    handle(Req:get(method), Req:resource([lowercase, urldecode]), Req, Port).

handle('GET', PathElems, Req, Port) ->
    Path = string:join(PathElems, "/"),
    serve(Path, Req, Port).

serve("", Req, Port) ->
    Req:ok([{"Content-Type", "text/html"}],
           index_html(Port));
serve(Path, Req, _) ->
    FilePath = "./static/" ++ Path,
    case file:read_file_info(FilePath) of
        {ok, _} ->
            {ok, Contents} = file:read_file(FilePath),
            Req:ok(Contents);
        _ ->
            Req:respond(404, [], "File not found")
    end.

index_html(Port) ->
    ["	
<html>
<head>
    
		</head>
		<body>
                        <canvas width=\"800\" height=\"600\"></canvas>
			<div id=\"status\"></div>

<script type=\"text/javascript\" src=\"/js/ws.js\"></script>
    <script type=\"text/javascript\" src=\"/js/jquery.js\"></script>
    <script type=\"text/javascript\" src=\"/js/canvas.js\"></script>
			<script type=\"text/javascript\">
                                $(document).ready(function() {
					if (\"WebSocket\" in window) {
                                                init(", integer_to_list(Port), ");
					} else {
						// browser does not support websockets
						addStatus(\"sorry, your browser does not support websockets.\");
					}
                                });
				function addStatus(text){
					var date = new Date();
					document.getElementById('status').innerHTML = document.getElementById('status').innerHTML + date + \": \" + text + \"<br>\";				
				}
			</script>

		</body>
	</html>"].

