% Patterned after: http://yaws.hyber.org/code.yaws?file=/ybed.erl

-module(frontend).
-compile(export_all).

start() ->
  {ok, spawn(?MODULE, init_server, [])}.

init_server() ->
  % Generate our configuration (including child specs) to pass to frontend_sup, 
  % which supervises our embedded instance of YAWS.
  Id = "voting_frontend",
  DocRoot = "./www",
  ServerConfList = [
    {docroot, DocRoot},
    {listen, {0,0,0,0}},
    {port, 8888},
    {servername, "vote"}
  ],
  GlobalConfList = [
    {id, Id}
  ],
  {ok, ServerConfRecord, GlobalConfRecord, ChildSpecs} =
    yaws_api:embedded_start_conf(DocRoot, ServerConfList, GlobalConfList, Id),

  % Start our YAWS workers and configure them. 
  % Note: this expects the supervisor process to be registered as frontend_sup,
  % which happens if you use start_link/3 with {local, ?MODULE}.
  [supervisor:start_child(frontend_sup, ChildSpec) || ChildSpec <- ChildSpecs],
  yaws_api:setconf(GlobalConfRecord, ServerConfRecord), 
  {ok, self()}.
