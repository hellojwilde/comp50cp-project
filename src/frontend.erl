% Patterned after: http://yaws.hyber.org/code.yaws?file=/ybed.erl

-module(frontend).
-compile(export_all).

start(VoteConfig) ->
    {ok, spawn(?MODULE, init_server, [VoteConfig])}.

init_server(VoteConfig) ->
    % Generate our config (including child specs) to pass to frontend_sup, 
    % which supervises our embedded instance of YAWS.
    Id = "voting_frontend",
    DocRoot = "./www",
    ServerConfList = [
        {docroot, DocRoot},
        {listen, {0,0,0,0}},
        {port, 8888},
        {servername, "vote"},
        {appmods, [{"/result_events", result_events}]},
        {opaque, VoteConfig}
    ],
    GlobalConfList = [
        {id, Id}
    ],
    
    {ok, ServerConfRecord, GlobalConfRecord, ChildSpecs} =
        yaws_api:embedded_start_conf(
            DocRoot, 
            ServerConfList, 
            GlobalConfList, 
            Id
        ),

    % Start our YAWS workers and configure them. 
    % Note: expects the frontend supervisor to be registered as frontend_sup,
    % which happens if you use start_link/3 with {local, ?MODULE}.
    lists:map(
        fun(ChildSpec) -> supervisor:start_child(frontend_sup, ChildSpec) end,
        ChildSpecs
    ),
    yaws_api:setconf(GlobalConfRecord, ServerConfRecord), 
    {ok, self()}.
