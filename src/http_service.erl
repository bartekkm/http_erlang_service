%%%-------------------------------------------------------------------
%% @doc http_service public API
%% @end
%%%-------------------------------------------------------------------

-module(http_service).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    inets:start(),
    %% Configuration for the HTTP server
    ServiceConfig = [
        {modules, [mod_esi]},
        {port, 8080},
        {server_name, "http_service"},
        {server_root, "."},
        {document_root, "."},
        {bind_address, "localhost"},
        {erl_script_alias, {"/handle", [task]}}
    ],
    %% Start the HTTP server with the configuration
    inets:start(httpd, ServiceConfig).


stop(_State) ->
    ok.