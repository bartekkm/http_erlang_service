Simple HTTP service
=====

A simple HTTP service wrote in Erlang.  
Handle requests with valid JSON data and reply with same data in proper order. In addition printouts in shell a valid bash script representation.  
Used jsx lib to decode/encode JSON.  
Used Inets for HTTP server.  

Build and run
-----

    $ rebar3 compile
    $ rebar3 shell

Usage
-----

All request are handled by address: `http://localhost:8080/handle/task/handle_request`  
When server is running you can use `curl` or Postman app to send the request, e.g.:  

    $ curl -d '<some_json_data>' http://localhost:8080/handle/task/handle_request


For Postman application usage plese check https://learning.postman.com/docs/getting-started/first-steps/sending-the-first-request/  
In Body put the valid JSON data.  

In response the same JSON with sorted data will be returned.  
Also a valid bash script representation of commands will be printed in run shell window.