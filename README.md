***
***
***
***

Misultin development has been discontinued.

There currently are three main webserver _libraries_ which basically do similar things:

 * [Mochiweb](https://github.com/mochi/mochiweb)
 * [Cowboy](https://github.com/extend/cowboy)
 * [Misultin](https://github.com/ostinelli/misultin)

**Mochiweb** has been around the block for a while and it's proven solid in production, I can only recommend it for all basic webserver needs you might have.
**Cowboy** has a very interesting approach since it allows to use multiple TCP and UDP protocols on top of a common acceptor pool. It is a very modern approach, is very actively maintained and many projects are starting to be built around it.

Especially since the recent heavy development of Cowboy's HTTP server, I believe there is way too much duplication of efforts going on here. This is why Misultin's current 'state of the art' has been frozen in the latest tag, [v0.9](https://github.com/ostinelli/misultin/tree/misultin-0.9), to support all the companies currently using Misultin in their production environment. I'm here to provide help, if needed, in moving away from it. Thus, this server should be robust and stable enough to continue serving your needs for some time.

Instead of letting this library stand here without this notice, and getting developers still use this project, I have preferred to explicitly state to gradually move away from it, so that efforts can be concentrated around one server library only. It's hard enough to let one 'child' like this one go, but I believe it's best for the whole Erlang community.

Thank you to everyone that has been supporting Misultin in these years. Hopefully its **code usability**, which I still believe to be unmatched (well, I have developed it so how could I feel differently about this ^^_), will provide inspiration for some library interfaces.

Best to you all,

r.

***
***
***
***

# MISULTIN

Misultin is an HTTP(S) library which can easily be embedded in your own application. 

https://github.com/ostinelli/misultin

`>-|-|-(°>`


# Features

 * _Very_ fast
 * HTTP and HTTPS
 * Supports multiple **Websocket** protocols (draft-hixie-68, draft-hixie-76, draft-hybi-10 and draft-hybi-17)
 * **Cookies**
 * **Session** Variables
 * Allows for Chunked and Byte **streaming responses**
 * Allows for **streaming file upload** (via Chunked Encoding)
 * Can serves static files from a ```static``` directory (though in production you should consider a specific server such as [nginx](http://nginx.org/) to do so)
 * Has Many customization options (maximum allowed connections, maximum body size, ...)
 * Has a callback function for your **logging** needs
 * Supports **Unicode**
 * Can start multiple servers on a single node
 * Can be used with or without Parametrized Modules
 * Can traps the client closing a browser in Comet applications
 * It's very easy to use


# Quick Start

 The typical 'Hello World" example code is:

```erlang

-module(misultin_hello_world).
-export([start/0, stop/0]).

% start misultin http server
start() ->
    misultin:start_link([{port, 8080}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop misultin
stop() ->
    misultin:stop().

% callback on request received
handle_http(Req) ->
    Req:ok("Hello World.").
```

Issuing the ```start/0``` command will start an HTTP server on port 8080, which will respond to every request with an "Hello World" text.

# Examples

Misultin comes [packed with examples](https://github.com/ostinelli/misultin/tree/master/examples/).

#### Simple Examples

 * [Hello World](https://github.com/ostinelli/misultin/tree/master/examples/misultin_hello_world.erl)
 * [Querystring Variables](https://github.com/ostinelli/misultin/tree/master/examples/misultin_get_variable.erl)
 * [Querystring and POST Variables](https://github.com/ostinelli/misultin/tree/master/examples/misultin_echo.erl)
 * [REST](https://github.com/ostinelli/misultin/tree/master/examples/misultin_rest.erl)
 * [Set and Get Cookies](https://github.com/ostinelli/misultin/tree/master/examples/misultin_cookies_example.erl)
 * [Set and Get Session Variables](https://github.com/ostinelli/misultin/tree/master/examples/misultin_sessions_example.erl)
 * [Serve a Static file for download](https://github.com/ostinelli/misultin/tree/master/examples/misultin_file.erl)
 * [Serving files from a Static directory](https://github.com/ostinelli/misultin/tree/master/examples/misultin_static.erl)
 * [File Upload](https://github.com/ostinelli/misultin/tree/master/examples/misultin_file_upload.erl)
 * [HTTPS example](https://github.com/ostinelli/misultin/tree/master/examples/misultin_ssl.erl)
 * [Performing a simple redirection](https://github.com/ostinelli/misultin/tree/master/examples/misultin_redirect.erl)
 * [Serving compressed content](https://github.com/ostinelli/misultin/tree/master/examples/misultin_compress.erl)
 * [Logging Access](https://github.com/ostinelli/misultin/tree/master/examples/misultin_access_log.erl)

#### Websockets

 * [Simple Websocket](https://github.com/ostinelli/misultin/tree/master/examples/misultin_websocket_example.erl)
 * [Simple Websocket on SSL](https://github.com/ostinelli/misultin/tree/master/examples/misultin_websocket_example_ssl.erl)
 * [Websocket exposing the close event](https://github.com/ostinelli/misultin/tree/master/examples/misultin_websocket_event_example.erl)
 * [Websocket exposing the close event, example 2](https://github.com/ostinelli/misultin/tree/master/examples/misultin_websocket_event_example2.erl)
 * [Access Session Variables from Websockets](https://github.com/ostinelli/misultin/tree/master/examples/misultin_websocket_sessions_example.erl)


#### Comets

 * [Long Polling](https://github.com/ostinelli/misultin/tree/master/examples/misultin_comet_long_polling.erl)
 * [iFrame Technique](https://github.com/ostinelli/misultin/tree/master/examples/misultin_comet_iframe.erl)
 * [iFrame Technique using the close event](https://github.com/ostinelli/misultin/tree/master/examples/misultin_comet_iframe_event.erl)


#### More Advanced

 * [Sending Chunked Content](https://github.com/ostinelli/misultin/tree/master/examples/misultin_chunked.erl)
 * [Sending Byte Streaming Content](https://github.com/ostinelli/misultin/tree/master/examples/misultin_stream.erl)
 * [Receiving endless streaming Upload](https://github.com/ostinelli/misultin/tree/master/examples/misultin_body_recv.erl)
 * [Unicode](https://github.com/ostinelli/misultin/tree/master/examples/misultin_unicode.erl)
 * [REST with UTF-8](https://github.com/ostinelli/misultin/tree/master/examples/misultin_rest_utf8.erl)
 * [Starting a nameless server](https://github.com/ostinelli/misultin/tree/master/examples/misultin_hello_world_nameless.erl)
 * [Starting multiple servers on a same node, with a custom name](https://github.com/ostinelli/misultin/tree/master/examples/misultin_multiple_servers_custom_name.erl)
 * [Using the HAProxy protocol](https://github.com/ostinelli/misultin/tree/master/examples/misultin_proxy_protocol.erl)

# Module Exports

The complete list of module exports can be found [here](https://github.com/ostinelli/misultin/tree/master/EXPORTS.md).

# Parametrized modules

Some developers hate them, some love them. Misultin allows you to choose if you want to use them or not. The same **Hello World** example shown here above, but without parametrized modules, looks like this:

```erlang

-module(misultin_hello_world).
-export([start/0, stop/0]).

% start misultin http server
start() ->
    misultin:start_link([{port, 8080}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop misultin
stop() ->
    misultin:stop().

% callback on request received
handle_http(Req) ->
    misultin_req:ok("Hello World.", Req).
```

# Dependencies

You will need:

 * [Erlang](http://www.erlang.org/download.html) >= R14B01
 * [Rebar](https://github.com/basho/rebar) to compile

# Under the hood
Misultin is built using the OTP principles. When you start it using the ```misultin:start_link/1``` command, you are actually starting a supervisor which handles all of Misultin's servers and modules.

Therefore, in real life applications you should always embed it in your own application. An easy example on how this can be done can be found in the Application Example [here](https://github.com/ostinelli/misultin/tree/master/examples/application).

# SSL Notes

If you are running misultin behind an SSL terminator such as stunnel or stud, and are using websockets, to make the websocket handshakes work, set in the starting options:

```erlang
{ws_force_ssl, true} 
```

If you are using stunnel to terminate, to make misultin expect a PROXY.. line as per the [proxy protocol](http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt) you can also set in the starting options:
```erlang
{proxy_protocol, true}
```

Newer versions of stunnel support this with the "protocol = proxy" config option.
