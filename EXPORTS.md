# Misultin modules list of exports

# 1. misultin #

## Description ##

This module contains the necessary functionalities to manage the misultin HTTP server.

## Exports ##

### start_link(Options) -> {ok, pid()} | {error, term()} ###
```erlang

Options = [Option]
 Option = {ip, list() | tuple()} |
  {port, integer()} |
  {name, atom()} |
  {backlog, integer()} |
  {recbuf, integer()} |
  {acceptors_poolsize, integer()} |
  {recv_timeout, integer()} |
  {max_connections, integer()} |
  {loop, fun()} |
  {autoexit, true|false} |
  {ws_loop, fun()|none} |
  {ws_autoexit, true|false} |
  {ws_versions, WsVersions()} |
  {post_max_size, integer()} |
  {get_url_max_size, integer()} |
  {compress, boolean()} |
  {ssl, SslOptions} |
  {ws_force_ssl, boolean()} |
  {proxy_protocol, boolean()} |
  {sessions_expire, integer()} |
  {access_log, fun()} |
  {auto_recv_body, boolean()} |
  {static, boolean()}
 SslOptions = [SslOption(§)]
 WsVersions = ['draft-hybi-17', 'draft-hybi-10', 'draft-hixie-76', 'draft-hixie-68']
Result = {ok, Pid} | ignore | {error, Error}
 Pid = pid()
 Error = {already_started,Pid} | term()
```

(§) `SslOption` is defined as a [ssl_new option](http://www.erlang.org/doc/man/new_ssl.html#id2263736).

Starts the misultin gen_server. Options are:

* *ip*: the IP address of the HTTP server. Can be expressed as string "0.0.0.0" or as tuple {0,0,0,0}. Defaults to "0.0.0.0".
* *port*: an integer specifying the port number where the HTTP server will run, defaults to 80
* *name*: an atom specifying the registered name of misultin. If set to false, misultin will start with no registered name. Defaults to 'misultin'.
* *backlog*: defines the maximum length that the queue of pending connections may grow to, defaults to 128
* *recbuf*: defines the size of the receiving buffer
* *acceptors_poolsize*: defines the acceptors' pool size. Defaults to 10.
* *recv_timeout*: defines the receive timeout on incoming TCP data, defaults to 30000 (30 seconds)
* *max_connections*: sets the maximum number of concurrent connections that the server is willing to handle, defaults to 1024
* *loop*: the callback function to be called on every incoming HTTP request
* *autoexit*: if set to true, then the loop specified as loop will be killed automatically by the server when the client closes the socket; if set to false, an event message is sent to loop which then needs to be exited in the most appropriate way.
* *ws_loop*: the loop of the listening process which handles a websocket. If a function is passed as `ws_loop` option then websockets requests will be treated, they will be ignored. The function passed as ws_loop is spawned by misultin to handle the connected websockets. Data coming from a browser will be sent to this process and will have the message format {browser, Data}, where Data is a list(). If you need to send data to the browser, you may do so by using the parametrized function Ws:send(Data), Data being a list() or an iolist() (see the [[ExamplesPage]]). Defaults to `none`.
* *ws_autoexit*: if set to true, then the loop specified as ws_loop will be killed automatically by the server when the socket closes; if set to false, an event message is sent to ws_loop which then needs to be exited in the most appropriate way.
* *ws_versions*: sets the accepted Websocket protocol versions. Defaults to `['draft-hybi-17', 'draft-hybi-10', 'draft-hixie-76']`.
* *compress*: set to `true` to send responses in compressed format, if available. Please note that files sent with `Req:file/1,2` and stream with `Req:stream/1,2,3` will not be compressed.
* *post_max_size*: sets the maximum size in bytes of a POST request body. Defaults to 4MB.
* *get_url_max_size*: sets the maximum length of the URI of a request. Defaults to `2000`.
* *compress*: sets if compression is to be used in responses, when available. Defaults to `false`.
* *ssl*: allows to start Misultin in SSL mode. Parameter is a list of [ssl_new options](http://www.erlang.org/doc/man/new_ssl.html#id2263736).
* *ws_force_ssl*: if you are running misultin behind an SSL terminator such as stunnel or stud, and are using websockets, to make the websocket handshakes work you should set this option to `true`. Defaults to `false`.
* *proxy_protocol*: if you are using stunnel to terminate, to make misultin expect a PROXY.. line as per the [proxy protocol](http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt) you can set this option to `true`. Defaults to `false`.
* *sessions_expire*: the timeout in seconds of when session variables should expire. Defaults to 600.
* *access_log*: a custom access log callback function. Defaults to undefined.
* *auto_recv_body*: sets if the body should be automatically read by misultin (`true`) or if it should be explicitly called in callback functions for more advanced functionalities (`false`). Defaults to `true`.
* *static*: if set to `{true, "my/dir/here}"` all URI pointing at http://myserver/static/ will be serving the files in the specified directory. Defaults to `false`.

The example here below starts a Misultin HTTP server on port 8080, and sets all incoming HTTP requests to be treated by a function `handle_http/1`:

```erlang

misultin:start_link([{port, 8080}, {loop, fun(Req) -> handle_http(Req) end}]).
```

### stop() -> true ###

Stops the misultin gen_server.

Example to stop the Misultin HTTP server:

```erlang

misultin:stop().
```

### stop(ServerRef) -> true ###

Stops the misultin gen_server when the name option has been defined.

Example to start and stop a Misultin HTTP server started without a registered name:

```erlang

{ok, ServerPid} = misultin:start_link([{port, 8080}, {name, false}, {loop, fun(Req) -> handle_http(Req) end}]),
misultin:stop(ServerPid).
```

# 2. Misultin Request #

## Description ##

This module contains the necessary functionalities to manage the incoming HTTP requests and provide a response. This module uses Erlang parametrized modules, therefore the syntax is to be understood accordingly.

## Exports ##

### Req:body_recv(head) -> {ok, binary()} | {chunk, binary()} | end_of_chunks | {error, term()} ###

If misultin was started with the `{auto_body_recv, false}` option, then this function allows you to receive the body of a request. If the received body is not chunked, the body will be returned as `{ok, Body}`. If the body is chunked, all the chunks will be returned as `{chunk, Body}` until `end_of_chunks` is received.

### Req:chunk(head) ###

Same as `Req:chunk(head, [])`.

### Req:chunk(head, Headers) ###
```erlang

Headers = {Header]
 Header = {HeaderName, HeaderContent}
  HeaderName = atom()
  HeaderContent = list()
```

Starts an HTTP response with chunked content.

`HeaderName` is the name of the HTTP header field; `HeaderContent` is the content of the header.

The functions `chunk/1,2,3` are used when the HTTP response has to be given progressively using chunked transfer encoding, and not altogether as it happens in `respond/2,3,4`.

In order to provide a chunked HTTP response, these functions have to be used in a specific manner, as shown in this example:

```erlang

% send headers
Req:chunk(head, [{"Content-Type", "text/html"}]),
% send chunk
Req:chunk("Sending CHUNK 1<br/>"),
timer:sleep(2000),
% send chunk
Req:chunk("Sending CHUNK 2<br/>"),
timer:sleep(2000),
% send chunk
Req:chunk("Sending CHUNK 3<br/>"),
% close
Req:chunk(done).
```

This will start a 200 HTTP response of text/plain Content-Type and send 3 chunks in sequence.

### Req:chunk(Template) ###
```erlang

Template = atom() | list() | binary() | iolist()
```

Sends data chunk into the open HTTP socket.

### Req:chunk(Template, Vars) ###
```erlang

Template = list()
Vars = [term()]
```

Sends data into the open HTTP socket. 

`Template` and `Vars` build a character list which represents `Vars` formatted in accordance with `Template`. See <http://erlang.org/doc/man/io.html#format-3 io:format/3> for a detailed description of the available formatting options. A fault is generated if there is an error in the format string or argument list.

### Req:chunk(done) ###

Ends the Chunked sequence.

### Req:get_cookies() -> [{CookieTag, CookieValue}] ###

Retrieve all the cookies from the Cookie header in a proplist format.

### Req:get_cookie_value(CookieName, Cookies) -> string() ###

Retrieve the cookie value from the Cookies list.

### Req:set_cookie(CookieName, CookieVal) -> ok ###

Sets a cookie value in the headers of the response. Typical usage is:

```erlang

Req:ok([Req:set_cookie("misultin_test_cookie", "value of the test cookie", [{max_age, 365*24*3600}])],
	"A cookie has been set. Refresh the browser to see it.");
```

### Req:delete_cookie(CookieName) -> ok ###

Deletes a cookie. Typical usage is:

```erlang

Req:ok([Req:delete_cookie("misultin_test_cookie")],
	["The set cookie value was set to \"", CookieVal,"\", and has now been removed. Refresh the browser to see this."])
```

### Req:file(FilePath) ###
```erlang

FilePath = list()
```

Same as `Ref:file(FilePath, [])`.

### Req:file(FilePath, Headers) ###
```erlang

FilePath = list()
Headers = {Header]
 Header = {HeaderName, HeaderContent}
  HeaderName = atom()
  HeaderContent = list()
```

Sends a file to be visualized in a browser, for example: 

```erlang

Req:file("/usr/local/files/content.html").
```

### Req:file(attachment, FilePath) ###
```erlang

FilePath = list()
```

Same as `Ref:file(attachment, FilePath, [])`.

### Req:file(attachment, FilePath, Headers) ###
```erlang

FilePath = list()
Headers = {Header]
 Header = {HeaderName, HeaderContent}
  HeaderName = atom()
  HeaderContent = list()
```

Sends a file as an attachment (so that a browser prompts for download), for example:

```erlang

Req:file(attachment, "/usr/local/files/test.zip").
```

### Req:get(ReqInfo) -> term() ###
```erlang

ReqInfo = socket |
	socket_mode |
	peer_addr |
	peer_port |
	peer_cert |
	connection |
	content_length |
	vsn |
	method |
	uri |
	uri_unquoted |
	args |
	headers |
	body
```

Retrieves the Request information. See `Req:raw/0` for additional information on the output format. 'uri_unquoted' returns the plain uri unquoted as a list().

### Req:ok(Template) ###

Same as `Req:ok([], Template)`.

### Req:ok(Headers, Template) ###

Same as `Req:respond(200, Headers, Template)`.

### Req:ok(Headers, Template, Vars) ###

Same as `Req:respond(200, Headers, Template, Vars)`.

### Req:options(Options) -> ok ###
```erlang

Options = [Option]
 Option = {OptionTag, OptionValue}
  OptionTag = comet
  OptionValue = true | false
```

Sets advanced per-request options. Currently only the 'comet' option is supported. It is to be used in [Comet](http://en.wikipedia.org/wiki/Comet_%28programming%29) applications to allow for the trapping of clients remotely closing their tcp connection. You may call it at the top of the function handling your incoming http requests, like this:

`Req:options([{comet, true}])`

When setting this option, the server will consider that you are using comet techniques (such as long polling), which are [Server Push](http://en.wikipedia.org/wiki/Push_technology) techniques. Hence, the server will not expect any other incoming Http requests from the client on the same socket. If such requests are received after setting the comet option to true, the server will raise an error and close the socket.

Set this option only on comet applications, otherwise [Http Pipelining](http://en.wikipedia.org/wiki/Http_pipelining) will not be supported.

### Req:parse_post() -> Result ###
```erlang

Result = [Argument]
 Argument = {ArgumentName, ArgumentValue}
  ArgumentName = list()
  ArgumentValue = list()
```

Parses the arguments passed in the body of the Request, when method is 'POST' and the Request _Content-Type_ header has been specified as _application/x-www-form-urlencoded_.


### Req:parse_qs() -> Result ###
```erlang

Result = [Argument]
 Argument = {ArgumentName, ArgumentValue}
  ArgumentName = list()
  ArgumentValue = list()
```

Parses the arguments passed in the querystring, when method is 'GET'.

### Req:raw() -> record() ###

Retrieves the Request complete record, defined as follows:
```erlang

-record(req, {
	socket,				% the socket handling the request
	socket_mode,			% http | ssl
	peer_addr,			% IP | undefined
	peer_port,			% Port | undefined
	peer_cert,			% undefined | the DER encoded peer certificate
	connection = keep_alive,	% keep_alive | close
	content_length,			% integer()
	vsn,				% {Maj, Min}
	method,				% 'GET' | 'POST'
	uri,				% {UriType, Uri}
	args = "",			% list()
	headers,			% [{Tag, Val}]
	body = <<>>			% bytes()
}).
```

To use this record in your own code, you will need to include the file _misultin.hrl_. We recommend using the appropriate function `Req:get/1`, since additional operations are performed then such as Uri checks.

### Req:redirect(Url) ###

Redirect temporarily to the specified Url.

### Req:redirect(permanent, Url) ###

Redirect permanently to the specified Url.

### Req:resource(Options) -> Resources ###
```erlang

Options = [Option]
 Option = lowercase | urldecode
Resources = [Resource]
 Resource = list()
```

This functionality provides support for RESTful services built with Misultin. It will split the URI into the list of its resources (i.e. it will separate the URI with the / token).

It also allows to provide two options:

* *lowercase*: convert all resources to lowercase;
* *urldecode*: decode urlencoded characters.

See the ExamplesPage for a more detailed usage of this function.

### Req:respond(HttpCode, Template) ###

Same as `Req:respond(HttpCode, [], Template)`.

### Req:respond(HttpCode, Headers, Template) ###
```erlang

HttpCode = integer()
Headers = {Header]
 Header = {HeaderName, HeaderContent}
  HeaderName = atom()
  HeaderContent = list()
Template = atom() | list() | binary() | iolist()
```

Formats an HTTP response of code `HttpCode`. 

`HeaderName` is the name of the HTTP header field; `HeaderContent` is the content of the header.

This example formats a static XML response:

```erlang

Req:respond(200, [{"Content-Type", "text/xml"}], "<misultin_test>ok</misultin_test>").
```

This example formats an XML response with the content of a parameter `Value`, using an `iolist()`:

```erlang

Req:respond(200, [{"Content-Type", "text/xml"}], ["<misultin_test><value>", Value, "</value></misultin_test>"]).
```


### Req:respond(HttpCode, Headers, Template, Vars) ###
```erlang

HttpCode = integer()
Headers = {Header]
 Header = {HeaderName, HeaderContent}
  HeaderName = atom()
  HeaderContent = list()
Template = list()
Vars = [term()]
```

Formats an HTTP response of code `HttpCode`. 

`HeaderName` is the name of the HTTP header field; `HeaderContent` is the content of the header.

`Template` and `Vars` build a character list which represents `Vars` formatted in accordance with `Template`. See <http://erlang.org/doc/man/io.html#format-3 io:format/3> for a detailed description of the available formatting options. A fault is generated if there is an error in the format string or argument list.

This example formats an XML response with the content of a parameter `Value`:

```erlang

Req:respond(200, [{"Content-Type", "text/xml"}], "<misultin_test><value>~s</value></misultin_test>", [Value]).
```

### Req:session() -> {SessionId, SessionState} | {error, Reason} ###

Starts or retrieves an existing session, returning the SessionId and the SessionState. SessionState defaults to an empty array `[]`.

### Req:session(Cookies) -> {SessionId, SessionState} | {error, Reason} ###

Same as `Req:session/1` but if Cookies have already been parsed and are available, this allows a performance increase.

### Req:session(SessionId, SessionState)  -> ok | {error, Reason} ###

Saves the new SessionState for a SessionId.

### Req:stream(head) ###

Same as `Req:stream(head, 200, [])`.

### Req:stream(head, Headers) ###

Same as `Req:stream(head, 200, Headers)`.

### Req:stream(head, HttpCode, Headers) ###
```erlang

HttpCode = integer()
Headers = {Header]
 Header = {HeaderName, HeaderContent}
  HeaderName = atom()
  HeaderContent = list()
```

Starts an HTTP response of code `HttpCode`. 

`HeaderName` is the name of the HTTP header field; `HeaderContent` is the content of the header.

The functions `stream/1,2,3` are used when the HTTP response has to be given progressively, and not altogether as it happens in `respond/2,3,4`.

In order to provide a 'streamed' HTTP response, these functions have to be used in a specific manner, as shown in this example:

```erlang

% send headers
Req:stream(head, [{"Content-Type", "text/plain"}]),
% send stream
Req:stream("1"),
timer:sleep(2000),
% send stream
Req:stream("2"),
timer:sleep(2000),
% send stream
Req:stream("3"),
% close socket
Req:stream(close).
```

This will start a 200 HTTP response of text/plain Content-Type and immediately send the character `1`. It will then wait 2 seconds, before sending character `2`. Finally, it will wait another 2 seconds before sending the last character `3` and close the socket.

Basically, the `stream/1,2,3` functions work by providing all HTTP headers with the exception of _Content-Length_, which would otherwise allow the HTTP client to close the socket when all data is received. Therefore, the HTTP client keeps on listening since it doesn't know how much bytes it is supposed to receive, and thus it can be sent data progressively. Finally, however, since in this example no Content-Length header has been set, the closing of the socket needs to be done server-side, and thus the use of the function `Req:stream(close)`.

### Req:stream(Template) ###
```erlang

Template = atom() | list() | binary() | iolist()
```

Sends data into the open HTTP socket.

### Req:stream(Template, Vars) ###
```erlang

Template = list()
Vars = [term()]
```

Sends data into the open HTTP socket. 

`Template` and `Vars` build a character list which represents `Vars` formatted in accordance with `Template`. See <http://erlang.org/doc/man/io.html#format-3 io:format/3> for a detailed description of the available formatting options. A fault is generated if there is an error in the format string or argument list.

### Req:stream(close) ###

Server-side closes the open socket. If Content-Length has NOT been specified, the socket must manually be closed by calling `Req:stream(close)`.

### Req:stream({error, Reason}) ###

If an error occurred when dealing with streams, you may call this function to terminate the connection and log the error.

### Req:uri_unquote(Uri) -> UnquotedUri ###

Convert Uri format into a string().

# 3. Misultin Websockets #

## Description ##

This module contains the necessary functionalities to manage websockets. For the draft websocket protocol, you may refer to <http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-76>. This module uses Erlang parametrized modules, therefore the syntax is to be understood accordingly.

## Exports ##

### Ws:get(WsInfo) -> term() ###
```erlang

WsInfo = socket | socket_mode | peer_addr | peer_port | peer_cert | vsn | origin | host | path | headers
```

Retrieves the Websocket information. See `Ws:raw/0` for additional information on the output format.

### Req:get_cookies() -> [{CookieTag, CookieValue}] ###

Retrieve all the cookies from the Cookie header in a proplist format.

### Req:get_cookie_value(CookieName, Cookies) -> string() ###

Retrieve the cookie value from the Cookies list. Note that being in a websocket, cookies cannot be set or deleted unless some specific javascript gets evaluated on the client browser. Therefore, these functionalities cannot be provided as part of the websocket module and have to be implemented accordingly, if needed.

### Ws:raw() -> record() ###

Retrieves the Websocket complete record, defined as follows:
```erlang

-record(req, {
	socket,				% the socket handling the websocket
	socket_mode,		% http | ssl
	peer_addr,			% IP | undefined
	peer_port,			% Port | undefined
	peer_cert,			% undefined | the DER encoded peer certificate
	vsn,				% {Maj,Min} | {'draft-hixie', Ver}
	origin,				% the originator
	host,				% the host
	path,				% the websocket GET request path
	headers				% [{Tag, Val}]
}).
```

To use this record in your own code, you will need to include the file _misultin.hrl_. We recommend using the appropriate function `Ws:get/1`, since additional operations are performed then such as Uri checks.

### Ws:send(Data) ###
```erlang

Data = list() | iolist()
```

Sends data into the open Websocket to the recipient browser. For additional information, see the [[ExamplesPage]].

### Req:session() -> {SessionId, SessionState} | {error, Reason} ###

Starts or retrieves an existing session, returning the SessionId and the SessionState. SessionState defaults to an empty array `[]`.

### Req:session(Cookies) -> {SessionId, SessionState} | {error, Reason} ###

Same as `Req:session/1` but if Cookies have already been parsed and are available, this allows a performance increase.

### Req:session(SessionId, SessionState)  -> ok | {error, Reason} ###

Saves the new SessionState for a SessionId.
