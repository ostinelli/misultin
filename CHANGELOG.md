# CHANGELOG

### 0.9:
 * added SESSIONS state, persistent across requests
 * added access log callback function, so that main application can log HTTP access
 * added streaming input for big files or endless input, using a manual ```body_recv function``` in conjunction with the ```{auto_recv_body, false}``` option
 * added static directory support, so that GET requests to /static/* can automatically send files from a specified directory (thanks to egobrain suggestion)
 * added request redirection helper method
 * consistently improved memory usage by not copying by default to handler processes the full request or websocket record
 * added configuration option to set which websocket versions must be supported by the server
 * added support for websocket draft-hybi-10
 * added support for websocket draft-hybi-17 (thanks to RJ)
 * added support for websockets on FireFox (thanks to Egobrain)
 * added support for 'If-Modified-Since' headers in file sending (thanks to davidgaleano)
 * added support for websockets when behind stunnel with ```{external_ssl, boolean()}``` option (thanks to RJ)
 * added support to see the correct client IP when behind stunnel, according to http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt (thanks to RJ)
 * added support for OPTIONS method (thanks to majek)
 * rebar-ized makefile
 * corrected minor bugs (thank you all - you know who you are!)

### 0.8:
 * Misultin has been redesigned to use supervisor behaviours where appropriate, to be more OTP compliant
 * added Cookie support
 * added preliminary support multipart/form-data and a file upload example (thanks to Max Lapshin)
 * added Date and Server headers
 * added support for headers being specified as binaries (thanks to akaspin)
 * added an example on how to properly embed misultin in your application
 * ```Req:get(peer_addr)``` now properly extracts peer information from headers "X-Real-Ip" or "X-Forwarded-For" if these are available (thanks to Max Lapshin)
 * solved bug on large data being sent over via websockets (thanks to omarkj)
 * corrected binary sending bug in websockets which would fail binaries on io_lib format (thanks to normanb)
 * added recbuf advanced option (issue #40)
 * added preliminary test suite
 * various optimizations using binary when needed

### 0.7.1:
 * considerably improved stability under heavy load
 * misultin now accepts incoming connections with a pool of acceptors instead of a single one
 * Misultin can now be used both with parametrized modules and with pure erlang code too (thanks to yrashk, nox and essen)
 * added support for HEAD, PUT, DELETE, TRACE and CONNECT methods
 * now websockets are on ```{active, once}``` mode to avoid malicious clients overflooding (thanks to essen)
 * ensured that body of request can be read on all methods except TRACE as per http specs
 * added support for iolist() in chunked resposes (thanks to RJ)

### 0.7:
 * added ```max_connections options``` parameter, which specifies maximum concurrent open connections accepted by the server
 * added ```post_max_size``` options parameter, which sets the maximum size of POST data
 * added ```get_url_max_size``` options parameter, which sets the maximum length of URI
 * added CHUNKED support, both for incoming requests and outgoing responses (thanks to yrashk suggestion)
 * added trapping of client closing a browser in Comet applications (thanks to yrashk)
 * added SSL support for websockets (enhancement track #25, thanks to viplifes)
 * Misultin can now be started without a registered name or with a different name, so that multiple versions of misultin can be started on a single node
 * added support for IP address specified in tuple format (thanks to okeuday suggestion)
 * added support to extract plain uri unquoted as a list() (thanks to okeuday)
 * added Comet Long Polling example
 * added Comet iFrame example
 * added the killing of alive processes on server shutdown
 * the GET uri parameters are now also available on POST requests
 * added custom headers on file sending to browser (thanks to josevalim)
 * additional minor adjustments

### 0.6.2:
 * refactored to considerably improve sending of static files
 * minor bug corrections

### 0.6.1:
 * added support to websocket protocol hixie draft 76 (thanks to sergio veiga)
 * added support to multiple websocket draft protocols (for backwards compatibility)
 * added ```ws_autoexit option``` which allows to get an event on websocket controlling processes (issue track #15, suggestion of esente)
 * added headers also in misultin websockets (thanks to jlirochon)
 * made it basho's rebar friendly (thanks to mrinalwadhwa)

### 0.6:
 * added HTTP compression option
 * refactoring of the main server loop, so that it is now isolated from the HTTP functionality
 * removed unnecessary compilation warnings
 * replaced ```proplists:get_value``` with much faster utility function

### 0.5:
 * added SSL support
 * refactoring of the acceptor loop

### 0.4:
 * added preliminary websocket support

### 0.3.4:
 * added Req support to return the socket handling the request
 * bug correction on Content-Length: 0 header causing timeout on POST requests (issue track #12, thanks to gdamjan)

### 0.3.3:
 * added echoing of the Connection header (issue track #7, thanks to thijsterlouw)
 * bug correction on acceptor respawning (issue track #10, thanks to thijsterlouw)

### 0.3.2:
 * optimized error handling (issue track #5, thanks to Max Lapshin)

### 0.3.1:
 * added flow control using inet options {active, once} (issue track #4, thanks to Max Lapshin)
 * added support to standard http headers response
 * added http 400 bad request error in socket handling
 * bug correction: removed erroneous sending of response timeout on listening open connections
 * added stream_support optimization option

### 0.3:
 * reengineering of the listener process, using active instead of passive mode in request parsing, except for BODY where passive is still used (thanks to Lev Walkin)
 * added better support for request timeout

### 0.2.2:
 * added .app file (thanks to Essien Ita Essien)
 * simplified get_options (thanks to Essien Ita Essien)
 * added ip address option (thanks to Essien Ita Essien)
 * added ipv6 support
 * added ```recv_timeout``` option
 * bug correction: requests peer address and port are now not reset on open connections multiple requests

### 0.2.1:
 * added support for Content-Type that specifies charset in POST data (thanks to Tuncer Ayaz)
 * added support for iolist in ```misultin_req:ok/1,2``` and ```misultin_req:respond/2,3```
 * code optimized taking out unnecessary binary conversion and lists:flatten (thanks to Feng Yu)

### 0.2:
 * added trap exit for acceptor failure
 * added backlog option
 * added fallback if no connection header is present (issue track #1, thanks to Ciconia)
 * added limit for parsing headers to avoid malicious attacks (thanks to Mazen Harake)
 * minor bug corrections

### 0.1:
 * initial release.

