==========================================================================================================
MISULTIN - An Erlang library for building fast lightweight HTTP servers.
<https://github.com/ostinelli/misultin>

>-|-|-(°>


INSTALL INSTRUCTIONS
==========================================================================================================

1. Compile

Run the appropriate script:

    * OSX | Linux users:  make
    * Windows users: make.bat. Note that Erlang bin directory (by default, C:\Program Files\erl5.7.2\bin\)
      must be in your path for the script to work.

This script will compile the .erl files in the Misultin src directory and save the compiled files into the
ebin directory.

2. (Optional) Copy Misultin files

This optional step will allow Misultin to be called from modules running from any directory on your file
system.

Locate the directory where Erlang is installed on your system. Under OSX and Linux, it should be something
similar to /usr/local/lib/erlang/ or /usr/lib/erlang/. Under Windows, Erlang is installed by default in
something similar to C:\Program Files\erl5.7.2\ (version changes may apply).

Browse into the lib directory under this Erlang root and copy the root misultin-0.x directory into this
directory. You should now have Misultin under something similar to:

    * OSX | Linux users: /usr/local/lib/erlang/lib/misultin-0.x/
    * Windows users: C:\Program Files\erl5.7.2\lib\misultin-0.x\ 

3. Test

If you did proceed with step 2, CD to the Misultin examples directory and start an Erlang shell.

If you did not proceed with step 2, copy the file misultin_hello_world.erl from the Misultin examples
directory to the directory where the .beam files compiled in step 1 are located, then CD to this
directory and start an Erlang shell.

In the shell, compile and run misultin_hello_world by issuing:

(one@rob.loc)1>c(misultin_hello_world).
{ok,misultin_hello_world}
(one@rob.loc)2>misultin_hello_world:start(8080).
{ok,<0.50.0>}

Open your favourite browser and point it to http://localhost:8080/, you should read "Hello World."
printed on your page.

You may now stop the misultin_hello_world HTTP server:

(one@rob.loc)3>misultin_hello_world:stop().

4. Congratulations!

You're ready to go.

>-|-|-(°>


DOCUMENTATION
==========================================================================================================

API Documentation is available online on the Misultin's wiki: https://github.com/ostinelli/misultin/wiki


CHANGELOG
==========================================================================================================

0.7.1-dev:
       - Misultin can now be used both with parametrized modules and with pure erlang code too [thanks to
         yrashk, nox and essen]

0.7:   - added max_connections options parameter, which specifies maximum concurrent open connections
         accepted by the server
       - added post_max_size options parameter, which sets the maximum size of POST data
       - added get_url_max_size options parameter, which sets the maximum length of URI
       - added CHUNKED support, both for incoming requests and outgoing responses [thanks to yrashk
         suggestion]
       - added trapping of client closing a browser in Comet applications [thanks to yrashk]
       - added SSL support for websockets [enhancement track #25, thanks to viplifes]
       - Misultin can now be started without a registered name or with a different name, so that multiple
         versions of misultin can be started on a single node
       - added support for IP address specified in tuple format [thanks to okeuday suggestion]
       - added support to extract plain uri unquoted as a list() [thanks to okeuday]
       - added Comet Long Polling example
       - added Comet iFrame example
       - added the killing of alive processes on server shutdown
       - the GET uri parameters are now also available on POST requests
       - added custom headers on file sending to browser [thanks to josevalim]
       - additional minor adjustments

0.6.2: - refactored to considerably improve sending of static files
       - minor bug corrections

0.6.1: - added support to websocket protocol hixie draft 76 [thanks to sergio veiga]
       - added support to multiple websocket draft protocols [for backwards compatibility]
       - added ws_autoexit option which allows to get an event on websocket controlling processes [issue
         track #15, suggestion of esente]
       - added headers also in misultin websockets [thanks to jlirochon]
       - made it basho's rebar friendly [thanks to mrinalwadhwa]

0.6:   - added HTTP compression option
       - refactoring of the main server loop, so that it is now isolated from the HTTP functionality
       - removed unnecessary compilation warnings
       - replaced proplists:get_value with much faster utility function

0.5:   - added SSL support
       - refactoring of the acceptor loop

0.4:   - added preliminary websocket support

0.3.4: - added Req support to return the socket handling the request
       - bug correction on Content-Length: 0 header causing timeout on POST requests [issue track #12,
         thanks to gdamjan]

0.3.3: - added echoing of the Connection header [issue track #7, thanks to thijsterlouw]
       - bug correction on acceptor respawning [issue track #10, thanks to thijsterlouw]

0.3.2: - optimized error handling [issue track #5, thanks to Max Lapshin]

0.3.1: - added flow control using inet options {active, once} [issue track #4, thanks to Max Lapshin]
       - added support to standard http headers response
       - added http 400 bad request error in socket handling
       - bug correction: removed erroneous sending of response timeout on listening open connections
       - added stream_support optimization option

0.3:   - reengineering of the listener process, using active instead of passive mode in request parsing,
         except for BODY where passive is still used [thanks to Lev Walkin]
       - added better support for request timeout

0.2.2: - added .app file [thanks to Essien Ita Essien]
       - simplified get_options [thanks to Essien Ita Essien]
       - added ip address option [thanks to Essien Ita Essien]
       - added ipv6 support
       - added recv_timeout option
       - bug correction: requests peer address and port are now not reset on open connections multiple
         requests

0.2.1: - added support for Content-Type that specifies charset in POST data [thanks to Tuncer Ayaz]
       - added support for iolist in misultin_req:ok/1,2 and misultin_req:respond/2,3
       - code optimized taking out unnecessary binary conversion and lists:flatten [thanks to Feng Yu]

0.2:   - added trap exit for acceptor failure
       - added backlog option
       - added fallback if no connection header is present [issue track #1, thanks to Ciconia]
       - added limit for parsing headers to avoid malicious attacks [thanks to Mazen Harake]
       - minor bug corrections

0.1:   - initial release.

