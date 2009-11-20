==========================================================================================================
MISULTIN - An Erlang library for building fast lightweight HTTP servers.
<http://code.google.com/p/misultin/>

>-|-|-(°>


INSTALL INSTRUCTIONS
==========================================================================================================

1. Compile

Run the appropriate script:

    * OSX | Linux users:  make
    * Windows users: compile.bat. Note that Erlang bin directory (by default, C:\Program Files\erl5.7.2\bin\) must be in your path for the script to work.

This script will compile the .erl files in the Misultin src directory and save the compiled files into the ebin directory.

2. (Optional) Copy Misultin files

This optional step will allow Misultin to be called from modules running from any directory on your file system.

Locate the directory where Erlang is installed on your system. Under OSX and Linux, it should be something similar to /usr/local/lib/erlang/ or /usr/lib/erlang/. Under Windows, Erlang is installed by default in something similar to C:\Program Files\erl5.7.2\ (version changes may apply).

Browse into the lib directory under this Erlang root and copy the root misultin-0.x directory into this directory. You should now have Misultin under something similar to:

    * OSX | Linux users: /usr/local/lib/erlang/lib/misultin-0.x/
    * Windows users: C:\Program Files\erl5.7.2\lib\misultin-0.x\ 

3. Test

If you did proceed with step 2, CD to the Misultin examples directory and start an Erlang shell.

If you did not proceed with step 2, copy the file misultin_hello_world.erl from the Misultin examples directory to the directory where the .beam files compiled in step 1 are located, then CD to this directory and start an Erlang shell.

In the shell, compile and run misultin_hello_world by issuing:

(one@rob.loc)1>c(misultin_hello_world).
{ok,example_simple}
(one@rob.loc)2>misultin_hello_world:start(8080).
{ok,<0.50.0>}

Open your favourite browser and point it to http://localhost:8080/, you should read "Hello World." printed on your page.

You may now stop the misultin_hello_world HTTP server:

(one@rob.loc)3>misultin_hello_world:stop().

4. Congratulations!

You're ready to go.

>-|-|-(°>


DOCUMENTATION
==========================================================================================================

API Documentation is available online on the Misultin's wiki: http://code.google.com/p/misultin/wiki/


CHANGELOG
==========================================================================================================

0.3.3: - added echoing of the Connection header [issue track #7, thanks to thijsterlouw

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

