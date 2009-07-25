==========================================================================================================
MISULTIN - An Erlang library for building fast lightweight HTTP servers.
<http://code.google.com/p/misultin/>

>-|-|-<°>


INSTALL INSTRUCTIONS
==========================================================================================================

1. Compile

Run the appropriate script:

    * OSX | Linux users: ./compile
    * Windows users: compile.bat. Note that Erlang bin directory (by default, C:\Program Files\erl5.7.2\bin\) must be in your path for the script to work. 

Optionally, you may consider setting a logging level with the compile options. Run the script with the -h option for more information.

You may also prefer to compile the files manually. If so, compile the files in the src directory and drop the resulting .beam files into the ebin directory.
2. (Optional) Copy Misultin files

This optional step is recommended and will allow Misultin to be called from modules running from any directory on your file system.

Locate the directory where Erlang is installed on your system. Under OSX and Linux, it should be something similar to /usr/local/lib/erlang/ and under Windows C:\Program Files\erl5.7.2\.

Browse into the lib directory under this Erlang root and create a misultin directory, then copy all Misultin files and directories into the newly created directory. You should now have Misultin under something similar to:

    * OSX | Linux users: /usr/local/lib/erlang/lib/misultin/
    * Windows users: C:\Program Files\erl5.7.2\lib\misultin\ 

If you do not want to proceed with this optional step, then all you need to do is copy the relevant Misultin files in the same directory of the code calling it.
3. Test

Open an erlang shell, compile and run misultin_hello_world from the Misultin examples directory by issuing:

(one@rob.loc)1>c(misultin_hello_world).
{ok,example_simple}
(one@rob.loc)2>misultin_hello_world:start().
{ok,<0.50.0>}

Open your favourite browser and point it to http://localhost:8080/, you should read "Hello World." printed on your page.

You may now stop the misultin_hello_world HTTP server:

(one@rob.loc)3>misultin_hello_world:stop().

4. Congratulations!

You're ready to go.

>-|-|-<°> 

DOCUMENTATION
==========================================================================================================

API Documentation is available online on the Misultin's wiki: http://code.google.com/p/misultin/wiki/
