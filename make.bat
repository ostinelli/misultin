@echo off
REM ==========================================================================================================
REM MISULTIN - compile
REM 
REM >-|-|-<°>
REM
REM Copyright (C) 2009, Roberto Ostinelli <roberto@ostinelli.net>
REM All rights reserved.
REM
REM BSD License
REM 
REM Redistribution and use in source and binary forms, with or without modification, are permitted provided
REM that the following conditions are met:
REM
REM  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
REM    following disclaimer.
REM  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
REM    the following disclaimer in the documentation and/or other materials provided with the distribution.
REM  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
REM    products derived from this software without specific prior written permission.
REM
REM THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
REM WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
REM PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
REM ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
REM TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
REM HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
REM NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
REM POSSIBILITY OF SUCH DAMAGE.
REM ==========================================================================================================

:BEGIN
IF "%1"=="debug" GOTO SETDEBUG
IF "%1"=="example" GOTO EXAMPLES
IF "%1"=="clean" GOTO CLEAN
GOTO COMPILE

:SETDEBUG
SET command=-D log_debug
GOTO COMPILE

:EXAMPLES
mkdir ebin
FOR %%f in (examples\*.erl) DO erlc -W %command% -pa ebin -I include -o ebin "%%f"

:COMPILE
mkdir ebin
FOR %%f in (src\*.erl) DO erlc -W %command% -pa ebin -I include -o ebin "%%f"
copy src\misultin.app.src ebin\misultin.app /Y
GOTO END

:CLEAN
FOR %%f in (ebin\*) DO del "%%f"

:END
