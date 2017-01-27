-module(index).
-export([make/1,init/1,loop/2]).
-define(DATABASE,?MODULE.ets).

make(Websocket) -> 
   spawn_link(?MODULE,init,[Websocket]).

init(Websocket) ->
	io:format("New Process: ~p~n", [?MODULE]),
	case filelib:is_regular(?DATABASE) of
		true -> {ok,Tab} = ets:file2tab(?DATABASE);
		false -> Tab = ets:new(points,[set,protected]),
			insert_defaults(Tab)
	end,
	[{rows,R}] = ets:lookup(Tab, rows),
	[{columns,C}] = ets:lookup(Tab, columns),
	Js = lists:flatten(io_lib:format("create_list(['~p','~p']);",[R,C])),
	Websocket:send(Js),
	loop(Websocket,Tab).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% receive erlang_term and send javascript code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Websocket,Tab) ->
	receive
		{chat,[Row|Data],Update} -> % {chat,[r3,4,4,5],{a1,4}}.
			Meanvalue = mean_value(Data),
			Js = lists:flatten(io_lib:format("summa = $$(~ss).elmsByTag('input'); 
				summa[0].value=~4.2f",[Row,Meanvalue])),
			io:format("Sending back to client: ~p~n", [Js]),
			Websocket:send(Js),
			update_values(Tab,Row,Update),
			?MODULE:loop(Websocket,Tab);
		{add_or_delete,Row,Col} -> 
			add_or_delete(Tab,Row,Col),
			[{rows,R}] = ets:lookup(Tab, rows),
			[{columns,C}] = ets:lookup(Tab, columns),
			Js = lists:flatten(io_lib:format("create_list(['~p','~p']);",[R,C])),
			Websocket:send(Js),
			?MODULE:loop(Websocket,Tab);
		{quit} -> io:format("~p quits~n",[?MODULE]);	
		Any -> io:format("~p got unknown msg: ~p~n",[?MODULE, Any]),
			?MODULE:loop(Websocket,Tab)
	after 600000 ->
		io:format("~p timedout~n",[?MODULE])	
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Takes a list of values and returns the mean value
% If there are 5 values or more, it first strips off highest and lowest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	mean_value(V) ->
		case length(V) of
			X when X >= 5 -> L1 = lists:sort(V),
				L2 = tl(L1), L3 = tl(lists:reverse(L2)),
				lists:sum(L3)/length(L3);
			_ -> lists:sum(V)/length(V)
		end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Inserts default values in the database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
insert_defaults(Table) ->
	ets:delete_all_objects(Table),
	ets:insert(Table, {rows,1}),
	ets:insert(Table, {columns,1}),
	ets:tab2file(Table,?DATABASE).

add_or_delete(Table,Row,Col) ->
		[{rows,R}] = ets:lookup(Table, rows),
		ets:insert(Table, {rows,R+Row}),
		[{columns,C}] = ets:lookup(Table, columns),
		ets:insert(Table, {columns,C+Col}),
		ets:tab2file(Table,?DATABASE).
		
update_values(Table,Row,{Col,Val}) ->
	ets:insert(Table,{Row,Col,Val}).
	