{application, misultin_app_example,
[
	{description, "Misultin Application Example"},
	{vsn, "0.1"},
	{modules, [
		misultin_app_example,
		misultin_app_example_sup,
		misultin_app_example_server,
		misultin,
		misultin_acceptor,
		misultin_acceptors_sup,
		misultin_http,
		misultin_req,
		misultin_server,
		misultin_socket,
		misultin_utility,
		misultin_websocket,
		misultin_ws
	]},
	{registered, []},
	{mod, {misultin_app_example, []}},
	{env, []},
	{applications, [kernel, stdlib]}
]}.
