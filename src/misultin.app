{application, misultin,
[
	{description, "Lightweight HTTP Server Library"},
	{vsn, '0.3.3'},
	{modules, [misultin_socket, misultin_req, misultin]},
	{registered, [misultin]},
	{env, []},
	{applications, [kernel, stdlib]}
]}.
