Module unsplit_server
=====================


<h1>Module unsplit_server</h1>

* [Function Index](#index)
* [Function Details](#functions)






__Behaviours:__ [`gen_server`](gen_server.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#remote_handle_query-1">remote_handle_query/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the Unsplit server.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="code_change-3"></a>

<h3>code_change/3</h3>





`code_change(OldVsn, State, Extra) -> any()`

<a name="handle_call-3"></a>

<h3>handle_call/3</h3>





`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

<h3>handle_cast/2</h3>





`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

<h3>handle_info/2</h3>





`handle_info(Info, State) -> any()`

<a name="init-1"></a>

<h3>init/1</h3>





`init(X1) -> any()`

<a name="remote_handle_query-1"></a>

<h3>remote_handle_query/1</h3>





`remote_handle_query(Q) -> any()`

<a name="start_link-0"></a>

<h3>start_link/0</h3>






<pre>start_link() -> {ok, pid()}</pre>

<br></br>




Starts the Unsplit server<a name="terminate-2"></a>

<h3>terminate/2</h3>





`terminate(Reason, State) -> any()`

