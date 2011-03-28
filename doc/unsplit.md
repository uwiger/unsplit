Module unsplit
==============


<h1>Module unsplit</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Framework for merging mnesia tables after netsplit.



__Behaviours:__ [`application`](application.md), [`supervisor`](supervisor.md).

__Authors:__ : Ulf Wiger ([`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com)).

<h2><a name="description">Description</a></h2>





...


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_reporter-0">get_reporter/0</a></td><td>Look up the predefined callback module for reporting inconsistencies.</td></tr><tr><td valign="top"><a href="#report_inconsistency-4">report_inconsistency/4</a></td><td>Report an inconcistency to the predefined reporter.</td></tr><tr><td valign="top"><a href="#report_inconsistency-5">report_inconsistency/5</a></td><td>Report an inconsistency to Reporter (an unsplit_reporter behaviour).</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Application start callback.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Application stop callback.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="get_reporter-0"></a>

<h3>get_reporter/0</h3>






<pre>get_reporter() -> <a href="#type-module">module()</a></pre>

<br></br>




Look up the predefined callback module for reporting inconsistencies
<a name="report_inconsistency-4"></a>

<h3>report_inconsistency/4</h3>






<pre>report_inconsistency(Tab::Table, Key, ObjA::ObjectA, ObjB::ObjectB) -> ok</pre>

<br></br>




Report an inconcistency to the predefined reporter
<a name="report_inconsistency-5"></a>

<h3>report_inconsistency/5</h3>






<pre>report_inconsistency(Reporter, Tab::Table, Key, ObjA::ObjectA, ObjB::ObjectB) -> ok</pre>

<br></br>




Report an inconsistency to Reporter (an unsplit_reporter behaviour)
<a name="start-2"></a>

<h3>start/2</h3>






<pre>start(X1::Type, X2::Arg) -> {ok, pid()}</pre>

<br></br>




Application start callback
<a name="stop-1"></a>

<h3>stop/1</h3>






<pre>stop(X1::State) -> ok</pre>

<br></br>




Application stop callback
