Module unsplit_reporter
=======================


<h1>Module unsplit_reporter</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Unsplit Inconsistency Reporter Behaviour.



__This module defines the `unsplit_reporter` behaviour.__
<br></br>
 Required callback functions: `childspec/0`, `inconsistency/4`.

<h2><a name="description">Description</a></h2>



This module implements a basic behaviour for reporting inconsistencies
encountered during the merge procedure.


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#childspec-0">childspec/0</a></td><td>Return a child start specification for the pre-defined reporter.</td></tr><tr><td valign="top"><a href="#inconsistency-4">inconsistency/4</a></td><td>Report an inconsistency encountered during the merge.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="behaviour_info-1"></a>

<h3>behaviour_info/1</h3>





`behaviour_info(X1) -> any()`

<a name="childspec-0"></a>

<h3>childspec/0</h3>






<pre>childspec() -> ignore | <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a></pre>

<br></br>






Return a child start specification for the pre-defined reporter

See [`supervisor`](supervisor.md).
Use `ignore` if no process should be started.<a name="inconsistency-4"></a>

<h3>inconsistency/4</h3>






<pre>inconsistency(Table, Key, ObjA::ObjectA, ObjB::ObjectB) -> ok</pre>

<br></br>






Report an inconsistency encountered during the merge

The default implementation raises an alarm via the SASL alarm_handler