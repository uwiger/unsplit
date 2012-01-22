

#Module unsplit_reporter#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Unsplit Inconsistency Reporter Behaviour.

<a name="description"></a>

##Description##


This module implements a basic behaviour for reporting inconsistencies
encountered during the merge procedure.
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#childspec-0">childspec/0</a></td><td>Return a child start specification for the pre-defined reporter.</td></tr><tr><td valign="top"><a href="#inconsistency-4">inconsistency/4</a></td><td>Report an inconsistency encountered during the merge.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="behaviour_info-1"></a>

###behaviour_info/1##




`behaviour_info(X1) -> any()`

<a name="childspec-0"></a>

###childspec/0##




<pre>childspec() -> ignore | <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a></pre>
<br></br>






Return a child start specification for the pre-defined reporter

See [`supervisor`](supervisor.md).
Use `ignore` if no process should be started.<a name="inconsistency-4"></a>

###inconsistency/4##




<pre>inconsistency(Table, Key, ObjA::ObjectA, ObjB::ObjectB) -&gt; ok</pre>
<br></br>






Report an inconsistency encountered during the merge

The default implementation raises an alarm via the SASL alarm_handler