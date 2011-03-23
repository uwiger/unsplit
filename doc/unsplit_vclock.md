Module unsplit_vclock
=====================


<h1>Module unsplit_vclock</h1>

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


A simple Erlang implementation of vector clocks as inspired by Lamport logical clocks.



Copyright © 2007-2008 Basho Technologies

__Authors:__ Justin Sheehy ([`justin@basho.com`](mailto:justin@basho.com)), Andy Gross ([`andy@basho.com`](mailto:andy@basho.com)).

__References__* Leslie Lamport (1978). "Time, clocks, and the ordering of events in a distributed system". Communications of the ACM 21 (7): 558-565.
* Friedemann Mattern (1988). "Virtual Time and Global States of Distributed Systems". Workshop on Parallel and Distributed Algorithms: pp. 215-226


<h2><a name="types">Data Types</a></h2>


<a name="type-counter"></a>


<h3 class="typedecl">counter()</h3>

<tt>counter() = integer()</tt>
<a name="type-node"></a>


<h3 class="typedecl">node()</h3>

<tt>node() = term()</tt>

Nodes can have any term() as a name, but they must differ from each other.
<a name="type-timestamp"></a>


<h3 class="typedecl">timestamp()</h3>

<tt>timestamp() = integer()</tt>
<a name="type-vc_entry"></a>


<h3 class="typedecl">vc_entry()</h3>

<tt>vc_entry() = {<a href="#type-node">node()</a>, {<a href="#type-counter">counter()</a>, <a href="#type-timestamp">timestamp()</a>}}</tt>

The timestamp is present but not used, in case a client wishes to inspect it.
<a name="type-vclock"></a>


<h3 class="typedecl">vclock()</h3>

<tt>vclock() = [vc_entry]</tt>

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_nodes-1">all_nodes/1</a></td><td>Return the list of all nodes that have ever incremented VClock.</td></tr><tr><td valign="top"><a href="#descends-2">descends/2</a></td><td>Return true if Va is a direct descendant of Vb, else false -- remember, a vclock is its own descendant!</td></tr><tr><td valign="top"><a href="#equal-2">equal/2</a></td><td>Compares two VClocks for equality.</td></tr><tr><td valign="top"><a href="#fresh-0">fresh/0</a></td><td>Create a brand new vclock.</td></tr><tr><td valign="top"><a href="#get_counter-2">get_counter/2</a></td><td>Get the counter value in VClock set from Node.</td></tr><tr><td valign="top"><a href="#get_timestamp-2">get_timestamp/2</a></td><td>Get the timestamp value in a VClock set from Node.</td></tr><tr><td valign="top"><a href="#increment-2">increment/2</a></td><td>Increment VClock at Node.</td></tr><tr><td valign="top"><a href="#merge-1">merge/1</a></td><td>Combine all VClocks in the input list into their least possible
common descendant.</td></tr><tr><td valign="top"><a href="#prune-3">prune/3</a></td><td>Possibly shrink the size of a vclock, depending on current age and size.</td></tr></table>


<a name="functions"></a>


<h2>Function Details</h2>


<a name="all_nodes-1"></a>


<h3>all_nodes/1</h3>





<tt>all_nodes(VClock::<a href="#type-vclock">vclock()</a>) -> [<a href="#type-node">node()</a>]</tt>



Return the list of all nodes that have ever incremented VClock.
<a name="descends-2"></a>


<h3>descends/2</h3>





<tt>descends(Va::<a href="#type-vclock">vclock()</a>, Vb::<a href="#type-vclock">vclock()</a>) -> bool()</tt>



Return true if Va is a direct descendant of Vb, else false -- remember, a vclock is its own descendant!
<a name="equal-2"></a>


<h3>equal/2</h3>





<tt>equal(VClockA::<a href="#type-vclock">vclock()</a>, VClockB::<a href="#type-vclock">vclock()</a>) -> true | false</tt>



Compares two VClocks for equality.
Not very fast.
<a name="fresh-0"></a>


<h3>fresh/0</h3>





<tt>fresh() -> <a href="#type-vclock">vclock()</a></tt>



Create a brand new vclock.
<a name="get_counter-2"></a>


<h3>get_counter/2</h3>





<tt>get_counter(Node::<a href="#type-node">node()</a>, VClock::<a href="#type-vclock">vclock()</a>) -> <a href="#type-counter">counter()</a></tt>



Get the counter value in VClock set from Node.
<a name="get_timestamp-2"></a>


<h3>get_timestamp/2</h3>





<tt>get_timestamp(Node::<a href="#type-node">node()</a>, VClock::<a href="#type-vclock">vclock()</a>) -> <a href="#type-timestamp">timestamp()</a></tt>



Get the timestamp value in a VClock set from Node.
<a name="increment-2"></a>


<h3>increment/2</h3>





<tt>increment(Node::<a href="#type-node">node()</a>, VClock::<a href="#type-vclock">vclock()</a>) -> <a href="#type-vclock">vclock()</a></tt>



Increment VClock at Node.
<a name="merge-1"></a>


<h3>merge/1</h3>





<tt>merge(VClocks::[<a href="#type-vclock">vclock()</a>]) -> <a href="#type-vclock">vclock()</a></tt>



Combine all VClocks in the input list into their least possible
common descendant.
<a name="prune-3"></a>


<h3>prune/3</h3>





<tt>prune(V::<a href="#type-vclock">vclock()</a>, Now::integer(), BucketProps::term()) -> <a href="#type-vclock">vclock()</a></tt>



Possibly shrink the size of a vclock, depending on current age and size.

_Generated by EDoc, Mar 23 2011, 15:59:44._