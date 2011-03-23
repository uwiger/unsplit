

<h1>The unsplit application</h1>

unsplit - A framework for resolving Mnesia netsplits

=====================================================
.

Copyright © 2010 Erlang Solutions Ltd.

__Version:__ 0.5



Unsplit is a framework for registering merge functions that will be 
called any time mnesia tries to heal from netsplits.



The default behaviour of mnesia is not to attempt automatic merge after
a 'partitioned network' event. It detects and reports  the condition, but 
leaves it up to the user to resolve the problem.



Mnesia itself offers a few remedies: restart from backup, or elect to 
unconditionally reload tables from one or more nodes - in both cases
data loss is very likely.



Unsplit starts a subscription on the 'partitioned network' event, and 
forces Mnesia to merge the "islands" that have been separated. It inserts
itself into the schema merge transaction, claiming table locks on all
affected tables. It then runs user-provided merge callbacks for each 
table, fetching data from one side, comparing the objects, and writing
back the data that should be kept.



Writing an Unsplit method
=========================



Unsplit methods are table-specific, although a default method can be set
using the `unsplit` application environment variable 



`{default_method, {Module, Function, ExtraArgs}}`



The given method is called with the following arguments:



`apply(Module, Function, [init, Table, Attributes | ExtraArgs]) -> ret()`



to set up the merge. Then, data will be fetched using a given fetch strategy,
and the fetched data will be handed to the merge function as:



`apply(Module, Function, [data(), state()]) -> ret()`



The return value, `ret()` is defined as:


<pre>
ret() :: stop
| {ok, state()}
| {ok, actions(), state()}
| {ok, actions(), strategy(), state()}

actions() :: [action()]

action() :: {write, Objects} | {delete, Objects}

strategy() :: all_keys | {ModS, FunS}
</pre>




If a custom fetch strategy function is given, it will be called as



`apply(ModS, FunS, [Table, RemoteNode, state()]) -> {ok, data(), state()}`



Note that `state()` is whatever the merge function creates. The unsplit 
framework treats it as an opaque object, but the fetch function needs to
do the same, or be aware of its definition.



The format of `data()` can be anything that the merge function accepts,
but if the built-in strategy `all_keys` is used, it will have the format:



`data() :: [{[object()], [object()]}]`

where each 2-tuple represents the data matching a given key on each
side of the split. Thus, `[{[{mytab, 1, a}], []}]` would mean that the 
object `{mytab, 1, a}` only exists on the side where the merge process is
running, but is not found on the other side. `[{[{mytab,2,a}], [{mytab,2,b}]}]`
would mean that conflicting versions of the object `{mytab,2}` were found.


<h2 class="indextitle">Modules</h2>



<table width="100%" border="0" summary="list of modules">
<tr><td><a href="unsplit.md" class="module">unsplit</a></td></tr>
<tr><td><a href="unsplit_lib.md" class="module">unsplit_lib</a></td></tr>
<tr><td><a href="unsplit_reporter.md" class="module">unsplit_reporter</a></td></tr>
<tr><td><a href="unsplit_server.md" class="module">unsplit_server</a></td></tr>
<tr><td><a href="unsplit_vclock.md" class="module">unsplit_vclock</a></td></tr></table>

