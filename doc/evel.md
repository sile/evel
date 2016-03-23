

# Module evel #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

An Eventual Leader Election Library.

<a name="description"></a>

## Description ##

This module provides functionality to elect the leader
which will be eventually agreed by all member of the same distributed erlang cluster.

```
  %%
  %% Elects the leader
  %%
  > Leader = evel:elect(foo, self()).
  %% Finds the leader of an election
  > {ok, Leader} = evel:find_leader(foo).
  > error = evel:find_leader(bar).
  %%
  %% Dismisses the leader
  %%
  > ok = evel:dismiss(foo).
  > error = evel:find_leader(foo).
```


<a name="types"></a>

## Data Types ##




### <a name="type-candidate">candidate()</a> ###


<pre><code>
candidate() = pid()
</code></pre>

 A candidate of an election.



### <a name="type-certificate">certificate()</a> ###


<pre><code>
certificate() = pid()
</code></pre>

 The certificate to gurantee the legitimacy of a leader.

If the certificate process is down, the corresponding candidate is no longer a leader.



### <a name="type-dismiss_option">dismiss_option()</a> ###


<pre><code>
dismiss_option() = {unlink, boolean()} | {async, boolean()}
</code></pre>

 unlink:
- If there is one, it removes the link between the candidate and the corresponding certificate process.
- Thus, the candidate process can survive after the dismissal.
- The default value is `false`.

async:
- If the value is `true`, the dismissal is processed by an asynchronous manner.
- The default value is `false`.



### <a name="type-elect_option">elect_option()</a> ###


<pre><code>
elect_option() = {priority, term()} | {link, boolean()} | <a href="#type-find_option">find_option()</a>
</code></pre>

 priority:
- The priority of the candidate.
- The smaller value means a higher priority.
- If conflict arises between multiple candidates in the same election, the highest priority one is eventually elected.
- The default value is `erlang:system_time(micro_seconds)`.

link:
- If the value is `true`, the candidate process and the certificate process will be linked.
- The default value is `true`.



### <a name="type-election_id">election_id()</a> ###


<pre><code>
election_id() = term()
</code></pre>

 The identifier of an election.
In each election, only one leader is elected.



### <a name="type-find_option">find_option()</a> ###


<pre><code>
find_option() = {timeout, timeout()} | {voter_count, pos_integer()}
</code></pre>

 timeout:
- If some voter do not respond in the period, their votes are ignored.
- The default value is `100`.

voter_count:
- The number of voters which vote for the election.
- The larger value is more tolerant to node failures and cluster member changes but more overhead occurs.
- Usually, but it is not mandatory, the same value in both [`elect/2`](#elect-2) and [`find_leader/2`](#find_leader-2) will be specified in the same election.
- The default value is `5`.



### <a name="type-leader">leader()</a> ###


<pre><code>
leader() = {<a href="#type-winner">winner()</a>, <a href="#type-certificate">certificate()</a>}
</code></pre>

 A candidate which wins the electoin and is certified as the leader.



### <a name="type-winner">winner()</a> ###


<pre><code>
winner() = <a href="#type-candidate">candidate()</a>
</code></pre>

 The winner of an election.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dismiss-1">dismiss/1</a></td><td>Equivalent to <a href="#dismiss-2"><tt>dismiss(Leader, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#dismiss-2">dismiss/2</a></td><td>Dismisses the leader.</td></tr><tr><td valign="top"><a href="#elect-2">elect/2</a></td><td>Equivalent to <a href="#elect-3"><tt>elect(ElectionId, Candidate, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#elect-3">elect/3</a></td><td>Elects the leader in the election.</td></tr><tr><td valign="top"><a href="#find_leader-1">find_leader/1</a></td><td>Equivalent to <a href="#find_leader-2"><tt>find_leader(ElectionId, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#find_leader-2">find_leader/2</a></td><td>Finds the leader elected in the election.</td></tr><tr><td valign="top"><a href="#get_certificate-1">get_certificate/1</a></td><td>Gets the certificate part of <code>Leader</code></td></tr><tr><td valign="top"><a href="#get_winner-1">get_winner/1</a></td><td>Gets the winner part of <code>Leader</code></td></tr><tr><td valign="top"><a href="#is_leader-1">is_leader/1</a></td><td>Returns <code>true</code> if <code>X</code> is a <code>leader()</code>, otherwise <code>false</code></td></tr><tr><td valign="top"><a href="#known_leaders-0">known_leaders/0</a></td><td>Returns a list of locally known leaders.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dismiss-1"></a>

### dismiss/1 ###

<pre><code>
dismiss(Leader::<a href="#type-leader">leader()</a>) -&gt; ok
</code></pre>
<br />

Equivalent to [`dismiss(Leader, [])`](#dismiss-2).

<a name="dismiss-2"></a>

### dismiss/2 ###

<pre><code>
dismiss(Leader::<a href="#type-leader">leader()</a>, Options::[<a href="#type-dismiss_option">dismiss_option()</a>]) -&gt; ok
</code></pre>
<br />

Dismisses the leader

It kills (i.e., `exit(Pid, kill)`) the corresponding certificate process.
As a result, the candidate process may exit if it have linked to the certificate process.

<a name="elect-2"></a>

### elect/2 ###

<pre><code>
elect(ElectionId::<a href="#type-election_id">election_id()</a>, Candidate::<a href="#type-candidate">candidate()</a>) -&gt; <a href="#type-leader">leader()</a>
</code></pre>
<br />

Equivalent to [`elect(ElectionId, Candidate, [])`](#elect-3).

<a name="elect-3"></a>

### elect/3 ###

<pre><code>
elect(ElectionId::<a href="#type-election_id">election_id()</a>, Candidate::<a href="#type-candidate">candidate()</a>, Options::[<a href="#type-elect_option">elect_option()</a>]) -&gt; Leader::<a href="#type-leader">leader()</a>
</code></pre>
<br />

Elects the leader in the election

If a leader have already been elected, it returns the leader.

If conflict arises between multiple candidates in the same election,
the highest priority one is eventually elected (i.e., the leader is agreed by all member of the same erlang cluster).

Point to notice is that temporary coexist of multiple leaders is not prohibited.
Some leaders will be eventually dismissed except highest priority one.

If you are interested in the expiration of the term of office (or the dismissal) of a leader,
please you monitor the certificate process (i.e., `monitor(process, evel:get_certificate(Leader))`).
The down of the certificate process indicates the retirement of the leader.

<a name="find_leader-1"></a>

### find_leader/1 ###

<pre><code>
find_leader(ElectionId::<a href="#type-election_id">election_id()</a>) -&gt; {ok, <a href="#type-leader">leader()</a>} | error
</code></pre>
<br />

Equivalent to [`find_leader(ElectionId, [])`](#find_leader-2).

<a name="find_leader-2"></a>

### find_leader/2 ###

<pre><code>
find_leader(ElectionId::<a href="#type-election_id">election_id()</a>, Options::[<a href="#type-find_option">find_option()</a>]) -&gt; {ok, <a href="#type-leader">leader()</a>} | error
</code></pre>
<br />

Finds the leader elected in the election

<a name="get_certificate-1"></a>

### get_certificate/1 ###

<pre><code>
get_certificate(Leader::<a href="#type-leader">leader()</a>) -&gt; <a href="#type-certificate">certificate()</a>
</code></pre>
<br />

Gets the certificate part of `Leader`

<a name="get_winner-1"></a>

### get_winner/1 ###

<pre><code>
get_winner(Leader::<a href="#type-leader">leader()</a>) -&gt; <a href="#type-winner">winner()</a>
</code></pre>
<br />

Gets the winner part of `Leader`

<a name="is_leader-1"></a>

### is_leader/1 ###

<pre><code>
is_leader(X::<a href="#type-leader">leader()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a `leader()`, otherwise `false`

<a name="known_leaders-0"></a>

### known_leaders/0 ###

<pre><code>
known_leaders() -&gt; [{<a href="#type-election_id">election_id()</a>, <a href="#type-leader">leader()</a>}]
</code></pre>
<br />

Returns a list of locally known leaders

