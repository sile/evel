

# Module evel #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

An Eventual Leader Election Library.

<a name="types"></a>

## Data Types ##




### <a name="type-candidate">candidate()</a> ###


<pre><code>
candidate() = pid()
</code></pre>




### <a name="type-certificate">certificate()</a> ###


<pre><code>
certificate() = pid()
</code></pre>




### <a name="type-dismiss_option">dismiss_option()</a> ###


<pre><code>
dismiss_option() = {unlink, boolean()}
</code></pre>




### <a name="type-elect_option">elect_option()</a> ###


<pre><code>
elect_option() = {priority, term()} | {link, boolean()} | <a href="#type-find_option">find_option()</a>
</code></pre>




### <a name="type-election_id">election_id()</a> ###


<pre><code>
election_id() = term()
</code></pre>




### <a name="type-find_option">find_option()</a> ###


<pre><code>
find_option() = {timeout, timeout()} | {voter_count, pos_integer()}
</code></pre>




### <a name="type-leader">leader()</a> ###


<pre><code>
leader() = {<a href="#type-winner">winner()</a>, <a href="#type-certificate">certificate()</a>}
</code></pre>




### <a name="type-winner">winner()</a> ###


<pre><code>
winner() = <a href="#type-candidate">candidate()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dismiss-1">dismiss/1</a></td><td>Equivalent to <a href="#dismiss-2"><tt>dismiss(Leader, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#dismiss-2">dismiss/2</a></td><td></td></tr><tr><td valign="top"><a href="#elect-2">elect/2</a></td><td>Equivalent to <a href="#elect-3"><tt>elect(ElectionId, Candidate, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#elect-3">elect/3</a></td><td></td></tr><tr><td valign="top"><a href="#find_leader-1">find_leader/1</a></td><td>Equivalent to <a href="#find_leader-2"><tt>find_leader(ElectionId, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#find_leader-2">find_leader/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_certificate-1">get_certificate/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_winner-1">get_winner/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_leader-1">is_leader/1</a></td><td></td></tr><tr><td valign="top"><a href="#known_leaders-0">known_leaders/0</a></td><td></td></tr></table>


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
elect(ElectionId::<a href="#type-election_id">election_id()</a>, Candidate::<a href="#type-candidate">candidate()</a>, Options::[<a href="#type-elect_option">elect_option()</a>]) -&gt; <a href="#type-leader">leader()</a>
</code></pre>
<br />

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

<a name="get_certificate-1"></a>

### get_certificate/1 ###

<pre><code>
get_certificate(Leader::<a href="#type-leader">leader()</a>) -&gt; <a href="#type-certificate">certificate()</a>
</code></pre>
<br />

<a name="get_winner-1"></a>

### get_winner/1 ###

<pre><code>
get_winner(Leader::<a href="#type-leader">leader()</a>) -&gt; <a href="#type-winner">winner()</a>
</code></pre>
<br />

<a name="is_leader-1"></a>

### is_leader/1 ###

<pre><code>
is_leader(X1::<a href="#type-leader">leader()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="known_leaders-0"></a>

### known_leaders/0 ###

<pre><code>
known_leaders() -&gt; [{<a href="#type-election_id">election_id()</a>, <a href="#type-leader">leader()</a>}]
</code></pre>
<br />

