

# Module evel #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

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




### <a name="type-election_id">election_id()</a> ###


<pre><code>
election_id() = term()
</code></pre>




### <a name="type-leader">leader()</a> ###


<pre><code>
leader() = {<a href="#type-candidate">candidate()</a>, <a href="#type-certificate">certificate()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dismiss-1">dismiss/1</a></td><td></td></tr><tr><td valign="top"><a href="#elect-2">elect/2</a></td><td></td></tr><tr><td valign="top"><a href="#find_leader-1">find_leader/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_certificate-1">get_certificate/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dismiss-1"></a>

### dismiss/1 ###

<pre><code>
dismiss(Leader::<a href="#type-leader">leader()</a>) -&gt; ok
</code></pre>
<br />

<a name="elect-2"></a>

### elect/2 ###

<pre><code>
elect(ElectionId::<a href="#type-election_id">election_id()</a>, Candidate::<a href="#type-candidate">candidate()</a>) -&gt; <a href="#type-leader">leader()</a>
</code></pre>
<br />

<a name="find_leader-1"></a>

### find_leader/1 ###

<pre><code>
find_leader(ElectionId::<a href="#type-election_id">election_id()</a>) -&gt; {ok, <a href="#type-leader">leader()</a>} | error
</code></pre>
<br />

<a name="get_certificate-1"></a>

### get_certificate/1 ###

<pre><code>
get_certificate(X1::<a href="#type-leader">leader()</a>) -&gt; <a href="#type-certificate">certificate()</a>
</code></pre>
<br />

