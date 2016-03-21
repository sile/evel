

# Module evel_name #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

An evel based global name registration facility.

<a name="types"></a>

## Data Types ##




### <a name="type-name">name()</a> ###


<pre><code>
name() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#register_name-2">register_name/2</a></td><td></td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td></td></tr><tr><td valign="top"><a href="#unregister_name-1">unregister_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#whereis_name-1">whereis_name/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="register_name-2"></a>

### register_name/2 ###

<pre><code>
register_name(Name::<a href="#type-name">name()</a>, Pid::pid()) -&gt; yes | no
</code></pre>
<br />

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Name::<a href="#type-name">name()</a>, Message::term()) -&gt; pid()
</code></pre>
<br />

<a name="unregister_name-1"></a>

### unregister_name/1 ###

<pre><code>
unregister_name(Name::<a href="#type-name">name()</a>) -&gt; ok
</code></pre>
<br />

<a name="whereis_name-1"></a>

### whereis_name/1 ###

<pre><code>
whereis_name(Name::<a href="#type-name">name()</a>) -&gt; pid() | undefined
</code></pre>
<br />

