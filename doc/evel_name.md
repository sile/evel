

# Module evel_name #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

An evel based global name registration facility.

<a name="description"></a>

## Description ##

This module is available as a `via` callback module for OTP behaviours (e.g., supervisor, gen_server, etc).

```
  %%
  %% Example
  %%
  gen_server:start_link({via, evel_name, SomeName}, ?MODULE, [], []).
```


<a name="types"></a>

## Data Types ##




### <a name="type-name">name()</a> ###


<pre><code>
name() = term()
</code></pre>

 A name of the process

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#register_name-2">register_name/2</a></td><td>Globally associates the name <code>Name</code> with a pid <code>Pid</code></td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Sends the message <code>Message</code> to the pid globally registered as <code>Name</code></td></tr><tr><td valign="top"><a href="#unregister_name-1">unregister_name/1</a></td><td>Removes the globally registered name <code>Name</code></td></tr><tr><td valign="top"><a href="#whereis_name-1">whereis_name/1</a></td><td>Returns the pid with the globally registered name <code>Name</code></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="register_name-2"></a>

### register_name/2 ###

<pre><code>
register_name(Name::<a href="#type-name">name()</a>, Pid::pid()) -&gt; yes | no
</code></pre>
<br />

Globally associates the name `Name` with a pid `Pid`

The function returns `yes` if successful, `no` if it fails.
For example, `no` is returned if an attempt is made to register a process with a name that is already in use.

Unlike `global:register_name/1`, this function allows a process to have multiple names.

If conlict arises between multiple processes during a registration, only one process survive.

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Name::<a href="#type-name">name()</a>, Message::term()) -&gt; pid()
</code></pre>
<br />

Sends the message `Message` to the pid globally registered as `Name`

Failure: If `Name` is not a registered name, the calling function will exit with reason `{badarg, {Name, Message}}`

<a name="unregister_name-1"></a>

### unregister_name/1 ###

<pre><code>
unregister_name(Name::<a href="#type-name">name()</a>) -&gt; ok
</code></pre>
<br />

Removes the globally registered name `Name`

<a name="whereis_name-1"></a>

### whereis_name/1 ###

<pre><code>
whereis_name(Name::<a href="#type-name">name()</a>) -&gt; pid() | undefined
</code></pre>
<br />

Returns the pid with the globally registered name `Name`

Returns `undefined` if the name is not registered.

