---
layout: page
title: Frequently Asked Questions with PropEr Answers
sitemap:
    priority: 1.0
---

**PropEr's integration with the type language of Erlang is very nice
but I do not like that `list(atom())` and `[atom()]` have different
meaning when used as generators while they are aliases in the type
language of Erlang. This is confusing. Did you make this choice just
to be compatible with other QuickCheck tools for Erlang?**

Thanks for your comments! While compatibility with other QuickCheck
tools for Erlang was in the back of our minds once upon a time, in
fact this is not the main reason for this discrepancy. While it would
be nice to have the type and the generator language completely
identical, this small discrepancy allows the language of generators to
specify lists of specific lengths and/or types as generators (e.g.
`[atom()]` for a single atom list, `[atom(), integer()]` for a two
element list consisting of an atom and an integer, etc.). Expressing
lists of specific lengths is currently not possible in the type
language of Erlang where `list(T)` and `[T]` are just synonyms. So,
while we agree that this discrepancy is a bit confusing, it's occasionally
something that comes very handy when wanting specific list generators.

**Despite the truly awesome feature of PropEr that it can handle type
declarations, I don't know how to put constraints like "I don't want
binaries that contain bytes other than 0..128". Is there some way to
do this?**


There are constraints, such as the one you mention, that cannot be
expressed in Erlang's type language. However, you can easily express
such constraints using a custom generator:

{% highlight erlang%}
good_binary() ->
    ?LET(L, list(good_byte()), list_to_binary(L)).

good_byte() ->
    range(0, 128).
{% endhighlight %}


**Is there any way to force constraints on generated test cases (other than just checking them in the test case itself)?**

You can add any sort of constraint to a generator using a `?SUCHTHAT` macro:

{% highlight erlang %}
even_integer() ->
    ?SUCHTHAT(X, integer(), is_even(X)).

is_even(X) ->
    X rem 2 =:= 0.
{% endhighlight %}


But beware that PropEr cannnot magically produce the values that you want;
what will happen is that PropEr will keep trying random values one after the
other until it finds one that passes the test. If a large percentage of all
the possible base values do not pass the test, then a better strategy is to
write a custom generator:

{% highlight erlang %}
even_integer() ->
    ?LET(X, integer(), 2*X).
{% endhighlight %}

**I am writing some recursive generators but I am facing some problems. I am wondering, what is the PropEr way to deal with recursive types?**

The PropEr way to deal with recursive types is to delay writing your own generators for as long as possible. You can start by writing just the recursive type definitions and use them as a <i>for free</i> generator. If that proves not to be adequate for your application, you can try to globally limit the recursion depth by passing an appropriate value for the `max_size` option to the `proper:quickcheck` function. Only if even that is not enough for some reason should you try to resort to writing your own recursive generators using `?SIZED`, `?LAZY`, `frequency`, `resize` and friends. If you encounter any problem at any point in the above we would like to hear about it, preferably with a small test that exhibits the problem in an easily reproducible way.

**I'm writing an abstract state machine specification for a stateful system. I know that I should use the `next_state/3` callback to update the model state and that all data related to this callback can be either symbolic or dynamic. How can I extract specific parts of the result of an API call, if I am not allowed to pattern match?**

Information has to be extracted in a symbolic way, i.e. by performing a symbolic call. For example, suppose that the `Result` of a `Call` was a tuple with at least one element and that you wanted to use the first element, so as to update the model state. An attempt to extract it using `erlang:element/2` would return a wrong result (in this case, the atom `var` because `Result` is a symbolic variable `{var,N}` during command generation). Instead, assuming the model state is a record `#state{}` with a field named `foo`, you should write:

{% highlight erlang %}
next_state(S, Result, Call) ->
    S#state{foo = {call,erlang,element,[1, Result]}}.
{% endhighlight %}

PropEr will automatically evaluate the symbolic call during command execution, therefore no extra action needs to be taken.

**Shouldn't there be more questions here?**

Yes, please send us some!
