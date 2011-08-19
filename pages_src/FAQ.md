Summary: Frequently Asked Questions with PropEr Answers
kate: replace-tabs-save on; replace-tabs on; tab-width 8;

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

**I'm writing an abstract state machine specification for a stateful system.
I know that I should use the `next_state/3` callback to update the model
state and that all data related to this callback can be either symbolic or
dynamic. How can I extract specific parts of the result of an API call, if
I am not allowed to pattern match?**

Information has to be extracted in a symbolic way, i.e. by performing a symbolic
call. For example, suppose that the `Result` of a `Call` was a tuple with at
least one element and that you wanted to use the first element, so as to update
the model state. An attempt to extract it using `erlang:element/2` would return
a wrong result (in this case, the atom `var` because `Result` is a symbolic
variable `{var,N}` during command generation).
Instead, assuming the model state is a record `#state{}` with a field named
`foo`, you should write:

    :::erlang
    next_state(S, Result, Call) ->
        S#state{foo = {call,erlang,element,[1, Result]}}.

PropEr will automatically evaluate the symbolic call during command execution,
therefore no extra action needs to be taken.

**Shouldn't there be more questions here?**

Yes, please send us some!
