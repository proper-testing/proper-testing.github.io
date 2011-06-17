Summary: Frequently Asked Questions with PropEr Answers
kate: replace-tabs-save on; replace-tabs on; tab-width 8;

**I'm writing an abstract state machine specification for a stateful system.
I know that I should use the `next_state/3` callback to update the model
state and that all data related to this callback can be either symbolic or
dynamic. How can I extract specific parts of the result of an API call, if
I am not allowed to pattern match?**

Information has to be extracted in a symbolic way, i.e. by performing a symbolic
call. For example, suppose that the `Result` of a `Call` was a tuple with at
least one element and that we wanted to use the first element, so as to update
the model state. An attempt to extract it using `erlang:element/2` would raise
an exception. Instead, assuming the model state is a record `#state{}` with a
field named `foo`, we should write:

    :::erlang
    next_state(S, Result, Call) ->
        S#state{foo = {call,erlang,element,[1, Result]}}.

PropEr will automatically evaluate the symbolic call during command
execution, therefore no extra action needs to be taken.

**Shouldn't there be more questions here?**

Yes, please send us some!
