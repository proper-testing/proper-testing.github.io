<!--[Erlang](http://www.erlang.org/) code:

    #!erlang
    prop_next_comb_produces_all_combinations_in_order() ->
        ?FORALL({Len, Colors},
                {range(0, 5), range(1, 10)},
                list_is_produced(Colors, lists:duplicate(Len, 0),
                                 all_combinations(Len,
                                                  lists:seq(0, Colors-1)))).

    list_is_produced(_Colors, done, []) ->
        true;
    list_is_produced(Colors, Same, [Same | Rest]) ->
        list_is_produced(Colors, next_comb(Colors, Same), Rest);
    list_is_produced(_Colors, _Comb, _List) ->
        false.

Erlang [shell](http://erlang.org/doc/man/erl.html):

    #!erl
    > lists:reverse([1,2,3]).
    [3,2,1]-->

[Erlang](http://www.erlang.org/) code (without line numbers):

    :::erlang
    prop_next_comb_produces_all_combinations_in_order() ->
        ?FORALL({Len, Colors},
                {range(0, 5), range(1, 10)},
                list_is_produced(Colors, lists:duplicate(Len, 0),
                                 all_combinations(Len,
                                                  lists:seq(0, Colors-1)))).

    list_is_produced(_Colors, done, []) ->
        true;
    list_is_produced(Colors, Same, [Same | Rest]) ->
        list_is_produced(Colors, next_comb(Colors, Same), Rest);
    list_is_produced(_Colors, _Comb, _List) ->
        false.

Erlang [shell](http://erlang.org/doc/man/erl.html) (without line numbers):

    :::erl
    > lists:reverse([1,2,3]).
    [3,2,1]

<!-- kate: replace-tabs-save on; replace-tabs on; tab-width 8; -->
