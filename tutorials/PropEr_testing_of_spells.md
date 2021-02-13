---
layout: page
title: Advanced tutorial for targeted property-based testing
author: Andreas Löscher and Kostis Sagonas
sitemap:
priority: 1.0
---

## Neighborhood Functions
This task is about writing a neighborhood function, a component needed for
the search strategy simulated annealing. The neighborhood function is
critical for the performance of simulated annealing.

### Magic
Let's say we want to design a role-play game (RPG) where our character has some
attributes like strength and intelligence
(see [Wikipedia](https://en.wikipedia.org/wiki/Attribute_(role-playing_games)))
that effect how good actions are performed. A strong character will typically
be better at fighting and an intelligent character will be better at using
magic. Furthermore we want our RPG to have a spell-based system that allows us
to customize these these attributes. The player can cast a spell that will
reduce some of these attributes to increase others.

The stats can be implemented as a record:

{% highlight erlang %}
-record(attr, {strength     = 0 :: integer(),
               constitution = 0 :: integer(),
               defense      = 0 :: integer(),
               dexterity    = 0 :: integer(),
               intelligence = 0 :: integer(),
               charisma     = 0 :: integer(),
               wisdom       = 0 :: integer(),
               willpower    = 0 :: integer(),
               perception   = 0 :: integer(),
               luck         = 0 :: integer()}).
{% endhighlight %}

Spells are represented with the same record and the record fields specifies how
much an attribute goes up or down when the spell is cast.

{% highlight erlang %}
-type attr() :: #attr{}.
-type spell() :: attr().
{% endhighlight %}

Our spell attribute API contains only one function `cast_spells(Attrs, Spell)`
that calculates the resulting attributes if a list of spells is cast. It can
happen that a spell in the list cannot be casts because the character does not
have the required attributes left. In such a case the spell does not have any
effect.

Now we only need some spells:

{% highlight erlang %}
-spec spells() -> list(spell()).
spells() ->
  [#attr{strength = 5, constitution = -2, dexterity = -3},
   #attr{defense = 4, willpower = -4},
   #attr{intelligence = -3, charisma = 1,
         wisdom = 1,  perception = 1},
   #attr{strength = 1,  constitution = 2,
         defense = 2, willpower = -3},
   #attr{intelligence = 1,  charisma = 1,
         wisdom = 1,  willpower = -3},
   #attr{strength = 1, intelligence = -3,
         charisma = 0, luck = 2},
   #attr{intelligence = 1,  charisma = 1,  wisdom = 1,
         willpower = -4, perception = 1},
   #attr{charisma = 2, perception = 1,  luck = -3},
   #attr{strength = 2,  constitution = -2},
   #attr{constitution = 2,  defense = -2},
   #attr{defense = 2,  dexterity = -2},
   #attr{dexterity = 2,  intelligence = -2},
   #attr{strength = -1, constitution = -1, defense = -1,
         dexterity = -1, intelligence = -1, charisma = -1,
         wisdom = -1, willpower = 10, perception = -1, luck = -1},
   #attr{intelligence = 2,  charisma = -2},
   #attr{charisma = 2,  wisdom = -2},
   #attr{wisdom = 2,  willpower = -2},
   #attr{willpower = 2,  perception = -2},
   #attr{perception = 2,  luck = -2},
   #attr{strength = -2, luck = 2},
   #attr{strength = 5, luck = -8}].
{% endhighlight %}

### PropEr Spells
With the spells in place, how can we make sure, that a player cannot
exploit our spell system and get lets say an twice the amount of attributes?
We can specify a property to test for this type of exploit as follows:

{% highlight erlang %}
prop_spells() ->
  ?FORALL(Spells, list_of_spells(),
          begin
            InitialAttr = #attr{strength     = 5,
                                constitution = 5,
                                defense      = 5,
                                dexterity    = 5,
                                intelligence = 5,
                                charisma     = 5,
                                wisdom       = 5,
                                willpower    = 5,
                                perception   = 5,
                                luck         = 5},
            BuffedAttr = cast_spells(InitialAttr, Spells),
            SumAttr = sum_attr(BuffedAttr),
            ?WHENFAIL(io:format("Number of Spells: ~p~nTotal Attr: ~p~n",
                                [length(Spells), SumAttr]),
                      SumAttr < 2 * sum_attr(InitialAttr))
          end).
{% endhighlight %}

We define some initial attributes and then cast a random list of spells.
Then we calculate the total number of attributes with `sum_attr(Attrs)`
and check if the spells somehow managed to double them compared to the initial
attributes. The generator `list_of_spells()` looks like this:

{% highlight erlang %}
list_of_spells() ->
  list(proper_types:noshrink(oneof(spells()))).
{% endhighlight %}

Note that we don't want PropEr to shrink the values inside the spell records.
We can prevent that by wrapping the generator with `proper_types:noshrink()`.

If we test this property now with PropEr, we will see that the property holds
for all randomly generated inputs even if the number of tests is very high:

{% highlight erlang %}
1> proper:quickcheck(magic:prop_spells(), 100000).
.... 100000 dots ...
OK: Passed 100000 test(s).
true
{% endhighlight %}

Unfortunately there is are list of spells but PropEr is unable to find it by
using the `list_of_spells()` generator. To find them we want to use PropEr's
targeted PBT extension.

### Task
We can change `prop_spells()` to make use of the search strategy as follows:

{% highlight erlang %}
prop_spells_targeted() ->
  ?FORALL_SA(Spells, ?TARGET(list_of_spells_sa()),
             begin
               InitialAttr = #attr{strength     = 5,
                                   constitution = 5,
                                   defense      = 5,
                                   dexterity    = 5,
                                   intelligence = 5,
                                   charisma     = 5,
                                   wisdom       = 5,
                                   willpower    = 5,
                                   perception   = 5,
                                   luck         = 5},
               BuffedAttr = cast_spells_penalty(InitialAttr, Spells),
               SumAttr = sum_attr(BuffedAttr),
               ?MAXIMIZE(SumAttr),
               ?WHENFAIL(io:format("Number of Spells: ~p~nTotal Attr: ~p~n",
                                   [length(Spells), SumAttr]),
                         SumAttr < 2 * sum_attr(InitialAttr))
             end).
{% endhighlight %}

We added the following elements:

* `FORALL_SA` specifies simulated annealing as search strategy
* `?TARGET` tells the strategy that it should generate input according to `list_of_spells_sa()`
* `?MAXIMIZE` specifies the search goal. In this case we want to maximize the attributes our character has after casting the spells.

Now we just need to tell simulated annealing how to generate the first element
and which neighborhood function it should use:

{% highlight erlang %}
list_of_spells_sa() ->
  #{first => list_of_spells(),
    next => list_of_spells_next()}.
{% endhighlight %}

We can use the random generator `list_of_spells()` for the first element.

The task is to implement `list_of_spells_next()` so that it returns a
neighborhood function. When writing the function you can make full use of
PropEr's language for defining custom generators:

{% highlight erlang %}
list_of_spells_next() ->
  fun (OldSpells, Temperature) ->
    ?LET(SomeInteger, integer(), ...)
  end.
{% endhighlight %}

The first element of the neighborhood function is the base input. The
neighborhood function should generate a new instance of the input that is
similar to this base input (in the neighborhood of). The second parameter is the
temperature. This is a `float()` parameter that typically decreases during
search
(see [Simulated Annealing](https://en.wikipedia.org/wiki/Simulated_annealing)).

It is possible to use the temperature to scale the size of the neighborhood in
which the neighborhood function generates new input:

* If the neighborhood is very large the input space can be traversed fast (global search), but the search might degrade to random testing. With a large neighborhood it can also take many steps to narrow down to a good input that is otherwise close in the input space of an already accepted input (local search).
* If the neighborhood is very small, the search can usually narrow down fast input that is close. However, it might require many steps to reach other parts of the input space or to escape a local optima.

The temperature makes it possible to reduce the size of the neighborhood during
the search, transitioning from global search to local search. This can
_sometimes_ be useful. In many cases it is sufficient to keep the neighborhood
size constant.

Some remarks:

* All of the input space must be reachable by consecutive calls of `list_of_spells_next()`. This means that lists should be able to grow and shrink in size.
* Use the temperature argument only if necessary.
* Even with targeted PBT the amount of tests needed can be in the few thousands but a counterexample should be found in under `10000` tests depending on your neighborhood function.
