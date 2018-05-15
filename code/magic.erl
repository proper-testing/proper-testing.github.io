-module(magic).
-export([spells/0, cast_spell/2, cast_spells/2]).
-export([run_random/1, run_generated/1, run_handwritten/1, count_spells/1]).


-include_lib("proper/include/proper.hrl").

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

-type attr() :: #attr{}.
-type spell() :: attr().

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

%% no penalty for wrongly casted spells
-spec cast_spell(attr(), spell()) -> attr().
cast_spell(Attrs, Spell) ->
  NewAttrs = raw_cast_spell(Attrs, Spell),
  if
    NewAttrs#attr.strength < 0 -> Attrs;
    NewAttrs#attr.constitution < 0 -> Attrs;
    NewAttrs#attr.defense < 0 -> Attrs;
    NewAttrs#attr.dexterity < 0 -> Attrs;
    NewAttrs#attr.intelligence < 0 -> Attrs;
    NewAttrs#attr.charisma < 0 -> Attrs;
    NewAttrs#attr.wisdom < 0 -> Attrs;
    NewAttrs#attr.willpower < 0 -> Attrs;
    NewAttrs#attr.perception < 0 -> Attrs;
    NewAttrs#attr.luck < 0 -> Attrs;
    true -> NewAttrs
  end.

raw_cast_spell(Attrs, Spell) ->
  Attrs#attr{strength     = Attrs#attr.strength     + Spell#attr.strength,
             constitution = Attrs#attr.constitution + Spell#attr.constitution,
             defense      = Attrs#attr.defense      + Spell#attr.defense,
             dexterity    = Attrs#attr.dexterity    + Spell#attr.dexterity,
             intelligence = Attrs#attr.intelligence + Spell#attr.intelligence,
             charisma     = Attrs#attr.charisma     + Spell#attr.charisma,
             wisdom       = Attrs#attr.wisdom       + Spell#attr.wisdom,
             willpower    = Attrs#attr.willpower    + Spell#attr.willpower,
             perception   = Attrs#attr.perception   + Spell#attr.perception,
             luck         = Attrs#attr.luck         + Spell#attr.luck}.

cast_spells(Attrs, []) -> Attrs;
cast_spells(Attrs, [Spell | LeftSpells]) ->
  cast_spells(cast_spell(Attrs, Spell), LeftSpells).

%% Properties
%% ----------
sum_attr(Attrs) ->
  Attrs#attr.strength + Attrs#attr.constitution +
    Attrs#attr.defense + Attrs#attr.dexterity +
    Attrs#attr.intelligence + Attrs#attr.charisma +
    Attrs#attr.wisdom + Attrs#attr.willpower +
    Attrs#attr.perception + Attrs#attr.luck.

list_of_spells() ->
  list(proper_types:noshrink(oneof(spells()))).

prop_spells() ->
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
               BuffedAttr = cast_spells(InitialAttr, Spells),
               SumAttr = sum_attr(BuffedAttr),
               ?MAXIMIZE(SumAttr),
               ?WHENFAIL(io:format("Number of Spells: ~p~nTotal Attr: ~p~n",
                                   [length(Spells), SumAttr]),
                         SumAttr < 2 * sum_attr(InitialAttr))
             end).

list_of_spells_sa() ->
  #{first => list_of_spells(),
    next => list_of_spells_next()}.

list_of_spells_next() ->
  fun (Base, T) ->
      todo
      %% your code comes here %%
  end.
