:- dynamic i_am_at/1, at/2, holding/1, knife_uses/1, spoon_uses/1, ventilation_open/0.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(knife_uses(_)), retractall(spoon_uses(_)).

/* Initial player location */
i_am_at(cage_center).

/* Paths within the cage */
path(cage_center, n, bed_area).
path(bed_area, s, cage_center).
path(cage_center, w, toilet).
path(toilet, e, cage_center).
path(cage_center, e, storage).
path(storage, w, cage_center).
path(cage_center, s, south_area).
path(south_area, n, cage_center).
path(storage, e, ventilation).
path(ventilation, w, storage).
path(toilet, w, sink).
path(sink, e, toilet).

/* Objects in each area */
at(paint, bed_area).
at(hair, bed_area).
at(paper, bed_area).
at(screwdriver, toilet).
at(spoon, storage).
at(knife, storage).
at(raincoat, south_area).
at(glue, south_area).
at(string, sink).
at(wire, sink).
at(cloth, sink).

/* Tool usage tracking */
knife_uses(0).
spoon_uses(0).

/* Movement rules */
go(Direction) :-
    clear_screen,
    i_am_at(Here),
    path(Here, Direction, There),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    !, look.

go(_) :-
    clear_screen,
    write('You can\'t go that way.'), nl.

/* Direction shortcuts */
n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).

/* Looking around */
look :-
    clear_screen,
    i_am_at(Place),
    describe(Place),
    nl,
    notice_objects_at(Place),
    nl,
    inventory,
    nl.

/* Listing objects in a location */
notice_objects_at(Place) :-
    at(X, Place),
    write('There is a '), write(X), write(' here.'), nl,
    fail.
notice_objects_at(_).

/* Taking objects */
take(X) :-
    clear_screen,
    holding(X),
    write('You\'re already holding it!'), nl, !.

take(X) :-
    clear_screen,
    i_am_at(Place),
    at(X, Place),
    retract(at(X, Place)),
    assert(holding(X)),
    write('You picked up the '), write(X), write('.'), nl, !.

take(_) :-
    clear_screen,
    write('That object is not here.'), nl.

/* Dropping objects */
drop(X) :-
    clear_screen,
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assert(at(X, Place)),
    write('You dropped the '), write(X), write('.'), nl, !.

drop(_) :-
    clear_screen,
    write('You aren\'t holding it!'), nl.

/* Crafting a mannequin */
make_mannequin :-
    holding(paint), holding(hair), holding(paper),
    retract(holding(paint)),
    retract(holding(hair)),
    retract(holding(paper)),
    assert(holding(mannequin)),
    write('You have created a mannequin!'), nl, !.
make_mannequin :-
    write('You don\'t have all the materials to make a mannequin.'), nl.

/* Placing the mannequin on the bed */
place_mannequin :-
    i_am_at(bed_area), holding(mannequin),
    retract(holding(mannequin)),
    assert(at(mannequin, bed_area)),
    write('You placed the mannequin on the bed.'), nl, !.
place_mannequin :-
    i_am_at(bed_area),
    write('You don\'t have a mannequin to place.'), nl, !.
place_mannequin :-
    write('You can only place the mannequin in the bed area.'), nl.

/* Making fake ventilation */
make_vent_mockup :-
    holding(string), holding(wire), holding(cloth),
    retract(holding(string)),
    retract(holding(wire)),
    retract(holding(cloth)),
    assert(holding(ventilation_mockup)),
    write('You crafted a fake ventilation cover!'), nl, !.
make_vent_mockup :-
    write('You need string, wire, and cloth to make a ventilation mockup.'), nl.

/* Placing fake ventilation */
place_vent_mockup :-
    i_am_at(ventilation),
    holding(ventilation_mockup),
    ventilation_open,
    retract(holding(ventilation_mockup)),
    assert(at(ventilation_mockup, ventilation)),
    write('You carefully place the fake ventilation cover in position. Looks real enough...'), nl, !.

place_vent_mockup :-
    i_am_at(ventilation),
    \+ holding(ventilation_mockup),
    write('You don\'t have the mockup with you.'), nl, !.

place_vent_mockup :-
    i_am_at(ventilation),
    \+ ventilation_open,
    write('You need to drill open the real ventilation before placing a fake one.'), nl, !.

place_vent_mockup :-
    write('You need to be at the ventilation to place the mockup.'), nl.

/* Drilling the ventilation */
drill :-
    i_am_at(ventilation),
    (holding(knife); holding(spoon)),

    (holding(knife), knife_uses(K), K < 3 ->
        retract(knife_uses(K)),
        K1 is K + 1,
        assert(knife_uses(K1))
    ; true),

    (holding(spoon), spoon_uses(S), S < 3 ->
        retract(spoon_uses(S)),
        S1 is S + 1,
        assert(spoon_uses(S1))
    ; true),

    (knife_uses(3), holding(knife) ->
        retract(holding(knife)),
        write('Your knife broke!'), nl
    ; true),

    (spoon_uses(3), holding(spoon) ->
        retract(holding(spoon)),
        write('Your spoon broke!'), nl
    ; true),

    (knife_uses(3), spoon_uses(3) ->
        ( \+ ventilation_open ->
            assert(ventilation_open),
            write('The ventilation is now open!'), nl
        ; true )
    ;
        write('You continue drilling...'), nl),

    !.

drill :-
    write('You need to be next to ventilation and have a knife or a spoon to drill.'), nl.

/* Display inventory */
inventory :-
    write('You are carrying: '), nl,
    holding(X),
    write('- '), write(X), nl,
    fail.
inventory :-
    write('Nothing.'), nl.

/* Clear screen */
clear_screen :-
    write('\e[H\e[2J').

/* Area descriptions */
describe(cage_center) :-
    write('You are in the middle of your cage, planning your escape. You can go to different areas to gather materials.'), nl.

describe(bed_area) :-
    write('You are at the bed area. There are materials here to build mannequins.'), nl.

describe(toilet) :-
    write('You are at the toilet. You see a screwdriver here.'), nl.

describe(storage) :-
    write('You are in the storage area. There are tools to loosen the ventilation.'), nl.

describe(south_area) :-
    write('You are in the southern area of the cage. Raincoats and glue are available to build a raft.'), nl.

describe(ventilation) :-
    write('You are next to the ventilation shaft. A possible way to escape!'), nl.

describe(sink) :-
    write('You are at the sink. You see old bits of string, wire, and some cloth—maybe you can fake a vent.'), nl.

/* Game instructions */
instructions :-
    clear_screen,
    write('ESCAPE THE CELL: YOUR MISSION'), nl,
    write('----------------------------------------------'), nl,
    write('You must escape through the ventilation shaft.'), nl,
    write('But first, you need to trick the guards...'), nl,
    nl,
    write('OBJECTIVE:'), nl,
    write('- Craft a mannequin (paint + hair + paper).'), nl,
    write('- Place it in the bed (bed_area).'), nl,
    write('- Drill open the real ventilation (ventilation).'), nl,
    write('- Craft a fake vent cover (string + wire + cloth).'), nl,
    write('- Place the fake vent where the real one was.'), nl,
    write('- Ensure you carry screwdriver, raincoat, and glue.'), nl,
    write('- Once all is ready, you may `can_escape.` to check if you are free!'), nl,
    nl,
    write('COMMANDS:'), nl,
    write('start.             -- to start the game.'), nl,
    write('n. s. e. w.        -- to move in that direction.'), nl,
    write('take(Object).      -- to pick up an object.'), nl,
    write('drop(Object).      -- to put down an object.'), nl,
    write('make_mannequin.    -- to craft a mannequin.'), nl,
    write('place_mannequin.   -- to place the mannequin on the bed.'), nl,
    write('make_vent_mockup.  -- to craft a fake ventilation cover.'), nl,
    write('place_vent_mockup. -- to place the fake vent.'), nl,
    write('drill.             -- to open the real ventilation.'), nl,
    write('inventory.         -- to see what you are carrying.'), nl,
    write('look.              -- to look around.'), nl,
    write('instructions.      -- to see this message again.'), nl,
    write('can_escape.        -- to check if you can escape.'), nl,
    write('halt.              -- to quit the game.'), nl,
    nl.

/* Start the game */
start :-
    instructions,
    look.

/* Escape logic */
can_escape :-
    at(mannequin, bed_area),
    at(ventilation_mockup, ventilation),
    ventilation_open,
    holding(screwdriver),
    holding(raincoat),
    holding(glue),
    write('All conditions met. You escape through the ventilation shaft into the night. You are free!'), nl, !.

can_escape :-
    write('You are not ready to escape yet. Make sure:'), nl,
    write('- Mannequin is on the bed.'), nl,
    write('- Ventilation is drilled open.'), nl,
    write('- Fake ventilation is placed.'), nl,
    write('- You are carrying screwdriver, raincoat, and glue.'), nl.
