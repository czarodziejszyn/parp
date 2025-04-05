:- dynamic i_am_at/1, at/2, holding/1, knife_uses/1, spoon_uses/1, ventilation_open/0, in_shaft/0.
:- dynamic bar_screws/2, bar_removed/1.
:- dynamic on_roof/0, roof_stage/1.

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(knife_uses(_)), retractall(spoon_uses(_)), retractall(in_shaft).
:- retractall(bar_screws(_, _)), retractall(bar_removed(_)), retractall(on_roof), retractall(roof_stage(_)).

/* Initial player location */
i_am_at(cage_center).

/* Bar screw tracking */
bar_screws(shaft3, 0).
bar_screws(shaft6, 0).
bar_screws(shaft10, 0).

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

/* Ventilation shaft maze paths */
path(ventilation, n, shaft1) :- can_escape, \+ in_shaft.
path(shaft1, n, shaft2).
path(shaft2, e, shaft3).
path(shaft3, e, shaft4) :- bar_removed(shaft3).
path(shaft4, s, shaft5).
path(shaft5, s, shaft6).
path(shaft6, n, shaft7) :- bar_removed(shaft6).
path(shaft7, n, shaft8).
path(shaft8, n, shaft9).
path(shaft9, n, shaft10).
path(shaft10, s, shaft11) :- bar_removed(shaft10).
path(shaft11, w, shaft12).
path(shaft12, n, roof).

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

go(n) :-
    i_am_at(ventilation),
    can_escape,
    \+ in_shaft,
    retract(i_am_at(ventilation)),
    assert(i_am_at(shaft1)),
    assert(in_shaft),
    clear_screen,
    write('You squeeze into the ventilation shaft. It\'s tight and dark... use your shortcuts to escape'), nl,
    look, !.

/* Roof descent sequence */
go(s) :-
    on_roof,
    roof_stage(Stage),
    Stage < 6,
    NextStage is Stage + 1,
    retract(roof_stage(Stage)),
    assert(roof_stage(NextStage)),
    clear_screen,
    format('You carefully descend using the wires... (~w/6)~n', [NextStage]),
    (NextStage = 6 -> end_game ; true), !.

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

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.
notice_objects_at(_).

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

make_mannequin :-
        holding(paint), holding(hair), holding(paper),
        retract(holding(paint)),
        retract(holding(hair)),
        retract(holding(paper)),
        assert(holding(mannequin)),
        write('You have created a mannequin!'), nl, !.
make_mannequin :-
        write('You don\'t have all the materials to make a mannequin.'), nl.

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

make_vent_mockup :-
        holding(string), holding(wire), holding(cloth),
        retract(holding(string)),
        retract(holding(wire)),
        retract(holding(cloth)),
        assert(holding(ventilation_mockup)),
        write('You crafted a fake ventilation cover!'), nl, !.
make_vent_mockup :-
        write('You need string, wire, and cloth to make a ventilation mockup.'), nl.

place_vent_mockup :-
    i_am_at(ventilation),
    holding(ventilation_mockup),
    ventilation_open,
    retract(holding(ventilation_mockup)),
    assert(at(ventilation_mockup, ventilation)),
    write('You carefully place the fake ventilation cover in position. Looks real enough..., press n to escape'), nl, !.
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

drill :-
    i_am_at(ventilation),
    (holding(knife); holding(spoon)),
    (holding(knife), knife_uses(K), K < 3 -> retract(knife_uses(K)), K1 is K + 1, assert(knife_uses(K1)); true),
    (holding(spoon), spoon_uses(S), S < 3 -> retract(spoon_uses(S)), S1 is S + 1, assert(spoon_uses(S1)); true),
    (knife_uses(3), holding(knife) -> retract(holding(knife)), write('Your knife broke!'), nl; true),
    (spoon_uses(3), holding(spoon) -> retract(holding(spoon)), write('Your spoon broke!'), nl; true),
    (knife_uses(3), spoon_uses(3) ->
        (ventilation_open -> true ; assert(ventilation_open)),
        write('The ventilation is now open!'), nl
    ; write('You continue drilling...'), nl), !.
drill :-
    write('You need to be next to ventilation and have a knife or a spoon to drill.'), nl.

unscrew :-
    i_am_at(Location),
    member(Location, [shaft3, shaft6, shaft10]),
    holding(screwdriver),
    bar_screws(Location, Count),
    Count < 4,
    NewCount is Count + 1,
    retract(bar_screws(Location, Count)),
    assert(bar_screws(Location, NewCount)),
    ( NewCount = 4 ->
        assert(bar_removed(Location)),
        write('You removed the final screw. The bar at this shaft is now open!'), nl
    ;
        write('You removed a screw. '), write(4 - NewCount), write(' left.'), nl
    ), !.
unscrew :-
    i_am_at(Location),
    member(Location, [shaft3, shaft6, shaft10]),
    \+ holding(screwdriver),
    write('You need a screwdriver to remove these screws.'), nl, !.
unscrew :-
    i_am_at(Location),
    member(Location, [shaft3, shaft6, shaft10]),
    bar_removed(Location),
    write('The bar here is already removed.'), nl, !.
unscrew :-
    write('There are no bars to unscrew here.'), nl.

inventory :-
        write('You are carrying: '), nl,
        holding(X),
        write('- '), write(X), nl,
        fail.
inventory :-
        write('Nothing.'), nl.

clear_screen :-
        write('\e[H\e[2J').

describe(cage_center) :- write('You are in the middle of your cage, planning your escape.'), nl.
describe(bed_area) :- write('You are at the bed area. Materials here might help build a manequinn.'), nl.
describe(toilet) :- write('You are at the toilet. You see a screwdriver here.'), nl.
describe(storage) :- write('You are in the storage area. Some tools are here.'), nl.
describe(south_area) :- write('You are in the southern area. Raincoats and glue are here.'), nl.
describe(ventilation) :- write('You are next to the ventilation shaft. A possible escape route!'), nl.
describe(sink) :- write('You are at the sink. You see string, wire, and cloth.'), nl.

describe(shaft1) :- write('You are crawling through a narrow vent. It bends up ahead.'), nl.
describe(shaft2) :- write('A tighter section. You can only move forward.'), nl.
describe(shaft3) :- ( bar_removed(shaft3) -> write('The shaft turns east. The metal bar has been removed.'), nl
                    ; write('The shaft turns east, but a metal bar blocks your way.'), nl).
describe(shaft4) :- write('You keep crawling, it dips downward.'), nl.
describe(shaft5) :- write('Still descending...'), nl.
describe(shaft6) :- ( bar_removed(shaft6) -> write('A brief climb begins. The bar has been removed.'), nl
                    ; write('A brief climb begins, but a metal bar blocks your path.'), nl).
describe(shaft7) :- write('Claustrophobic. Keep moving.'), nl.
describe(shaft8) :- write('You hear something above. Almost there?'), nl.
describe(shaft9) :- write('It smells like fresh air...'), nl.
describe(shaft10) :- ( bar_removed(shaft10) -> write('You slip a little downward. The bar is gone.'), nl
                    ; write('You slip a little downward, but a metal bar blocks the way.'), nl).
describe(shaft11) :- write('It turns west. Must be near the end.'), nl.
describe(shaft12) :- write('There\'s light ahead!'), nl.
describe(roof) :-
    \+ on_roof ->
        assert(on_roof),
        assert(roof_stage(0)),
        write('You made it to the roof! You can see the wires leading down the wall.'), nl,
        write('Carefully, you begin to climb down... Press "s." repeatedly to descend.'), nl
    ; write('You are on the roof. Climb down the wires using "s." (~w/6)', [roof_stage(_)]), nl.

can_escape :-
    at(mannequin, bed_area),
    ventilation_open,
    at(ventilation_mockup, ventilation),
    holding(screwdriver),
    holding(raincoat),
    holding(glue).

end_game :-
    write('You reached the ground safely. You are free!'), nl,
    write('🎉 Congratulations — You Escaped the Prison! 🎉'), nl,
    halt.

instructions :-
        clear_screen,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n. s. e. w.        -- to move in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('make_mannequin.    -- to craft a mannequin.'), nl,
        write('place_mannequin.   -- to place the mannequin on the bed.'), nl,
        write('make_vent_mockup.  -- to craft a fake ventilation cover.'), nl,
        write('place_vent_mockup. -- to place the fake vent cover.'), nl,
        write('drill.             -- to drill the real ventilation.'), nl,
        write('unscrew.           -- to remove a screw from a barred shaft.'), nl,
        write('look.              -- to look around you.'), nl,
        write('inventory.         -- to see what you are carrying.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game.'), nl, nl.

start :-
        instructions,
        look.
