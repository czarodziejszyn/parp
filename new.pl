:- dynamic i_am_at/1, at/2, holding/1, knife_uses/1, spoon_uses/1, ventilation_open/0, in_shaft/0.
:- dynamic bar_screws/2, bar_removed/1.

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(knife_uses(_)), retractall(spoon_uses(_)), retractall(in_shaft).
:- retractall(bar_screws(_, _)), retractall(bar_removed(_)), retractall(ventilation_open).

/* Lokalizacja startowa gracza */
i_am_at(cage_center).

/* Liczba śrub w kratkach */
bar_screws(shaft3, 0).
bar_screws(shaft6, 0).
bar_screws(shaft10, 0).

/* Ścieżki w celi */
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

/* Tunel wentylacyjny */
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

/* Etapy zejścia z dachu */
path(roof, s, descent1).
path(descent1, s, descent2).
path(descent2, s, descent3).
path(descent3, s, descent4).
path(descent4, s, descent5).
path(descent5, s, ground).

/* Przedmioty */
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

/* Narzędzia */
knife_uses(0).
spoon_uses(0).

/* Poruszanie się */
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
    write('Wciskasz się do szybu wentylacyjnego. Jest ciasno i ciemno... użyj skrótów, by uciec.'), nl,
    look, !.

go(_) :-
    clear_screen,
    write('Nie możesz tam iść.'), nl.

/* Skróty */
n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).

look :-
        clear_screen,
        i_am_at(Place),
        describe(Place), nl,
        notice_objects_at(Place), nl,
        inventory, nl.

notice_objects_at(Place) :-
        at(X, Place),
        write('Widzisz tutaj: '), write(X), write('.'), nl,
        fail.
notice_objects_at(_).

take(X) :-
        clear_screen,
        holding(X),
        write('Już to masz przy sobie!'), nl, !.

take(X) :-
        clear_screen,
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('Podniosłeś: '), write(X), write('.'), nl, !.

take(_) :-
        clear_screen,
        write('Tutaj nie ma takiego przedmiotu.'), nl.

drop(X) :-
        clear_screen,
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('Upuściłeś: '), write(X), write('.'), nl, !.

drop(_) :-
        clear_screen,
        write('Nie masz tego przy sobie.'), nl.

make_mannequin :-
        holding(paint), holding(hair), holding(paper),
        retract(holding(paint)),
        retract(holding(hair)),
        retract(holding(paper)),
        assert(holding(mannequin)),
        write('Stworzyłeś manekina!'), nl, !.
make_mannequin :-
        write('Brakuje Ci materiałów, by stworzyć manekina.'), nl.

place_mannequin :-
        i_am_at(bed_area), holding(mannequin),
        retract(holding(mannequin)),
        assert(at(mannequin, bed_area)),
        write('Położyłeś manekina na łóżku.'), nl, !.
place_mannequin :-
        i_am_at(bed_area),
        write('Nie masz manekina, żeby go położyć.'), nl, !.
place_mannequin :-
        write('Manekina można położyć tylko w miejscu łóżka.'), nl.

make_vent_mockup :-
        holding(string), holding(wire), holding(cloth),
        retract(holding(string)),
        retract(holding(wire)),
        retract(holding(cloth)),
        assert(holding(ventilation_mockup)),
        write('Stworzyłeś atrapę wentylacji!'), nl, !.
make_vent_mockup :-
        write('Potrzebujesz sznurka, drutu i materiału, by zrobić atrapę.'), nl.

place_vent_mockup :-
    i_am_at(ventilation),
    holding(ventilation_mockup),
    ventilation_open,
    retract(holding(ventilation_mockup)),
    assert(at(ventilation_mockup, ventilation)),
    write('Umieściłeś atrapę wentylacji. Wygląda całkiem realistycznie... wpisz "n", aby uciec.'), nl, !.
place_vent_mockup :-
    i_am_at(ventilation),
    \+ holding(ventilation_mockup),
    write('Nie masz atrapy przy sobie.'), nl, !.
place_vent_mockup :-
    i_am_at(ventilation),
    \+ ventilation_open,
    write('Musisz najpierw rozwiercić prawdziwą wentylację.'), nl, !.
place_vent_mockup :-
    write('Musisz być przy wentylacji, aby umieścić atrapę.'), nl.

drill :-
    i_am_at(ventilation),
    (holding(knife); holding(spoon)),
    (holding(knife), knife_uses(K), K < 3 -> retract(knife_uses(K)), K1 is K + 1, assert(knife_uses(K1)); true),
    (holding(spoon), spoon_uses(S), S < 3 -> retract(spoon_uses(S)), S1 is S + 1, assert(spoon_uses(S1)); true),
    (knife_uses(3), holding(knife) -> retract(holding(knife)), write('Nóż się złamał!'), nl; true),
    (spoon_uses(3), holding(spoon) -> retract(holding(spoon)), write('Łyżka się złamała!'), nl; true),
    (knife_uses(3), spoon_uses(3) ->
        (ventilation_open -> true ; assert(ventilation_open)),
        write('Wentylacja została otwarta!'), nl
    ; write('Wciąż wiercisz...'), nl), !.
drill :-
    write('Potrzebujesz noża lub łyżki i musisz być przy wentylacji.'), nl.

unscrew :-
    i_am_at(Location),
    member(Location, [shaft3, shaft6, shaft10]),
    holding(screwdriver),
    bar_screws(Location, Count),
    Count < 4,
    NewCount is Count + 1,
    retract(bar_screws(Location, Count)),
    assert(bar_screws(Location, NewCount)),
    (NewCount = 4 ->
        assert(bar_removed(Location)),
        write('Odkręciłeś ostatnią śrubę. Kratka jest usunięta!'), nl
    ;
        write('Odkręciłeś śrubę. Zostało: '), write(4 - NewCount), nl
    ), !.
unscrew :-
    i_am_at(Location),
    member(Location, [shaft3, shaft6, shaft10]),
    \+ holding(screwdriver),
    write('Potrzebujesz śrubokręta.'), nl, !.
unscrew :-
    i_am_at(Location),
    member(Location, [shaft3, shaft6, shaft10]),
    bar_removed(Location),
    write('Kratka już jest usunięta.'), nl, !.
unscrew :-
    write('Nie ma tu kratki do odkręcenia.'), nl.

inventory :-
        write('Masz przy sobie: '), nl,
        holding(X),
        write('- '), write(X), nl,
        fail.
inventory :-
        write('Na razie nic nie podniosłeś.'), nl.

clear_screen :-
        write('\e[H\e[2J').

describe(cage_center) :- write('Jesteś w centrum swojej celi. Xbierz ekwipunek do ucieczki.'), nl.
describe(bed_area) :- write('Strefa łóżkowa. Może znajdziesz tu coś, z czego zrobisz manekina.'), nl.
describe(toilet) :- write('Jesteś przy toalecie. Widzisz śrubokręt.'), nl.
describe(storage) :- write('Magazynek. Znajdziesz tu narzędzia.'), nl.
describe(south_area) :- write('Południowy zakątek. Są tu płaszcze przeciwdeszczowe i klej.'), nl.
describe(ventilation) :- write('Stoisz przy kratce wentylacyjnej. Możliwe wyjście?'), nl.
describe(sink) :- write('Zlew. Jest tu sznurek, drut i kawałek materiału.'), nl.

describe(shaft1) :- write('Wpełzasz do ciasnego kanału. Przed Tobą zakręt.'), nl.
describe(shaft2) :- write('Bardzo ciasno. Możesz iść tylko naprzód.'), nl.
describe(shaft3) :- ( bar_removed(shaft3) -> write('Kratka wschodnia usunięta. Można przejść.'), nl ; write('Kratka blokuje drogę na wschód.'), nl).
describe(shaft4) :- write('Kanał schodzi w dół.'), nl.
describe(shaft5) :- write('Dalej w dół...'), nl.
describe(shaft6) :- ( bar_removed(shaft6) -> write('Wejście w górę wolne.'), nl ; write('Kratka blokuje wejście w górę.'), nl).
describe(shaft7) :- write('Duszne, ciasne przejście. Trzeba iść dalej.'), nl.
describe(shaft8) :- write('Coś słychać nad Tobą... już blisko?'), nl.
describe(shaft9) :- write('Pachnie świeżym powietrzem!'), nl.
describe(shaft10) :- ( bar_removed(shaft10) -> write('Kratka zdjęta. Można schodzić niżej.'), nl ; write('Kratka blokuje zejście.'), nl).
describe(shaft11) :- write('Kanał skręca na zachód. To już prawie koniec.'), nl.
describe(shaft12) :- write('Widzisz światło!'), nl.

describe(roof) :- write('Udało Ci się wyjść na dach! Przed Tobą kable prowadzące w dół. Wciskaj s aby zejść na dół'), nl.
describe(descent1) :- write('Zacząłeś schodzić po kablach. Ślisko, ale idzie.'), nl.
describe(descent2) :- write('Jesteś na wysokości około 4. piętra. Trzymaj się mocno!'), nl.
describe(descent3) :- write('Połowa drogi. Nie ma odwrotu...'), nl.
describe(descent4) :- write('Już blisko ziemi. Nie puść się!'), nl.
describe(descent5) :- write('Jeszcze kawałek... już prawie!'), nl.
describe(ground) :-
    write('Bezpiecznie dotarłeś na dół. Jesteś wolny!'), nl,
    write(' Gratulacje — Uciekłeś z więzienia! '), nl,
    halt.

can_escape :-
    at(mannequin, bed_area),
    ventilation_open,
    at(ventilation_mockup, ventilation),
    holding(screwdriver),
    holding(raincoat),
    holding(glue).

instructions :-
        clear_screen,
        write('Wpisuj komendy w składni Prologa.'), nl,
        write('Dostępne komendy:'), nl,
        write('start.             -- rozpocznij grę'), nl,
        write('n. s. e. w.        -- poruszanie się'), nl,
        write('take(Obiekt).      -- podnieś przedmiot'), nl,
        write('drop(Obiekt).      -- upuść przedmiot'), nl,
        write('make_mannequin.    -- stwórz manekina'), nl,
        write('place_mannequin.   -- połóż manekina na łóżku'), nl,
        write('make_vent_mockup.  -- zrób atrapę wentylacji'), nl,
        write('place_vent_mockup. -- umieść atrapę'), nl,
        write('drill.             -- rozwierć wentylację'), nl,
        write('unscrew.           -- odkręć kratkę'), nl,
        write('look.              -- rozejrzyj się'), nl,
        write('inventory.         -- sprawdź ekwipunek'), nl,
        write('instructions.      -- pokaż instrukcje'), nl,
        write('halt.              -- zakończ grę'), nl, nl.

start :- instructions, look.
