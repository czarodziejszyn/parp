/* dynamic states */
:- dynamic i_am_at/1, at/2, holding/1, fence_can_cross/1, guards_present/1, warned/2.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(fence_can_cross), retractall(warned(_)).

/* map out area*/
i_am_at(wall).

path(wall, s, fence).
path(fence, n, wall).

path(fence, s, beach).
path(beach, n, fence).

path(beach, s, sea).
path(beach, w, docks).

path(docks, e, beach).
path(docks, e, sea).

path(sea, s, city).
path(sea, n, island).
path(sea, w, shore).

/* map out objects */

/* at(object, place)*/

/* map out npcs */

/* npc_at(npc, place) */

/* COMMANDS */
/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        (
        i_am_at(fence)
        -> (
                fence_can_cross
                -> cross_fence(waited)
                ;(
                        warned(fence)
                        -> die(fence),!
                        ; warn_about(fence),!
                )
        )
        ; retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look
        ).

go(_) :-
        write('Nie ma tam przejścia.').

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.

notice_objects_at(Place) :-
        at(X, Place),
        write('Jest tu '), write(X), nl,
        fail.
notice_objects_at(_).

wait :-
        /* deduct time*/
        i_am_at(Place),
        wait_at(Place).

wait_at(fence) :-
        write("czekasz przy płocie"), nl.

wait_at(_) :-
        write("zmarnowałeś nieco czasu"),nl.


die(_) :-
        write("Umierasz, koniec gry"), nl.
        finish.


finish :-
        nl,
        write('Gra dobiegła końca, wypisz halt. by wyjść'),
        nl.

/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        /*
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        */
        write("wait.              -- to wait for a more opportune moment"), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.

/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.

/* ---- places descriptions ---- */

/* wall - stwaring place of part 3*/
describe(wall) :-
        write("Po dłużącym się zejściu z radością witasz grunt pod stopami."), nl,
        write("Mimo, że mury więzienia już macie za sobą, do pokonania została jeszcze bariera z drutu kolczastego i wody zatoki san francisco."), nl, nl,
        write("Noc niedługo się skończy a wraz z nią twoja szansa na ucieczkę."), nl,
        write("Wiesz, że nie masz za dużo czasu"), nl, nl,
        write("Na południe od ciebie znajduje się ogrodzeie z drutu."), nl.

/* fence - first obstacle */
describe(fence) :-
        (fence_can_cross
        -> write("Nie masz po co warać się za płot"), nl
        ; write("Przed tobą znajduje się bariera wykonana z drutu kolczastego otaczająca budynek więzienny."),nl,
        write("Teren wokół niej przeszukują reflektory. Wiesz, że jak was zobaczą to koniec, strzelcy w wieżach strażniczych mają rozkazy zabijać na miejscu."), nl, nl,
        write("Sam drut byłby dość nieprzyjemną przeszkodą ale wizja dostania kulką powoduje konieczność przemyślanego podejścia do problemu."), nl, nl,
        write("Możesz także tuląc się sciany budynku dotrzeć do doku. Może uda znaleźć sie tam łódź."), nl,
        write("Ale to nie rozwiąże problemu płotu ..."), nl, nl,
        write("Musisz przedostać się przez płot."), nl
        ).

/* assert(warned(fence)), nl,*/

warn_about(fence) :-
        assert(warned(fence)),
        write("Na pewno chcesz rzucić się przez płot tu i teraz?"), nl,
        write("Najpewnej ci się nie uda bez wczęsniejszego przygotowania."), nl,
        nl,
        fail.

warn_about(_).

cross_fence(waited) :-
        retract(i_am_at(fence)),
        assert(i_am_at(beach)),
        write("Wyczekujesz najdłuższego okna i wspinasz sie na płot."), nl,
        write("Po największym wysiłku od kilku dni spadasz na drugą stronę."), nl, nl,
        !, look.

cross_fence(_) :-
        write("Rzucasz się na ogrodzenie ja tylko reflektor się od niego odsuwa."), nl,
        write("Niestety źle wybrałeś chwilę i zanim wespniesz się na połowę wysokości okawa cie snop światła."), nl,
        write("Słyszysz syreny alarmowe..."), nl,
        die(fence).
