/* dynamic states */
:- dynamic i_am_at/1, at/2, holding/1, fence_can_cross/1, guards_present/1, warned/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(fence_can_cross), retractall(warned(_)).

/* carry ober from prev stages */
/* float, weapon, clothes, friend from outside*/
:- assert(holding(float_device)), assert(holding(weapon)).
/*retractall(holding(_))*/

/* map out area */
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
                        -> cross_fence(),!
                        ; warn_about(fence),!
                ),!
        ),!
        ; retract(i_am_at(Here)),
        assert(i_am_at(There)),
        look,!
        ).

go(_) :-
        write('Nie ma tam przejścia.').

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl, !.

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
        assert(fence_can_cross),
        write("Czekasz i obserwujesz sposób poruszania się świateł."),nl,
        write("Po kilku cyklach jesteś pewien swojej oceny."), nl,
        write("Masz okazję do przekroczenia ogrodzenia."),!, nl.

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
        write('start.             -- aby zacząć grę.'), nl,
        write('n.  s.  e.  w.     -- aby pójść w danym kierunku.'), nl,
        /*
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        */
        write("wait.              -- aby zaczekać na korzystniejszy moment do działania"), nl,
        write('look.              -- aby ponownie się rozejrzeć.'), nl,
        write('instructions.      -- aby ponownie wyświetlić tą wiadomość.'), nl,
        write('halt.              -- aby skończyć grę i wyjść.'), nl,
        nl.

/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.

/* ---- places descriptions ---- */

/* wall - staring place of part 3*/
describe(wall) :-
        write("Po dłużącym się zejściu z radością witasz grunt pod stopami."), nl,
        write("Mimo, że mury więzienia już macie za sobą, do pokonania została jeszcze bariera z drutu kolczastego i wody zatoki san francisco."), nl, nl,
        write("Noc niedługo się skończy a wraz z nią twoja szansa na ucieczkę."), nl,
        write("Wiesz, że nie masz za dużo czasu"), nl, nl,
        write("Na południe od ciebie znajduje się ogrodzenie z drutu."), nl.

/* fence - first obstacle */
describe(fence) :-
        (fence_can_cross
        -> write("Nie masz po co wracać się za płot"), nl
        ; write("Przed tobą znajduje się bariera wykonana z drutu kolczastego otaczająca budynek więzienny."), nl,
        write("Teren wokół niej przeszukują reflektory. Wiesz, że jak was zobaczą to koniec, strzelcy w wieżach strażniczych mają rozkazy zabijać na miejscu."), nl, nl,
        write("Sam drut byłby dość nieprzyjemną przeszkodą ale wizja dostania kulką powoduje konieczność przemyślanego podejścia do problemu."), nl, nl,
        write("Ale najpierw musisz przedostać się przez płot."), nl, nl,
        write("Reflektory obracają się w stałym tempie. Ich droga jest przewidywalna."), nl,
        write("Jeśli spędzisz trochę czasu, znajdziesz moment kiedy nikt nie patrzy na tak kawałek płotu dość długo by się przeprawić."), nl
        ).

describe(beach) :-
        write("Czarna tafla wody rozciąga się coraz szerzej przed twoimi oczami."), nl,
        write("Nocna cisza przerywana jest ciągłym szumem fal rozbijających się o brzeg."), nl,
        write("Pod twoimi stopami czujesz szorstkie wyboiste kamyki."), nl,
        write("Dotarłeś do plaży."), nl,nl,
        write("Kilka godzin jakie ci pozostało zmusza cię do wybrania jednej drogi."), nl, nl , !,
        (
                (holding(float_device); holding(weapon))
                ->(
                        holding(float_device)
                        -> write("Masz przygotowany improwizowany ponton."), nl,
                        write("Wystarczy tylko go napompować i odpłynąć"), nl,nl,!
                        ; nl,!
                ),
                (
                        holding(weapon)
                        -> write("W kieszeni nadal znajduje się twoja improwizowana broń."), nl,
                        write("Jeżeli masz trochę szczęścia może będziesz w stanie obezwładnic strażników przy dokach i ukraść motorówkę?"), nl,nl,!
                        ; nl,!
                ),!
                ; write("Nie masz niczego co pomogło by wam przeprawic się przez wody zatoki. Chyba nie przemyślałeś tego planu za dobrze."), nl,
                write("Pozostaje ci spróbować płynąc wpław..."), nl,!
        ),
        !.


/* warnings*/
warn_about(fence) :-
        assert(warned(fence)),
        write("Na pewno chcesz rzucić się przez płot tu i teraz?"), nl,
        write("Najpewniej ci się nie uda bez wcześniejszego przygotowania."), nl,
        nl,
        fail.

warn_about(_).


/* fence crossing*/
cross_fence(waited) :-
        retract(i_am_at(fence)),
        assert(i_am_at(beach)),
        write("Wyczekujesz najdłuższego okna i wspinasz sie na płot."), nl,
        write("Po największym wysiłku od kilku dni spadasz na drugą stronę."), nl, nl,!,
        look, !.

cross_fence(_) :-
        write("Rzucasz się na ogrodzenie ja tylko reflektor się od niego odsuwa."), nl,
        write("Niestety źle wybrałeś chwilę i zanim wespniesz się na połowę wysokości otacza cię snop światła."), nl,
        write("Słyszysz syreny alarmowe..."), nl,
        die(fence).
