:- use_module(library(ansi_term)).

lokacja(biblioteka, 'Jesteś w bibliotece.').
lokacja(cela, 'Jesteś w swojej celi.').
lokacja(pralnia, 'Jesteś w pralni.').
lokacja(spacerniak, 'Jesteś na spacerniaku.').
lokacja(stolowka, 'Jesteś na więziennej stołówce.').

:- dynamic ekwipunek/1, kwestia/2, postac_w/2, pozycja_gracza/1, przedmiot_w/2.

akcja_uzycia(pamietnik) :-
    Do_ucieczki = [
        'sztućce',
        'płaszcze przeciwdeszczorwe',
        'silnik od odkurzacza',
        'broń',
        'ubrania na zmiane',
        'kontakt poza więzieniem'
    ],
    write('Potrzebne do ucieczki:'), nl,
    forall(member(Tekst, Do_ucieczki),
      (write('- '), write(Tekst), nl)).

daj(red, atlas) :-
    ekwipunek(atlas),
    retract(ekwipunek(atlas)),
    retractall(kwestia(red, _)),
    assertz(kwestia(red, 'Super, dzięki za atlas! Sprawę kontaktu możesz uznać za załatwioną.'))

dialog(Postac, Tekst) :-
    ansi_format([fg(cyan), bold], '~w: ', [Postac]),
    ansi_format([fg(white)], '~w~n', [Tekst]).

dialog_postaci(Postac) :-
    kwestia(Postac, Tekst),
    dialog(Postac, Tekst).

dialog_sad :-
    nl,
    dialog('Prokurator', 'Niech Pan opowowie jak z Pana perspektywy wyglądała rozmowa z szefem tego dnia.'),
    dialog('Andy Dufrasne', 'Był bardzo szorstki. Powiedział, że nikt już nie korzysta z vima, że zmniejsza on moją produktywność i zwolnienie mnie to jego jedyne wyjście.'),
    dialog('Prokurator', 'Czy po tym incydencie wypchnął Pan na maina gałąź, w której cały kod był pisany w jednej linii, a nazwy funkcji nie oddawały ich działania? Tu przykład: funkcja launch_missle() wypisywała na konsolę "Hello, World".'),
    dialog('Andy Dufrasne', 'Jak już mówiłem, zostałem wrobiony. Nigdy nie wypchnąłbym kodu przed pokryciu jego przynajmniej dziewięćdziesięciu procent testami jednostkowymi.'),
    write('...'), nl,
    dialog('Sędzia', 'Panie Andrew Dufrasne, za pisanie brzydkiego kodu zostaje Pan skazany na dwa dożywocia w więzieniu o zaostrzonym rygorze!'),
    nl.

idz(Miejsce) :-
    lokacja(Miejsce, Opis),
    retractall(pozycja_gracza(_)),
    assertz(pozycja_gracza(Miejsce)),
    ansi_format([fg(green)], '~w~n', [Opis]).

idz(_) :-
    ansi_format([fg(red)], 'Nie ma takiego miejsca!~n', []).

instrukcje :-
    nl,
    write('Dostępne komendy:'), nl,
    write('start.               -- rozpoczęcie gry.'), nl,
    write('idz(miejsce)         -- przejdź do "miejsce".'), nl,
    write('instrukcje           -- wyświetl te komendy.'), nl,
    write('mapa                 -- wyświetl mapę więzienia.'), nl,
    write('porozmawiaj(imie)    -- rozpocznij rozmowę z postacią w tej lokacji'), nl,
    write('rozejrzyj            -- rozejrzyj się dookoła.'), nl,
    write('uzyj(przedmiot)      -- spróbuj skorzystać z przedmiotu w ekwipunku'), nl,
    write('wez(przedmiot)       -- weź przedmiot z aktualnej lokacji.'), nl,
    write('halt                 -- zakończ rozgrywkę i wyjdź.'), nl,
    nl.

mapa :-
    nl,
    write('Miejsca, do których możesz przejść:'), nl,
    write('biblioteka           -- więzienna biblioteka'), nl,
    write('cela                 -- twoja cela'), nl,
    write('pralnia              -- pralnia, tu pracujesz'), nl,
    write('spacerniak           -- miejsce do spotkań z współwięźniami na dworze'), nl,
    write('stolowka             -- więzienna stołówka'), nl,
    nl.

porozmawiaj(Rozmowca) :-
    pozycja_gracza(Miejsce),
    postac_w(Rozmowca, Miejsce),
    dialog_postaci(Rozmowca), !.

porozmawiaj(Rozmowca) :-
    ansi_format([fg(red)], 'Nie ma tu nikogo o imieniu2 "~w".~n', [Rozmowca]).

rozejrzyj :-
    pozycja_gracza(Miejsce),
    % przedmioty
    findall(Przedmiot, przedmiot_w(Przedmiot, Miejsce), Przedmioty),
    wypisz_przedmioty(Przedmioty),
    % postacie
    findall(Postac, postac_w(Postac, Miejsce), Postacie),
    wypisz_postacie(Postacie).

start :-
    instrukcje,
    dialog_sad,
    start_przedmioty,
    start_postacie,
    idz(cela).

start_przedmioty :-
    retractall(przedmiot_w(_, _)),
    assertz(przedmiot_w(atlas, biblioteka)),
    assertz(przedmiot_w(sztucce, stolowka)),
    assertz(przedmiot_w(ubrania, pralnia)),
    assertz(przedmiot_w(klej, biblioteka)),
    assertz(przedmiot_w(pamietnik, cela)),
    assertz(przedmiot_w(plaszcze, pralnia)).

start_postacie :-
    retractall(postac_w(_, _)),
    assertz(postac_w(bibliotekarz, biblioteka)),
    assertz(postac_w(red, spacerniak)),
    assertz(postac_w(klawisz, pralnia)),
    assertz(postac_w(kucharz, stolowka)).

start_rozmowy :-
    retractall(kwestia(_, _)),
    assertz(kwestia('Red', 'Czyli próbujesz uciec i potrzebujesz kogoś na zewnątrz do pomocy? Znajdź mi w bibliotece atlas, a w zamian zobaczę co da się zrobić.')),
    assertz(kwestia('Bibliotekarz', 'Ciii... Próbuję się skupić na tej książcę o relacyjnych bazach danych.')),
    assertz(kwestia('Klawisz', 'Nie masz dziś zmiany w pralni? Lepiej się pośpiesz!')),
    assertz(kwestia('Kucharz', 'To co zwykle?')),.

uzyj(Przedmiot) :-
    ekwipunek(Przedmiot),
    akcja_uzycia(Przedmiot), !.

uzyj(Przedmiot) :-
    ansi_format([fg(red)], 'Nie masz przedmiotu: ~w~n', [Przedmiot]).

wez(Przedmiot) :-
    pozycja_gracza(Miejsce),
    przedmiot_w(Przedmiot, Miejsce),
    retract(przedmiot_w(Przedmiot, Miejsce)),
    assertz(ekwipunek(Przedmiot)),
    ansi_format([fg(green)], 'Zabrałeś: ~w~n', [Przedmiot]), !.

wypisz_postacie([]).

wypisz_postacie(Postacie) :-
    ansi_format([fg(cyan)], 'Spotykasz tutaj:~n', []),
    forall(member(P,Postacie), ansi_format([fg(cyan)], '-~w~n', [P])).

wypisz_przedmioty([]) :-
    ansi_format([fg(blue)], 'Nie ma tu nic ciekawego.~n', []).

wypisz_przedmioty(Przedmioty) :-
    ansi_format([fg(magenta)], 'Widzisz:~n', []),
    forall(member(P, Przedmioty), ansi_format([fg(magenta)], '-~w~n', [P])).
