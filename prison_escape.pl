:- use_module(library(ansi_term)).

lokacja(cela, 'Jesteś w swojej celi.').
lokacja(stolowka, 'Jesteś na więziennej stołówce.').
lokacja(spacerniak, 'Jesteś na spacerniaku.').
lokacja(biblioteka, 'Jesteś w bibliotece.').
lokacja(pralnia, 'Jesteś w pralni.').

:- dynamic pozycja_gracza/1, wykonane/1.
pozycja_gracza(cela).

idz(Miejsce) :-
    lokacja(Miejsce, Opis),
    ansi_format([fg(green)], '~w~n', [Opis]).

idz(_) :-
    ansi_format([fg(red)], 'Nie ma takiego miejsca!~n', []).

dialog(Postac, Tekst) :-
    ansi_format([fg(cyan), bold], '~w: ', [Postac]),
    ansi_format([fg(white)], '~w~n', [Tekst]).

instrukcje :-
    nl,
    write('Dostępne komendy:'), nl,
    write('start.               -- rozpoczęcie gry.'), nl,
    write('rozejrzyj            -- rozejrzyj się dookoła.'), nl,
    write('stop                 -- zakończ rozgrywkę i wyjdź.'), nl,
    nl.

dialog_sad :-
    nl,
    dialog('Prokurator', 'Niech Pan opowowie jak z Pana perspektywy wyglądała rozmowa z szefem tego dnia.'),
    dialog('Andy Dufrasne', 'Był bardzo szorstki. Powiedział, że nikt już nie korzysta z vima, że zmniejsza on moją produktywność i zwolnienie mnie to jego jedyne wyjście.'),
    dialog('Prokurator', 'Czy po tym incydencie wypchnął Pan na maina gałąź, w której cały kod był pisany w jednej linii, a nazwy funkcji nie oddawały ich działania? Tu przykład: funkcja launch_missle() wypisywała na konsolę "Hello, World".'),
    dialog('Andy Dufrasne', 'Jak już mówiłem, zostałem wrobiony. Nigdy nie wypchnąłbym kodu przed pokryciu jego przynajmniej dziewięćdziesięciu procent testami jednostkowymi.'),
    dialog('Sędzia', 'Panie Andrew Dufrasne, za pisanie brzydkiego kodu zostaje Pan skazany na dwa dożywocia w więzieniu o zaostrzonym rygorze!'),
    nl.

start :-
    instrukcje,
    dialog_sad.