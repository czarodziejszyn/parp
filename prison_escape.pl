lokacja(cela, 'Jesteś w swojej celi.')
lokacja(stolowka, 'Jesteś na więziennej stołówce.')
lokacja(spacerniak, 'Jesteś na spacerniaku.')
lokacja(biblioteka, 'Jesteś w bibliotece.')
lokacja(korytarz, 'Jesteś na korytarzu.')

:- dynamic jestem/1.

instrukcje :-
    nl,
    write('Dostępne komendy:'), nl,
    write('start.               -- rozpoczęcie gry.'), nl,
    write('rozejrzyj            -- rozejrzyj się dookoła.'), nl,
    write('stop                 -- zakończ rozgrywkę i wyjdź.'), nl,
    nl.

start :-
    instrukcje,
    rozejrzyj.