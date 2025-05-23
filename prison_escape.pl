:- use_module(library(ansi_term)).

lokacja(biblioteka, 'Jesteś w bibliotece.').
lokacja(cela, 'Jesteś w swojej celi.').
lokacja(pralnia, 'Jesteś w pralni.').
lokacja(spacerniak, 'Jesteś na spacerniaku.').
lokacja(stolowka, 'Jesteś na więziennej stołówce.').

:- dynamic ekwipunek/1, kwestia/2, postac_w/2, pozycja_gracza/1, przedmiot_w/2.

akcja_uzycia(pamietnik) :-
    Do_ucieczki = [
        'broń (ze skarpetki i mydła)',
        'drut (z odkurzacza)',
        'farba',
        'kontakt poza więzieniem',
        'materiał (wyciąć z piżamy)',
        'papier',
        'płaszcze przeciwdeszczorwe',
        'sztućce',
        'sznurek (wyciąć z piżamy)',
        'śrubokręt',
        'ubrania na zmiane',
        'włosy (obciąć własne)'
    ],
    write('Potrzebne do ucieczki:'), nl,
    forall(member(Tekst, Do_ucieczki),
      (write('- '), write(Tekst), nl)), !.

akcja_uzycia(nozyczki) :-
    \+ pozycja_gracza(cela),
    ansi_format([fg(red)], 'Chcesz obciąć włosy? Mądre... ale zrób to w celi, żeby nikt się nie zainteresował.~n', []), !.

akcja_uzycia(nozyczki) :-
    pozycja_gracza(cela),
    \+ ekwipunek(wlosy),
    assertz(ekwipunek(wlosy)),
    ansi_format([fg(green)], 'Obciąłeś włosy.~n', []), !.

% daj(red, atlas) :-
%     ekwipunek(atlas),
%     retract(ekwipunek(atlas)),
%     retractall(kwestia(red, _)),
%     assertz(ekwipunek(kontakt)),
%     assertz(kwestia(red, 'Super, dzięki za atlas! Sprawę kontaktu możesz uznać za załatwioną.')).

dialog(Postac, Tekst) :-
    ansi_format([fg(cyan), bold], '~w: ', [Postac]),
    ansi_format([fg(white)], '~w~n', [Tekst]), !.

dialog_postaci(Postac) :-
    kwestia(Postac, Tekst),
    dialog(Postac, Tekst), !.

dialog_sad :-
    nl,
    dialog('Prokurator', 'Niech Pan opowowie jak z Pana perspektywy wyglądała rozmowa z szefem tego dnia.'),
    dialog('Andy Dufrasne', 'Był bardzo szorstki. Powiedział, że nikt już nie korzysta z vima, że zmniejsza on moją produktywność i zwolnienie mnie to jego jedyne wyjście.'),
    dialog('Prokurator', 'Czy po tym incydencie wypchnął Pan na maina gałąź, w której cały kod był pisany w jednej linii, a nazwy funkcji nie oddawały ich działania? Tu przykład: funkcja launch_missle() wypisywała na konsolę "Hello, World".'),
    dialog('Andy Dufrasne', 'Jak już mówiłem, zostałem wrobiony. Nigdy nie wypchnąłbym kodu przed pokryciu jego przynajmniej dziewięćdziesięciu procent testami jednostkowymi.'),
    write('...'), nl,
    dialog('Sędzia', 'Panie Andrew Dufrasne, za pisanie brzydkiego kodu zostaje Pan skazany na dwa dożywocia w więzieniu o zaostrzonym rygorze!'),
    nl, !.

ekwipunek :-
    findall(Przedmiot, ekwipunek(Przedmiot), Lista),
    ( Lista = [] ->
        ansi_format([fg(blue)], 'Twój ekwipunek jest pusty.~n', [])
    ;
        ansi_format([fg(blue)], 'Masz przy sobie:~n', []),
        forall(member(P, Lista), ansi_format([fg(blue)], '-~w~n', [P]))
    ),
    sprawdz_ucieczke, !.

idz(Miejsce) :-
    lokacja(Miejsce, Opis),
    retractall(pozycja_gracza(_)),
    assertz(pozycja_gracza(Miejsce)),
    ansi_format([fg(green)], '~w~n', [Opis]), !,
    rozejrzyj.

idz(_) :-
    ansi_format([fg(red)], 'Nie ma takiego miejsca!~n', []), !.

instrukcje :-
    nl,
    write('Dostępne komendy:'), nl,
    write('start.               -- rozpoczęcie gry.'), nl,
    % write('daj(imie, przedmiot) -- daj przedmiot z ekwipunku innej postaci.'), nl,
    write('ekwipunek.           -- sprawdź czy masz już wszystko do ucieczki.'), nl,
    write('idz(miejsce).        -- przejdź do "miejsce".'), nl,
    write('instrukcje.          -- wyświetl te komendy.'), nl,
    write('mapa.                -- wyświetl mapę więzienia.'), nl,
    write('porozmawiaj(imie).   -- rozpocznij rozmowę z postacią w tej lokacji'), nl,
    write('rozejrzyj.           -- rozejrzyj się dookoła.'), nl,
    write('uzyj(przedmiot).     -- spróbuj skorzystać z przedmiotu w ekwipunku'), nl,
    write('wez(przedmiot).      -- weź przedmiot z aktualnej lokacji.'), nl,
    write('halt.                -- zakończ rozgrywkę i wyjdź.'), nl,
    nl, !.

mapa :-
    nl,
    write('Miejsca, do których możesz przejść:'), nl,
    write('biblioteka           -- więzienna biblioteka'), nl,
    write('cela                 -- twoja cela'), nl,
    write('pralnia              -- pralnia, tu pracujesz'), nl,
    write('spacerniak           -- miejsce do spotkań z współwięźniami na dworze'), nl,
    write('stolowka             -- więzienna stołówka'), nl,
    nl, !.

porozmawiaj(red) :-
    \+ ekwipunek(kontakt),
    \+ ekwipunek(atlas),
    \+ przedmiot_w(atlas, biblioteka),
    pozycja_gracza(Miejsce),
    postac_w(red, Miejsce),
    dialog_postaci(red),
    assertz(przedmiot_w(atlas, biblioteka)), !.

porozmawiaj(red) :-
    ekwipunek(atlas),
    pozycja_gracza(spacerniak),
    dialog_postaci(red),
    retract(ekwipunek(atlas)),
    assertz(ekwipunek(kontakt)), !.

porozmawiaj(Rozmowca) :-
    pozycja_gracza(Miejsce),
    postac_w(Rozmowca, Miejsce),
    dialog_postaci(Rozmowca), !.

porozmawiaj(Rozmowca) :-
    ansi_format([fg(red)], 'Nie ma tu nikogo o imieniu "~w".~n', [Rozmowca]), !.

potrzebne_do_ucieczki([
    drut,
    farba,
    kontakt,
    material,
    mydlo,
    papier,    
    plaszcze,
    srubokret,
    sznurek,
    sztucce,
    ubrania,
    wlosy
]).

rozejrzyj :-
    pozycja_gracza(Miejsce),
    % przedmioty
    findall(Przedmiot, przedmiot_w(Przedmiot, Miejsce), Przedmioty),
    wypisz_przedmioty(Przedmioty),
    % postacie
    findall(Postac, postac_w(Postac, Miejsce), Postacie),
    wypisz_postacie(Postacie), !.

sprawdz_ucieczke :-
    potrzebne_do_ucieczki(Wymagane),
    ( forall(member(Rzecz, Wymagane), ekwipunek(Rzecz)) ->
        ucieczka_z_wiezienia
    ;
        true
    ).

start :-
    instrukcje,
    dialog_sad,
    start_przedmioty,
    start_postacie,
    start_rozmowy,
    idz(cela), !.

start_przedmioty :-
    retractall(przedmiot_w(_, _)),
    % assertz(przedmiot_w(atlas, biblioteka)),
    assertz(przedmiot_w(sztucce, stolowka)),
    assertz(przedmiot_w(ubrania, pralnia)),
    assertz(przedmiot_w(klej, biblioteka)),
    assertz(przedmiot_w(pamietnik, cela)),
    assertz(przedmiot_w(plaszcze, pralnia)),
    assertz(przedmiot_w(papier, biblioteka)),
    assertz(przedmiot_w(farba, spacerniak)),
    % assertz(przedmiot_w(nozyczki, biblioteka)),
    % assertz(przedmiot_w(jablko, stolowka)),
    assertz(przedmiot_w(odkurzacz, pralnia)),
    assertz(przedmiot_w(pizama, cela)),
    assertz(przedmiot_w(mydlo, cela)).

start_postacie :-
    retractall(postac_w(_, _)),
    assertz(postac_w(bibliotekarz, biblioteka)),
    assertz(postac_w(red, spacerniak)),
    assertz(postac_w(klawisz, pralnia)),
    assertz(postac_w(kucharz, stolowka)).

start_rozmowy :-
    retractall(kwestia(_, _)),
    assertz(kwestia('red', 'Czyli próbujesz uciec i potrzebujesz kogoś na zewnątrz do pomocy? Znajdź mi w bibliotece atlas, a w zamian zobaczę co da się zrobić.')),
    assertz(kwestia('bibliotekarz', 'Ciii... Próbuję się skupić na tej książcę o relacyjnych bazach danych.')),
    assertz(kwestia('klawisz', 'Nie masz dziś zmiany w pralni? Lepiej się pośpiesz!')),
    assertz(kwestia('kucharz', 'To co zwykle?')), !.

ucieczka_z_wiezienia :-
    nl,
    write('Masz wszystko czego potrzeba do ucieczki. Z mydła i skarpetek znalezionych w ubraniach robisz prowizoryczną broń. Pora poukrywać przedmioty po celi, żeby nie wzbudzały podejrzeń, poczekać na noc i rozpocząć ucieczkę.'), nl, nl,
    write('Otrzymujesz nowy zestaw instrukcji dostępny po wpisaniu polecenia "instrukcje".'), nl,
    reset_game, !.

uzyj(Przedmiot) :-
    ekwipunek(Przedmiot),
    akcja_uzycia(Przedmiot), !.

uzyj(Przedmiot) :-
    ansi_format([fg(red)], 'Nie masz albo nie możesz użyć przedmiotu: ~w~n', [Przedmiot]), !.

wez(atlas) :-
    pozycja_gracza(biblioteka),
    przedmiot_w(atlas, biblioteka),
    assertz(ekwipunek(atlas)),
    retract(przedmiot_w(atlas, biblioteka)),
    retractall(kwestia(red, _)),
    assertz(kwestia(red, 'Super, dzięki za atlas! Sprawę kontaktu możesz uznać za załatwioną.')), !.

wez(plaszcze) :-
    pozycja_gracza(pralnia),
    \+ przedmiot_w(jablko, stolowka),
    \+ ekwipunek(jablko),
    ansi_format([fg(red)], 'Klawisz patrzy! Musisz go czymś zająć, może jedzeniem?~n', []), !,
    assertz(przedmiot_w(jablko, stolowka)), !.

wez(plaszcze) :-
    pozycja_gracza(pralnia),
    ekwipunek(jablko),
    \+ ekwipunek(plaszcze),
    retract(przedmiot_w(plaszcze, pralnia)),
    retract(ekwipunek(jablko)),
    assertz(ekwipunek(plaszcze)),
    ansi_format([fg(green)], 'Zająłeś klawisza jabłkiem i zabrałeś płaszcze.~n', []), !.

wez(plaszcze) :-
    przedmiot_w(jablko, stolowka),
    ansi_format([fg(red)], 'Klawisz patrzy! Musisz go czymś zająć, może jedzeniem?~n', []), !.

wez(pizama) :-
    pozycja_gracza(cela),
    \+ przedmiot_w(nozyczki, biblioteka),
    \+ ekwipunek(nozyczki),
    assertz(przedmiot_w(nozyczki, biblioteka)),
    ansi_format([fg(red)], 'Bezużyteczna piżama... chyba że znajdziesz coś czym wytnie się z niej sznurek i materiał.~n', []), !.

wez(pizama) :-
    pozycja_gracza(cela),
    ekwipunek(nozyczki),
    retract(przedmiot_w(pizama, cela)),
    \+ ekwipunek(sznurek),
    assertz(ekwipunek(sznurek)),
    assertz(ekwipunek(material)),
    ansi_format([fg(green)], 'Wyciąłeś z piżamy sznurek i materiał.~n', []), !.

wez(pizama) :-
    przedmiot_w(nozyczki, biblioteka),
    ansi_format([fg(red)], 'Bezużyteczna piżama... chyba że znajdziesz coś czym wytnie się z niej sznurek i materiał.~n', []), !.

wez(odkurzacz) :-
    pozycja_gracza(pralnia),
    \+ przedmiot_w(srubokret, spacerniak),
    \+ ekwipunek(srubokret),
    assertz(przedmiot_w(srubokret, spacerniak)),
    ansi_format([fg(red)], 'Odkurzacz. Przydałoby się jakoś go rozkręcić, żeby dostać się do wnętrzności...~n', []), !.
    
wez(odkurzacz) :-
    pozycja_gracza(pralnia),
    ekwipunek(srubokret),
    \+ ekwipunek(drut),
    assertz(ekwipunek(drut)),
    retract(przedmiot_w(odkurzacz, pralnia)),
    ansi_format([fg(green)], 'Rozkręciłeś odkurzacz i wyjąłeś z niego drut.~n', []), !.

wez(odkurzacz) :-
    przedmiot_w(srubokret, spacerniak),
    ansi_format([fg(green)], 'Znajdź coś do rozkręcenia odkurzacza.~n', []), !.

wez(Przedmiot) :-
    pozycja_gracza(Miejsce),
    przedmiot_w(Przedmiot, Miejsce),
    retract(przedmiot_w(Przedmiot, Miejsce)),
    assertz(ekwipunek(Przedmiot)),
    ansi_format([fg(green)], 'Zabrałeś: ~w~n', [Przedmiot]), !.

wypisz_postacie([]).

wypisz_postacie(Postacie) :-
    ansi_format([fg(cyan)], 'Spotykasz tutaj:~n', []),
    forall(member(P,Postacie), ansi_format([fg(cyan)], '-~w~n', [P])), !.

wypisz_przedmioty([]) :-
    ansi_format([fg(blue)], 'Nie ma tu nic ciekawego.~n', []), !.

wypisz_przedmioty(Przedmioty) :-
    ansi_format([fg(magenta)], 'Widzisz:~n', []),
    forall(member(P, Przedmioty), ansi_format([fg(magenta)], '-~w~n', [P])), !.



reset_game :-
    retractall(ekwipunek(_)),
    retractall(kwestia(_, _)),
    retractall(postac_w(_, _)),
    retractall(pozycja_gracza(_)),
    retractall(przedmiot_w(_, _)),

    abolish(ekwipunek/1),
    abolish(kwestia/2),
    abolish(postac_w/2),
    abolish(pozycja_gracza/1),
    abolish(przedmiot_w/2),
    abolish(akcja_uzycia/1),
    abolish(daj/2),
    abolish(dialog/2),
    abolish(dialog_postaci/1),
    abolish(dialog_sad/0),
    abolish(ekwipunek/0),
    abolish(idz/1),
    abolish(instrukcje/0),
    abolish(mapa/0),
    abolish(porozmawiaj/1),
    abolish(potrzebne_do_ucieczki/1),
    abolish(rozejrzyj/0),
    abolish(sprawdz_ucieczke/0),
    abolish(start/0),
    abolish(start_przedmioty/0),
    abolish(start_postacie/0),
    abolish(start_rozmowy/0),
    abolish(ucieczka_z_wiezienia/0),
    abolish(uzyj/1),
    abolish(wez/1),
    abolish(wypisz_postacie/1),
    abolish(wypisz_przedmioty/1),

    consult('cell_escape.pl').
