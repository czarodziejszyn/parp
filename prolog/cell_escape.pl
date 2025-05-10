:- dynamic jestem_w/1, znajduje_sie/2, mam/1, ilosc_uzyc_noza/1, ilosc_uzyc_lyzki/1, wentylacja_otwarta/0, w_szybie/0.
:- dynamic sruby_kratki/2, kratka_usunieta/1.

:- retractall(znajduje_sie(_, _)), retractall(jestem_w(_)), retractall(ilosc_uzyc_noza(_)), retractall(ilosc_uzyc_lyzki(_)), retractall(w_szybie).
:- retractall(sruby_kratki(_, _)), retractall(kratka_usunieta(_)), retractall(wentylacja_otwarta).

jestem_w(centrum_celi).

sruby_kratki(szyb3, 0).
sruby_kratki(szyb6, 0).
sruby_kratki(szyb10, 0).

sciezka(centrum_celi, n, lozko).
sciezka(lozko, s, centrum_celi).
sciezka(centrum_celi, w, toaleta).
sciezka(toaleta, e, centrum_celi).
sciezka(centrum_celi, e, magazynek).
sciezka(magazynek, w, centrum_celi).
sciezka(centrum_celi, s, poludnie).
sciezka(poludnie, n, centrum_celi).
sciezka(magazynek, e, wentylacja).
sciezka(wentylacja, w, magazynek).
sciezka(toaleta, w, zlew).
sciezka(zlew, e, toaleta).

sciezka(wentylacja, n, szyb1) :- moge_uciec, \+ w_szybie.
sciezka(szyb1, n, szyb2).
sciezka(szyb2, e, szyb3).
sciezka(szyb3, e, szyb4) :- kratka_usunieta(szyb3).
sciezka(szyb4, s, szyb5).
sciezka(szyb5, s, szyb6).
sciezka(szyb6, n, szyb7) :- kratka_usunieta(szyb6).
sciezka(szyb7, n, szyb8).
sciezka(szyb8, n, szyb9).
sciezka(szyb9, n, szyb10).
sciezka(szyb10, s, szyb11) :- kratka_usunieta(szyb10).
sciezka(szyb11, w, szyb12).
sciezka(szyb12, n, dach).

sciezka(dach, s, zejscie1).
sciezka(zejscie1, s, zejscie2).
sciezka(zejscie2, s, zejscie3).
sciezka(zejscie3, s, zejscie4).
sciezka(zejscie4, s, zejscie5).
sciezka(zejscie5, s, ziemia).

znajduje_sie(farba, lozko).
znajduje_sie(wlosy, lozko).
znajduje_sie(papier, lozko).
znajduje_sie(srubokret, toaleta).
znajduje_sie(lyzka, magazynek).
znajduje_sie(noz, magazynek).
znajduje_sie(plaszcz, poludnie).
znajduje_sie(klej, poludnie).
znajduje_sie(sznurek, zlew).
znajduje_sie(drut, zlew).
znajduje_sie(material, zlew).

ilosc_uzyc_noza(0).
ilosc_uzyc_lyzki(0).


write_czerwony(Tekst) :-
    format('\033[31m~w\033[0m', [Tekst]).

write_zielony(Tekst) :-
    format('\033[32m~w\033[0m', [Tekst]).

write_zolty(Tekst) :-
    format('\033[33m~w\033[0m', [Tekst]).

write_niebieski(Tekst) :-
    format('\033[34m~w\033[0m', [Tekst]).



idz(Kierunek) :-
        jestem_w(Tutaj),
        sciezka(Tutaj, Kierunek, Tam),
        retract(jestem_w(Tutaj)),
        assert(jestem_w(Tam)),
        !, rozejrzyj,
        (jestem_w(ziemia) -> reset_game_1 ; true).

idz(n) :-
    jestem_w(wentylacja),
    moge_uciec,
    \+ w_szybie,
    retract(jestem_w(wentylacja)),
    assert(jestem_w(szyb1)),
    assert(w_szybie),
    write_czerwony('Wciskasz się do szybu wentylacyjnego. Jest ciasno i ciemno... użyj skrótów, by uciec.'), nl,
    rozejrzyj, !.

idz(_) :-
    write_zolty('Nie możesz tam iść.'), nl.

n :- idz(n).
s :- idz(s).
e :- idz(e).
w :- idz(w).

rozejrzyj :-
        jestem_w(Miejsce),
        opis(Miejsce), nl,
        obiekty_w_miejscu(Miejsce), nl,
        ekwipunek, nl.

obiekty_w_miejscu(Miejsce) :-
        znajduje_sie(X, Miejsce),
        write_niebieski('Widzisz tutaj: '), write_zielony(X), write('.'), nl,
        fail.
obiekty_w_miejscu(_).

wez(X) :-
        mam(X),
        write_zolty('Już to masz przy sobie!'), nl, !.

wez(X) :-
        jestem_w(Miejsce),
        znajduje_sie(X, Miejsce),
        retract(znajduje_sie(X, Miejsce)),
        assert(mam(X)),
        write_niebieski('Podniosłeś: '), write_zielony(X), write('.'), nl, !.

wez(_) :-
        write_zolty('Tutaj nie ma takiego przedmiotu.'), nl.

upusc(X) :-
        mam(X),
        jestem_w(Miejsce),
        retract(mam(X)),
        assert(znajduje_sie(X, Miejsce)),
        write_niebieski('Upuściłeś: '), write_zielony(X), write('.'), nl, !.

upusc(_) :-
        write_zolty('Nie masz tego przy sobie.'), nl.

zrob_manekina :-
        mam(farba), mam(wlosy), mam(papier),
        retract(mam(farba)),
        retract(mam(wlosy)),
        retract(mam(papier)),
        assert(mam(manekin)),
        write_czerwony('Stworzyłeś manekina!'), nl, !.
zrob_manekina :-
        write_zolty('Brakuje Ci materiałów, by stworzyć manekina.'), nl.

poloz_manekina :-
        jestem_w(lozko), mam(manekin),
        retract(mam(manekin)),
        assert(znajduje_sie(manekin, lozko)),
        write_niebieski('Położyłeś manekina na łóżku.'), nl, !.
poloz_manekina :-
        jestem_w(lozko),
        write_zolty('Nie masz manekina, żeby go położyć.'), nl, !.
poloz_manekina :-
        write_zolty('Manekina można położyć tylko na łóżku.'), nl.

zrob_atrape :-
        mam(sznurek), mam(drut), mam(material),
        retract(mam(sznurek)),
        retract(mam(drut)),
        retract(mam(material)),
        assert(mam(atrapa_wentylacji)),
        write_czerwony('Stworzyłeś atrapę wentylacji!'), nl, !.
zrob_atrape :-
        write_zolty('Potrzebujesz sznurka, drutu i materiału, by zrobić atrapę.'), nl.

poloz_atrape :-
    jestem_w(wentylacja),
    mam(atrapa_wentylacji),
    wentylacja_otwarta,
    retract(mam(atrapa_wentylacji)),
    assert(znajduje_sie(atrapa_wentylacji, wentylacja)),
    write_czerwony('Założono atrapę wentylacji. Wygląda całkiem realistycznie... wpiścij  "n", aby uciec.'), nl, !.
poloz_atrape :- jestem_w(wentylacja), \+ mam(atrapa_wentylacji), write_zolty('Nie masz atrapy przy sobie.'), nl, !.
poloz_atrape :- jestem_w(wentylacja), \+ wentylacja_otwarta, write_zolty('Musisz najpierw rozwiercić prawdziwą wentylację.'), nl, !.
poloz_atrape :- write_zolty('Musisz być przy wentylacji, aby umieścić atrapę.'), nl.

wierc :-
    jestem_w(wentylacja),
    (mam(noz); mam(lyzka)),
    (mam(noz), ilosc_uzyc_noza(K), K < 3 -> retract(ilosc_uzyc_noza(K)), K1 is K + 1, assert(ilosc_uzyc_noza(K1)); true),
    (mam(lyzka), ilosc_uzyc_lyzki(S), S < 3 -> retract(ilosc_uzyc_lyzki(S)), S1 is S + 1, assert(ilosc_uzyc_lyzki(S1)); true),
    (ilosc_uzyc_noza(3), mam(noz) -> retract(mam(noz)), write_zolty('Nóż się złamał!'), nl; true),
    (ilosc_uzyc_lyzki(3), mam(lyzka) -> retract(mam(lyzka)), write_zolty('Łyżka się złamała!'), nl; true),
    (ilosc_uzyc_noza(3), ilosc_uzyc_lyzki(3) ->
        (wentylacja_otwarta -> true ; assert(wentylacja_otwarta)),
        write_czerwony('Wentylacja została otwarta!'), nl
    ; write_zolty('Wierć dalej...'), nl), !.
wierc :- write_zolty('Potrzebujesz noża i łyżki, musisz być przy wentylacji.'), nl.

odkrec :-
    jestem_w(Lokacja),
    member(Lokacja, [szyb3, szyb6, szyb10]),
    mam(srubokret),
    sruby_kratki(Lokacja, Liczba),
    Liczba < 4,
    Nowa is Liczba + 1,
    retract(sruby_kratki(Lokacja, Liczba)),
    assert(sruby_kratki(Lokacja, Nowa)),
    (Nowa = 4 ->
        assert(kratka_usunieta(Lokacja)),
        write_niebieski('Odkręciłeś ostatnią śrubę. Kratka usunięta!'), nl
    ;
        write_zolty('Odkręciłeś śrubę. Pozostało: '), write_niebieski(4 - Nowa), nl
    ), !.
odkrec :- jestem_w(L), member(L, [szyb3, szyb6, szyb10]), \+ mam(srubokret), write_niebieski('Potrzebujesz śrubokręta.'), nl, !.
odkrec :- jestem_w(L), member(L, [szyb3, szyb6, szyb10]), kratka_usunieta(L), write_niebieski('Kratka już jest usunięta.'), nl, !.
odkrec :- write_niebieski('Nie ma tu kratki do odkręcenia.'), nl.

ekwipunek :-
    \+ mam(_), 
    write_niebieski('Na razie nic nie podniosłeś.'), nl, !.

ekwipunek :-
    write_niebieski('Masz przy sobie: '), nl,
    forall(mam(X), (write_niebieski('- '), write_zielony(X), nl)).



opis(centrum_celi) :- write_zolty('Jesteś w centrum swojej celi. Skompletuj ekwipunek do ucieczki.').
opis(lozko) :- write_zolty('Łóżko. Może znajdziesz tu coś, z czego zrobisz manekina?'), nl.
opis(toaleta) :- write_zolty('Jesteś przy toalecie. Widzisz śrubokręt.'), nl.
opis(magazynek) :- write_zolty('Magazynek. Znajdziesz tu narzędzia.'), nl.
opis(poludnie) :- write_zolty('Południowy zakątek. Są tu płaszcze przeciwdeszczowe i klej.'), nl.
opis(wentylacja) :- write_zolty('Stoisz przy kratce wentylacyjnej. Chyba tędy musisz uciec?'), nl.
opis(zlew) :- write_zolty('Zlew. Jest tu sznurek, drut i kawałek materiału.'), nl.

opis(szyb1) :- write_zolty('Wpełzasz do ciasnego kanału. Przed Tobą zakręt.'), nl.
opis(szyb2) :- write_zolty('Bardzo ciasno.'), nl.
opis(szyb3) :- (kratka_usunieta(szyb3) -> write_zolty('Kratka usunięta. Można przejść.'), nl ; write_zolty('Kratka blokuje dalszą drogę.'), nl).
opis(szyb4) :- write_zolty('Kanał schodzi w dół.'), nl.
opis(szyb5) :- write_zolty('Dalej w dół...'), nl.
opis(szyb6) :- (kratka_usunieta(szyb6) -> write_zolty('Droga wolna.'), nl ; write_zolty('Kratka blokuje wejście w górę.'), nl).
opis(szyb7) :- write_zolty('Duszne, ciasne przejście. Trzeba iść dalej.'), nl.
opis(szyb8) :- write_zolty('Coś słychać nad Tobą... już blisko?'), nl.
opis(szyb9) :- write_zolty('Pachnie świeżym powietrzem!'), nl.
opis(szyb10) :- (kratka_usunieta(szyb10) -> write_zolty('Kratka zdjęta. Można schodzić niżej.'), nl ; write_zolty('Kratka blokuje zejście.'), nl).
opis(szyb11) :- write_zolty('Kanał skręca na zachód. To już prawie koniec.'), nl.
opis(szyb12) :- write_zolty('Widzisz światło!'), nl.

opis(dach) :- write_zolty('Udało Ci się wyjść na dach! Przed Tobą kable prowadzące w dół. Wciskaj "s", aby zejść na dół'), nl.
opis(zejscie1) :- write_zolty('Zacząłeś schodzić po kablach. Ślisko, ale idzie.'), nl.
opis(zejscie2) :- write_zolty('Jesteś na wysokości około 4. piętra. Trzymaj się mocno!'), nl.
opis(zejscie3) :- write_zolty('Połowa drogi. Nie ma odwrotu...'), nl.
opis(zejscie4) :- write_zolty('Już blisko ziemi. Nie puść się!'), nl.
opis(zejscie5) :- write_zolty('Jeszcze kawałek... już prawie!'), nl.
opis(ziemia) :- write_zolty('Bezpiecznie dotarłeś na dół. Jesteś wolny!'), nl.

moge_uciec :-
    znajduje_sie(manekin, lozko),
    wentylacja_otwarta,
    znajduje_sie(atrapa_wentylacji, wentylacja),
    mam(srubokret),
    mam(plaszcz),
    mam(klej).

koniec :-
    reset_game_1.

instrukcje :-
        write('Dostępne komendy:'), nl,
        write('n. s. e. w.          -- poruszanie się.'), nl,
        write('wez(Przedmiot).      -- podnieś przedmiot.'), nl,
        write('upusc(Przedmiot).    -- upuść przedmiot.'), nl,
        write('zrob_manekina.       -- stwórz manekina.'), nl,
        write('poloz_manekina.      -- połóż manekina.'), nl,
        write('zrob_atrape.         -- zrób atrapę wentylacji.'), nl,
        write('poloz_atrape.        -- umieść atrapę.'), nl,
        write('wierc.               -- rozwierć wentylację.'), nl,
        write('odkrec.              -- odkręć kratkę.'), nl,
        write('rozejrzyj.           -- rozejrzyj się.'), nl,
        write('ekwipunek.           -- sprawdź ekwipunek.'), nl,
        write('instrukcje.          -- pokaż instrukcje.'), nl,
        write('mapa.                -- pokaż mapę celi.'), nl,
        write('halt.                -- zakończ grę.'), nl, nl.


mapa :-
    % nl,
    % write_niebieski('W twojej celi znajdują się następujące miejsca:'), nl,
    % write_niebieski('zlew           '), nl,
    % write_niebieski('środek celi    '), nl,
    % write_niebieski('łóżko          '), nl,
    % write_niebieski('magazyn        '), nl,
    % write_niebieski('krata wentylacyjna '), nl,
    % write_niebieski('toaleta '), nl,
    % write_niebieski('południowy zakątek '), nl,
    % nl.

    write_niebieski("                ┌───────────┐\n
                │    bed    │\n
                └───────────┘\n
                      │\n
                ┌───━─────────┐\n
        ┌─────▶ │ cell center │◀─────┐\n
        │       └─────────────┘      │\n
        │             │              │\n
   ┌────────┐   ┌──────────┐   ┌────────┐\n
   │storage │   │south area│   │ toilet │\n
   └────────┘   └──────────┘   └────────┘\n
        │                            │\n
   ┌───────────┐                 ┌──────┐\n
   │ventilation│                 │ sink │\n
   └───────────┘                 └──────┘\n").




start :-
    instrukcje, 
    rozejrzyj.


reset_game_1 :-
    retractall(jestem_w(_)),
    retractall(znajduje_sie(_, _)),
    retractall(mam(_)),
    retractall(ilosc_uzyc_noza(_)),
    retractall(ilosc_uzyc_lyzki(_)),
    retractall(wentylacja_otwarta),
    retractall(w_szybie),
    retractall(sruby_kratki(_, _)),
    retractall(kratka_usunieta(_)),

    abolish(jestem_w/1),
    abolish(znajduje_sie/2),
    abolish(mam/1),
    abolish(ilosc_uzyc_noza/1),
    abolish(ilosc_uzyc_lyzki/1),
    abolish(wentylacja_otwarta/0),
    abolish(w_szybie/0),
    abolish(sruby_kratki/2),
    abolish(kratka_usunieta/1),

    abolish(idz/1),
    abolish(n/0),
    abolish(e/0),
    abolish(w/0),
    abolish(s/0),
    abolish(rozejrzyj/0),
    abolish(obiekty_w_miejscu/1),
    abolish(wez/1),
    abolish(upusc/1),
    abolish(zrob_manekina/0),
    abolish(poloz_manekina/0),
    abolish(zrob_atrape/0),
    abolish(poloz_atrape/0),
    abolish(wierc/0),
    abolish(odkrec/0),
    abolish(ekwipunek/0),
    abolish(opis/1),
    abolish(moge_uciec/0),
    abolish(instrukcje/0),
    abolish(mapa/0),
    abolish(start/0),

    write('Udało ci się zejść z dachu więzienia, czas na ostatni etap ucieczki. Nowe instrukcje dostępne sa po wpisaniu komendy: "instrukcje."\n'),

    consult('escape_island.pl').
