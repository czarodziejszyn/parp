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

idz(Kierunek) :-
        wyczysc_ekran,
        jestem_w(Tutaj),
        sciezka(Tutaj, Kierunek, Tam),
        retract(jestem_w(Tutaj)),
        assert(jestem_w(Tam)),
        !, rozejrzyj_sie.

idz(n) :-
    jestem_w(wentylacja),
    moge_uciec,
    \+ w_szybie,
    retract(jestem_w(wentylacja)),
    assert(jestem_w(szyb1)),
    assert(w_szybie),
    wyczysc_ekran,
    write('Wciskasz się do szybu wentylacyjnego. Jest ciasno i ciemno... użyj skrótów, by uciec.'), nl,
    rozejrzyj_sie, !.

idz(_) :-
    wyczysc_ekran,
    write('Nie możesz tam iść.'), nl.

n :- idz(n).
s :- idz(s).
e :- idz(e).
w :- idz(w).

rozejrzyj_sie :-
        wyczysc_ekran,
        jestem_w(Miejsce),
        opis(Miejsce), nl,
        obiekty_w_miejscu(Miejsce), nl,
        ekwipunek, nl.

obiekty_w_miejscu(Miejsce) :-
        znajduje_sie(X, Miejsce),
        write('Widzisz tutaj: '), write(X), write('.'), nl,
        fail.
obiekty_w_miejscu(_).

wez(X) :-
        wyczysc_ekran,
        mam(X),
        write('Już to masz przy sobie!'), nl, !.

wez(X) :-
        wyczysc_ekran,
        jestem_w(Miejsce),
        znajduje_sie(X, Miejsce),
        retract(znajduje_sie(X, Miejsce)),
        assert(mam(X)),
        write('Podniosłeś: '), write(X), write('.'), nl, !.

wez(_) :-
        wyczysc_ekran,
        write('Tutaj nie ma takiego przedmiotu.'), nl.

upusc(X) :-
        wyczysc_ekran,
        mam(X),
        jestem_w(Miejsce),
        retract(mam(X)),
        assert(znajduje_sie(X, Miejsce)),
        write('Upuściłeś: '), write(X), write('.'), nl, !.

upusc(_) :-
        wyczysc_ekran,
        write('Nie masz tego przy sobie.'), nl.

zrob_manekina :-
        mam(farba), mam(wlosy), mam(papier),
        retract(mam(farba)),
        retract(mam(wlosy)),
        retract(mam(papier)),
        assert(mam(manekin)),
        write('Stworzyłeś manekina!'), nl, !.
zrob_manekina :-
        write('Brakuje Ci materiałów, by stworzyć manekina.'), nl.

poloz_manekina :-
        jestem_w(lozko), mam(manekin),
        retract(mam(manekin)),
        assert(znajduje_sie(manekin, lozko)),
        write('Położyłeś manekina na łóżku.'), nl, !.
poloz_manekina :-
        jestem_w(lozko),
        write('Nie masz manekina, żeby go położyć.'), nl, !.
poloz_manekina :-
        write('Manekina można położyć tylko na łóżku.'), nl.

zrob_atrape_wentylacji :-
        mam(sznurek), mam(drut), mam(material),
        retract(mam(sznurek)),
        retract(mam(drut)),
        retract(mam(material)),
        assert(mam(atrapa_wentylacji)),
        write('Stworzyłeś atrapę wentylacji!'), nl, !.
zrob_atrape_wentylacji :-
        write('Potrzebujesz sznurka, drutu i materiału, by zrobić atrapę.'), nl.

poloz_atrape :-
    jestem_w(wentylacja),
    mam(atrapa_wentylacji),
    wentylacja_otwarta,
    retract(mam(atrapa_wentylacji)),
    assert(znajduje_sie(atrapa_wentylacji, wentylacja)),
    write('Założono atrapę wentylacji. Wygląda całkiem realistycznie... wpiścij  "n", aby uciec.'), nl, !.
poloz_atrape :- jestem_w(wentylacja), \+ mam(atrapa_wentylacji), write('Nie masz atrapy przy sobie.'), nl, !.
poloz_atrape :- jestem_w(wentylacja), \+ wentylacja_otwarta, write('Musisz najpierw rozwiercić prawdziwą wentylację.'), nl, !.
poloz_atrape :- write('Musisz być przy wentylacji, aby umieścić atrapę.'), nl.

wierc :-
    jestem_w(wentylacja),
    (mam(noz); mam(lyzka)),
    (mam(noz), ilosc_uzyc_noza(K), K < 3 -> retract(ilosc_uzyc_noza(K)), K1 is K + 1, assert(ilosc_uzyc_noza(K1)); true),
    (mam(lyzka), ilosc_uzyc_lyzki(S), S < 3 -> retract(ilosc_uzyc_lyzki(S)), S1 is S + 1, assert(ilosc_uzyc_lyzki(S1)); true),
    (ilosc_uzyc_noza(3), mam(noz) -> retract(mam(noz)), write('Nóż się złamał!'), nl; true),
    (ilosc_uzyc_lyzki(3), mam(lyzka) -> retract(mam(lyzka)), write('Łyżka się złamała!'), nl; true),
    (ilosc_uzyc_noza(3), ilosc_uzyc_lyzki(3) ->
        (wentylacja_otwarta -> true ; assert(wentylacja_otwarta)),
        write('Wentylacja została otwarta!'), nl
    ; write('Wierć dalej...'), nl), !.
wierc :- write('Potrzebujesz noża i łyżki, musisz być przy wentylacji.'), nl.

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
        write('Odkręciłeś ostatnią śrubę. Kratka usunięta!'), nl
    ;
        write('Odkręciłeś śrubę. Pozostało: '), write(4 - Nowa), nl
    ), !.
odkrec :- jestem_w(L), member(L, [szyb3, szyb6, szyb10]), \+ mam(srubokret), write('Potrzebujesz śrubokręta.'), nl, !.
odkrec :- jestem_w(L), member(L, [szyb3, szyb6, szyb10]), kratka_usunieta(L), write('Kratka już jest usunięta.'), nl, !.
odkrec :- write('Nie ma tu kratki do odkręcenia.'), nl.

ekwipunek :-
    \+ mam(_), 
    write('Na razie nic nie podniosłeś.'), nl, !.

ekwipunek :-
    write('Masz przy sobie: '), nl,
    forall(mam(X), (write('- '), write(X), nl)).

wyczysc_ekran :- write('\e[H\e[2J').

opis(centrum_celi) :- write('Jesteś w centrum swojej celi. Skompletuj ekwipunek do ucieczki.'), nl.
opis(lozko) :- write('łóżko. Może znajdziesz tu coś, z czego zrobisz manekina?'), nl.
opis(toaleta) :- write('Jesteś przy toalecie. Widzisz śrubokręt.'), nl.
opis(magazynek) :- write('Magazynek. Znajdziesz tu narzędzia.'), nl.
opis(poludnie) :- write('Południowy zakątek. Są tu płaszcze przeciwdeszczowe i klej.'), nl.
opis(wentylacja) :- write('Stoisz przy kratce wentylacyjnej. Chyba tędy musisz uciec?'), nl.
opis(zlew) :- write('Zlew. Jest tu sznurek, drut i kawałek materiału.'), nl.

opis(szyb1) :- write('Wpełzasz do ciasnego kanału. Przed Tobą zakręt.'), nl.
opis(szyb2) :- write('Bardzo ciasno.'), nl.
opis(szyb3) :- (kratka_usunieta(szyb3) -> write('Kratka wschodnia usunięta. Można przejść.'), nl ; write('Kratka blokuje drogę na wschód.'), nl).
opis(szyb4) :- write('Kanał schodzi w dół.'), nl.
opis(szyb5) :- write('Dalej w dół...'), nl.
opis(szyb6) :- (kratka_usunieta(szyb6) -> write('Wejście w górę wolne.'), nl ; write('Kratka blokuje wejście w górę.'), nl).
opis(szyb7) :- write('Duszne, ciasne przejście. Trzeba iść dalej.'), nl.
opis(szyb8) :- write('Coś słychać nad Tobą... już blisko?'), nl.
opis(szyb9) :- write('Pachnie świeżym powietrzem!'), nl.
opis(szyb10) :- (kratka_usunieta(szyb10) -> write('Kratka zdjęta. Można schodzić niżej.'), nl ; write('Kratka blokuje zejście.'), nl).
opis(szyb11) :- write('Kanał skręca na zachód. To już prawie koniec.'), nl.
opis(szyb12) :- write('Widzisz światło!'), nl.

opis(dach) :- write('Udało Ci się wyjść na dach! Przed Tobą kable prowadzące w dół. Wciskaj "s", aby zejść na dół'), nl.
opis(zejscie1) :- write('Zacząłeś schodzić po kablach. Ślisko, ale idzie.'), nl.
opis(zejscie2) :- write('Jesteś na wysokości około 4. piętra. Trzymaj się mocno!'), nl.
opis(zejscie3) :- write('Połowa drogi. Nie ma odwrotu...'), nl.
opis(zejscie4) :- write('Już blisko ziemi. Nie puść się!'), nl.
opis(zejscie5) :- write('Jeszcze kawałek... już prawie!'), nl.
opis(ziemia) :- write('Bezpiecznie dotarłeś na dół. Jesteś wolny!'), nl, write(' Gratulacje — Uciekłeś z więzienia! '), nl, halt.

moge_uciec :-
    znajduje_sie(manekin, lozko),
    wentylacja_otwarta,
    znajduje_sie(atrapa_wentylacji, wentylacja),
    mam(srubokret),
    mam(plaszcz),
    mam(klej).

instrukcje :-
        wyczysc_ekran,
        write('Dostępne polecenia:'), nl,
        write('start.             -- rozpocznij grę'), nl,
        write('n. s. e. w.        -- poruszanie się'), nl,
        write('wez(Przedmiot).     -- podnieś przedmiot'), nl,
        write('upusc(Przedmiot).   -- upuść przedmiot'), nl,
        write('zrob_manekina.      -- stwórz manekina'), nl,
        write('poloz_manekina.     -- połóż manekina'), nl,
        write('zrob_atrape_wentylacji.  -- zrób atrapę wentylacji'), nl,
        write('poloz_atrape.       -- umieść atrapę'), nl,
        write('wierc.              -- rozwierć wentylację'), nl,
        write('odkrec.             -- odkręć kratkę'), nl,
        write('rozejrzyj_sie.      -- rozejrzyj się'), nl,
        write('ekwipunek.          -- sprawdź ekwipunek'), nl,
        write('instrukcje.         -- pokaż instrukcje'), nl,
        write('mapa.         -- pokaż mapę celi'), nl,
        write('halt.               -- zakończ grę'), nl, nl.


mapa :-
    nl,
    write('W twojej celi znajdują się następujące miejsca:'), nl,
    write('zlew           '), nl,
    write('środek celi    '), nl,
    write('łóżko          '), nl,
    write('magazyn        '), nl,
    write('krata wentylacyjna '), nl,
    write('toaleta '), nl,
    write('południowy zakątek '), nl,
    nl.




start :- instrukcje, rozejrzyj_sie.
