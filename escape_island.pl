:- use_module(library(ansi_term)).

:- dynamic i_am_at/1, at/2, holding/1, fence_can_cross/1, guards_at/1, warned/1, can_go_on_water/1, know/1, time_left/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(fence_can_cross), retractall(warned(_)), retractall(guards_at(_)), retractall(can_go_on_water).
:- assert(guards_at(docks)).
:- retractall(time_left(_)).
:- assert(time_left(5)).

:- assert(holding(ponton)), assert(holding(bron)), assert(know(friend)), assert(know(blindspot)), assert(holding(ubrania)).

% mapa
i_am_at(wall).

path(wall, s, fence).
path(fence, n, wall).

path(fence, s, beach).
path(beach, n, fence).
path(fence, e, blindspot).
path(blindspot, s, beach).

path(beach, s, sea).
path(beach, w, docks).

path(docks, e, beach).
path(docks, s, sea).

path(sea, s, city).
path(sea, e, island).
path(sea, w, shore).

path(city, car, car_ending).
path(city, bus, bus_ending).

% KOMENDY
% kierunki

n :- idz(n).
s :- idz(s).
e :- idz(e).
w :- idz(w).

idz(Direction) :-
    i_am_at(Here),
    path(Here, Direction, There),
    (
        i_am_at(fence)
        -> determine(fence, Direction),!
        ; i_am_at(docks)
        -> determine(docks), !
        ; (
            There = sea
            -> (
                can_go_on_water
                -> retract(i_am_at(Here)),
                assert(i_am_at(There)),
                deduct_time(1),
                rozejrzyj,!
                ; ansi_format([fg(red)], 'Nie masz czym płynąć!~n', []), !
                % ; write("Nie masz na czym płynąć!"), nl, !
            )
            ; retract(i_am_at(Here)),
            assert(i_am_at(There)),
            rozejrzyj,!
        )
    ).

idz(_) :-
    ansi_format([fg(red)], 'Nie ma tam przejścia!~n', []), !.
    % write('Nie ma tam przejścia.').

rozejrzyj :-
    i_am_at(Place),
    opisz(Place),
    nl,
    notice_objects_at(Place),
    sprawdz_czas,
    nl, !.

notice_objects_at(Place) :-
    at(X, Place),
    write('Jest tu '), write(X), nl,
    fail.

notice_objects_at(_).

czekaj :-
    deduct_time(1),
    i_am_at(Place),
    wait_at(Place), !.

wait_at(fence) :-
    % drugie odjecie czasu jest celowe
    deduct_time(1),
    fence_can_cross
    -> fail
    ; assert(fence_can_cross),
    write("Czekasz i obserwujesz sposób poruszania się świateł. Po kilku cyklach jesteś pewien swojej oceny - masz okazję do przekroczenia ogrodzenia."),!, nl,
    sprawdz_czas.

wait_at(docks) :-
    deduct_time(1),
    retract(guards_at(docks)),
    assert(guards_at(someplace)),
    write("Usiadłeś w miejscu, którego nie skanują reflektory i czekasz... "),
    write("Po dłuższej chwili jeden ze strażników zaczyna głośno narzekać, że na tym posterunku nic się nigdy nie dzieje. "),
    write("Przysłuchujesz się rozmowie i z radością przyjmujesz ich decyzję o skoczeniu po karty."), nl,
    write("Strażnicy odchodzą, masz okazję do działania."), !, nl,
    ansi_format([fg(green)], 'Strażnicy odchodzą, masz okazję do działania.~n', []), !,
    sprawdz_czas.

wait_at(_) :-
    write("zmarnowałeś nieco czasu."),nl,
    sprawdz_czas,!.


determine(fence, Direction) :-
    s = Direction
    ->(
        fence_can_cross
        -> cross_fence(waited)
        ;(
            warned(fence)
            -> die(fence),!
            ; warn_about(fence),!
        ),!
    ),!
    ;(
        know(blindspot)
        -> retract(i_am_at(fence)),
        assert(i_am_at(blindspot)),
        deduct_time(1),
        rozejrzyj
        ; ansi_format([fg(red)], 'Nie ma tam przejścia.~n', []), !
        % ;write('Nie ma tam przejścia.')
    ).

determine(docks) :-
    (
        guards_at(docks)
        -> (     warned(docks)
            -> die(docks)
            ; warn_about(docks)
        )
        ; get_boat()
    ), !.


die(fence) :-
    write("Rzucasz się na ogrodzenie, gdy tylko światło reflektora się od niego odsuwa. "),
    write("Niestety źle wybrałeś chwilę i zanim wspiąłeś się na połowę wysokości otacza cię snop światła."), nl,
    write("Słyszysz syreny alarmowe..."), nl,
    ansi_format([fg(red)],"Umierasz, koniec gry", []), nl,
    finish.

die(docks) :-
    write("Udaje ci się zakraść niedaleko jednego ze strażników. Jednak gdy jest on na wyciągnięcie ręki drugi obraca się w twoją stronę. "),
    write("Rzucasz się w stronę łodzi w akcie desperacji. Skaczesz i wpadasz do niej z impetem, ale z pomostu słyszysz: Wwyłaź! Na ziemię! Podnoś ręce!'."), nl,nl,
    ansi_format([fg(red)],"Umierasz, koniec gry, []"), nl,!,
    finish.

die(_) :-
    ansi_format([fg(red)],"Umierasz, koniec gry", []), nl,
    finish.


uzyj(ponton) :-
    holding(ponton)
    -> (
        i_am_at(beach)
        ->(
            can_go_on_water
            % -> write("Ponton jest już napompowany!"), nl
            -> ansi_format([fg(green)], 'Ponton jest już napompowany!~n', []), !
            ; assert(can_go_on_water),
            deduct_time(2),
            write("Po dłuższym czasie pompowania ponton nabrał kształtu. "), 
            write("Twój improwizowany majstersztyk czeka gotowy na dziewiczą podróż. "),
            write("Masz tylko nadzieję, że zdoła unieść twój ciężar... przynajmniej na tyle długo, by resztę drogi pokonać wpław. "),
            write("Na twoje szczęście morze jest dziś bardzo spokojne, żadna fala nie powinna pokrzyżować twoich planów."),!, nl,nl,
            sprawdz_czas
        )
        % ; write("Nie jest to dobre miejsce do napompowania pontonu!"),!, nl
        ; ansi_format([fg(red)], 'Nie jest to dobre miejsce do napompowania pontonu!~n', []), !
    ),!
    % ; write("Nie masz pontonu!"), nl,!.
    ; ansi_format([fg(red)], 'Nie masz pontonu!~n', []), !. 

uzyj(bron) :-
    holding(bron)
    -> (
        i_am_at(docks)
        ->(
            guards_at(docks)
            -> assert(guards_at(someplace)),
            assert(can_go_on_water),
            write("Strażnicy nie są przygotowani na twój atak. "),
            write("Udaje ci się zakraść niedaleko jednego ze strażników. "),
            write("Rzucasz się na bliższego sobie strażnika, i zdzieliłeś go po głowie. "),
            write("Zanim drugi zorientuje się co się dzieje, także dostaje po głowie."), nl,nl,
            write("Jesteś sam na doku..."), !,nl
            % ; write("Nie ma tu przeciwników."), nl,!
            ; ansi_format([fg(red)], 'Nie ma tu przeciwników.~n', []), !
        )
        % ; write("Nie ma tu przeciwników."), nl,!
        ; ansi_format([fg(red)], 'Nie ma tu przeciwników.~n', []), !
    )
    % ; write("Nie masz broni!"), nl,!.
    ; ansi_format([fg(red)], 'Nie masz broni!.~n', []), !. 

uzyj(_) :-
    % write("Nie możesz tego tu użyć!"), nl.
    ansi_format([fg(red)], 'Nie możesz tego tu użyć!~n', []).

deduct_time(1) :-
    time_left(Past),
    retract(time_left(Past)),
    Now is Past - 1,
    assert(time_left(Now)).

deduct_time(2) :-
    time_left(Past),
    retract(time_left(Past)),
    Now is Past - 1,
    assert(time_left(Now)).

sprawdz_czas :-
    time_left(Hours),
    (
        Hours =< 0
        -> ansi_format([fg(red)], "Słońce wyłoniło się już w pełni nad horyzont. Wiesz, że o tej porze w więzieniu jest pobudka.", []), nl,
        ansi_format([fg(red)], "Z gmachu więzienia wydobywa się wycie syren. Wiedzą o twojej uciecze i mają cię jak na dłoni...", []), nl,
        ansi_format([fg(red)], "Przynajmniej spróbowałeś ...", []), nl,
        die(czas)
        ; (
            Hours = 5
            -> ansi_format([fg(red)], "Zostało ci ~w godzin!~n", [Hours]), !
            ; (
                Hours = 1
                -> write("Horyzont zaczyna odmieniać niewyraźna łuna światła."), nl,
                write("Za niespełna godzinę straznicy odkryją twoją ucieczkę, ale ty będziesz wtedy już daleko... racja?"), nl, nl,
                ansi_format([fg(red)],"Została ci jedna godzina!~n", []), !
                ; ansi_format([fg(red)], "Zostały ci ~w godziny!~n", [Hours]), !
            )
        )
    ).


finish :-
    nl,
    % write('Gra dobiegła końca, wpisz halt. by wyjść'),
    ansi_format([fg(blue)], 'Gra dobiegła końca, wpisz halt. by wyjść.~n', []),
    % nl,
    retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(fence_can_cross), retractall(warned(_)), retractall(guards_at(_)), retractall(can_go_on_water),
    assert(guards_at(docks)),
    retractall(time_left(_)),
    assert(time_left(5)),

    assert(holding(ponton)), assert(holding(bron)), assert(know(friend)), assert(know(blindspot)),assert(holding(ubrania)),
    assert(i_am_at(wall)).

instrukcje :-
    nl,
    write('Dostępne komendy:'), nl,
    write('start.                   -- aby zacząć grę.'), nl,
    write('n.  s.  e.  w.           -- aby pójść w danym kierunku.'), nl,
    write('wez(przedmiot).          -- to pick up an object.'), nl,
    write('upusc(przedmiot).        -- to put down an object.'), nl,
    write("uzyj(przedmiot)          -- to uzyj object from inventory."), nl,
    write("czekaj.                  -- aby zaczekać na korzystniejszy moment do działania"), nl,
    write('rozejrzyj.               -- aby ponownie się rozejrzeć.'), nl,
    write('instrukcje.              -- aby ponownie wyświetlić tą wiadomość.'), nl,
    write('halt.                    -- aby skończyć grę i wyjść.'), nl,
    nl.

start :-
    retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(fence_can_cross), retractall(warned(_)), retractall(guards_at(_)), retractall(can_go_on_water),
    assert(guards_at(docks)),
    retractall(time_left(_)),
    assert(time_left(5)),

    assert(holding(ponton)), assert(holding(bron)), assert(know(friend)), assert(know(blindspot)), assert(holding(ubrania)),
    assert(i_am_at(wall)),
    instrukcje,
    rozejrzyj.

opisz(wall) :-
    write("Po dłużącym się zejściu z radością witasz grunt pod stopami. "),
    write("Mimo, że mury więzienia już masz za sobą, do pokonania została jeszcze bariera z drutu kolczastego i wody zatoki San Francisco."), nl, nl,
    write("Noc niedługo się skończy, a wraz z nią twoja szansa na ucieczkę. "),
    write("Wiesz, że nie masz za dużo czasu."), nl, nl,
    ansi_format([fg(cyan)],"Na południe od ciebie znajduje się ogrodzenie z drutu.~n", []).

% płot - pierwsza przeszkoda
opisz(fence) :-
    (fence_can_cross
    % -> write("Nie masz po co wracać się za płot"), nl
    -> ansi_format([fg(red)], 'Nie masz po co wracać się za płot!~n', [])
    ; write("Przed tobą znajduje się bariera wykonana z drutu kolczastego otaczająca budynek więzienny. "),
    write("Teren wokół niej przeszukują reflektory. Wiesz, że jak cię zobaczą, to koniec. Strzelcy w wieżach strażniczych mają rozkazy zabijać na miejscu."), nl, nl,
    write("Sam drut byłby dość nieprzyjemną przeszkodą, ale wizja dostania kulką powoduje konieczność przemyślanego podejścia do problemu."), nl, nl,
    write("Ale najpierw musisz przedostać się przez płot. "),
    write("Reflektory obracają się w stałym tempie. Ich droga jest przewidywalna."), nl, nl,
    ansi_format([fg(cyan)],"Jeśli spędzisz trochę czasu, znajdziesz moment kiedy nikt nie patrzy na kawałek płotu na tyle długo, by się przeprawić.", []), nl,
    (
        know(blindspot)
        -> nl,ansi_format([fg(cyan)],"Słyszałeś o miejscu, którego nie dosięgają reflektory. ", []),
        write("Jeśli udało by ci się je znaleźć, to drut nie powinien sprawiać większych kłopotów."), nl, nl,
        ansi_format([fg(cyan)], "Powinno być gdzieś na wschód...~n", [])
        ; nl
    ),
    % write("Na południu znajduje sie plaża"), nl
    ansi_format([fg(cyan)], 'Na południu znajduje sie plaża.~n', [])
    ).

opisz(blindspot) :-
    write("Ostrożnie poruszasz się przy murze więzienia dopóki nie znajdziesz się w okolicy o której słyszałeś. "),
    write("Rzeczywiście, reflektory omijają to miejsce! "), nl,
    % write("Spokojnie możesz tu przekroczyć płot i udać się na południe, na plażę.").
    ansi_format([fg(cyan)], 'Spokojnie możesz tu przekroczyć płot i udać się na południe, na plażę.~n', []).

opisz(beach) :-
    write("Czarna tafla wody rozciąga się coraz szerzej przed twoimi oczami. "),
    write("Nocna cisza przerywana jest ciągłym szumem fal rozbijających się o brzeg. "),
    write("Pod twoimi stopami czujesz szorstkie wyboiste kamyki. "),
    write("Dotarłeś do plaży."), nl,nl,
    write("Kilka godzin jakie ci pozostało zmusza cię do wybrania jednej drogi."), nl, nl , !,
    (
        (holding(ponton); holding(bron))
        ->(
            holding(ponton)
            -> ansi_format([fg(cyan)],"Masz przygotowany improwizowany ponton.~n", []),
            ansi_format([fg(cyan)],"Wystarczy go tylko napompować i odpłynąć.~n", []), !
            ; nl,!
        ),
        (
            holding(bron)
            -> ansi_format([fg(cyan)],"W kieszeni nadal znajduje się twoja improwizowana broń. ", []),
            ansi_format([fg(cyan)],"Jeżeli masz trochę szczęścia może będziesz w stanie obezwładnic strażników przy dokach i ukraść motorówkę?~n", []), !
            ; nl,!
        ),!
        ; write("Nie masz niczego co pomogło by wam przeprawic się przez wody zatoki. Chyba nie przemyślałeś tego planu za dobrze."), nl,
        write("Pozostaje ci szukać szczesliwego trafu..."), nl,nl,!
    ),
    % write("Na zachód - doki, na północ ogrodzenie"),nl,
    ansi_format([fg(green)], 'Na zachód - doki, na północ ogrodzenie.~n', []),
    !.


opisz(docks) :-
    write("Bardzo ostrożnymi ruchami, idziesz w stronę doku. Droga jest ciężka i długa. "),
    write("Każda minuta obłożona jest ryzykiem wykrycia, każdy krok musi być wyliczony tak aby nie wejść w snop światła reflektorów. "),
    write("Mimo tego, udaje ci się. "),
    write("Docierasz do doków."),nl,nl,

    write("Dookoła molo kręci się para strażników. Na twoje szczęście, nikt się ciebie tu nie spodziewa. "),
    write("To daje ci okazję. "),
    write("Możesz spróbować ukraść łódź, ale nieważne jak szybko to zrobisz i tak zostaniesz zauważony tak długo jak strażnicy tu stoją. "), nl,
    ansi_format([fg(cyan)],"Możesz poczekać na zmianę warty, przy odrobinie szczęścia tych dwoje postanowi zrobić sobie fajrant wcześniej.~n", []),
    (
        holding(bron)
        -> ansi_format([fg(cyan)], "Masz też swoją broń. Może w końcu jest szansa jej użyć...~n", [])
    ),
    % write("Na południe - morze, na wschód - plaża"), nl.
    ansi_format([fg(green)], 'Na południe - morze, na wschód - plaża.~n', []).

opisz(sea) :-
    write("Wypłynąłeś na otwartą wodę."),
    write("Nie możesz uwierzyć swojemu szczęściu, serce łomocze ci z podniecenia. "),
    write("Mimo, że opuszczasz już wyspę nie oznacza to jeszcze spokoju. "),
    write("Nadal ryzykujesz, że dzień zastanie cię na otwartej wodzie. "),
    write("Wtedy gliny bardzo szybko zrobią z tobą porządek. "),
    write("Twoja ucieczka prawie dobiegła końca. Została tylko kwestia gdzie popłynąć..."),nl,nl,
    ansi_format([fg(cyan)], "Na południu rozciągają się doki i plaże San Francisco, może uda ci się wtopić w tłum jeśli masz cywilne ubrania.~n", []),
    ansi_format([fg(cyan)], "Na wschodzie znajduje się niezamieszkała wyspa. Jest na niej kilka starych fortów w których mógłbyś się schować na pewien czas.~n", []),
    ansi_format([fg(cyan)], "Na zachodzie jest nadbrzeze,", []),
    (
        know(friend)
        -> ansi_format([fg(cyan)]," twój kontakt obiecał że będzie tam czekać.~n", [])
        ; ansi_format([fg(cyan)]," ale nikt tam na ciebie nie czeka, a na piechotę ciężko ci będzie gdzieś dojść.~n", [])
    ).


opisz(island) :-
    write("Płyniesz w stronę pobliskiej wyspy Angel Island. "),
    write("Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami."),nl,nl,
    write("Dopływasz do plaży wyspy. "),
    write("Uciekłeś, na razie. "),
    write("Rosnący tu las i stare budynki dadzą ci schronienie na jakiś czas. Zdołasz przeczekać dzień lub kilka, ale co dalej? "),
    write("Nie masz jedzenia ani pitnej wody. Będziesz musiał niedługo popłynąć na ląd, ale tam będą cię szukać. "),
    write("Wyciągasz ponton na brzeg. Wschodzące słońce pomaga ci szukać miejsca na kryjówkę."), nl,
    win.

opisz(shore):-
    write("Płyniesz w stronę zatoki Kirbiego."),
    write("Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami."),nl,nl,
    write("Dopływasz do brzegu"),
    write("Przed tobą widnieją stare fortyfikacje nadbrzeżne wyrastające ze stromej skarpy."),
    write("Dziurawisz swój ponton i topisz go kilka metrów od brzegu. To powinno opóźnić pościg, na jakiś czas."), nl,
    (
        know(friend)
        -> write("Niedaleko powinien czekać twój znajomy. "),
        write("Jeśli rzeczywiście sie pojawił, nie powinieneś mieć dziś więcej trudności. "),
        write("Zaoferował przetrzymanie cie kilka tygodni w bezpiecznym miejscu, ale dalej co? "),
        write("Nie wrócisz do normalnego życia, nie w tym kraju. "),
        write("Z rozmyśleń wybudza cię dźwięk uruchamianego silnika. Kierujesz się do samochodu na skraju drogi... "),nl,
        win
        ; write("Kierujesz sie w stronę skarpy. Nie wiesz co dalej ze sobą zrobić."), nl,
        write("Idziesz po odsłoniętym stoku na kilka godzin zanim zjawi się tu tłum turystów z całego kraju."), nl,
        (
            holding(ubranie)
            -> write("Przynajmniej masz szansę wtopić się w tłum na jakiś czas. "),
            write("Próbujesz odprężyć się zanim ktoś cię zobaczy. Zostaje ci jedynie iść naprzód."),nl,
            win
            ; write("A oni natychmiast rozpoznają pasiak który masz na sobie. "),
            write("Przynajmniej będziesz mieć co opowiadać po powrocie do celi."),nl,
            lose
        )
    ).

opisz(city) :-
    write("Płyniesz w stronę świateł San francisco. "),
    write("Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami."),nl,nl,
    write("Dopływasz do turystycznego molo niedaleko Golden Gate Beach. "),
    write("Nie spodziewasz się tu dużej ilości ludzi tak blisko do świtu."),nl,nl,
    write("Wychodzisz na brzeg"),nl,nl,
    write("Teraz wystarczy wydostać się z miasta. "),
    write("Gdzieś niedaleko musi znajdowac się jakiś przystanek autobusowy. "),
    write("Możesz wsiąść do byle jakiego i pojechać najdalej jak to możliwe. "),
    (
        holding(ubranie)
        -> write("Masz na sobie cywilne ubrania więc nikt nie powinien cię natychmiast rozpoznać."), nl
        ; write("ale nadal masz na sobie więzienny pasiak, byle kto od razu cię rozpozna."), nl
    ),
    write("Zawsze można ukraść samochód."),nl,
    ansi_format([fg(cyan)],"idz(bus) albo idz(car)~n", []),!.

opisz(bus_ending) :-
    write("Czekasz na przystanku kilkanaście minut, aż przyjedzie autobus. "),
    write("Drzwi otwierają się, a za kierownica siedzi stary kruchy mężczyzna. "),
    write("Wydaje się zmęczony... "),
    (
        holding(ubranie)
        -> write("dzięki czemu nie zauważa, że nie kupujesz biletu. "),
        write("Widocznie uznał, że masz czasowy... albo nie chce się awanturować."), nl,nl,
        win
        ; write("ale widok twojego pasiaka natychmiast go rozbudza. "),
        write("Wiesz, że gdziekolwiek nie wysiądziesz, pogoń będzie dyszeć ci na kark."), nl,nl,
        lose
    ).


opisz(car_ending) :-
    write("Na pobliskim parkingu stoi kilka aut. Zbliżając się dostrzegasz w kilku z nich śpiących ludzi. "),
    write("Po otaczającej cię woni wnioskujesz, że są to imprezowicze. "),
    write("Próbujesz szczęścia z kilkoma samochodami, dopóki nie znajdujesz tego czego szukasz. Ktoś zapomniał zamknąć tylnych drzwi. "),
    write("Szybko wchodzisz do auta i przeciskasz się na siedzenie kierowcy. "),
    write("Na twoje nieszczęście, kiedy próbujesz odpalić zwierając przewody uruchamia się alarm."), nl,
    lose.

% ostrzeżenia
warn_about(fence) :-
    assert(warned(fence)),
    ansi_format([fg(yellow)],"Na pewno chcesz rzucić się przez płot tu i teraz? ", []),
    ansi_format([fg(yellow)],"Najpewniej ci się nie uda bez wcześniejszego przygotowania.~n", []),
    nl,
    fail.

warn_about(docks) :-
    assert(warned(docks)),
    ansi_format([fg(yellow)],"Na pewno chcesz pójść do łodzi mimo obecności strażników?~n", []).

warn_about(_).

% przekraczanie płotu
cross_fence(waited) :-
    retract(i_am_at(fence)),
    assert(i_am_at(beach)),
    write("Wyczekujesz najdłuższego okna i wspinasz sie na płot. "),
    write("Po największym wysiłku od kilku dni spadasz na drugą stronę."), nl, nl,!,
    rozejrzyj, !.

% do łódki
get_boat :-
    write("Wykorzystujesz swoją okazję i szybko wskakujesz do łodzi. "),
    write("Szybko odwiązujesz cumę i zaczynasz wiosłować. "), nl,nl,
    retract(i_am_at(docks)),
    assert(i_am_at(sea)), nl, !,
    deduct_time(1),
    rozejrzyj.

% wygrana i przegrana
win :-
    write("Z twojego starego domu dobiega odległe wycie syren..."), nl,nl,
    ansi_format([fg(green)],"Gratuluje! Udało ci się uciec z więzienia!", []), nl,
    finish.

lose :-
    ansi_format([fg(red)],"Mimo twoich starań, twój plan nie powiódł się na jego ostatnim etapie.~n", []),
    finish.
