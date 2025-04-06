/* dynamic states */
:- dynamic i_am_at/1, at/2, holding/1, fence_can_cross/1, guards_at/1, warned/1, can_go_on_water/1, know/1, time_left/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(fence_can_cross), retractall(warned(_)), retractall(guards_at(_)), retractall(can_go_on_water).
:- assert(guards_at(docks)).
:- retractall(time_left(_)).
:- assert(time_left(5)).
/* ------ carry over from prev stages ------- */
/* ponton, bron, ubranie, know(friend), know(blindspot)*/

 :- assert(holding(ponton)), assert(holding(bron)), assert(know(friend)), assert(know(blindspot)).
/* :- retractall(holding(_)), retractall(know(_)). */

/* map out area */
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

/* map out objects */

/* at(object, place)*/

/* map out npcs */

/* npc_at(npc, place) */

/* COMMANDS */
/* These rules define the direction letters as calls to idź/1. */

n :- idź(n).

s :- idź(s).

e :- idź(e).

w :- idź(w).

idź(Direction) :-
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
                                ; write("Nie masz na czym płynąć"), nl, !
                        )
                        ; retract(i_am_at(Here)),
                        assert(i_am_at(There)),
                        rozejrzyj,!
                )
        ).

idź(_) :-
        write('Nie ma tam przejścia.').

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
        wait_at(Place).

wait_at(fence) :-
        /* second deduct time is on pupouse */
        deduct_time(1),
        fence_can_cross
        -> fail
        ; assert(fence_can_cross),
        write("Czekasz i obserwujesz sposób poruszania się świateł."),nl,
        write("Po kilku cyklach jesteś pewien swojej oceny."), nl,
        write("Masz okazję do przekroczenia ogrodzenia."),!, nl,
        sprawdz_czas.

wait_at(docks) :-
        deduct_time(1),
        retract(guards_at(docks)),
        assert(guards_at(someplace)),
        write("Usiadłeś w miejscu którego nie skanują reflektory i czekasz."), nl,
        write("Po dłuższej chwili jeden ze strażników zaczyna głośno narzekać, że na tym posterunku nic się nigdy nie dzieje."), nl,
        write("Przysłuchujesz się rozmowie i z radością przyjmujesz ich decyzję o skoczeniu po karty."), nl, nl,
        write("Strażnicy odchodzą, masz okazję do działania"), !, nl,
        sprawdz_czas.

wait_at(_) :-
        write("zmarnowałeś nieco czasu"),nl,
        sprawdz_czas.


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
                ;write('Nie ma tam przejścia.')
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
        write("Rzucasz się na ogrodzenie ja tylko reflektor się od niego odsuwa."), nl,
        write("Niestety źle wybrałeś chwilę i zanim wespniesz się na połowę wysokości otacza cię snop światła."), nl,
        write("Słyszysz syreny alarmowe..."), nl,
        write("Umierasz, koniec gry"), nl,
        finish.

die(docks) :-
        write("Udaje ci się zakraść niedaleko jednego ze strażników. Jednak gdy jesteś na wyciągnięcie ręki jeden z nich obraca się w twoją stronę."), nl,
        write("Rzucasz się w stronę łodzi w ostatnim akcie desperacji. Skaczesz i wpadasz do niej z impetem ale z pomostu słyszysz 'wyłaź! na ziemię! podnoś ręce!'."), nl,nl,
        write("Umierasz, koniec gry"), nl,!,
        finish.

die(_) :-
        write("Umierasz, koniec gry"), nl,
        finish.


uzyj(ponton) :-
        holding(ponton)
        -> (
                i_am_at(beach)
                ->(
                        can_go_on_water
                        ->  write("Ponton jest już napompowany!"), nl
                        ; assert(can_go_on_water),
                        deduct_time(2),
                        write("Po dłuższym czasie pompowania ponton nabrał kształtu."), nl,
                        write("Twój improwizowany majstersztyk czeka gotowy na dziewiczą podróż."),nl,
                        write("Masz tylko nadzieję, że zdoła unieść twój ciężar ... przynajmniej na tyle długo by resztę drogi pokonać wpław."), nl,
                        write("Na twoje szczęście morze jest dziś bardzo spokojne, żadna fala nie powinna pokrzyżować twoich planów."),!, nl,nl,
                        sprawdz_czas
                )
                ; write("Nie jest to dobre miejsce do napompowania pontonu"),!, nl
        ),!
        ; write("Nie masz pontonu!"), nl,!.

uzyj(bron) :-
        holding(bron)
        -> (
                i_am_at(docks)
                ->(
                        guards_at(docks)
                        -> assert(guards_at(someplace)),
                        assert(can_go_on_water),
                        write("Strażnicy nie są przygotowani na twój atak."), nl,
                        write("Udaje ci się zakraść niedaleko jednego ze strażników."),nl,
                        write(" Rzucasz się na bliższego sobie strażnika, i zdzieliłeś go po głowie."), nl,
                        write("Zanim drugi zorientuje się co się dzieje, także dostaje po głowie."), nl,nl,
                        write("Jesteś sam na doku..."), !,nl
                        ; write("Nie ma tu przeciwników."), nl,!
                )
                ; write("Nie ma tu przeciwników."), nl,!
        )
        ; write("Nie masz broni!"), nl,!.

uzyj(_) :-
        write("Nie możesz tego tu użyć"), nl.

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
                Hours = 0
                -> write("Słońce wyłoniło się już w pełni nad horyzont. Wiesz, że o tej porze w więzieniu jest pobudka."), nl,
                write("Z gmachu więzienia wydobywa się wycie syren. Wiedzą o twojej uciecze i mają cię jak na dłoni..."), nl,
                write("Przynajmniej spróbowałeś ..."), nl,
                die(czas)
                ; (
                        Hours = 5
                        -> write("Zostało ci "), write(Hours), write(" godzin"),nl,!
                        ; (
                                Hours = 1
                                -> write("Horyzont zaczyna odmieniać niewyraźna łuna światła."), nl,
                                write("Za niespełna godzinę straznicy odkryją twoją ucieczkę, ale ty będziesz wtedy już daleko ... racja?"), nl, nl,
                                write("Została ci jedna godzina"), nl,!
                                ; write("Zostały ci "), write(Hours), write(" godziny"), nl,!
                        )
                )
        ).


finish :-
        nl,
        write('Gra dobiegła końca, wypisz halt. by wyjść'),
        nl.

/* This rule just writes out game insktrukcje. */

insktrukcje :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- aby zacząć grę.'), nl,
        write('n.  s.  e.  w.           -- aby pójść w danym kierunku.'), nl,
        write('wez(Object).             -- to pick up an object.'), nl,
        write('upusc(Object).           -- to put down an object.'), nl,
        write("uzyj(Object)             -- to uzyj object from inventory."), nl,
        write("czekaj.                  -- aby zaczekać na korzystniejszy moment do działania"), nl,
        write('rozejrzyj.               -- aby ponownie się rozejrzeć.'), nl,
        write('instrukcje.              -- aby ponownie wyświetlić tą wiadomość.'), nl,
        write('halt.                    -- aby skończyć grę i wyjść.'), nl,
        nl.

/* This rule prints out insktrukcje and tells where you are. */

start :-
        insktrukcje,
        rozejrzyj.

/* ---- places descriptions ---- */

/* wall - staring place of part 3*/
opisz(wall) :-
        write("Po dłużącym się zejściu z radością witasz grunt pod stopami."), nl,
        write("Mimo, że mury więzienia już macie za sobą, do pokonania została jeszcze bariera z drutu kolczastego i wody zatoki san francisco."), nl, nl,
        write("Noc niedługo się skończy a wraz z nią twoja szansa na ucieczkę."), nl,
        write("Wiesz, że nie masz za dużo czasu"), nl, nl,
        write("Na południe od ciebie znajduje się ogrodzenie z drutu."), nl.

/* fence - first obstacle */
opisz(fence) :-
        (fence_can_cross
        -> write("Nie masz po co wracać się za płot"), nl
        ; write("Przed tobą znajduje się bariera wykonana z drutu kolczastego otaczająca budynek więzienny."), nl,
        write("Teren wokół niej przeszukują reflektory. Wiesz, że jak was zobaczą to koniec, strzelcy w wieżach strażniczych mają rozkazy zabijać na miejscu."), nl, nl,
        write("Sam drut byłby dość nieprzyjemną przeszkodą ale wizja dostania kulką powoduje konieczność przemyślanego podejścia do problemu."), nl, nl,
        write("Ale najpierw musisz przedostać się przez płot."), nl, nl,
        write("Reflektory obracają się w stałym tempie. Ich droga jest przewidywalna."), nl,
        write("Jeśli spędzisz trochę czasu, znajdziesz moment kiedy nikt nie patrzy na tak kawałek płotu dość długo by się przeprawić."), nl,
        (
                know(blindspot)
                -> nl, write("Słyszałeś o miejscu, którego nie dosięgają reflektory."), nl,
                write("Jeśli udało buy ci się je znaleźć to drut nie powinien sprawiać większych kłopotów."), nl,
                write("Powinno być gdzieś na wschód..."),nl,nl
                ; nl
        ),
        write("Na południu znajduje sie plaża"), nl
        ).

opisz(blindspot) :-
        write("Ostrożnie poruszasz się przy murze więzienia dopóki nie znajdziesz się w okolicy o której słyszałeś."),nl,
        write("Rzeczywiście, reflektory omijają to miejsce! "), nl,
        write("Spokojnie możesz tu przekroczyć płot i udać się na południe, na plażę.").

opisz(beach) :-
        write("Czarna tafla wody rozciąga się coraz szerzej przed twoimi oczami."), nl,
        write("Nocna cisza przerywana jest ciągłym szumem fal rozbijających się o brzeg."), nl,
        write("Pod twoimi stopami czujesz szorstkie wyboiste kamyki."), nl,
        write("Dotarłeś do plaży."), nl,nl,
        write("Kilka godzin jakie ci pozostało zmusza cię do wybrania jednej drogi."), nl, nl , !,
        (
                (holding(ponton); holding(bron))
                ->(
                        holding(ponton)
                        -> write("Masz przygotowany improwizowany ponton."), nl,
                        write("Wystarczy tylko go napompować i odpłynąć"), nl,nl,!
                        ; nl,!
                ),
                (
                        holding(bron)
                        -> write("W kieszeni nadal znajduje się twoja improwizowana broń."), nl,
                        write("Jeżeli masz trochę szczęścia może będziesz w stanie obezwładnic strażników przy dokach i ukraść motorówkę?"), nl,nl,!
                        ; nl,!
                ),!
                ; write("Nie masz niczego co pomogło by wam przeprawic się przez wody zatoki. Chyba nie przemyślałeś tego planu za dobrze."), nl,
                write("Pozostaje ci szukać szczesliwego trafu..."), nl,nl,!
        ),
        write("Na zachód - doki, na północ ogrodzenie"),nl,
        !.


opisz(docks) :-
        write("Bardzo ostrożnymi ruchami, idziesz w stronę doku. Droga jest ciężka i długa."), nl,
        write("Każda minuta obłożona jest ryzykiem wykrycia, każdy krok musi być wyliczony tak aby nie wejść w snop światła reflektorów."), nl,
        write("Mimo tego, udaje ci się."), nl,nl,
        write("Docierasz do doków."), nl,nl,

        write("Dookoła molo kręci się para strażników. Na twoje szczęście, nikt się ciebie tu nie spodziewa."), nl,
        write("To daje ci okazję."), nl,
        write("Możesz spróbować ukraść łódź, ale nieważne jak szybko to zrobisz i tak zostaniesz zauważony tak długo jak strażnicy tu stoją."), nl,
        write("Możesz poczekać na zmianę warty, przy odrobinie szczęścia ci dwoje postanowią zrobić sobie fajrant wcześniej."), nl, nl,
        write("Na południe - morze, na wschód - plaża"), nl.

opisz(sea) :-
        write("Wypłynąłeś na otwartą wodę."), nl,
        write("Nie możesz uwierzyć swojemu szczęściu, serce łomocze ci z podniecenia."), nl,nl,
        write("Mimo, że opuszczasz już wyspę nie oznacza to jeszcze spokoju."),nl,
        write("Nadal ryzykujesz, że dzien zastanie cię na otwartej wodzie."),nl,
        write("Wtedy gliny bardzo szybko zrobią z tobą porządek."), nl,
        write("Twoja ucieczka prawie dobiegła końca. Została tylko kwesta gdzie popłynąć..."),nl,nl,
        write("Na południu rozciągają się doki i plaże San Francisco, może uda ci się wtopić w tłum jeśli masz cywilne ubrania"), nl,
        write("Na wschodzie znajduje się niezamieszkałą wyspa. Jest na niej kilka starych fortów w których mógłyś się schować na pewien czas."), nl,
        write("Na zachodzie jest nadbrzerze,"),
        (
                know(friend)
                -> write(" twój przyjaciel obiecał że będzie tam czekać"), nl
                ; write(" ale nikt tam na ciebie nie czeka, a na piechotę ciężko ci będzie gdzieś dojść.")
        ),
        nl.


opisz(island) :-
        write("Płyniesz w stronę pobliskiej Angel island."), nl,
        write("Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami."),nl,nl,
        write("Dopływasz do plaży wyspy."),nl,
        write("Uciekłeś, na razie."),nl,
        write("Rosnący tu las i stare budynki dadzą ci schronienie na jakiś czas. Zdołasz przeczekać dzień lub kilka, ale co dalej?"),nl,
        write("Nie masz jedzenia ani pitnej wody. Będziesz musiał niedługo popłynąć na ląd, ale tam będą cię szukać."),nl,
        write("Wyciągasz ponton na brzeg. Wschodzące słońce pomaga ci szukać miejsca na kryjówkę."), nl,
        win.

opisz(shore):-
        write("Płyniesz w stronę zatoki Kirbiego."), nl,
        write("Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami."),nl,nl,
        write("Dopływasz do brzegu"),nl,
        write("Przed tobą widnieją stare fortyfikacje nadbrzeżne wyrastające ze stromej skarpy."),nl,
        write("Dziurawisz swój ponton i topisz go kilka metrów od brzegu. To powinno opóźnić pościg, na jakiś czas."),nl,
        (
                know(friend)
                -> write("Niedaleko powinien czekać twój znajomy."),nl,
                write("Jeśli rzeczywiście sie pojawił, nie powinieneś mieć dziś więcej trudności."), nl,
                write("Zaoferował przetrzymanie cie kilka tygodni w bezpiecznym miejscu, ale dalej co?"), nl,
                write("Nie wrócisz do normalnego życia, nie w tym kraju."), nl,
                write("Z rozmyśleń wybudza cię dźwięk uruchamianego silnika. Kierujesz się do samochodu na skraju drogi..."),nl,
                win
                ; write("Kierujesz sie w stronę skarpy. Nie wiesz co dalej ze sobą zrobić."), nl,
                write("Idziesz po odsłoniętym stoku na kilka godzin zanim zjawi się tu tłum turystów z całego kraju."), nl,
                (
                        holding(ubranie)
                        -> write("Przynajmniej masz szansę wtopić się w tłum na jakiś czas."),nl,
                        write("Próbujesz odprężyć się zanim ktoś cię zobaczy. Zostaje ci jedynie iść naprzód."),nl,
                        win
                        ; write("A oni natychmiast rozpoznają pasiak który masz na sobie."), nl,
                        write("Przynajmniej będziesz mieć co opowiadać po powrocie do celi."),nl,
                        lose
                )
        ).

opisz(city) :-
        write("Płyniesz w stronę świateł San francisco."),nl,
        write("Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami."),nl,nl,
        write("Dopływasz do turystycznego molo niedaleko golden gate beach."),nl,
        write("Nie spodziewasz się tu dużej ilości ludzi tak blisko do świtu."),nl,nl,
        write("Wchodzisz na brzeg"),nl,nl,
        write("Teraz wystarczy wydostać się z miasta."),
        write("Gdzieś niedaleko musi znajdowac się jakiś przystanek autobusowy."), nl,
        write("Możesz wsiąść do byle jakiego i pojechać najdalej jak to możliwe."), nl,
        (
                holding(ubranie)
                -> write("Masz na sobie cywilne ubrania więc nik nie powinien cię natychmiast rozpoznać."), nl
                ; write("ale nadal masz na sobie więzienny pasiak, byle kto od razu cię rozpozna."), nl
        ),
        write("Zawsze można ukraść samochód."),nl,
        write("idź(bus) albo bo(car)"),!, nl.

opisz(bus_ending) :-
        write("Czekasz na przystanku kilkanaście minut, aż przyjedzie autobus."), nl,
        write("Drzwi otwierają się, a za kierownica siedzi stary kruchy mężczyzna."), nl,
        write("Wydaje się zmęczony ..."),
        (
                holding(ubranie)
                -> write("dzięki czemu nie zauważa, że nie kupujesz biletu."), nl,
                write("Widocznie uznał, że masz czasowy... albo nie chce się awanturować."), nl,nl,
                win
                ; write("ale widok towjego pasiaka natychmiast go rozbudza."), nl,
                write("Wiesz, że gdziekolwiek nie wysiądziesz, pogoń będzie dyszeć ci na kark."), nl,nl,
                lose
        ).


opisz(car_ending) :-
        write("Na pobliskim parkingu stoi kilka aut. Zbliżając się dostrzegasz w kilku z nich śpiących ludzi."), nl,
        write("Po otaczającej cię woni wnioskujesz, że są to imprezowicze."), nl,
        write("Próbujesz szczęścia z kilkoma samochodami, dopóki nie znajdujesz tego czego szukasz. Ktoś zapomniał zamknąć tylnych drzwi."), nl,
        write("Szybko wchodzisz do auta i przeciskasz się na siedzenie kierowcy."), nl,
        write("Na twoje nieszczęście, kiedy próbujesz odpalić zwierając przewody uruchamia się alarm"), nl,
        lose.

/* warnings*/
warn_about(fence) :-
        assert(warned(fence)),
        write("Na pewno chcesz rzucić się przez płot tu i teraz?"), nl,
        write("Najpewniej ci się nie uda bez wcześniejszego przygotowania."), nl,
        nl,
        fail.

warn_about(docks) :-
        assert(warned(docks)),
        write("Na pewno chcesz pójść do łodzi mimo obecności strażników?"), nl.

warn_about(_).


/* fence crossing*/
cross_fence(waited) :-
        retract(i_am_at(fence)),
        assert(i_am_at(beach)),
        write("Wyczekujesz najdłuższego okna i wspinasz sie na płot."), nl,
        write("Po największym wysiłku od kilku dni spadasz na drugą stronę."), nl, nl,!,
        rozejrzyj, !.

/* get in boat*/
get_boat :-
        write("Wykorzystujesz swoją okazję i szybko wskakujesz do łodzi. "), nl,
        write("Szybko odwiązujesz cumę i zaczynasz wiosłować. "), nl,nl,
        retract(i_am_at(docks)),
        assert(i_am_at(sea)), nl, !,
        deduct_time(1),
        rozejrzyj.

/* win and lose */

win :-
        write("Z twojego starego domu dobiega odległe wycie syren..."), nl,nl,
        write("Gratuluje! Udało ci się uciec z więzienia!"), nl,
        finish.

lose :-
        write("Mimo twoich starań, twój plan nie powiódł się na jego ostatnim etapie"), nl,
        finish.