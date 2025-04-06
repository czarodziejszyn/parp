/* dynamic states */
:- dynamic i_am_at/1, at/2, holding/1, fence_can_cross/1, guards_at/1, warned/1, can_go_on_water/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(fence_can_cross), retractall(warned(_)), retractall(guards_at(_)), retractall(can_go_on_water).
:- assert(guards_at(docks)).
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
path(docks, w, sea).

path(sea, s, city).
path(sea, n, island).
path(sea, w, shore).

path(city, car, car_ending).
path(city, bus, bus_ending).

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
        -> determine(fence),!
        ; i_am_at(docks)
        -> determine(docks), !
        ; (
                There = sea
                -> (
                        can_go_on_water
                        -> retract(i_am_at(Here)),
                        assert(i_am_at(There)),
                        look,!
                        ; write("Nie masz na czym płynąć"), nl
                )
                ; retract(i_am_at(Here)),
                assert(i_am_at(There)),
                look,!
        )
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
        fence_can_cross
        -> fail
        ; assert(fence_can_cross),
        write("Czekasz i obserwujesz sposób poruszania się świateł."),nl,
        write("Po kilku cyklach jesteś pewien swojej oceny."), nl,
        write("Masz okazję do przekroczenia ogrodzenia."),!, nl.

wait_at(docks) :-
        retract(guards_at(docks)),
        assert(guards_at(someplace)),
        write("Usiadłeś w miejscu którego nie skanują reflektory i czekasz."), nl,
        write("Po dłuższej chwili jeden ze strażników zaczyna głośno narzekać, że na tym posterunku nic się nigdy nie dzieje."), nl,
        write("Przysłuchujesz się rozmowie i z radością przyjmujesz ich decyzję o skoczeniu po karty."), nl, nl,
        write("Strażnicy odchodzą, masz okazję do działania"), !, nl.

wait_at(_) :-
        write("zmarnowałeś nieco czasu"),nl.


determine(fence) :-
        (
                fence_can_cross
                -> cross_fence(waited)
                ;(
                        warned(fence)
                        -> die(fence),!
                        ; warn_about(fence),!
                ),!
        ),!.

determine(docks) :-
        (
                guards_at(someplace)
                -> get_boat()
                ; (     warned(docks)
                        -> die(docks)
                        ; warn_about(docks)
                )
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


use(float) :-
        (
                i_am_at(beach)
                -> assert(can_go_on_water),
                write("Po dłuższym czasie pompowania ponton nabrał kształtu."), nl,
                write("Twój improwizowany majstersztyk czeka gotowy na dziewiczą podróż."),nl,
                write("Masz tylko nadzieję, że zdoła unieść twój ciężar ... przynajmniej na tyle długo by resztę drogi pokonać wpław."), nl,
                write("Na twoje szczęście morze jest dziś bardzo spokojne, żadna fala nie powinna pokrzyżować twoich planów."), nl,nl
                ; write("Nie jest to dobre miejsce do napompowania pontonu"), nl
        ).

use(_) :-
        write("Nie możesz tego tu użyć"), nl.


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
        write("use(Object)        -- to use object from inventory."), nl,

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
        write("Jeśli spędzisz trochę czasu, znajdziesz moment kiedy nikt nie patrzy na tak kawałek płotu dość długo by się przeprawić."), nl, nl,
        write("Na południu znajduje sie plaża"), nl
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
                write("Pozostaje ci spróbować płynąc wpław..."), nl,nl,!
        ),
        write("Na zachód - doki, na północ ogrodzenie"),nl,
        !.


describe(docks) :-
        write("Bardzo ostrożnymi ruchami, idziesz w stronę doku. Droga jest ciężka i długa."), nl,
        write("Każda minuta obłożona jest ryzykiem wykrycia, każdy krok musi być wyliczony tak aby nie wejść w snop światła reflektorów."), nl,
        write("Mimo tego, udaje ci się."), nl,nl,
        write("Docierasz do doków."), nl,nl,

        write("Dookoła molo kręci się para strażników. Na twoje szczęście, nikt się ciebie tu nie spodziewa."), nl,
        write("To daje ci okazję."), nl,
        write("Możesz spróbować ukraść łódź, ale nieważne jak szybko to zrobisz i tak zostaniesz zauważony tak długo jak strażnicy tu stoją."), nl,
        write("Możesz poczekać na zmianę warty, przy odrobinie szczęścia ci dwoje postanowią zrobić sobie fajrant wcześniej."), nl, nl,
        write("Na zachód - morze, na wschód - plaża"), nl.

describe(sea) :-
        write("Wypłynąłeś na otwartą wodę."), nl,
        write("Nie możesz uwierzyć swojemu szczęściu, serce łomocze ci z podniecenia."), nl,nl,
        write("Mimo, że opuszczasz już wyspę nie oznacza to jeszcze spokoju."),nl,
        write("Nadal ryzykujesz, że dzien zastanie cię na otwartej wodzie."),nl,
        write("Wtedy gliny bardzo szybko zrobią z tobą porządek."), nl,
        write("Twoja ucieczka prawie dobiegła końca. Została tylko kwesta gdzie popłynąć..."),nl,nl,
        write("Na południu rozciągają się doki i plaże San Francisco, może uda ci się wtopić w tłum jeśli masz cywilne ubrania"), nl,
        write("Na północy znajduja się niezamieszkałą wyspa. Jest na niej kilka starych fortów w których mógłyś się schować na pewien czas."), nl,
        write("Na zachodzie jest nadbrzerze,"),
        (
                friend
                -> write(" twój przyjaciel obiecał że będzie tam czekać"), nl
                ; write(" ale nikt tam na ciebie nie czeka, a na piechotę ciężko ci będzie gdzieś dojść.")
        ),
        nl.


describe(island) :-
        write("Płyniesz w stronę pobliskiej Angel island."), nl,
        write("Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami."),nl,nl,
        write("Dopływasz do plaży wyspy."),nl,
        write("Uciekłeś, na razie."),nl,
        write("Rosnący tu las i stare budynki dadzą ci schronienie na jakiś czas. Zdołasz przeczekać dzień lub kilka, ale co dalej?"),nl,
        write("Nie masz jedzenia ani pitnej wody. Będziesz musiał niedługo popłynąć na ląd, ale tam będą cię szukać."),nl,
        write("Wyciągasz ponton na brzeg. Wschodzące słońce pomaga ci szukać miejsca na kryjówkę."), nl,
        win.

describe(shore):-
        write("Płyniesz w stronę zatoki Kirbiego."), nl,
        write("Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami."),nl,nl,
        write("Dopływasz do brzegu"),nl,
        write("Przed tobą widnieją stare fortyfikacje nadbrzeżne wyrastające ze stromej skarpy."),nl,
        write("Dziurawisz swój ponton i topisz go kilka metrów od brzegu. To powinno opóźnić pościg, na jakiś czas."),nl,
        (
                friend
                -> write("Niedaleko powinien czekać twój znajomy."),nl,
                write("Jeśli rzeczywiście sie pojawił, nie powinieneś mieć dziś więcej trudności."), nl,
                write("Zaoferował przetrzymanie cie kilka tygodni w bezpiecznym miejscu, ale dalej co?"), nl,
                write("Nie wrócisz do normalnego życia, nie w tym kraju."), nl,
                write("Z rozmyśleń wybudza cię dźwięk uruchamianego silnika. Kierujesz się do samochodu na skraju drogi..."),nl,
                win
                ; write("Kierujesz sie w stronę skarpy. Nie wiesz co dalej ze sobą zrobić."), nl,
                write("Idziesz po odsłoniętym stoku na kilka godzin zanim zjawi się tu tłum turystów z całego kraju."), nl,
                (
                        holding(clothes)
                        -> write("Przynajmniej masz szansę wtopić się w tłum na jakiś czas."),nl,
                        write("Próbujesz odprężyć się zanim ktoś cię zobaczy. Zostaje ci jedynie iść naprzód."),nl,
                        win
                        ; write("A oni natychmiast rozpoznają pasiak który masz na sobie."), nl,
                        write("Przynajmniej będziesz mieć co opowiadać po powrocie do celi."),nl,
                        lose
                )
        ).

describe(city) :-
        write("Płyniesz w stronę świateł San francisco."),nl,
        write("Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami."),nl,nl,
        write("Dopływasz do turystycznego molo niedaleko golden gate beach."),nl,
        write("Nie spodziewasz się tu dużej ilości ludzi tak blisko do świtu."),nl,nl,
        write("Wchodzisz na brzeg"),nl,nl,
        write("Teraz wystarczy wydostać się z miasta."),
        write("Gdzieś niedaleko musi znajdowac się jakiś przystanek autobusowy."), nl,
        write("Możesz wsiąść do byle jakiego i pojechać najdalej jak to możliwe."), nl,
        (
                holding(clothes)
                -> write("Masz na sobie cywilne ubrania więc nik nie powinien cię natychmiast rozpoznać."), nl
                ; write("ale nadal masz na sobie więzienny pasiak, byle kto od razu cię rozpozna."), nl
        ),
        write("Zawsze można ukraść samochód."),nl,
        write("go(bus) albo bo(car)"),!, nl.

describe(bus_ending) :-
        write("Czekasz na przystanku kilkanaście minut, aż przyjedzie autobus."), nl,
        write("Drzwi otwierają się, a za kierownica siedzi stary kruchy mężczyzna."), nl,
        write("Wydaje się zmęczony ..."),
        (
                holding(clothes)
                -> write("dzięki czemu nie zauważa, że nie kupujesz biletu."), nl,
                write("Widocznie uznał, że masz czasowy... albo nie chce się awanturować."), nl,nl,
                win
                ; write("ale widok towjego pasiaka natychmiast go rozbudza."), nl,
                write("Wiesz, że gdziekolwiek nie wysiądziesz, pogoń będzie dyszeć ci na kark."), nl,nl,
                lose
        ).


describe(car_ending) :-
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
        look, !.

/* get in boat*/
get_boat :-
        write("Wykorzystujesz swoją okazję i szybko wskakujesz do łodzi. "), nl,
        write("Szybko odwiązujesz cumę i zaczynasz wiosłować. "), nl,nl,
        retract(i_am_at(docks)),
        assert(i_am_at(sea)), nl, !,
        look.

/* win and lose */

win :-
        write("Z twojego starego domu dobiega odległe wycie syren..."), nl,nl,
        write("Gratuluje! Udało ci się uciec z więzienia!"), nl,
        finish.

lose :-
        write("Mimo twoich starań, twój plan nie powiódł się na jego ostatnim etapie"), nl,
        finish.