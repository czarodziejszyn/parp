import qualified Data.HashMap.Strict as HM


-- common --


-- print --
printBlue :: [String] -> IO ()
printBlue text = putStr $ "\x1b[34m" ++ unlines text ++ "\x1b[0m"
printRed :: [String] -> IO ()
printRed text = putStr $ "\x1b[31m" ++ unlines text ++ "\x1b[0m"
printYellow :: [String] -> IO ()
printYellow text = putStr $ "\x1b[33m" ++ unlines text ++ "\x1b[0m"
printGreen :: [String] -> IO ()
printGreen text = putStr $ "\x1b[32m" ++ unlines text ++ "\x1b[0m"


-- paths

paths :: HM.HashMap (String, String) String
paths = HM.fromList [
    (("wall", "S"), "fence"),

    (("fence", "N"), "wall"),
    (("fence", "S"), "beach"),
    (("fence", "E"), "blindspot"),

    (("blindspot", "S"), "beach"),

    (("beach", "W"), "docks"),
    (("beach", "S"), "sea"),

    (("docks", "E"), "beach"),
    (("docks", "S"), "sea"),

    (("sea", "S"), "city"),
    (("sea", "W"), "shore"),
    (("sea", "E"), "island"),

    (("city", "W"), "car"),
    (("city", "E"), "bus")
    ]

-- state --

data State = State {
    location :: String,
    inventory :: [String],
    canWater :: Bool,
    canFence :: Bool,
    guardsPresent :: Bool,
    time :: Int
}

-- inventory

checkInventory state item = elem item (inventory state)

-- move

move from dir = do
    case HM.lookup (from, dir) paths of
        Nothing ->  from
        Just new_loc ->  new_loc

idz state dir = state {location = move (location state) dir}
    --let location = location state


-- time

deductTime state t = state{time  = (time state) -t}

sprawdzCzas state = do
    let hours = time state
    if hours <= 0 then do
        printRed [
            "Słońce wyłoniło się już w pełni nad horyzont. Wiesz, że o tej porze w więzieniu jest pobudka."
            , "Z gmachu więzienia wydobywa się wycie syren. Wiedzą o twojej uciecze i mają cię jak na dłoni..."
            , "Przynajmniej spróbowałeś ..."
            ]
    else if hours == 5 then do
        printRed ["Zostało ci 5 godzin"]
    else if hours == 1 then do
        printRed [
            "Horyzont zaczyna odmieniać niewyraźna łuna światła."
            , "Za niespełna godzinę straznicy odkryją twoją ucieczkę, ale ty będziesz wtedy już daleko... racja?"
            ]
    else do
        printRed ["Zostały ci " ++ show hours ++ " godziny!"]




-- look

look :: State -> IO()
look state = do
    describeDispatch state
    sprawdzCzas state

describeDispatch :: State -> IO()
describeDispatch state =
    describe (location state)
--    where
--        lokacja =

describe :: String -> IO ()
describe "wall" = do
    printYellow [
        "Po dłużącym się zejściu z radością witasz grunt pod stopami."
        , "Mimo, że mury więzienia już masz za sobą, do pokonania została jeszcze bariera z drutu kolczastego i wody zatoki San Francisco.\n\n"
        , "Noc niedługo się skończy, a wraz z nią twoja szansa na ucieczkę. "
        , "Wiesz, że nie masz za dużo czasu.\n\n"]
    printGreen ["Na południe od ciebie znajduje się ogrodzenie z drutu."]

describe "fence" = do
    printYellow [
        "Przed tobą znajduje się bariera wykonana z drutu kolczastego otaczająca budynek więzienny."
        , "Teren wokół niej przeszukują reflektory. Wiesz, że jak cię zobaczą, to koniec. Strzelcy w wieżach strażniczych mają rozkazy zabijać na miejscu."
        , "Sam drut byłby dość nieprzyjemną przeszkodą, ale wizja dostania kulką powoduje konieczność przemyślanego podejścia do problemu.\n"
        , "Ale najpierw musisz przedostać się przez płot. \n\n"
        , "Reflektory obracają się w stałym tempie. Ich droga jest przewidywalna.\n"
        ]
    printBlue [
        "Jeśli spędzisz trochę czasu, znajdziesz moment kiedy nikt nie patrzy na kawałek płotu na tyle długo, by się przeprawić.\n"
        ,"Słyszałeś o miejscu, którego nie dosięgają reflektory. "
        ]
    printYellow ["Jeśli udało by ci się je znaleźć, to drut nie powinien sprawiać większych kłopotów.\n\n"]
    printGreen [
        "Powinno być gdzieś na wschód..."
        , "Na południu znajduje sie plaża"
        ]

describe "blindspot" = do
    printYellow [
        "Ostrożnie poruszasz się przy murze więzienia dopóki nie znajdziesz się w okolicy o której słyszałeś. "
        , "Rzeczywiście, reflektory omijają to miejsce! \n"
        ]
    printGreen ["Spokojnie możesz tu przekroczyć ogrodzenie i udać się na południe, na plażę."]

describe "beach" = do
    printYellow [
        "Czarna tafla wody rozciąga się coraz szerzej przed twoimi oczami. "
        , "Nocna cisza przerywana jest ciągłym szumem fal rozbijających się o brzeg. "
        , "Pod twoimi stopami czujesz szorstkie wyboiste kamyki. "
        , "Dotarłeś do plaży.\n\n"
        , "Kilka godzin jakie ci pozostało zmusza cię do wybrania jednej drogi.\n\n"
        ]
    printBlue [
        "Masz przygotowany improwizowany ponton"
        , "Wystarczy go tylko napompować i odpłynąć\n"
        , "W kieszeni nadal znajduje się twoja improwizowana broń. "
        , "Jeżeli masz trochę szczęścia może będziesz w stanie obezwładnic strażników przy dokach i ukraść motorówkę?\n"
        ]
    printGreen ["Na zachód - doki, na północ ogrodzenie."]

describe "docks" = do
    printYellow [
        "Bardzo ostrożnymi ruchami, idziesz w stronę doku. Droga jest ciężka i długa. "
        , "Każda minuta obłożona jest ryzykiem wykrycia, każdy krok musi być wyliczony tak aby nie wejść w snop światła reflektorów. "
        , "Mimo tego, udaje ci się. \n"
        , "Docierasz do doków.\n"
        , "Dookoła molo kręci się para strażników. Na twoje szczęście, nikt się ciebie tu nie spodziewa."
        , "To daje ci okazję. "
        , "Możesz spróbować ukraść łódź, ale nieważne jak szybko to zrobisz i tak zostaniesz zauważony tak długo jak strażnicy tu stoją. \n\n"
        ]
    printBlue [
        "Możesz poczekać na zmianę warty, przy odrobinie szczęścia tych dwoje postanowi zrobić sobie fajrant wcześniej.\n"
        , "Masz też swoją broń. Może w końcu jest szansa jej użyć...\n\n"
        ]
    printGreen ["Na południe - morze, na wschód - plaża."]

describe "sea" = do
    printYellow [
        "Wypłynąłeś na otwartą wodę."
        , "Nie możesz uwierzyć swojemu szczęściu, serce łomocze ci z podniecenia. "
        , "Mimo, że opuszczasz już wyspę nie oznacza to jeszcze spokoju. "
        , "Nadal ryzykujesz, że dzień zastanie cię na otwartej wodzie. "
        , "Wtedy gliny bardzo szybko zrobią z tobą porządek. "
        , "Twoja ucieczka prawie dobiegła końca. Została tylko kwestia gdzie popłynąć...\n\n"
        ]
    printGreen [
        "Na południu rozciągają się doki i plaże San Francisco, może uda ci się wtopić w tłum jeśli masz cywilne ubrania.\n"
        , "Na wschodzie znajduje się niezamieszkała wyspa. Jest na niej kilka starych fortów w których mógłbyś się schować na pewien czas.\n"
        , "Na zachodzie jest nadbrzeze, twój kontakt obiecał że będzie tam czekać.\n"
        ]

describe "island" = do
    printYellow [
        "Płyniesz w stronę pobliskiej wyspy Angel Island. "
        , "Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami.\n\n"
        , "Dopływasz do plaży wyspy. \n"
        , "Uciekłeś, na razie. "
        , "Rosnący tu las i stare budynki dadzą ci schronienie na jakiś czas. Zdołasz przeczekać dzień lub kilka, ale co dalej? "
        , "Nie masz jedzenia ani pitnej wody. Będziesz musiał niedługo popłynąć na ląd, ale tam będą cię szukać. "
        , "Wyciągasz ponton na brzeg. Wschodzące słońce pomaga ci szukać miejsca na kryjówkę.\n\n"
        ]

describe "shore" = do
    printYellow [
        "Płyniesz w stronę zatoki Kirbiego."
        , "Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami.\n\n"
        , "Dopływasz do brzegu"
        , "Przed tobą widnieją stare fortyfikacje nadbrzeżne wyrastające ze stromej skarpy."
        , "Dziurawisz swój ponton i topisz go kilka metrów od brzegu. To powinno opóźnić pościg, na jakiś czas."
        , "Niedaleko powinien czekać twój znajomy. "
        , "Jeśli rzeczywiście sie pojawił, nie powinieneś mieć dziś więcej trudności. "
        , "Zaoferował przetrzymanie cie kilka tygodni w bezpiecznym miejscu, ale dalej co? "
        , "Nie wrócisz do normalnego życia, nie w tym kraju. "
        , "Z rozmyśleń wybudza cię dźwięk uruchamianego silnika. Kierujesz się do samochodu na skraju drogi...\n\n "
        ]

describe "city" = do
    printYellow [
        "Płyniesz w stronę świateł San francisco. "
        , "Przeprawa wydaje się trwać znacznie dłużej niż powinna. Emocje szarpią twoimi nerwami.\n\n"
        , "Dopływasz do turystycznego molo niedaleko Golden Gate Beach. "
        , "Nie spodziewasz się tu dużej ilości ludzi tak blisko do świtu. \n\n"
        , "Wychodzisz na brzeg\n\n"
        , "Teraz wystarczy wydostać się z miasta. \n\n"
        ]
    printBlue [
        "Gdzieś niedaleko musi znajdowac się jakiś przystanek autobusowy. "
        , "Możesz wsiąść do byle jakiego i pojechać najdalej jak to możliwe. "
        , "Masz na sobie cywilne ubrania więc nikt nie powinien cię natychmiast rozpoznać.\n\n"
        , "Zawsze możesz też ukraść samochód.\n\n"
        ]
    printGreen [
        "Przystanek jest na wschodzie, parking na zachodzie"
        ]

describe "bus" = do
    printYellow [
        "Czekasz na przystanku kilkanaście minut, aż przyjedzie autobus. "
        , "Drzwi otwierają się, a za kierownica siedzi stary kruchy mężczyzna. "
        , "Wydaje się zmęczony...\n "
        , "dzięki czemu nie zauważa, że nie kupujesz biletu."
        , "Widocznie uznał, że masz czasowy... albo nie chce się awanturować."
        ]

describe "car" = do
    printYellow [
        "Na pobliskim parkingu stoi kilka aut. Zbliżając się dostrzegasz w kilku z nich śpiących ludzi. "
        , "Po otaczającej cię woni wnioskujesz, że są to imprezowicze. "
        , "Próbujesz szczęścia z kilkoma samochodami, dopóki nie znajdujesz tego czego szukasz. Ktoś zapomniał zamknąć tylnych drzwi. "
        , "Szybko wchodzisz do auta i przeciskasz się na siedzenie kierowcy. "
        , "Na twoje nieszczęście, kiedy próbujesz odpalić zwierając przewody uruchamia się alarm."
        ]

win = do
    printYellow ["Z twojego starego domu dobiega odległe wycie syren..."]
    printGreen ["Gratuluje! Udało ci się uciec z więzienia!"]

lose = do
    printRed ["Mimo twoich starań, twój plan nie powiódł się na jego ostatnim etapie."]

die "fence" = do
    printYellow [
        "Rzucasz się na ogrodzenie, gdy tylko światło reflektora się od niego odsuwa. "
        , "Niestety źle wybrałeś chwilę i zanim wspiąłeś się na połowę wysokości otacza cię snop światła."
        , "Słyszysz syreny alarmowe...\n\n"
        ]
    printRed ["Umierasz, koniec gry"]

die "docks" = do
    printYellow [
        "Udaje ci się zakraść niedaleko jednego ze strażników. Jednak gdy jest on na wyciągnięcie ręki drugi obraca się w twoją stronę. "
        , "Rzucasz się w stronę łodzi w akcie desperacji. Skaczesz i wpadasz do niej z impetem, ale z pomostu słyszysz: Wyłaź! Na ziemię! Podnoś ręce!'."
        ]
    printRed ["Umierasz, koniec gry"]

die _ = do
    printRed ["Umierasz, koniec gry"]

readCmd = do
    putStr $ "\x1b[32m" ++ " > " ++ "\x1b[0m"
    cmd <- getLine
    return cmd

gameLoop :: State -> IO()
gameLoop state = do
    describeDispatch state
    cmd <- readCmd
    printRed [cmd]
    -- let next_state = state{}
    -- gameLoop next_state

main :: IO ()
main = do
    let initState = State{
        location = "wall",
        inventory = ["ponton"],
        canWater = False,
        canFence = False,
        guardsPresent = True,
        time = 5
    }
    printGreen ["loaded \n"]
    gameLoop initState


-- initState = State{location = "wall",inventory = ["ponton"],canWater = False,canFence = False,guardsPresent = True, time = 5}