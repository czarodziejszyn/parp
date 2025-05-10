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

-- directions --

-- data Direction = "N" | "S" | "E" | "W"

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

    (("city", "car"), "car_ending"),
    (("city", "bus"), "bus_ending")
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
        printRed ["Słońce wyłoniło się już w pełni nad horyzont. Wiesz, że o tej porze w więzieniu jest pobudka."]
        printRed ["Z gmachu więzienia wydobywa się wycie syren. Wiedzą o twojej uciecze i mają cię jak na dłoni..."]
        printRed ["Przynajmniej spróbowałeś ..."]
    else if hours == 5 then do
        printRed ["Zostało ci 5 godzin"]
    else if hours == 1 then do
        printRed ["Horyzont zaczyna odmieniać niewyraźna łuna światła."]
        printRed ["Za niespełna godzinę straznicy odkryją twoją ucieczkę, ale ty będziesz wtedy już daleko... racja?"]
    else do
        printRed ["Zostały ci " ++ show hours ++ " godziny!"]


-- rozejrzyj

rozejrzyj :: State -> IO()
rozejrzyj state = do
    opisz state
    sprawdzCzas state

opisz :: State -> IO()
opisz state =
    opis lokacja state
    where
        lokacja = location state

opis :: String -> State -> IO ()
opis "wall" _ = do
    printYellow ["Po dłużącym się zejściu z radością witasz grunt pod stopami."]
    printYellow ["Mimo, że mury więzienia już masz za sobą, do pokonania została jeszcze bariera z drutu kolczastego i wody zatoki San Francisco.\n"]
    printYellow ["Noc niedługo się skończy, a wraz z nią twoja szansa na ucieczkę. "]
    printYellow ["Wiesz, że nie masz za dużo czasu.\n"]
    printBlue ["Na południe od ciebie znajduje się ogrodzenie z drutu."]

opis "fence" state  = do
    printYellow ["Przed tobą znajduje się bariera wykonana z drutu kolczastego otaczająca budynek więzienny."]
    printYellow ["Teren wokół niej przeszukują reflektory. Wiesz, że jak cię zobaczą, to koniec. Strzelcy w wieżach strażniczych mają rozkazy zabijać na miejscu.\n"]
    printYellow ["Sam drut byłby dość nieprzyjemną przeszkodą, ale wizja dostania kulką powoduje konieczność przemyślanego podejścia do problemu.\n" ]
    printYellow ["Ale najpierw musisz przedostać się przez płot. "]
    printYellow ["Reflektory obracają się w stałym tempie. Ich droga jest przewidywalna.\n"]
    printBlue ["Jeśli spędzisz trochę czasu, znajdziesz moment kiedy nikt nie patrzy na kawałek płotu na tyle długo, by się przeprawić."]
    if (canFence state)
        then do
            printBlue ["\n Słyszałeś o miejscu, którego nie dosięgają reflektory. "]
            printYellow ["Jeśli udało by ci się je znaleźć, to drut nie powinien sprawiać większych kłopotów.\n"]
            printBlue ["Powinno być gdzieś na wschód..."]
        else do
            printYellow ["\n"]
    printBlue ["Na południu znajduje sie plaża"]

opis "blindspot" _ = do
    printYellow ["Ostrożnie poruszasz się przy murze więzienia dopóki nie znajdziesz się w okolicy o której słyszałeś. "]
    printYellow ["Rzeczywiście, reflektory omijają to miejsce! \n"]
    printBlue ["Spokojnie możesz tu przekroczyć fence i udać się na południe, na plażę."]


readCmd = do
    putStr $ "\x1b[32m" ++ " > " ++ "\x1b[0m"
    cmd <- getLine
    return cmd

gameLoop :: State -> IO()
gameLoop state = do
    opisz state
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