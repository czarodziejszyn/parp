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
    (("fence", "N"), "wall")
    ]

-- state --

data State = State {
    location :: String,
    inventory :: [String]
}

move from dir = do
    case HM.lookup (from, dir) paths of
        Nothing ->  from
        Just new_loc ->  new_loc




idz state dir = state {location = move (location state) dir}
    --let location = location state




opisDispatcher state =
    opis lokacja
    where
        lokacja = location state

opis :: String -> IO ()
opis "wall" = do
    printYellow ["Po dłużącym się zejściu z radością witasz grunt pod stopami."]
    printYellow ["Mimo, że mury więzienia już masz za sobą, do pokonania została jeszcze bariera z drutu kolczastego i wody zatoki San Francisco.\n"]
    printYellow ["Noc niedługo się skończy, a wraz z nią twoja szansa na ucieczkę. "]
    printYellow ["Wiesz, że nie masz za dużo czasu.\n"]
    printBlue ["Na południe od ciebie znajduje się ogrodzenie z drutu."]

-- opis "fence"  = do
--    printYellow ["]

opis "blindspot" = do
    printYellow ["Ostrożnie poruszasz się przy murze więzienia dopóki nie znajdziesz się w okolicy o której słyszałeś. "]
    printYellow ["Rzeczywiście, reflektory omijają to miejsce! \n"]
    printBlue ["Spokojnie możesz tu przekroczyć fence i udać się na południe, na plażę."]


readCmd = do
    putStr $ "\x1b[32m" ++ " > " ++ "\x1b[0m"
    cmd <- getLine
    return cmd

gameLoop state = do
    opisDispatcher state
    cmd <- readCmd
    printRed [cmd]
    let next_state = State "blindspot" ["ponton"]
    gameLoop next_state


main = do
    let initState = State {
        location = "wall",
        inventory = ["ponton"]
    }
    printGreen ["loaded \n"]
    gameLoop initState
