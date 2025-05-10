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


-- instructions --

-- end common --

opis "mur" = do
    printYellow ["Po dłużącym się zejściu z radością witasz grunt pod stopami."]
    printYellow ["Mimo, że mury więzienia już masz za sobą, do pokonania została jeszcze bariera z drutu kolczastego i wody zatoki San Francisco.\n"]
    printYellow ["Noc niedługo się skończy, a wraz z nią twoja szansa na ucieczkę. "]
    printYellow ["Wiesz, że nie masz za dużo czasu.\n"]
    printBlue ["Na południe od ciebie znajduje się ogrodzenie z drutu."]