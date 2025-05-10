import Control.Monad.State
import System.IO
import Data.List (isPrefixOf)

type Lokacja = String
type Kierunek = String
type Przedmiot = String
type StanGry = (Lokacja, [(Przedmiot, Lokacja)], [Przedmiot])
type Gra a = StateT StanGry IO a

tekstInstrukcji :: [String]
tekstInstrukcji =
  [ "Dostępne komendy:"
  , ""
  , "n. s. e. w.            -- poruszanie się"
  , "wez <Przedmiot>        -- podnieś przedmiot."
  , "upusc <Przedmiot>      -- upuść przedmiot."
  , "zrob_manekina          -- stwórz manekina."
  , "poloz_manekina         -- połóż manekina."
  , "zrob_atrape            -- zrób atrapę wentylacji."
  , "poloz_atrape           -- umieść atrapę."
  , "wierc                  -- rozwierć wentylację."
  , "odkrec                 -- odkręć kratkę."
  , "rozejrzyj              -- rozejrzyj się."
  , "ekwipunek              -- sprawdź ekwipunek."
  , "instrukcje             -- pokaż instrukcje."
  , "mapa                   -- pokaż mapę celi."
  , "exit                   -- zakończ grę."
  , ""
  ]

schematMapy :: [String]
schematMapy = 
    [ "W twojej celi znajdują się następujące miejsca:"
  , "zlew           "
  , "srodek celi    "
  , "lozko          "
  , "magazyn        "
  , "krata wentylacyjna "
  , "toaleta        "
  , "poludnie "
  , ""
  , "                ┌───────────┐"
  , "                │    lozko  │"
  , "                └───────────┘"
  , "                      │"
  , "                ┌───━─────────┐"
  , "        ┌─────  │ srodek celi │ ─────┐"
  , "        │       └─────────────┘      │"
  , "        │             │              │"
  , "   ┌────────┐   ┌────────── ┐   ┌────────┐"
  , "   │ magazyn│   │ zakątek p │   │ toaleta│"
  , "   └────────┘   └────────── ┘   └────────┘"
  , "        │                            │"
  , "   ┌───────────┐                 ┌──────┐"
  , "   │ wentylacja│                 │ zlew │"
  , "   └───────────┘                 └──────┘"
  ]

printBlue, printRed, printYellow, printGreen :: [String] -> IO ()
printBlue text = putStr $ "\x1b[34m" ++ unlines text ++ "\x1b[0m"
printRed text = putStr $ "\x1b[31m" ++ unlines text ++ "\x1b[0m"
printYellow text = putStr $ "\x1b[33m" ++ unlines text ++ "\x1b[0m"
printGreen text = putStr $ "\x1b[32m" ++ unlines text ++ "\x1b[0m"

opis :: String -> IO ()
opis "srodek celi" = printYellow ["Jesteś w centrum swojej celi. Skompletuj ekwipunek do ucieczki."]
opis "lozko" = printYellow ["lozko. Może znajdziesz tu coś, z czego zrobisz manekina?"]
opis "toaleta" = printYellow ["Jesteś przy toalecie. Widzisz śrubokręt."]
opis "magazyn" = printYellow ["Magazynek. Znajdziesz tu narzędzia."]
opis "poludnie" = printYellow ["poludnie. Są tu płaszcze przeciwdeszczowe i klej."]
opis "krata wentylacyjna" = printYellow ["Stoisz przy kratce wentylacyjnej. Chyba tędy musisz uciec?"]
opis "zlew" = printYellow ["Zlew. Jest tu sznurek, drut i kawałek materiału."]
opis "szyb1" = printYellow ["Wpełzasz do ciasnego kanału. Przed Tobą zakręt."]
opis "szyb2" = printYellow ["Bardzo ciasno."]
opis "szyb4" = printYellow ["Kanał schodzi w dół."]
opis "szyb5" = printYellow ["Dalej w dół..."]
opis "szyb7" = printYellow ["Duszne, ciasne przejście. Trzeba iść dalej."]
opis "szyb8" = printYellow ["Coś słychać nad Tobą... już blisko?"]
opis "szyb9" = printYellow ["Pachnie świeżym powietrzem!"]
opis "szyb11" = printYellow ["Kanał skręca na zachód. To już prawie koniec."]
opis "szyb12" = printYellow ["Widzisz światło!"]
opis "dach" = printYellow ["Udało Ci się wyjść na dach! Przed Tobą kable prowadzące w dół. Wciskaj \"s\", aby zejść na dół."]
opis "zejscie1" = printYellow ["Zacząłeś schodzić po kablach. Ślisko, ale idzie."]
opis "zejscie2" = printYellow ["Jesteś na wysokości około 4. piętra. Trzymaj się mocno!"]
opis "zejscie3" = printYellow ["Połowa drogi. Nie ma odwrotu..."]
opis "zejscie4" = printYellow ["Już blisko ziemi. Nie puść się!"]
opis "zejscie5" = printYellow ["Jeszcze kawałek... już prawie!"]
opis "ziemia" = printYellow ["Bezpiecznie dotarłeś na dół. Jesteś wolny!"]
opis _ = printYellow ["Nieznana lokacja."]

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

instrukcje :: IO ()
instrukcje = printYellow tekstInstrukcji

mapa :: IO ()
mapa = printBlue schematMapy

sciezki :: [(Lokacja, Kierunek, Lokacja)]
sciezki =
  [ ("srodek celi", "n", "lozko")
  , ("lozko", "s", "srodek celi")
  , ("srodek celi", "w", "toaleta")
  , ("toaleta", "e", "srodek celi")
  , ("srodek celi", "e", "magazyn")
  , ("magazyn", "w", "srodek celi")
  , ("srodek celi", "s", "poludnie")
  , ("poludnie", "n", "srodek celi")
  , ("magazyn", "e", "krata wentylacyjna")
  , ("krata wentylacyjna", "w", "magazyn")
  , ("toaleta", "w", "zlew")
  , ("zlew", "e", "toaleta")
  ]

znajdzPrzejscie :: Lokacja -> Kierunek -> Maybe Lokacja
znajdzPrzejscie skad kierunek = znajdz sciezki
  where
    znajdz [] = Nothing
    znajdz ((z, k, d):xs)
      | z == skad && k == kierunek = Just d
      | otherwise = znajdz xs


    
wykonajKomende :: String -> Gra ()
wykonajKomende wejscie
  | wejscie `elem` ["n", "s", "e", "w"] = do
      (lok, ps, eq) <- get
      case znajdzPrzejscie lok wejscie of
        Just nowaLokacja -> do
          put (nowaLokacja, ps, eq)
          liftIO $ putStrLn $ "Idziesz na " ++ wejscie ++ " -> " ++ nowaLokacja
          liftIO $ opis nowaLokacja 
        Nothing -> liftIO $ putStrLn "Nie ma przejścia w tym kierunku."
  | wejscie == "rozejrzyj" = do
      (lok, przedmioty, _) <- get
      let lokalne = [p | (p, l) <- przedmioty, l == lok]
      liftIO $ putStrLn $ "Jesteś w: " ++ lok
      liftIO $ opis lok
      if null lokalne
        then liftIO $ putStrLn "Nie ma tu żadnych przedmiotów."
        else liftIO $ putStrLn $ "Widzisz tutaj: " ++ unwords lokalne
  | wejscie == "instrukcje" = liftIO instrukcje
  | wejscie == "mapa" = liftIO mapa
  | wejscie == "ekwipunek" = do
      (_, _, eq) <- get
      if null eq
        then liftIO $ putStrLn "Twój ekwipunek jest pusty."
        else liftIO $ putStrLn $ "Ekwipunek: " ++ unwords eq
  | "wez " `isPrefixOf` wejscie =
    let przedmiot = drop 4 wejscie in do
      (lok, przedmioty, eq) <- get
      if (przedmiot, lok) `elem` przedmioty
        then do
          let nowePrzedmioty = filter (/= (przedmiot, lok)) przedmioty
          put (lok, nowePrzedmioty, przedmiot:eq)
          liftIO $ putStrLn $ "Podnosisz " ++ przedmiot ++ "."
        else liftIO $ putStrLn "Nie ma tu takiego przedmiotu."

 | "upusc " `isPrefixOf` wejscie =
  let przedmiot = drop 6 wejscie in do
    (lok, przedmioty, eq) <- get
    if przedmiot `elem` eq
      then do
        let nowyEq = filter (/= przedmiot) eq
        put (lok, (przedmiot, lok):przedmioty, nowyEq)
        liftIO $ putStrLn $ "Upuszczasz " ++ przedmiot ++ "."
      else liftIO $ putStrLn "Nie masz takiego przedmiotu."
  | wejscie == "wyjscie" = liftIO $ putStrLn "Zakończono grę."
  | otherwise = liftIO $ putStrLn "Nie rozumiem polecenia."

petla :: Gra ()
petla = do
  liftIO $ putStr "> "
  liftIO $ hFlush stdout
  komenda <- liftIO getLine
  if komenda == "wyjscie"
    then wykonajKomende komenda
    else wykonajKomende komenda >> petla

main :: IO ()
main = do
  putStrLn "Witaj w grze!"
  let poczatkowyStan = ("srodek celi",
        [ ("farba", "lozko")
        , ("wlosy", "lozko")
        , ("papier", "lozko")
        , ("srubokret", "toaleta")
        , ("lyzka", "magazyn")
        , ("noz", "magazyn")
        , ("plaszcz", "poludnie")
        , ("klej", "poludnie")
        , ("sznurek", "zlew")
        , ("drut", "zlew")
        , ("material", "zlew")
        ], [])
  opis "srodek celi"
  evalStateT petla poczatkowyStan
