import Control.Monad.State
import System.IO
import Data.List (isPrefixOf)

type Lokacja = String
type Kierunek = String
type Przedmiot = String
type StanGry = (Lokacja, [(Przedmiot, Lokacja)], [Przedmiot], Int, Int)
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
  , ("szyb1", "n", "szyb2")
  , ("szyb2", "e", "szyb3")
  , ("szyb3", "e", "szyb4")
  , ("szyb4", "s", "szyb5")
  , ("szyb5", "s", "szyb6")
  , ("szyb6", "n", "szyb7")
  , ("szyb7", "n", "szyb8")
  , ("szyb8", "n", "szyb9")
  , ("szyb9", "n", "szyb10")
  , ("szyb10", "s", "szyb11")
  , ("szyb11", "w", "szyb12")
  , ("szyb12", "n", "dach")
  ]

spelnioneWarunkiUcieczki :: StanGry -> Bool
spelnioneWarunkiUcieczki (lok, ps, eq, _, _) =
  ("manekin", "lozko") `elem` ps &&
  ("atrapa_wentylacji", "krata wentylacyjna") `elem` ps &&
  all (`elem` eq) ["srubokret", "plaszcz", "klej"]

znajdzPrzejscie :: Lokacja -> Kierunek -> StanGry -> Maybe Lokacja
znajdzPrzejscie "krata wentylacyjna" "n" stan
  | spelnioneWarunkiUcieczki stan = Just "szyb1"
  | otherwise = Nothing
znajdzPrzejscie skad kierunek _ =
  znajdz sciezki
  where
    znajdz [] = Nothing
    znajdz ((z, k, d):xs)
      | z == skad && k == kierunek = Just d
      | otherwise                  = znajdz xs

    
wykonajKomende :: String -> Gra ()
wykonajKomende wejscie
  | wejscie `elem` ["n", "s", "e", "w"] = do
      (lok, ps, eq, nozUzycia, lyzkaUzycia)  <- get
      case znajdzPrzejscie lok wejscie (lok, ps, eq, nozUzycia, lyzkaUzycia) of
        Just nowaLokacja -> do
              liftIO $ printGreen [lok]
              liftIO $ printGreen [wejscie]
              liftIO $ print (spelnioneWarunkiUcieczki (lok, ps, eq, nozUzycia, lyzkaUzycia))
              
              put (nowaLokacja, ps, eq, nozUzycia, lyzkaUzycia)
              liftIO $ putStrLn $ "Idziesz na " ++ wejscie ++ " -> " ++ nowaLokacja
              liftIO $ opis nowaLokacja 
        Nothing -> liftIO $ putStrLn "Nie ma przejścia w tym kierunku."
  | wejscie == "rozejrzyj" = do
      (lok, przedmioty, _, nozUzycia, lyzkaUzycia) <- get
      let lokalne = [p | (p, l) <- przedmioty, l == lok]
      liftIO $ putStrLn $ "Jesteś w: " ++ lok
      liftIO $ opis lok
      if null lokalne
        then liftIO $ putStrLn "Nie ma tu żadnych przedmiotów."
        else liftIO $ putStrLn $ "Widzisz tutaj: " ++ unwords lokalne
  | wejscie == "instrukcje" = liftIO instrukcje
  | wejscie == "mapa" = liftIO mapa
  | wejscie == "ekwipunek" = do
      (_, _, eq, nozUzycia, lyzkaUzycia) <- get
      if null eq
        then liftIO $ putStrLn "Twój ekwipunek jest pusty."
        else liftIO $ putStrLn $ "Ekwipunek: " ++ unwords eq
  | "wez " `isPrefixOf` wejscie =
    let przedmiot = drop 4 wejscie in do
      (lok, przedmioty, eq, nozUzycia, lyzkaUzycia) <- get
      if (przedmiot, lok) `elem` przedmioty
        then do
          let nowePrzedmioty = filter (/= (przedmiot, lok)) przedmioty
          put (lok, nowePrzedmioty, przedmiot:eq, nozUzycia, lyzkaUzycia)
          liftIO $ putStrLn $ "Podnosisz " ++ przedmiot ++ "."
        else liftIO $ putStrLn "Nie ma tu takiego przedmiotu."

 | "upusc " `isPrefixOf` wejscie =
  let przedmiot = drop 6 wejscie in do
    (lok, przedmioty, eq, nozUzycia, lyzkaUzycia) <- get
    if przedmiot `elem` eq
      then do
        let nowyEq = filter (/= przedmiot) eq
        put (lok, (przedmiot, lok):przedmioty, nowyEq, nozUzycia, lyzkaUzycia)
        liftIO $ putStrLn $ "Upuszczasz " ++ przedmiot ++ "."
      else liftIO $ putStrLn "Nie masz takiego przedmiotu."

    | wejscie == "zrob_manekina" = do
      (lok, ps, eq, nozUzycia, lyzkaUzycia) <- get
      if all (`elem` eq) ["farba", "wlosy", "papier"]
        then do
          let eq' = filter (`notElem` ["farba", "wlosy", "papier"]) eq
          put (lok, ps, "manekin" : eq', nozUzycia, lyzkaUzycia)
          liftIO $ printRed ["Stworzyłeś manekina!"]
        else liftIO $ printYellow ["Brakuje Ci materiałów, by stworzyć manekina."]

  | wejscie == "poloz_manekina" = do
      (lok, ps, eq, nozUzycia, lyzkaUzycia) <- get
      if lok == "lozko"
        then if "manekin" `elem` eq
          then do
            let eq' = filter (/= "manekin") eq
            put (lok, ("manekin", lok):ps, eq', nozUzycia, lyzkaUzycia)
            liftIO $ printBlue ["Położyłeś manekina na łóżku."]
          else liftIO $ printYellow ["Nie masz manekina, żeby go położyć."]
        else liftIO $ printYellow ["Manekina można położyć tylko na łóżku."]
      | wejscie == "zrob_atrape" = do
      (lok, ps, eq, nozUzycia, lyzkaUzycia) <- get
      if all (`elem` eq) ["sznurek", "drut", "material"]
        then do
          let eq' = filter (`notElem` ["sznurek", "drut", "material"]) eq
          put (lok, ps, "atrapa_wentylacji" : eq', nozUzycia, lyzkaUzycia)
          liftIO $ printRed ["Stworzyłeś atrapę wentylacji!"]
        else liftIO $ printYellow ["Potrzebujesz sznurka, drutu i materiału, by zrobić atrapę."]

  | wejscie == "poloz_atrape" = do
      (lok, ps, eq, nozUzycia, lyzkaUzycia) <- get
      if lok /= "krata wentylacyjna"
        then liftIO $ printYellow ["Musisz być przy wentylacji, aby umieścić atrapę."]
      else if not ("atrapa_wentylacji" `elem` eq)
        then liftIO $ printYellow ["Nie masz atrapy przy sobie."]
      else if not (("krata_otwarta", lok) `elem` ps)
        then liftIO $ printYellow ["Musisz najpierw rozwiercić prawdziwą wentylację."]
      else do
        let eq' = filter (/= "atrapa_wentylacji") eq
        put (lok, ("atrapa_wentylacji", lok):ps, eq', nozUzycia, lyzkaUzycia)
        liftIO $ printRed ["Założono atrapę wentylacji. Wygląda całkiem realistycznie... wpisz \"n\", aby uciec."]
       | wejscie == "wierc" = do
      (lok, ps, eq, nozUzycia, lyzkaUzycia) <- get
      if lok /= "krata wentylacyjna"
        then liftIO $ printYellow ["Musisz być przy wentylacji."]
      else if not ("noz" `elem` eq || "lyzka" `elem` eq)
        then liftIO $ printYellow ["Potrzebujesz noża lub łyżki, żeby wiercić."]
      else do
        let (nozUzycia', eqNoz) =
              if "noz" `elem` eq then
                let k = nozUzycia + 1 in
                if k >= 3
                  then (k, filter (/= "noz") eq)
                  else (k, eq)
              else (nozUzycia, eq)

        let (lyzkaUzycia', eqLyzka) =
              if "lyzka" `elem` eq then
                let s = lyzkaUzycia + 1 in
                if s >= 3
                  then (s, filter (/= "lyzka") eqNoz)
                  else (s, eqNoz)
              else (lyzkaUzycia, eqNoz)

        put (lok, ps, eqLyzka, nozUzycia', lyzkaUzycia')

        if "noz" `elem` eq && nozUzycia' >= 3
          then liftIO $ printYellow ["Nóż się złamał!"]
          else return ()

        if "lyzka" `elem` eq && lyzkaUzycia' >= 3
          then liftIO $ printYellow ["Łyżka się złamała!"]
          else return ()

        if nozUzycia' >= 3 && lyzkaUzycia' >= 3
          then do
                    put (lok, ("krata_otwarta", lok) : ps, eqLyzka, nozUzycia', lyzkaUzycia')
                    liftIO $ printRed ["Wentylacja została otwarta!"]
          else liftIO $ printYellow ["Wierć dalej..."]


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
        ], [], 0, 0) 

  opis "srodek celi"
  evalStateT petla poczatkowyStan
