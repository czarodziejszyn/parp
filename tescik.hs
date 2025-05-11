import Control.Monad.State
import System.IO
import Data.List (isPrefixOf, delete, isInfixOf)

type Lokacja = String
type Kierunek = String
type Przedmiot = String
type StanGry = (Lokacja, [(Przedmiot, Lokacja)], [Przedmiot], Int, Int, [Lokacja])
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
  , "wyjscie                   -- zakończ grę."
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
opis "toaleta" = printYellow ["Jesteś przy toalecie."]
opis "magazyn" = printYellow ["Magazynek. Znajdziesz tu narzędzia."]
opis "poludnie" = printYellow ["poludnie."]
opis "krata wentylacyjna" = printYellow ["Stoisz przy kratce wentylacyjnej. Chyba tędy musisz uciec?"]
opis "zlew" = printYellow ["Zlew."]
opis "szyb1" = printYellow ["Wpełzasz do ciasnego kanału. Widać coś na północy."]
opis "szyb2" = printYellow ["Bardzo ciasno. Przed tobą kratka, którą trzeba odkręcić"]
opis "szyb3"  = printYellow ["Brawo udało ci się odkręcić kratkę!! kontynuuj ucieczkę"]
opis "szyb4" = printYellow ["Kanał schodzi w dół."]
opis "szyb5" = printYellow ["Dalej w dół..."]
opis "szyb6" = printYellow ["Przed tobą druga kratka"]
opis "szyb7" = printYellow ["Udało ci się z drugą kratką! Duszne, ciasne przejście. Trzeba iść dalej."]
opis "szyb8" = printYellow ["Coś słychać nad Tobą... już blisko?"]
opis "szyb9" = printYellow ["Pachnie świeżym powietrzem!"]
opis "szyb10" = printYellow ["Ostatnia kratka na twojej drodze"]
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
  , ("dach", "s", "zejscie1")
  , ("zejscie1", "s", "zejscie2")
  , ("zejscie2", "s", "zejscie3")
  , ("zejscie3", "s", "zejscie4")
  , ("zejscie4", "s", "zejscie5")
  , ("zejscie5", "s", "ziemia")
  ]

spelnioneWarunkiUcieczki :: StanGry -> Bool
spelnioneWarunkiUcieczki (lok, ps, eq, _, _, _) =
  ("manekin", "lozko") `elem` ps &&
  ("atrapa_wentylacji", "krata wentylacyjna") `elem` ps &&
  all (`elem` eq) ["srubokret", "plaszcz", "klej"]

-- spelnioneWarunkiUcieczki (lok, ps, eq, _, _, _) = True
znajdzPrzejscie :: Lokacja -> Kierunek -> StanGry -> Maybe Lokacja
znajdzPrzejscie "krata wentylacyjna" "n" stan
  | spelnioneWarunkiUcieczki stan = Just "szyb1"
  | otherwise = Nothing

znajdzPrzejscie "szyb2" "e" (_, _, _, _, _, odk)
  | "szyb2" `elem` odk = Just "szyb3"
  | otherwise = Nothing


znajdzPrzejscie "szyb6" "n" (_, _, _, _, _, odk)
  | "szyb6" `elem` odk = Just "szyb7"
  | otherwise = Nothing

znajdzPrzejscie "szyb10" "s" (_, _, _, _, _, odk)
  | "szyb10" `elem` odk = Just "szyb11"
  | otherwise = Nothing


znajdzPrzejscie skad kierunek _ = znajdz sciezki
  where
    znajdz [] = Nothing
    znajdz ((z, k, dokad):xs)
      | z == skad && k == kierunek = Just dokad
      | otherwise = znajdz xs



kratkiDoOdkrecenia :: [Lokacja]
kratkiDoOdkrecenia = ["szyb2", "szyb6", "szyb10"]

rozejrzyjSie :: StanGry -> IO ()
rozejrzyjSie (lok, przedmioty, _, _, _, _) = do
  let lokalne = [p | (p, l) <- przedmioty, l == lok]
  putStrLn $ "Jesteś w: " ++ lok
  opis lok
  if null lokalne
    then putStrLn "Nie ma tu żadnych przedmiotów."
    else putStrLn $ "Widzisz tutaj: " ++ unwords lokalne

pokazEkwipunek :: StanGry -> IO ()
pokazEkwipunek (_, _, eq, _, _, _) =
  if null eq
    then putStrLn "Twój ekwipunek jest pusty."
    else putStrLn $ "Ekwipunek: " ++ unwords eq

wezPrzedmiot :: String -> StanGry -> IO (Maybe StanGry, String)
wezPrzedmiot przedmiot (lok, przedmioty, eq, nozUzycia, lyzkaUzycia, kratki)
  | (przedmiot, lok) `elem` przedmioty =
      let nowePrzedmioty = filter (/= (przedmiot, lok)) przedmioty
          nowyStan = (lok, nowePrzedmioty, przedmiot:eq, nozUzycia, lyzkaUzycia, kratki)
      in return (Just nowyStan, "Podnosisz " ++ przedmiot ++ ".")
  | otherwise = return (Nothing, "Nie ma tu takiego przedmiotu.")

upuscPrzedmiot :: String -> StanGry -> IO (Maybe StanGry, String)
upuscPrzedmiot przedmiot (lok, przedmioty, eq, nozUzycia, lyzkaUzycia, kratki)
  | przedmiot `elem` eq =
      let nowyEq = filter (/= przedmiot) eq
          nowyStan = (lok, (przedmiot, lok):przedmioty, nowyEq, nozUzycia, lyzkaUzycia, kratki)
      in return (Just nowyStan, "Upuszczasz " ++ przedmiot ++ ".")
  | otherwise = return (Nothing, "Nie masz takiego przedmiotu.")




zrobManekina :: StanGry -> (StanGry, String)
zrobManekina (lok, ps, eq, nozUzycia, lyzkaUzycia, kratki)
  | all (`elem` eq) ["farba", "wlosy", "papier"] =
      let eq' = filter (`notElem` ["farba", "wlosy", "papier"]) eq
      in ((lok, ps, "manekin" : eq', nozUzycia, lyzkaUzycia, kratki), "Stworzyłeś manekina!")
  | otherwise = ((lok, ps, eq, nozUzycia, lyzkaUzycia, kratki), "Brakuje Ci materiałów, by stworzyć manekina.")



polozManekina :: StanGry -> (StanGry, String)
polozManekina (lok, ps, eq, noz, lyz, kratki)
  | lok /= "lozko" = ((lok, ps, eq, noz, lyz, kratki), "Manekina można położyć tylko na łóżku.")
  | "manekin" `notElem` eq = ((lok, ps, eq, noz, lyz, kratki), "Nie masz manekina, żeby go położyć.")
  | otherwise =
      let eq' = filter (/= "manekin") eq
      in ((lok, ("manekin", lok):ps, eq', noz, lyz, kratki), "Położyłeś manekina na łóżku.")



zrobAtrape :: StanGry -> (StanGry, String)
zrobAtrape (lok, ps, eq, noz, lyz, kratki)
  | all (`elem` eq) ["sznurek", "drut", "material"] =
      let eq' = filter (`notElem` ["sznurek", "drut", "material"]) eq
      in ((lok, ps, "atrapa_wentylacji" : eq', noz, lyz, kratki), "Stworzyłeś atrapę wentylacji!")
  | otherwise = ((lok, ps, eq, noz, lyz, kratki), "Potrzebujesz sznurka, drutu i materiału, by zrobić atrapę.")




polozAtrape
  :: (Lokacja, [(Przedmiot, Lokacja)], [Przedmiot], Int, Int, [Lokacja])
  -> ((Lokacja, [(Przedmiot, Lokacja)], [Przedmiot], Int, Int, [Lokacja]), [String], Bool)
polozAtrape (lok, ps, eq, nozUzycia, lyzkaUzycia, kratki)
  | lok /= "krata wentylacyjna" =
      ((lok, ps, eq, nozUzycia, lyzkaUzycia, kratki), ["Musisz być przy wentylacji, aby umieścić atrapę."], False)
  | not ("atrapa_wentylacji" `elem` eq) =
      ((lok, ps, eq, nozUzycia, lyzkaUzycia, kratki), ["Nie masz atrapy przy sobie."], False)
  | not (("krata_otwarta", lok) `elem` ps) =
      ((lok, ps, eq, nozUzycia, lyzkaUzycia, kratki), ["Musisz najpierw rozwiercić prawdziwą wentylację."], False)
  | otherwise =
      let eq' = filter (/= "atrapa_wentylacji") eq
          ps' = ("atrapa_wentylacji", lok) : ps
      in ((lok, ps', eq', nozUzycia, lyzkaUzycia, kratki),
          ["Założono atrapę wentylacji. Wygląda całkiem realistycznie... wpisz \"n\", aby uciec."], True)


wierc
  :: (Lokacja, [(Przedmiot, Lokacja)], [Przedmiot], Int, Int, [Lokacja])
  -> ((Lokacja, [(Przedmiot, Lokacja)], [Przedmiot], Int, Int, [Lokacja]), [String])
wierc (lok, ps, eq, nozUzycia, lyzkaUzycia, kratki)
  | lok /= "krata wentylacyjna" =
      ((lok, ps, eq, nozUzycia, lyzkaUzycia, kratki), ["Musisz być przy wentylacji."])
  | not ("noz" `elem` eq || "lyzka" `elem` eq) =
      ((lok, ps, eq, nozUzycia, lyzkaUzycia, kratki), ["Potrzebujesz noża lub łyżki, żeby wiercić."])
  | otherwise =
      let 
          (nozUzycia', eqNoz, msgNoz) =
            if "noz" `elem` eq
              then let k = nozUzycia + 1
                   in if k >= 3 then (k, filter (/= "noz") eq, ["Nóż się złamał!"])
                                else (k, eq, [])
              else (nozUzycia, eq, [])

          
          (lyzkaUzycia', eqLyzka, msgLyzka) =
            if "lyzka" `elem` eq
              then let s = lyzkaUzycia + 1
                   in if s >= 3 then (s, filter (/= "lyzka") eqNoz, ["Łyżka się złamała!"])
                                else (s, eqNoz, [])
              else (lyzkaUzycia, eqNoz, [])

          ps' =
            if nozUzycia' >= 3 && lyzkaUzycia' >= 3
              then ("krata_otwarta", lok) : ps
              else ps

          msg =
            msgNoz ++ msgLyzka ++
              if nozUzycia' >= 3 && lyzkaUzycia' >= 3
                then ["Wentylacja została otwarta!"]
                else ["Wierć dalej..."]

      in ((lok, ps', eqLyzka, nozUzycia', lyzkaUzycia', kratki), msg)

odkrec
  :: (Lokacja, [(Przedmiot, Lokacja)], [Przedmiot], Int, Int, [Lokacja])
  -> [Lokacja]
  -> ((Lokacja, [(Przedmiot, Lokacja)], [Przedmiot], Int, Int, [Lokacja]), [String], Bool)
odkrec (lok, ps, eq, noz, lyz, odk) kratkiDoOdkrecenia
  | not (lok `elem` kratkiDoOdkrecenia) =
      ((lok, ps, eq, noz, lyz, odk), ["Tutaj nie ma kratki do odkręcenia."], False)
  | not ("srubokret" `elem` eq) =
      ((lok, ps, eq, noz, lyz, odk), ["Potrzebujesz śrubokręta, by odkręcić kratkę."], False)
  | lok `elem` odk =
      ((lok, ps, eq, noz, lyz, odk), ["Ta kratka jest już odkręcona."], False)
  | otherwise =
      ((lok, ps, eq, noz, lyz, lok : odk), ["Odkręciłeś kratkę! Możesz iść dalej."], True)



wykonajKomende :: String -> Gra ()
wykonajKomende wejscie
  | wejscie `elem` ["n", "s", "e", "w"] = do
      (lok, ps, eq, nozUzycia, lyzkaUzycia,kratki) <- get
      case znajdzPrzejscie lok wejscie (lok, ps, eq, nozUzycia, lyzkaUzycia, kratki) of
        Just nowaLokacja -> do
              put (nowaLokacja, ps, eq, nozUzycia, lyzkaUzycia, kratki)
              liftIO $ putStrLn $ "Idziesz na " ++ wejscie ++ " -> " ++ nowaLokacja
              liftIO $ opis nowaLokacja 
        Nothing -> liftIO $ putStrLn "Nie ma przejścia w tym kierunku."

  | wejscie == "rozejrzyj" = do
    stan <- get
    liftIO $ rozejrzyjSie stan
  | wejscie == "instrukcje" = liftIO instrukcje
  | wejscie == "mapa" = liftIO mapa
  | wejscie == "ekwipunek" = do
    stan <- get
    liftIO $ pokazEkwipunek stan



  | "wez " `isPrefixOf` wejscie = do
    let przedmiot = drop 4 wejscie
    stan <- get
    (mozeNowyStan, komunikat) <- liftIO $ wezPrzedmiot przedmiot stan
    case mozeNowyStan of
      Just nowyStan -> put nowyStan
      Nothing -> return ()
    liftIO $ putStrLn komunikat

 | "upusc " `isPrefixOf` wejscie = do
    let przedmiot = drop 6 wejscie
    stan <- get
    (mozeNowyStan, komunikat) <- liftIO $ upuscPrzedmiot przedmiot stan
    case mozeNowyStan of
      Just nowyStan -> put nowyStan
      Nothing -> return ()
    liftIO $ putStrLn komunikat


    | wejscie == "zrob_manekina" = do
    stan <- get
    let (nowyStan, komunikat) = zrobManekina stan
    put nowyStan
    liftIO $ putStrLn komunikat


  | wejscie == "poloz_manekina" = do
    stan <- get
    let (nowyStan, komunikat) = polozManekina stan
    put nowyStan
    liftIO $ putStrLn komunikat


    | wejscie == "zrob_atrape" = do
    stan <- get
    let (nowyStan, komunikat) = zrobAtrape stan
    put nowyStan
    liftIO $ putStrLn komunikat


  | wejscie == "poloz_atrape" = do
    (lok, ps, eq, nozUzycia, lyzkaUzycia, kratki) <- get
    let (nowyStan, komunikaty, sukces) = polozAtrape (lok, ps, eq, nozUzycia, lyzkaUzycia, kratki)
    put nowyStan
    if sukces
      then liftIO $ printRed komunikaty
      else liftIO $ printYellow komunikaty

     | wejscie == "wierc" = do
    stan <- get
    let (nowyStan, komunikaty) = wierc stan
    put nowyStan
    sequence_ [liftIO $ printYellow [m] | m <- komunikaty]

    | wejscie == "odkrec" = do
    (lok, ps, eq, noz, lyz, odk) <- get
    let (nowyStan, komunikaty, sukces) = odkrec (lok, ps, eq, noz, lyz, odk) kratkiDoOdkrecenia
    put nowyStan
    mapM_ (liftIO . printYellow . (:[])) komunikaty


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
        ], [], 0, 0, []) 

  opis "srodek celi"
  evalStateT petla poczatkowyStan
