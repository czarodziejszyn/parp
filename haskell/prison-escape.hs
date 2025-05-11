{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State
import Control.Monad (unless)
import Control.Monad (when)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import System.IO
import Data.List (isPrefixOf, delete, isInfixOf)
import System.Exit


-------------
-- czesc 1 --
-------------


data Miejsce = Biblioteka | Cela | Pralnia | Spacerniak | Stolowka
    deriving (Show, Eq, Ord, Enum)


data Przedmiot = Atlas | Drut | Farba | Jablko | Klej | Kontakt | Material | Mydlo | Nozyczki | Odkurzacz | Pamietnik | Papier | Pizama | Plaszcze | Sztucce | Sznurek | Srubokret | Ubrania | Wlosy
    deriving (Show, Eq, Ord)


data Postac = Redding | Bibliotekarz | Klawisz | Kucharz
    deriving (Show, Eq, Ord)


data StanGry = StanGry
    { miejsceGracza :: Miejsce
    , ekwipunek :: [Przedmiot]
    , przedmiotyLokacji :: Map.Map Miejsce [Przedmiot]
    , postacieLokacji :: Map.Map Miejsce [Postac]
    , dialogi :: Map.Map Postac String
    , przedmiotyDoUcieczki :: [Przedmiot]
    }


stanPoczatkowy :: StanGry
stanPoczatkowy = StanGry
    { miejsceGracza = Cela
    , ekwipunek = []
    , przedmiotyLokacji = Map.fromList
        [ (Biblioteka, [Klej, Papier])
        , (Cela, [Mydlo, Pamietnik, Pizama])
        , (Pralnia, [Odkurzacz, Plaszcze, Ubrania])
        , (Spacerniak, [Farba])
        , (Stolowka, [Sztucce])
        ]
    , postacieLokacji = Map.fromList
        [ (Biblioteka, [Bibliotekarz])
        , (Pralnia, [Klawisz])
        , (Spacerniak, [Redding])
        , (Stolowka, [Kucharz])
        ]
    , dialogi = Map.fromList
        [ (Bibliotekarz, "Ciii... Próbuję się skupić na tej książcę o relacyjnych bazach danych.")
        , (Klawisz, "Nie masz dziś zmiany w pralni? Lepiej się pośpiesz!")
        , (Kucharz, "To samo co zwykle?")
        , (Redding, "Czyli próbujesz uciec i potrzebujesz kogoś na zewnątrz do pomocy? Znajdź mi w bibliotece atlas, a w zamian zobaczę co da się zrobić.")
        ]
    , przedmiotyDoUcieczki = [Drut, Farba, Kontakt, Material, Mydlo, Papier, Plaszcze, Srubokret, Sznurek, Sztucce, Ubrania, Wlosy]
    }


type Gra a = StateT StanGry IO a


wypiszKolor :: String -> String -> IO ()
wypiszKolor kolor tekst = putStrLn $ koloruj kolor tekst
  where
    reset = "\x1b[0m"
    koloruj "Red"     txt = "\x1b[31m" ++ txt ++ reset
    koloruj "Green"   txt = "\x1b[32m" ++ txt ++ reset
    koloruj "Cyan"    txt = "\x1b[36m" ++ txt ++ reset
    koloruj "Blue"    txt = "\x1b[34m" ++ txt ++ reset
    koloruj "White"   txt = "\x1b[37m" ++ txt ++ reset
    koloruj "Magenta" txt = "\x1b[35m" ++ txt ++ reset
    koloruj _         txt = txt


opis :: Miejsce -> String
opis Biblioteka = "Jesteś w bibliotece."
opis Cela = "Jesteś w swojej celi."
opis Pralnia = "Jesteś w pralni."
opis Spacerniak = "Jesteś na spacerniaku."
opis Stolowka = "Jesteś na więziennej stołówce."





idz :: Miejsce -> Gra ()
idz miejsce = do
    state <- get
    put state {miejsceGracza = miejsce}
    liftIO $ wypiszKolor "Green" (opis miejsce)
    rozejrzyj


rozejrzyj :: Gra ()
rozejrzyj = do
    StanGry {..} <- get
    let przedmioty = fromMaybe [] (Map.lookup miejsceGracza przedmiotyLokacji)
        postacie = fromMaybe [] (Map.lookup miejsceGracza postacieLokacji)
    liftIO $ do
        if null przedmioty
            then wypiszKolor "Blue" "Nie ma tu nic ciekawego."
            else do
                wypiszKolor "Magenta" "Widzisz: "
                mapM_ (\przedmiot -> wypiszKolor "Magenta" $ "- " ++ show przedmiot) przedmioty
        if not (null postacie)
            then do
                wypiszKolor "Cyan" "Spotykasz tutaj: "
                mapM_ (\postac -> wypiszKolor "Cyan" $ "- " ++ show postac) postacie
            else return ()


wez :: Przedmiot -> Gra ()
wez przedmiot = do
    state@StanGry {..} <- get
    let przedmiotyTutaj = fromMaybe [] (Map.lookup miejsceGracza przedmiotyLokacji)
    if przedmiot `elem` przedmiotyTutaj
        then case (przedmiot, miejsceGracza) of
            -- przypadek dla pizamy
            (Pizama, Cela) | Nozyczki `notElem` ekwipunek && not (Nozyczki `elem` fromMaybe [] (Map.lookup Biblioteka przedmiotyLokacji)) -> do
                let nowePrzedmiotyLokacji = Map.adjust (Nozyczki :) Biblioteka przedmiotyLokacji
                put state { przedmiotyLokacji = nowePrzedmiotyLokacji}
                liftIO $ wypiszKolor "Red" "Bezużyteczna piżama... chyba że znajdziesz coś czym wytnie się z niej sznurek i materiał."
            (Pizama, Cela) | Nozyczki `elem` ekwipunek && Sznurek `notElem` ekwipunek && Material `notElem` ekwipunek -> do
                let nowePrzedmiotyTutaj = filter (/= Pizama) przedmiotyTutaj
                    nowePrzedmiotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                put state { ekwipunek = Sznurek : Material : ekwipunek, przedmiotyLokacji = nowePrzedmiotyMapa }
                liftIO $ wypiszKolor "Green" "Wyciąłeś z piżamy sznurek i materiał."
            (Pizama, Cela) | Nozyczki `elem` fromMaybe [] (Map.lookup Biblioteka przedmiotyLokacji) -> do
                liftIO $ wypiszKolor "Red" "Bezużyteczna piżama... chyba że znajdziesz coś czym wytnie się z niej sznurek i materiał."

            -- przypadek dla odkurzacza
            (Odkurzacz, Pralnia) | Srubokret `notElem` ekwipunek && not (Srubokret `elem` fromMaybe [] (Map.lookup Spacerniak przedmiotyLokacji)) -> do
                let nowePrzedmiotyLokacji = Map.adjust (Srubokret :) Spacerniak przedmiotyLokacji
                put state { przedmiotyLokacji = nowePrzedmiotyLokacji }
                liftIO $ wypiszKolor "Red" "Odkurzacz. Przydałoby się jakoś go rozkręcić, żeby dostać się do wnętrzności..."
            (Odkurzacz, Pralnia) | Srubokret `elem` ekwipunek && Drut `notElem` ekwipunek -> do
                let nowePrzedmiotyTutaj = filter (/= Odkurzacz) przedmiotyTutaj
                    nowePrzedmiotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                put state { ekwipunek = Drut : ekwipunek, przedmiotyLokacji = nowePrzedmiotyMapa }
                liftIO $ wypiszKolor "Green" "Rozkręciłeś odkurzacz i wyjąłeś z niego drut."
            (Odkurzacz, Pralnia) | Srubokret `elem` fromMaybe [] (Map.lookup Spacerniak przedmiotyLokacji) -> do
                liftIO $ wypiszKolor "Red" "Odkurzacz. Przydałoby się jakoś go rozkręcić, żeby dostać się do wnętrzności..."

            -- przypadek dla plaszczy
            (Plaszcze, Pralnia) | Jablko `notElem` ekwipunek && not (Jablko `elem` fromMaybe [] (Map.lookup Stolowka przedmiotyLokacji)) -> do
                let nowePrzedmiotyLokacji = Map.adjust (Jablko :) Stolowka przedmiotyLokacji
                put state { przedmiotyLokacji = nowePrzedmiotyLokacji }
                liftIO $ wypiszKolor "Red" "Klawisz patrzy! Musisz go czymś zająć, może jedzeniem?"
            (Plaszcze, Pralnia) | Jablko `elem` ekwipunek && Plaszcze `notElem` ekwipunek -> do
                let nowePrzedmiotyTutaj = filter (/= Plaszcze) przedmiotyTutaj
                    nowePrzedimotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                put state { ekwipunek = Plaszcze : filter (/= Jablko) ekwipunek, przedmiotyLokacji = nowePrzedimotyMapa }
                liftIO $ wypiszKolor "Green" "Zająłeś klawisza jabłkiem i zabrałeś płaszcze."
            (Plaszcze, Pralnia) | Jablko `elem` fromMaybe [] (Map.lookup Stolowka przedmiotyLokacji) -> do
                liftIO $ wypiszKolor "Red" "Klawisz patrzy! Musisz go czymś zająć, może jedzeniem?"

            -- przypadek dla atlasu
            (Atlas, Biblioteka) -> do
                let nowePrzedmiotyTutaj = filter (/= Atlas) przedmiotyTutaj
                    nowePrzedimotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                    noweDialogi = Map.insert Redding "Super, dzięki za atlas! Sprawę kontaktu możesz uznać za załatwioną." dialogi
                put state { ekwipunek = Kontakt : Atlas : ekwipunek, przedmiotyLokacji = nowePrzedimotyMapa, dialogi = noweDialogi }
                liftIO $ wypiszKolor "Green" $ "Zabrałeś: " ++ show przedmiot

            -- domyslny przypadek
            _ -> do
                let nowePrzedmiotyTutaj = filter (/= przedmiot) przedmiotyTutaj
                    nowePrzedimotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                put state { ekwipunek = przedmiot : ekwipunek, przedmiotyLokacji = nowePrzedimotyMapa }
                liftIO $ wypiszKolor "Green" $ "Zabrałeś: " ++ show przedmiot
            else liftIO $ wypiszKolor "Red" $ "Nie ma tu przedmiotu: " ++ show przedmiot


uzyj :: Przedmiot -> Gra ()
uzyj przedmiot = do
    state@StanGry {..} <- get
    if przedmiot `elem` ekwipunek
        then case przedmiot of
            Pamietnik -> do
                liftIO $ do
                    wypiszKolor "White" $ "Potrzebne do ucieczki:"
                    mapM_ (\przedmiot -> wypiszKolor "White" $ "- " ++ show przedmiot) przedmiotyDoUcieczki
            Nozyczki -> if miejsceGracza == Cela && Wlosy `notElem` ekwipunek
                then do
                    put state {ekwipunek = Wlosy : ekwipunek}
                    liftIO $ wypiszKolor "Green" "Obciąłeś włosy."
                else liftIO $ wypiszKolor "Red" "Chcesz obciąć włosy? Mądre... ale zrób to w celi, żeby nikt się nie zainteresował."
            _ -> liftIO $ wypiszKolor "Red" $ "Nie możesz użyć przedmiotu: " ++ show przedmiot
    else liftIO $ wypiszKolor "Red" $ "Nie masz przedmiotu: " ++ show przedmiot


porozmawiaj :: Postac -> Gra ()
porozmawiaj postac = do
    state@StanGry {..} <- get
    let postacieTutaj = fromMaybe [] (Map.lookup miejsceGracza postacieLokacji)
    if postac `elem` postacieTutaj
        then do
            liftIO $ do
                wypiszDialog (show postac) $ fromMaybe "" (Map.lookup postac dialogi)
            when (postac == Redding && Kontakt `notElem` ekwipunek && Atlas `notElem` ekwipunek && not (Atlas `elem` fromMaybe [] (Map.lookup Biblioteka przedmiotyLokacji))) $ do
                let nowePrzedmiotyLokacji = Map.adjust (Atlas :) Biblioteka przedmiotyLokacji
                put state { przedmiotyLokacji = nowePrzedmiotyLokacji}
        else liftIO $ wypiszKolor "Red" $ "Nie ma tu nikogo o imieniu: " ++ show postac


pokazEkwipunek :: Gra Bool
pokazEkwipunek = do
    StanGry {..} <- get
    liftIO $ if null ekwipunek
        then wypiszKolor "Blue" "Twój ekwipunek jest pusty."
        else  do
            wypiszKolor "Blue" "Masz przy sobie:"
            mapM_ (\przedmiot -> wypiszKolor "Blue" $ "- " ++ show przedmiot) ekwipunek
    sprawdzUcieczke


sprawdzUcieczke :: Gra Bool
sprawdzUcieczke = do
    StanGry {..} <- get
    if all (`elem` ekwipunek) przedmiotyDoUcieczki
        then do
            liftIO $ do
                wypiszKolor "White" "Masz wszystko czego potrzeba do ucieczki. Z mydła i skarpetek znalezionych w ubraniach robisz prowizoryczną broń. Pora poukrywać prz    edmioty po celi, żeby nie wzbudzały podejrzeń, poczekać na noc i rozpocząć ucieczkę."
                wypiszKolor "White" "Otrzymujesz nowy zestaw instrukcji dostępny po wpisaniu polecenia 'instrukcje'."
            return True
        else return False


wypiszDialogStartowy :: IO ()
wypiszDialogStartowy = do
    let dialogStartowy =
            [ ("Prokurator", "Niech Pan opowie jak z Pana perspektywy wyglądała rozmowa z szefem tego dnia.")
            , ("Andy Dufresne", "Był bardzo szorstki. Powiedział, że nikt już nie korzysta z vima, że zmniejsza on moją produktywność i zwolnienie mnie to jego jedyne wyjście.")
            , ("Prokurator", "Czy po tym incydencie wypchnął Pan na maina gałąź, w której cały kod był pisany w jednej linii, a nazwy funkcji nie oddawały ich działania? Tu przykład: funkcja launch_missle() wypisywała na konsolę 'Hello, World'.")
            , ("Andy Dufrasne", "Jak już mówiłem, zostałem wrobiony. Nigdy nie wypchnąłbym kodu przed pokryciu jego przynajmniej dziewięćdziesięciu procent testami jednostkowymi.")
            , ("Sędzia", "Panie Andrew Dufrasne, za pisanie brzydkiego kodu zostaje Pan skazany na dwa dożywocia w więzieniu o zaostrzonym rygorze!")
            ]
    mapM_ (uncurry wypiszDialog) dialogStartowy


wypiszDialog :: String -> String -> IO ()
wypiszDialog postac tekst = do
    let cyan   = "\x1b[36m"
        white  = "\x1b[37m"
        reset  = "\x1b[0m"
    putStr $ cyan ++ postac ++ reset ++ ": "
    putStrLn $ white ++ tekst ++ reset


tekstInstrukcje1 :: [String]
tekstInstrukcje1 = [
    "Dostępne komendy:"
    , "ekwipunek             -- sprawdź czy masz już wszystko do ucieczki."
    , "idz <miejsce>         -- przejdź do miejsca."
    , "instrukcje            -- wyświetl te komendy."
    , "mapa                  -- wyświetl mapę więzienia."
    , "porozmawiaj <imie>    -- rozpocznij rozmowę z postacią w tej lokacji."
    , "rozejrzyj             -- rozejrzyj się dookoła."
    , "uzyj <przedmiot>      -- spróbuj skorzystać z przedmiotu w ekwipunku."
    , "wez <przedmiot>       -- weź przedmiot z aktualnej lokacji."
    , "exit                  -- zakończ rozgrywkę i wyjdź."
    ]


mapaWiezienia :: [String]
mapaWiezienia = [
    "Miejsca, do których możesz przejść:"
    , "biblioteka           -- więzienna biblioteka"
    , "cela                 -- twoja cela"
    , "pralnia              -- pralnia, tu pracujesz"
    , "spacerniak           -- miejsce do spotkań z współwięźniami na dworze"
    , "stolowka             -- więzienna stołówka"
    ]


petlaGry :: Gra ()
petlaGry = do
    liftIO $ putStr "> "
    cmd <- liftIO getLine
    koniecCzesci <- case words cmd of
        ["ekwipunek"] -> pokazEkwipunek
        ["idz", miejsce] -> case miejsce of
            "biblioteka" -> idz Biblioteka >> return False
            "cela" -> idz Cela >> return False
            "pralnia" -> idz Pralnia >> return False
            "spacerniak" -> idz Spacerniak >> return False
            "stolowka" -> idz Stolowka >> return False
            _ -> liftIO $ wypiszKolor "Red" "Nie ma takiego miejsca!" >> return False
        ["instrukcje"] -> do liftIO $ instrukcje tekstInstrukcje1 >> return False
        ["mapa"] -> liftIO $ instrukcje mapaWiezienia >> return False
        ["porozmawiaj", postac] -> case postac of
            "bibliotekarz" -> porozmawiaj Bibliotekarz >> return False
            "klawisz" -> porozmawiaj Klawisz >> return False
            "kucharz" -> porozmawiaj Kucharz >> return False
            "redding" -> porozmawiaj Redding >> return False
            _ -> liftIO ( wypiszKolor "Red" $ "Nie ma tu nikogo o imieniu " ++ show postac) >> return False
        ["rozejrzyj"] -> rozejrzyj >> return False
        ["uzyj", przedmiot] -> case przedmiot of
            "pamietnik" -> uzyj Pamietnik >> return False
            "nozyczki" -> uzyj Nozyczki >> return False
            _ -> liftIO ( wypiszKolor "Red" $ "Nie można użyć przedmiotu: " ++ show przedmiot ) >> return False
        ["wez", przedmiot] -> case przedmiot of
            "atlas" -> wez Atlas >> return False
            "sztucce" -> wez Sztucce >> return False
            "ubrania" -> wez Ubrania >> return False
            "klej" -> wez Klej >> return False
            "pamietnik" -> wez Pamietnik >> return False
            "plaszcze" -> wez Plaszcze >> return False
            "papier" -> wez Papier >> return False
            "farba" -> wez Farba >> return False
            "nozyczki" -> wez Nozyczki >> return False
            "jablko" -> wez Jablko >> return False
            "odkurzacz" -> wez Odkurzacz >> return False
            "pizama" -> wez Pizama >> return False
            "mydlo" -> wez Mydlo >> return False
            "srubokret" -> wez Srubokret >> return False
            _ -> liftIO ( wypiszKolor "Red" $ "Nie ma tu przedmiotu: " ++ przedmiot ) >> return False
        ["exit"] -> liftIO $ exitSuccess >> return False
        _ -> liftIO $ wypiszKolor "Red" "Nieznana komenda!" >> return False
    unless koniecCzesci petlaGry



-------------
-- czesc 2 --
-------------
-- import Control.Monad.State


type Lokacja = String
type Kierunek = String
type Rzecz = String
type StanGry2 = (Lokacja, [(Rzecz, Lokacja)], [Rzecz], Int, Int, [Lokacja])
type Gra2 a = StateT StanGry2 IO a

tekstInstrukcji :: [String]
tekstInstrukcji =
  [ "Dostępne komendy:"
  , ""
  , "n. s. e. w.            -- poruszanie się"
  , "wez <przedmiot>        -- podnieś przedmiot."
  , "upusc <przedmiot>      -- upuść przedmiot."
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

opisMiejsca "srodek celi" = wypiszKolor "Green" "Jesteś w centrum swojej celi. Skompletuj ekwipunek do ucieczki."
opisMiejsca "lozko" = wypiszKolor "Green" "lozko. Może znajdziesz tu coś, z czego zrobisz manekina?"
opisMiejsca "toaleta" = wypiszKolor "Green" "Jesteś przy toalecie."
opisMiejsca "magazyn" = wypiszKolor "Green" "Magazynek. Znajdziesz tu narzędzia."
opisMiejsca "poludnie" = wypiszKolor "Green" "poludnie."
opisMiejsca "krata wentylacyjna" = wypiszKolor "Green" "Stoisz przy kratce wentylacyjnej. Chyba tędy musisz uciec?"
opisMiejsca "zlew" = wypiszKolor "Green" "Zlew."
opisMiejsca "szyb1" = wypiszKolor "Green" "Wpełzasz do ciasnego kanału. Widać coś na północy."
opisMiejsca "szyb2" = wypiszKolor "Green" "Bardzo ciasno. Przed tobą kratka, którą trzeba odkręcić"
opisMiejsca "szyb3"  = wypiszKolor "Green" "Brawo udało ci się odkręcić kratkę!! kontynuuj ucieczkę"
opisMiejsca "szyb4" = wypiszKolor "Green" "Kanał schodzi w dół."
opisMiejsca "szyb5" = wypiszKolor "Green" "Dalej w dół..."
opisMiejsca "szyb6" = wypiszKolor "Green" "Przed tobą druga kratka"
opisMiejsca "szyb7" = wypiszKolor "Green" "Udało ci się z drugą kratką! Duszne, ciasne przejście. Trzeba iść dalej."
opisMiejsca "szyb8" = wypiszKolor "Green" "Coś słychać nad Tobą... już blisko?"
opisMiejsca "szyb9" = wypiszKolor "Green" "Pachnie świeżym powietrzem!"
opisMiejsca "szyb10" = wypiszKolor "Green" "Ostatnia kratka na twojej drodze"
opisMiejsca "szyb11" = wypiszKolor "Green" "Kanał skręca na zachód. To już prawie koniec."
opisMiejsca "szyb12" = wypiszKolor "Green" "Widzisz światło!"
opisMiejsca "dach" = wypiszKolor "Green" "Udało Ci się wyjść na dach! Przed Tobą kable prowadzące w dół. Wciskaj \"s\", aby zejść na dół."
opisMiejsca "zejscie1" = wypiszKolor "Green" "Zacząłeś schodzić po kablach. Ślisko, ale idzie."
opisMiejsca "zejscie2" = wypiszKolor "Green" "Jesteś na wysokości około 4. piętra. Trzymaj się mocno!"
opisMiejsca "zejscie3" = wypiszKolor "Green" "Połowa drogi. Nie ma odwrotu..."
opisMiejsca "zejscie4" = wypiszKolor "Green" "Już blisko ziemi. Nie puść się!"
opisMiejsca "zejscie5" = wypiszKolor "Green" "Jeszcze kawałek... już prawie!"
opisMiejsca "ziemia" = wypiszKolor "Green" "Bezpiecznie dotarłeś na dół. Jesteś wolny!"
opisMiejsca _ = wypiszKolor "Green" "Nieznana lokacja."

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

instrukcje :: [String] -> IO ()
instrukcje = mapM_ (wypiszKolor "White")



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

spelnioneWarunkiUcieczki :: StanGry2 -> Bool
-- spelnioneWarunkiUcieczki (lok, ps, eq, _, _, _) =
--   ("manekin", "lozko") `elem` ps &&
--   ("atrapa_wentylacji", "krata wentylacyjna") `elem` ps &&
--   all (`elem` eq) ["srubokret", "plaszcz", "klej"]

spelnioneWarunkiUcieczki (lok, ps, eq, _, _, _) = True
znajdzPrzejscie :: Lokacja -> Kierunek -> StanGry2 -> Maybe Lokacja
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

rozejrzyjSie :: StanGry2 -> IO ()
rozejrzyjSie (lok, przedmioty, _, _, _, _) = do
  let lokalne = [p | (p, l) <- przedmioty, l == lok]
  putStrLn $ "Jesteś w: " ++ lok
  opisMiejsca lok
  if null lokalne
    then putStrLn "Nie ma tu żadnych Rzeczów."
    else putStrLn $ "Widzisz tutaj: " ++ unwords lokalne

ukazEkwipunek :: StanGry2 -> IO ()
ukazEkwipunek (_, _, eq, _, _, _) =
  if null eq
    then putStrLn "Twój ekwipunek jest pusty."
    else putStrLn $ "Ekwipunek: " ++ unwords eq



wezRzecz :: String -> StanGry2 -> IO (Maybe StanGry2, String)
wezRzecz przedmiot (lok, przedmioty, eq, nozUzycia, lyzkaUzycia, kratki)
  | (przedmiot, lok) `elem` przedmioty =
      let nowePrzedmioty = filter (/= (przedmiot, lok)) przedmioty
          nowyStan = (lok, nowePrzedmioty, przedmiot:eq, nozUzycia, lyzkaUzycia, kratki)
      in return (Just nowyStan, "Podnosisz " ++ przedmiot ++ ".")
  | otherwise = return (Nothing, "Nie ma tu takiego przedmiotu.")

upuscRzecz :: String -> StanGry2 -> IO (Maybe StanGry2, String)
upuscRzecz przedmiot (lok, przedmioty, eq, nozUzycia, lyzkaUzycia, kratki)
  | przedmiot `elem` eq =
      let nowyEq = filter (/= przedmiot) eq
          nowyStan = (lok, (przedmiot, lok):przedmioty, nowyEq, nozUzycia, lyzkaUzycia, kratki)
      in return (Just nowyStan, "Upuszczasz " ++ przedmiot ++ ".")
  | otherwise = return (Nothing, "Nie masz takiego Rzeczu.")


zrobManekina :: StanGry2 -> (StanGry2, String)
zrobManekina (lok, ps, eq, nozUzycia, lyzkaUzycia, kratki)
  | all (`elem` eq) ["farba", "wlosy", "papier"] =
      let eq' = filter (`notElem` ["farba", "wlosy", "papier"]) eq
      in ((lok, ps, "manekin" : eq', nozUzycia, lyzkaUzycia, kratki), "Stworzyłeś manekina!")
  | otherwise = ((lok, ps, eq, nozUzycia, lyzkaUzycia, kratki), "Brakuje Ci materiałów, by stworzyć manekina.")



polozManekina :: StanGry2 -> (StanGry2, String)
polozManekina (lok, ps, eq, noz, lyz, kratki)
  | lok /= "lozko" = ((lok, ps, eq, noz, lyz, kratki), "Manekina można położyć tylko na łóżku.")
  | "manekin" `notElem` eq = ((lok, ps, eq, noz, lyz, kratki), "Nie masz manekina, żeby go położyć.")
  | otherwise =
      let eq' = filter (/= "manekin") eq
      in ((lok, ("manekin", lok):ps, eq', noz, lyz, kratki), "Położyłeś manekina na łóżku.")



zrobAtrape :: StanGry2 -> (StanGry2, String)
zrobAtrape (lok, ps, eq, noz, lyz, kratki)
  | all (`elem` eq) ["sznurek", "drut", "material"] =
      let eq' = filter (`notElem` ["sznurek", "drut", "material"]) eq
      in ((lok, ps, "atrapa_wentylacji" : eq', noz, lyz, kratki), "Stworzyłeś atrapę wentylacji!")
  | otherwise = ((lok, ps, eq, noz, lyz, kratki), "Potrzebujesz sznurka, drutu i materiału, by zrobić atrapę.")




polozAtrape
  :: (Lokacja, [(Rzecz, Lokacja)], [Rzecz], Int, Int, [Lokacja])
  -> ((Lokacja, [(Rzecz, Lokacja)], [Rzecz], Int, Int, [Lokacja]), [String], Bool)
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
  :: (Lokacja, [(Rzecz, Lokacja)], [Rzecz], Int, Int, [Lokacja])
  -> ((Lokacja, [(Rzecz, Lokacja)], [Rzecz], Int, Int, [Lokacja]), [String])
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
  :: (Lokacja, [(Rzecz, Lokacja)], [Rzecz], Int, Int, [Lokacja])
  -> [Lokacja]
  -> ((Lokacja, [(Rzecz, Lokacja)], [Rzecz], Int, Int, [Lokacja]), [String], Bool)
odkrec (lok, ps, eq, noz, lyz, odk) kratkiDoOdkrecenia
  | not (lok `elem` kratkiDoOdkrecenia) =
      ((lok, ps, eq, noz, lyz, odk), ["Tutaj nie ma kratki do odkręcenia."], False)
  | not ("srubokret" `elem` eq) =
      ((lok, ps, eq, noz, lyz, odk), ["Potrzebujesz śrubokręta, by odkręcić kratkę."], False)
  | lok `elem` odk =
      ((lok, ps, eq, noz, lyz, odk), ["Ta kratka jest już odkręcona."], False)
  | otherwise =
      ((lok, ps, eq, noz, lyz, lok : odk), ["Odkręciłeś kratkę! Możesz iść dalej."], True)



wykonajKomende :: String -> Gra2 ()
wykonajKomende wejscie
  | wejscie `elem` ["n", "s", "e", "w"] = do
      (lok, ps, eq, nozUzycia, lyzkaUzycia,kratki) <- get
      case znajdzPrzejscie lok wejscie (lok, ps, eq, nozUzycia, lyzkaUzycia, kratki) of
        Just nowaLokacja -> do
              put (nowaLokacja, ps, eq, nozUzycia, lyzkaUzycia, kratki)
              liftIO $ putStrLn $ "Idziesz na " ++ wejscie ++ " -> " ++ nowaLokacja
              liftIO $ opisMiejsca nowaLokacja
        Nothing -> liftIO $ putStrLn "Nie ma przejścia w tym kierunku."

  | wejscie == "rozejrzyj" = do
    stan <- get
    liftIO $ rozejrzyjSie stan
  | wejscie == "instrukcje" = liftIO $ instrukcje tekstInstrukcji
  | wejscie == "mapa" = liftIO $ instrukcje schematMapy
  | wejscie == "ekwipunek" = do
    stan <- get
    liftIO $ ukazEkwipunek stan
  | wejscie == "exit" = liftIO $ exitSuccess



  | "wez " `isPrefixOf` wejscie = do
    let rzecz = drop 4 wejscie
    stan <- get
    (mozeNowyStan, komunikat) <- liftIO $ wezRzecz rzecz stan
    case mozeNowyStan of
      Just nowyStan -> put nowyStan
      Nothing -> return ()
    liftIO $ putStrLn komunikat

 | "upusc " `isPrefixOf` wejscie = do
    let rzecz = drop 6 wejscie
    stan <- get
    (mozeNowyStan, komunikat) <- liftIO $ upuscRzecz rzecz stan
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

petla :: Gra2 ()
petla = do
  (lok, _, _, _, _, _) <- get
  if lok == "ziemia"
    then return ()
  else
    do
        liftIO $ putStr "> "
        liftIO $ hFlush stdout
        komenda <- liftIO getLine
        if komenda == "wyjscie"
            then wykonajKomende komenda
            else wykonajKomende komenda >> petla

czesc2 :: IO ()
czesc2 = do
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

                opisMiejsca "srodek celi"
                evalStateT petla poczatkowyStan


-- czesc 3!

-- paths

paths :: Map.Map (String, String) String
paths = Map.fromList [
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

data StanGry3 = StanGry3 {
    location :: String,
    canWater :: Bool,
    canFence :: Bool,
    guardsPresent :: Bool,
    warnedFence :: Bool,
    warnedDocks :: Bool,
    time :: Int
}

-- move

-- modify state,
moveInternal :: String -> String -> String
moveInternal from dir = do
    case Map.lookup (from, dir) paths of
        Nothing ->  from
        Just new_loc ->  new_loc


move :: StanGry3 -> String -> IO(StanGry3)
move state dir = do
    -- check if locations with danger
    if (location state) == "fence" && dir == "S" then do
        st <- determine state "fence"
        return st
    else if (location state) == "docks" && dir == "S" then do
        st <- determine state "docks"
        return st
    else do
        let loc = moveInternal (location state) dir
        -- dispaly info if there is no path
        if loc == location state then do
            printRed [
                "Nie możesz tam przejść!\n"
                ]
            return state
        else do
            return state {location = loc}



determine :: StanGry3 -> String -> IO(StanGry3)
determine state "fence" = do
    if canFence state then do
        crossFenceText
        return state {location = "beach"}
    else if not (warnedFence state) then do
        warn "fence"
        return state{warnedFence = True}
    else do
        playerDie "fence"
        return state -- to keep signature

determine state "docks" = do
    if not (guardsPresent state) then do
        getBoatText
        return state {location = "sea"}
    else if not (warnedDocks state) then do
        warn "docks"
        return state{warnedDocks = True}
    else do
        playerDie "docks"
        return state -- to keep signature

warn :: String -> IO()
warn "fence" = do
    printRed [
        "Na pewno chcesz rzucić się przez płot tu i teraz? ",
        "Najpewniej ci się nie uda bez wcześniejszego przygotowania.\n"
        ]

warn "docks" = do
    printRed [
            "Na pewno chcesz pójść do łodzi mimo obecności strażników?\n"
        ]

crossFenceText :: IO()
crossFenceText = do
    printYellow [
        "Wyczekujesz najdłuższego okna i wspinasz sie na płot. "
        , "Po sporym wysiłku spadasz na drugą stronę.\n"
        ]

getBoatText :: IO()
getBoatText = do
    printYellow [
        "Wykorzystujesz swoją okazję i szybko wskakujesz do łodzi. "
        , "Szybko odwiązujesz cumę i zaczynasz wiosłować. \n"
        ]

-- time

deductTime :: StanGry3 -> Int -> StanGry3
deductTime state t = state{time  = (time state) -t}

-- check remaining time, print message and kill player if needed
checkTime :: StanGry3 -> IO(StanGry3)
checkTime state = do
    let hours = time state
    if hours <= 0 then do
        printRed [
            "Słońce wyłoniło się już w pełni nad horyzont. Wiesz, że o tej porze w więzieniu jest pobudka."
            , "Z gmachu więzienia wydobywa się wycie syren. Wiedzą o twojej uciecze i mają cię jak na dłoni..."
            , "Przynajmniej spróbowałeś ..."
            ]
        playerDie ""
        return state -- to keep signature
    else if hours == 5 then do
        printRed ["Zostało ci 5 godzin"]
        return state
    else if hours == 1 then do
        printRed [
            "Horyzont zaczyna odmieniać niewyraźna łuna światła."
            , "Za niespełna godzinę straznicy odkryją twoją ucieczkę, ale ty będziesz wtedy już daleko... racja?"
            ]
        return state
    else do
        printRed ["Zostały ci " ++ show hours ++ " godziny!"]
        return state

-- wait

waitAt :: StanGry3 -> String -> StanGry3
waitAt state "fence" = st {canFence =True}
    where st = deductTime state 2

waitAt state "docks" = st {guardsPresent = False}
    where st = deductTime state 2

waitAt state _ = deductTime state 1

-- deduct time, print text
wait state = do
    waitText (location state)
    return ( waitAt state (location state))


waitText :: String -> IO()
waitText "fence" = do
    printGreen ["\nCzekasz i obserwujesz sposób poruszania się świateł. Po kilku cyklach jesteś pewien swojej oceny - masz okazję do przekroczenia ogrodzenia."]

waitText "docks" = do
    printYellow [
        "Usiadłeś w miejscu, którego nie skanują reflektory i czekasz... "
        , "Po dłuższej chwili jeden ze strażników zaczyna głośno narzekać, że na tym posterunku nic się nigdy nie dzieje. "
        , "Przysłuchujesz się rozmowie i z radością przyjmujesz ich decyzję o skoczeniu po karty.\n"
        ]
    printGreen ["Strażnicy odchodzą, masz okazję do działania."]

waitText _ = do
    printYellow ["zmarnowałeś nieco czasu."]

-- look

look :: StanGry3 -> IO(StanGry3)
look state = do
    describeDispatch state
    st <- checkTime state
    return st

-- so that look can just pass state
describeDispatch :: StanGry3 -> IO()
describeDispatch state =
    describe (location state)

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
    win

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
    win

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
    win

describe "car" = do
    printYellow [
        "Na pobliskim parkingu stoi kilka aut. Zbliżając się dostrzegasz w kilku z nich śpiących ludzi. "
        , "Po otaczającej cię woni wnioskujesz, że są to imprezowicze. "
        , "Próbujesz szczęścia z kilkoma samochodami, dopóki nie znajdujesz tego czego szukasz. Ktoś zapomniał zamknąć tylnych drzwi. "
        , "Szybko wchodzisz do auta i przeciskasz się na siedzenie kierowcy. "
        , "Na twoje nieszczęście, kiedy próbujesz odpalić zwierając przewody uruchamia się alarm."
        ]
    lose

describe "fight" = do
    printYellow [
        "Strażnicy nie są przygotowani na twój atak. "
        , "Udaje ci się zakraść niedaleko jednego ze strażników. "
        , "Rzucasz się na bliższego sobie strażnika, i zdzieliłeś go po głowie. "
        ,"Zanim drugi zorientuje się co się dzieje, także dostaje po głowie."
        ]
    printGreen ["Jesteś sam na doku..."]

describe "float" = do
    printYellow [
        "Po dłuższym czasie pompowania ponton nabrał kształtu. "
        , "Twój improwizowany majstersztyk czeka gotowy na dziewiczą podróż. "
        , "Masz tylko nadzieję, że zdoła unieść twój ciężar... przynajmniej na tyle długo, by resztę drogi pokonać wpław. "
        , "Na twoje szczęście morze jest dziś bardzo spokojne, żadna fala nie powinna pokrzyżować twoich planów.\n"
        ]

-- only 2 items to use, always there no need to keep trac of inventory
use :: StanGry3 -> String -> IO(StanGry3)
use state "bron" = do
    if (location state) == "docks" && (guardsPresent state) then do
        describe "fight"
        return state{guardsPresent = False}
    else do
        printRed ["Nie ma tu przeciwników."]
        return state

use state "ponton" = do
    if (location state) == "beach" then do
        if not (canWater state) then do
            describe "float"
            return state{canWater = True}
        else do
            printRed ["Ponton jest już napompowany!"]
            return state
    else do
        printRed ["Nie jest to dobre miejsce do napompowania pontonu!"]
        return state

use state _ = do
    printRed ["Nie masz tego przedmiotu"]


-- win lose die - write text and end program

win :: IO()
win = do
    printYellow ["Z twojego starego domu dobiega odległe wycie syren..."]
    printGreen ["Gratuluje! Udało ci się uciec z więzienia!"]
    exitSuccess

lose :: IO()
lose = do
    printRed ["Mimo twoich starań, twój plan nie powiódł się na jego ostatnim etapie."]
    exitSuccess

playerDie :: String -> IO()
playerDie "fence" = do
    printYellow [
        "Rzucasz się na ogrodzenie, gdy tylko światło reflektora się od niego odsuwa. "
        , "Niestety źle wybrałeś chwilę i zanim wspiąłeś się na połowę wysokości otacza cię snop światła."
        , "Słyszysz syreny alarmowe...\n\n"
        ]
    printRed ["Umierasz, koniec gry"]
    exitSuccess

playerDie "docks" = do
    printYellow [
        "Udaje ci się zakraść niedaleko jednego ze strażników. Jednak gdy jest on na wyciągnięcie ręki drugi obraca się w twoją stronę. "
        , "Rzucasz się w stronę łodzi w akcie desperacji. Skaczesz i wpadasz do niej z impetem, ale z pomostu słyszysz: Wyłaź! Na ziemię! Podnoś ręce!'."
        ]
    printRed ["Umierasz, koniec gry"]
    exitSuccess

playerDie _ = do
    printRed ["Umierasz, koniec gry"]
    exitSuccess


-- game loop

-- state is given and returned to implement interface for later case structure
tekstInstrukcje3 :: StanGry3 -> IO(StanGry3)
tekstInstrukcje3 state = do
    printGreen [
        "Dostępne komendy:",
        "idz N | S | W | E        -- aby pójść w danym kierunku.",
        "uzyj(przedmiot)          -- aby użyć przedmiotu z ekwipunku.",
        "czekaj                   -- aby zaczekać na korzystniejszy moment do działania",
        "rozejrzyj                -- aby ponownie się rozejrzeć.",
        "instrukcje               -- aby ponownie wyświetlić tą wiadomość.",
        "czas                     -- aby sprawdzić pozostały czas  ",
        "exit                     -- aby skończyć grę i wyjść."
        ]
    return state

readCmd :: IO(String)
readCmd = do
    -- put prompt char
    putStr $ "\x1b[32m" ++ " > " ++ "\x1b[0m"
    cmd <- getLine
    return cmd

interpretCmd :: StanGry3 -> IO(StanGry3)
interpretCmd state = do
    cmd <- readCmd
    -- check first as exitSucces does not return IO(StanGry3)
    if cmd == "exit" then do exitSuccess
    else do
        -- each called function needs to return IO(StanGry3)
        st <- case (words cmd) of
            ["idz", dir]    -> move state dir
            ["uzyj", item]  -> use state item
            ["czekaj"]      -> wait state
            ["rozejrzyj"]   -> look state
            ["czas"]        -> checkTime state
            ["instrukcje"]  -> tekstInstrukcje3 state
            [_]             -> do
                printRed["nieznana komenda"]
                return state
        return st

-- game loop for part 3 of the game - escape island
gameLoop :: StanGry3 -> IO()
gameLoop state = do
    -- describe scene and check time
    look state
    -- state is modified in functions called by interpretCmd
    st <- interpretCmd state
    -- go to next state
    gameLoop st

-- start part 3 of the game - escape island
czesc3 :: IO ()
czesc3 = do
    let initState = StanGry3{
        location = "wall",
        canWater = False,
        canFence = False,
        guardsPresent = True,
        warnedFence = False,
        warnedDocks = False,
        time = 5
    }
    -- print instruction list
    _ <- tekstInstrukcje3 initState
    gameLoop initState


main :: IO ()
main = do
        -- czesc 1
        wypiszDialogStartowy
        instrukcje tekstInstrukcje1
        evalStateT ( idz Cela >> petlaGry) stanPoczatkowy
        -- czesc 2
        czesc2
        -- czesc 3
        czesc3
