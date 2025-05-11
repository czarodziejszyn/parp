{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State
import Control.Monad (unless)
import Control.Monad (when)
import System.Console.ANSI
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)


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


wypiszKolor :: Color -> String -> IO ()
wypiszKolor kolor tekst = do
    setSGR [SetColor Foreground Vivid kolor]
    putStrLn tekst
    setSGR [Reset]


opisMiejsca :: Miejsce -> String
opisMiejsca Biblioteka = "Jesteś w bibliotece."
opisMiejsca Cela = "Jesteś w swojej celi."
opisMiejsca Pralnia = "Jesteś w pralni."
opisMiejsca Spacerniak = "Jesteś na spacerniaku."
opisMiejsca Stolowka = "Jesteś na więziennej stołówce."


idz :: Miejsce -> Gra ()
idz miejsce = do
    state <- get
    put state {miejsceGracza = miejsce}
    liftIO $ wypiszKolor Green (opisMiejsca miejsce)
    rozejrzyj


rozejrzyj :: Gra ()
rozejrzyj = do
    StanGry {..} <- get
    let przedmioty = fromMaybe [] (Map.lookup miejsceGracza przedmiotyLokacji)
        postacie = fromMaybe [] (Map.lookup miejsceGracza postacieLokacji)
    liftIO $ do
        if null przedmioty 
            then wypiszKolor Blue "Nie ma tu nic ciekawego."
            else do
                wypiszKolor Magenta "Widzisz: "
                mapM_ (\przedmiot -> wypiszKolor Magenta $ "- " ++ show przedmiot) przedmioty
        if not (null postacie)
            then do
                wypiszKolor Cyan "Spotykasz tutaj: "
                mapM_ (\postac -> wypiszKolor Cyan $ "- " ++ show postac) postacie
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
                liftIO $ wypiszKolor Red "Bezużyteczna piżama... chyba że znajdziesz coś czym wytnie się z niej sznurek i materiał."
            (Pizama, Cela) | Nozyczki `elem` ekwipunek && Sznurek `notElem` ekwipunek && Material `notElem` ekwipunek -> do
                let nowePrzedmiotyTutaj = filter (/= Pizama) przedmiotyTutaj
                    nowePrzedmiotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                put state { ekwipunek = Sznurek : Material : ekwipunek, przedmiotyLokacji = nowePrzedmiotyMapa }
                liftIO $ wypiszKolor Green "Wyciąłeś z piżamy sznurek i materiał."
            (Pizama, Cela) | Nozyczki `elem` fromMaybe [] (Map.lookup Biblioteka przedmiotyLokacji) -> do
                liftIO $ wypiszKolor Red "Bezużyteczna piżama... chyba że znajdziesz coś czym wytnie się z niej sznurek i materiał."

            -- przypadek dla odkurzacza
            (Odkurzacz, Pralnia) | Srubokret `notElem` ekwipunek && not (Srubokret `elem` fromMaybe [] (Map.lookup Spacerniak przedmiotyLokacji)) -> do
                let nowePrzedmiotyLokacji = Map.adjust (Srubokret :) Spacerniak przedmiotyLokacji
                put state { przedmiotyLokacji = nowePrzedmiotyLokacji }
                liftIO $ wypiszKolor Red "Odkurzacz. Przydałoby się jakoś go rozkręcić, żeby dostać się do wnętrzności..."
            (Odkurzacz, Pralnia) | Srubokret `elem` ekwipunek && Drut `notElem` ekwipunek -> do
                let nowePrzedmiotyTutaj = filter (/= Odkurzacz) przedmiotyTutaj
                    nowePrzedmiotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                put state { ekwipunek = Drut : ekwipunek, przedmiotyLokacji = nowePrzedmiotyMapa }
                liftIO $ wypiszKolor Green "Rozkręciłeś odkurzacz i wyjąłeś z niego drut."
            (Odkurzacz, Pralnia) | Srubokret `elem` fromMaybe [] (Map.lookup Spacerniak przedmiotyLokacji) -> do
                liftIO $ wypiszKolor Red "Odkurzacz. Przydałoby się jakoś go rozkręcić, żeby dostać się do wnętrzności..."

            -- przypadek dla plaszczy
            (Plaszcze, Pralnia) | Jablko `notElem` ekwipunek && not (Jablko `elem` fromMaybe [] (Map.lookup Stolowka przedmiotyLokacji)) -> do
                let nowePrzedmiotyLokacji = Map.adjust (Jablko :) Stolowka przedmiotyLokacji
                put state { przedmiotyLokacji = nowePrzedmiotyLokacji }
                liftIO $ wypiszKolor Red "Klawisz patrzy! Musisz go czymś zająć, może jedzeniem?"
            (Plaszcze, Pralnia) | Jablko `elem` ekwipunek && Plaszcze `notElem` ekwipunek -> do
                let nowePrzedmiotyTutaj = filter (/= Plaszcze) przedmiotyTutaj
                    nowePrzedimotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                put state { ekwipunek = Plaszcze : filter (/= Jablko) ekwipunek, przedmiotyLokacji = nowePrzedimotyMapa }
                liftIO $ wypiszKolor Green "Zająłeś klawisza jabłkiem i zabrałeś płaszcze."
            (Plaszcze, Pralnia) | Jablko `elem` fromMaybe [] (Map.lookup Stolowka przedmiotyLokacji) -> do
                liftIO $ wypiszKolor Red "Klawisz patrzy! Musisz go czymś zająć, może jedzeniem?"

            -- przypadek dla atlasu
            (Atlas, Biblioteka) -> do
                let nowePrzedmiotyTutaj = filter (/= Atlas) przedmiotyTutaj
                    nowePrzedimotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                    noweDialogi = Map.insert Redding "Super, dzięki za atlas! Sprawę kontaktu możesz uznać za załatwioną." dialogi 
                put state { ekwipunek = Kontakt : Atlas : ekwipunek, przedmiotyLokacji = nowePrzedimotyMapa, dialogi = noweDialogi }
                liftIO $ wypiszKolor Green $ "Zabrałeś: " ++ show przedmiot

            -- domyslny przypadek
            _ -> do
                let nowePrzedmiotyTutaj = filter (/= przedmiot) przedmiotyTutaj
                    nowePrzedimotyMapa = Map.insert miejsceGracza nowePrzedmiotyTutaj przedmiotyLokacji
                put state { ekwipunek = przedmiot : ekwipunek, przedmiotyLokacji = nowePrzedimotyMapa }
                liftIO $ wypiszKolor Green $ "Zabrałeś: " ++ show przedmiot
            else liftIO $ wypiszKolor Red $ "Nie ma tu przedmiotu: " ++ show przedmiot


uzyj :: Przedmiot -> Gra ()
uzyj przedmiot = do
    state@StanGry {..} <- get
    if przedmiot `elem` ekwipunek
        then case przedmiot of
            Pamietnik -> do
                liftIO $ do
                    wypiszKolor White $ "Potrzebne do ucieczki:"
                    mapM_ (\przedmiot -> wypiszKolor White $ "- " ++ show przedmiot) przedmiotyDoUcieczki
            Nozyczki -> if miejsceGracza == Cela && Wlosy `notElem` ekwipunek
                then do
                    put state {ekwipunek = Wlosy : ekwipunek}
                    liftIO $ wypiszKolor Green "Obciąłeś włosy."
                else liftIO $ wypiszKolor Red "Chcesz obciąć włosy? Mądre... ale zrób to w celi, żeby nikt się nie zainteresował."
            _ -> liftIO $ wypiszKolor Red $ "Nie możesz użyć przedmiotu: " ++ show przedmiot
    else liftIO $ wypiszKolor Red $ "Nie masz przedmiotu: " ++ show przedmiot


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
        else liftIO $ wypiszKolor Red $ "Nie ma tu nikogo o imieniu: " ++ show postac


pokazEkwipunek :: Gra ()
pokazEkwipunek = do
    StanGry {..} <- get
    liftIO $ if null ekwipunek
        then wypiszKolor Blue "Twój ekwipunek jest pusty."
        else do
            wypiszKolor Blue "Masz przy sobie:"
            mapM_ (\przedmiot -> wypiszKolor Blue $ "- " ++ show przedmiot) ekwipunek
    sprawdzUcieczke


sprawdzUcieczke :: Gra ()
sprawdzUcieczke = do
    StanGry {..} <- get
    if all (`elem` ekwipunek) przedmiotyDoUcieczki
        then liftIO $ do
            wypiszKolor White "Masz wszystko czego potrzeba do ucieczki. Z mydła i skarpetek znalezionych w ubraniach robisz prowizoryczną broń. Pora poukrywać prz    edmioty po celi, żeby nie wzbudzały podejrzeń, poczekać na noc i rozpocząć ucieczkę."
            wypiszKolor White "Otrzymujesz nowy zestaw instrukcji dostępny po wpisaniu polecenia 'instrukcje'."
        else return ()


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
    setSGR [SetColor Foreground Vivid Cyan]
    putStr postac
    putStr ": "
    setSGR [SetColor Foreground Vivid White]
    putStrLn tekst
    setSGR [Reset]


instrukcje :: Gra ()
instrukcje = liftIO $ do
    putStrLn "Dostępne komendy:"
    putStrLn "start                 -- rozpoczęcie gry."
    putStrLn "ekwipunek             -- sprawdź czy masz już wszystko do ucieczki."
    putStrLn "idz <miejsce>         -- przejdź do miejsca."
    putStrLn "instrukcje            -- wyświetl te komendy."
    putStrLn "mapa                  -- wyświetl mapę więzienia."
    putStrLn "porozmawiaj <imie>    -- rozpocznij rozmowę z postacią w tej lokacji."
    putStrLn "rozejrzyj             -- rozejrzyj się dookoła."
    putStrLn "uzyj <przedmiot>      -- spróbuj skorzystać z przedmiotu w ekwipunku."
    putStrLn "wez <przedmiot>       -- weź przedmiot z aktualnej lokacji."
    putStrLn "wyjdz                 -- zakończ rozgrywkę i wyjdź."


mapa :: Gra ()
mapa = liftIO $ do
  putStrLn "Miejsca, do których możesz przejść:"
  putStrLn "biblioteka           -- więzienna biblioteka"
  putStrLn "cela                 -- twoja cela"
  putStrLn "pralnia              -- pralnia, tu pracujesz"
  putStrLn "spacerniak           -- miejsce do spotkań z współwięźniami na dworze"
  putStrLn "stolowka             -- więzienna stołówka"


petlaGry :: Gra ()
petlaGry = do
    liftIO $ putStr "> "
    cmd <- liftIO getLine
    case words cmd of
        ["ekwipunek"] -> pokazEkwipunek
        ["idz", miejsce] -> case miejsce of
            "biblioteka" -> idz Biblioteka
            "cela" -> idz Cela
            "pralnia" -> idz Pralnia
            "spacerniak" -> idz Spacerniak
            "stolowka" -> idz Stolowka
            _ -> liftIO $ wypiszKolor Red "Nie ma takiego miejsca!"
        ["instrukcje"] -> do instrukcje 
        ["mapa"] -> mapa
        ["porozmawiaj", postac] -> case postac of
            "bibliotekarz" -> porozmawiaj Bibliotekarz
            "klawisz" -> porozmawiaj Klawisz
            "kucharz" -> porozmawiaj Kucharz 
            "redding" -> porozmawiaj Redding 
            _ -> liftIO $ wypiszKolor Red $ "Nie ma tu nikogo o imieniu " ++ show postac
        ["rozejrzyj"] -> rozejrzyj
        ["uzyj", przedmiot] -> case przedmiot of
            "pamietnik" -> uzyj Pamietnik
            "nozyczki" -> uzyj Nozyczki
            _ -> liftIO $ wypiszKolor Red $ "Nie można użyć przedmiotu: " ++ show przedmiot
        ["wez", przedmiot] -> case przedmiot of
            "atlas" -> wez Atlas
            "sztucce" -> wez Sztucce
            "ubrania" -> wez Ubrania
            "klej" -> wez Klej
            "pamietnik" -> wez Pamietnik
            "plaszcze" -> wez Plaszcze
            "papier" -> wez Papier
            "farba" -> wez Farba
            "nozyczki" -> wez Nozyczki
            "jablko" -> wez Jablko
            "odkurzacz" -> wez Odkurzacz
            "pizama" -> wez Pizama
            "mydlo" -> wez Mydlo
            "srubokret" -> wez Srubokret
            _ -> liftIO $ wypiszKolor Red $ "Nie ma tu przedmiotu: " ++ przedmiot 
        ["wyjdz"] -> return ()
        _ -> liftIO $ wypiszKolor Red "Nieznana komenda!"
    unless (cmd == "wyjdz") petlaGry


main :: IO ()
main = do
    wypiszDialogStartowy
    evalStateT (instrukcje >> idz Cela >> petlaGry) stanPoczatkowy
