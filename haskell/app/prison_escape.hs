import System.Console.ANSI
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)


data Miejsce = Biblioteka | Cela | Pralnia | Spacerniak | Stolowka
    deriving (Show, Eq, Ord, Enum)


data Przedmiot = Atlas | Drut | Farba | Jablko | Klej | Kontakt | Material | Nozyczki | Odkurzacz | Pamietnik | Papier | Pizama | Plaszcze | Sztucce | Sznurek | Srubokret | Ubrania | Wlosy
    deriving (Show, Eq, Ord)


data Postac = Red | Bibliotekarz | Klawisz | Kucharz
    deriving (Show, Eq, Ord)


data StanGry = StanGry
    { miejsceGracza :: Miejsce
    , ekwipunek :: [Przedmiot]
    , przedmiotyLokacji :: Map.Map Miejsce [Przedmiot]
    , postacieLokacji :: Map.Map Miejsce Postac
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
        [ (Biblioteka, Bibliotekarz)
        , (Pralnia, Klawisz)
        , (Spacerniak, Red)
        , (Stolowka, Kucharz)
        ]
    , dialogi = Map.fromList
        [ (Bibliotekarz, "Ciii... Próbuję się skupić na tej książcę o relacyjnych bazach danych.")
        , (Klawisz, "Nie masz dziś zmiany w pralni? Lepiej się pośpiesz!")
        , (Kucharz, "To samo co zwykle?")
        , (Red, "Czyli próbujesz uciec i potrzebujesz kogoś na zewnątrz do pomocy? Znajdź mi w bibliotece atlas, a w zamian zobaczę co da się zrobić.")
        ]
    , przedmiotyDoUcieczki = [Drut, Farba, Kontakt, Material, Mydlo, Papier, Plaszcze, Srubokret, Sznurek, Sztucce, Ubrania, Wlosy]
    }


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


wypisz :: [String] -> IO ()
wypisz xs = putStr (unlines xs)


instrukcje :: [String]
instrukcje = [
    ""
    , "Dostępne komendy:"
    , "ekwipunek                -- sprawdź czy masz już wszystko do ucieczki."
    , ""
    ]


wypiszInstrukcje :: IO ()
wypiszInstrukcje = wypisz instrukcje


wczytajKomende :: IO String
wczytajKomende = do
    putStr "> "
    xs <- getLine
    return xs


petlaGry :: IO ()
petlaGry = do
    cmd <- wczytajKomende
    case cmd of
        "instrukcje" -> do wypiszInstrukcje
                           petlaGry
        "quit" -> return ()
        _ -> do wypisz ["Nieznana komenda.", ""]
                petlaGry


main :: IO ()
main = do
    wypiszDialogStartowy
    wypiszInstrukcje
    petlaGry
