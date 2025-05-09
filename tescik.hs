import Control.Monad.State
import Data.List
import System.IO

type Lokacja = String
type Gra a = StateT Lokacja IO a

dozwoloneMiejsca :: [String]
dozwoloneMiejsca =
  [ "zlew"
  , "środek celi"
  , "łóżko"
  , "magazyn"
  , "krata wentylacyjna"
  , "toaleta"
  , "południowy zakątek"
  ]


wykonajKomende :: String -> Gra ()
wykonajKomende wejscie
  | "idź do " `isPrefixOf` wejscie =
      let cel = drop 7 wejscie
      in if cel `elem` dozwoloneMiejsca
         then put cel >> liftIO (putStrLn $ "Idziesz do: " ++ cel)
         else liftIO $ putStrLn "Nieznana lokalizacja!"
  | wejscie == "gdzie jestem" = do
      lok <- get
      liftIO $ putStrLn $ "Jesteś w: " ++ lok
  | wejscie == "pomoc" = liftIO $ putStrLn "Dostępne polecenia: 'idź do <miejsce>', 'gdzie jestem', 'wyjście'"
  | wejscie == "wyjście" = liftIO $ putStrLn "Zakończono grę."
  | otherwise = liftIO $ putStrLn "Nie rozumiem polecenia."


petla :: Gra ()
petla = do
  liftIO $ putStr "> "
  liftIO $ hFlush stdout
  komenda <- liftIO getLine
  if komenda == "wyjście"
    then wykonajKomende komenda
    else wykonajKomende komenda >> petla


main :: IO ()
main = evalStateT petla "środek celi"
