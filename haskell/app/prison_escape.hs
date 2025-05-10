import System.Console.ANSI

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

main :: IO ()
main = do
  wypiszDialogStartowy

