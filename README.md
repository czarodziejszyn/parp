##  Komendy sterujące

```prolog
start.             % uruchom grę
instructions.      % wyświetl instrukcje
look.              % rozejrzyj się
inventory.         % pokaż ekwipunek
n. s. e. w.        % poruszanie się
take(X).           % weź przedmiot X
drop(X).           % upuść przedmiot X
make_mannequin.    % stwórz manekina
place_mannequin.   % umieść manekina w łóżku
make_vent_mockup.  % zrób atrapę kratki
place_vent_mockup. % umieść atrapę kratki
drill.             % wywierć prawdziwą kratkę
unscrew.           % odkręć metalowe pręty w szybie
halt.              % zakończ grę
```

---

##  Mapa celi

```
                ┌───────────┐
                │  bed_area  │
                └───━────┘
                      │
                ┌───━─────────┐
        ┌─────▶│ cage_center│◀─────┐
        │       └───━────┘       │
        │             │              │
   ┌────━───┐   ┌─────────┐    ┌────━───┐
   │ storage │   │south_area│   │ toilet  │
   └───━───┘   └─────────┘   └────━───┘
        │                          │
   ┌────━───┐               ┌───━──────┐
   │ventilation│             │   sink    │
   └─────────┘             └──────────┘
```


| Lokalizacja   | Przedmioty                               |
|---------------|------------------------------------------|
| `bed_area`    | `paint`, `hair`, `paper`                 |
| `storage`     | `knife`, `spoon`                         |
| `south_area`  | `raincoat`, `glue`                       |
| `toilet`      | `screwdriver`                            |
| `sink`        | `string`, `wire`, `cloth`                |
| `ventilation` | wejście do szybu, miejsce na atrapę      |

### Mapa szybu wentylacyjnego

```
ventilation
     |
   shaft1
     |
   shaft2
     |
   shaft3  -- [KRATKA 1] → unscrew. x4
     |
   shaft4
     |
   shaft5
     |
   shaft6  -- [KRATKA 2] → unscrew. x4
     |
   shaft7
     |
   shaft8
     |
   shaft9
     |
  shaft10 -- [KRATKA 3] → unscrew. x4
     |
  shaft11
     |
  shaft12
     |
    ROOF ()
```

###  Metalowe pręty blokujące drogę

| Lokalizacja | Akcja         | Ilość |
|-------------|---------------|-------|
| shaft3      | `unscrew.`    | 4×    |
| shaft6      | `unscrew.`    | 4×    |
| shaft10     | `unscrew.`    | 4×    |

Wymagany przedmiot: `screwdriver`  
Bez niego nie przejdziesz przez szyby.

