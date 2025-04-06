:- dynamic i_am_at/1, at/2, holding/1, start_time/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)), retractall(start_time(_)).

i_am_at(cell).

/* Required items for escape */
required_items([spoon, cutlery, paper, paint, hair, drill]).

/* Timer */
start_timer :- 
    write("You have 10 seconds to escape."), nl,
    get_time(T),  
    assert(start_time(T)).  

check_timer :- 
    get_time(Now),
    start_time(Start), 
    Elapsed is Now - Start,
    Remaining is 10 - Elapsed,  
    (Remaining =< 0 -> write('Time is up! Game over.'), nl, halt ; 
     write('You have '), write(Remaining), write(' seconds left to escape.'), nl, true).

/* Taking an item */
take(X) :-
        holding(X),
        write('You are already holding it!'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('Picked up '), write(X), write('.'),
        nl, !.

take(_) :-
        write('I don\'t see it here.'),
        nl.

/* Dropping an item */
drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('Dropped '), write(X), write('.'),
        nl, !.

drop(_) :-
        write('You aren\'t holding it!'),
        nl.

can_escape :-
    has_all_items,
    write('You have everything needed for the first escape stage!'), nl.

can_escape :-
    write('You are missing some items to proceed with the escape.'), nl.

/* Instructions */
instructions :-
    nl,
    write('Commands available:'), nl,
    write('take(Object).      -- pick up an object'), nl,
    write('drop(Object).      -- put down an object'), nl,
    write('can_escape.        -- check if you can escape'), nl,
    write('start_timer.       -- start the escape timer'), nl,
    write('check_timer.       -- check how much time is left'), nl,
    write('instructions.      -- see this message again'), nl,
    write('halt.              -- quit the game'), nl, nl.
