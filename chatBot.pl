

item(drums, acoustic, roland, 8695.99, 5).
item(drums, acoustic, sonor, 1499.95, 3).
item(drums, acoustic, sonor, 1299.95, 3).
item(drums, acoustic,  pearl, 1399.00, 6).
item(drums, acoustic, tama, 1995.00, 2).
item(drums, acoustic, tama, 995.00, 9).
item(drums, acoustic, gretsch, 1049.00, 4).
item(drums, acoustic, pacific, 995.00, 5).

item(guitar, acoustic, gibson, 3999.00, 2).
item(guitar, acoustic, gibson, 1899.00, 6).
item(guitar, acoustic, gibson, 2799.00, 5).
item(guitar, acoustic, epiphone, 899.00, 4).
item(guitar, acoustic, martin, 1599.00, 7).
item(guitar, acoustic, martin, 1899.00, 2).
item(guitar, acoustic, taylor, 3599.00, 3).

item(guitar, electric, gibson, 1799.00, 3).
item(guitar, electric, gibson, 1899.00, 4).
item(guitar, electric, jackson, 1069.00, 1).
item(guitar, electric, jackson, 1199.00, 0).
item(guitar, electric, epiphone, 899.00, 5).
item(guitar, electric, epiphone, 219.00, 7).

item(bass, electric, ibanez, 899.00, 6).
item(bass, electric, ibanez, 2199.00, 9).
item(bass, electric, ibanez, 1999.00, 1).
item(bass, electric, ibanez, 1899.00, 10).
item(bass, electric, fender, 999.00, 12).
item(bass, electric, fender, 4199.00, 11).
item(bass, electric, jackson, 399.00, 3).
item(bass, electric, gibson, 399.00, 3).

item(keyboard, electric, roland, 6399.00, 4).
item(keyboard, electric, casio, 1399.00, 0).
item(keyboard, electric, casio, 239.99, 5).
item(keyboard, electric, yamaha, 1299.00, 6).
item(keyboard, electric, yamaha, 269.00, 5).
item(keyboard, electric, novation, 249.00, 4).
item(keyboard, electric, novation, 299.00, 3).

item(saxophone, acoustic, carlton, 995.00, 3).
item(saxophone, acoustic, yamaha, 1599.00, 3).
item(saxophone, acoustic, yamaha, 1899.00, 3).
item(saxophone, acoustic, selmer, 8525.00, 3).
item(saxophone, acoustic, selmer, 1725.00, 3).
item(saxophone, acoustic, selmer, 2195.00, 3).
item(saxophone, acoustic, jupiter, 1375.00, 3).

item(trumpet, acoustic, carlton, 595.00, 3).
item(trumpet, acoustic, yamaha, 795.00, 3).
item(trumpet, acoustic, jupiter, 750.00, 3).
item(trumpet, acoustic, bach, 1995.00, 3).
item(trumpet, acoustic, bach, 3899.00, 3).

item(clarinet, acoustic, carlton, 475.00, 3).
item(clarinet, acoustic, yamaha, 799.00, 3).
item(clarinet, acoustic, selmer, 795.00, 3).
item(clarinet, acoustic, selmer, 18750.00, 3).
item(clarinet, acoustic, jupiter, 650.00, 3).

%brands
brand(pearl).
brand(roland).
brand(sonor).
brand(tama).
brand(gretsch).
brand(pacific).
brand(gibson).
brand(martin).
brand(taylor).
brand(epiphone).
brand(jackson).
brand(ibanez).
brand(fender).
brand(yamaha).
brand(casio).
brand(novation).
brand(carlton).
brand(slemer).
brand(jupiter).
brand(bach). 


%%%%%%RULES%%%%%%%%%

cart(item(guitar, electric, gibson, 1799.00, 3)).

get_item_by_instrument(I) :-
    forall(item(I,T,B,C,S), format('Instrument:~p~nType:~p, Brand:~p Cost:~2f, In Stock:~p~n',[I, T, B, C, S])).
get_item_by_type(T) :-
    forall(item(I,T,B,C,S), format('Instrument:~p~nType:~p, Brand:~p~nCost:~2f, In Stock:~p~n',[I, T, B, C, S])).

get_item_by_brand(B) :-
    forall(item(I,T,B,C,S), format('Instrument:~p~nType:~p, Brand:~p~nCost:~2f, In Stock:~p~n',[I, T, B, C, S])).

get_instrument_by_brand(I, B) :-
    item(I,T,B,C,S),
    format('Instrument:~p~nType:~p, Brand:~p Cost:~2f~n, In Stock:~p~n',[I, T, B, C, S]).

get_instrument_by_type(I, T) :-
    item(I,T,B,C,S),
    format('Instrument:~p~nType:~p, Brand:~p~nCost:~2f, In Stock:~p~n',[I, T, B, C, S]).

get_instrument_below(I, LT) :-
    item(I,T,B,C,S),
    C =< LT, %>
    format('Instrument:~p~nType:~p, Brand:~p~nCost:~2f, In Stock:~p~n',[I, T, B, C, S]).

get_cart(L) :-
    cart(X),
    append([cart(X)],[],L),
    write(L).


%read_line_to_codes(user_input, Input),
%string_to_atom(Input,IA),
%atomic_list_concat(AlistI,' ',IA),

print_prompt(me):-
    write('ChatBot: is there anything else I can help you with?'),nl.
print_prompt(you):-
    write('User: ').
is_quit(S):- 
        subset([bye], S).
print_welcome:-
    write('ChatBot: Hello! Welcome to Chris\' Music Store, how can I help you?'), nl. 

 %%%%%%%%ENTRANCE%%%%%%%%   

run:-
    print_welcome,
    conversations.

conversations:-
    repeat, % repeat through backtracking 
    read(UI),
    print_prompt(you), nl,
    split_string(UI, " ", "", L),
    maplist(atom_string, NL, L),
    ask(NL),nl,
    print_prompt(me),
    is_quit(L),
    !.

    %findall(call, member(X, L), NL),
    %forall(member(X, L), (atom_string(A, X), append(NL, A))), 
    %ask(L).

prop(chatbot, identity, "CPSC 312 Chatbot").
prop(drums, type, acoustic).
prop(guitars, type, acoustic).
prop(guitars, type, electric).
prop(bass, type, electric).
prop(keyboard, type, electric).
prop(saxophone, type, acoustic).
prop(trumpet, type, acoustic).
prop(clarinet, type, acoustic).

super(instrument, acoustic).
super(instrument, electric).


% sameas(A, B) is true if A is same as B
sameas(X, X). % itself is same as itself
sameas(you, chatbot).
sameas(is, are). % synonymous
sameas(i, im).
sameas(interested, looking).
verb(looking).
verb(interested).
noun(instrument).
noun(brand).
% Self-awareness


%%%%%%%%SENTENCE PARSING%%%%%%%%%

instrument(X) :- istype(X, instrument).
multitype(O) :- istype(O, X), istype(O, Y), atomic(X), atomic(Y), !.
%isacousticinstrument(I) :- istype(I, acoustic), istype(I, instrument).

% istype(O, T) is true if object O is of type T or its superclasses
istype(O, T) :- prop(O, type, T).
istype(O, T) :- super(T, S), istype(O, S).

% phrase parsers: get the actual object from phrases like "the sky", "a rose"
% noun(N, NP) is true if N is the main object of noun phrase NP (list)
noun(N, [N]).
noun(N, [the | N1]) :- noun(N, N1).
noun(N, [a | N1]) :- noun(N, N1).
noun(N, [an | N1]) :- noun(N, N1).

% adjective(V, ADJ)
adjective(V, [V]).
adjective(V, [H|T]) :- adjective(V, H) ; adjective(V, T).
    
   
% askwhatis(NP) is true if object O of noun phrase NP has some property P that has value V

showme(NP) :- noun(O, NP), get_item_by_instrument(O), nl.
showmebrand(NP) :- noun(O, NP), get_item_by_brand(O), nl.

askwhatis(NP) :- noun(O, NP), istype(O, T), istype(O, S), 
    super(S, T), atomic(T), atomic(S), 
    print_list([O, "is", T, " ", S, "."]), nl.
askwhatis(guitar) :- 
    write('Guitar is an acoustic and/or an electric instrument'), nl. 

askwhatis(NP) :- noun(O, NP), istype(O, T), prop(O, TT, V), dif(TT, type), atomic(T),
    print_list([O, "is", V, T, "."]), nl.
askwhatis(NP) :- noun(O, NP), prop(O, identity, I), atomic(I), 
    print_list([O, "is", I, "."]), nl.

% askwhatis(PPofNP) is true if PPofNP is delimited by "of" to PP and NP and askprop(NP, PP) is true. 
askwhatis(PPofNP) :- append(PP, _, PPofNP), append(PP, [of], L), append(L, NP, PPofNP), askprop(NP, PP).


% ask: sentence parsing - main driver
ask([show, me | O]) :- showme(O).
ask([i, am, looking, for | O]) :- showmebrand(O).
ask([what, is | O]) :- askwhatis(O).
ask([what, is, guitar]) :- askwhatis(guitar).

% askprop(OP, PP) is true if object O of noun phrase OP has some property P of noun phrase PP. The value V is printed as byproduct.
% If O does not have a property P, suggestions are offered.
askprop(NP, PP) :- noun(O, NP), noun(P, PP), prop(O, P, V), atomic(P),
    print_list(["The", P, "of", NP, "is", V, "."]), nl.
askprop(NP, PP) :- noun(O, NP), noun(P, PP), \+prop(O, P, _), prop(O, X, _), dif(X, P),
    print_list([NP, has, no, P, but, it, does, have, X, "."]), nl.
askprop(NP, PP) :- noun(O, NP), noun(P, PP), \+prop(O, _, _), prop(OO, P, _),
    print_list(["I don't know about", NP, "but I can tell you about", PP, "of", OO, "."]), nl.



% misc., including greetings and hails

greet :- print_list(["Hi."]), nl.
goodbye :- print_list(["Goodbye."]), nl.

% print_list from https://stackoverflow.com/a/6877478
% flatten from https://stackoverflow.com/a/9059827
flatten([], []) :- !.
flatten([L|Ls], FlatL) :-
    !,
    flatten(L, NewL),
    flatten(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten(L, [L]).

print_list([X]):-
    is_list(X),
    flatten(X, Fx),
    print_list(Fx).
print_list([X]):-
    \+is_list(X),
    write(X),
    !.
print_list([H|T]):-
    is_list(H),
    flatten(H, Fh),
    print_list(Fh),
    write(' '),
    print_list(T).
print_list([H|T]):-
    \+is_list(H),
    write(H),
    write(' '),
    print_list(T).






%%%%%%EXTRE MAINLY FROM EXAMPLE%%%%%%%%%%%%


isLookingFor(instruments) :-
    instrument(X),
    write(X).
isLookingFor(instrument) :-
    instrument(X),
    write(X).
isLookingFor(brand) :-
    brand(X),
    write(X).
isLookingFor(type) :-
    type(X),
    write(X).
isLookingFor(Other) :-
    format('We do not have ~s~n.', [Other]).


%ask([why, is | NpAdj]) :- append(NP, ADJ, NpAdj), askwhyis(NP, ADJ).
%ask([who, are, you]) :- whoami.
%ask([Be | NpAdj]) :- sameas(Be, is), append(NP, ADJ, NpAdj), askis(NP, ADJ).


% stateiam(ADJ) is true if there is an object O that satisfies value V for some property P in ADJ
stateiam(ADJ) :- adjective(V, ADJ), dif(V, true), prop(O, _, V),
    print_list(["If you are", ADJ, "then you must be", O, "."]), nl.

% stateiam(X) is true if the statement is of the form "I am X"
% where X is a list containing an adjective or a modifying noun phrase
stateiam(X) :- append([since, when, are, you], X, R), append(R, [?], R1), print_list(R1), nl.

% stateyouare(ADJ)
stateyouare(ADJ) :- noun(O, ADJ), prop(O, P, V), dif(P, type), dif(V, true), 
    print_list(["I am not", V, "enough to be", ADJ, "."]), nl.
stateyouare(ADJ) :- adjective(V, ADJ), prop(O, P, V), dif(P, type), dif(V, true),
    print_list(["I might be", V, "but I am not a", O, "."]), nl.
stateyouare(ADJ) :- adjective(V, ADJ), prop(O, V, true),
    print_list(["I might be", V, "but I am not a", O, "."]), nl.
stateyouare(ADJ) :- 
    print_list(["Are you sure I am", ADJ, "?"]), nl.

% state(G) is true is G is a greeting from user.
state(G) :- isgreeting(G), greet.
state([i, am | ADJ]) :- stateiam(ADJ).
state([you, are | ADJ]) :- stateyouare(ADJ).

% subjunctive clause handling
% state([i, V | S]) is true if the statement starts with "i, V" where V is a subjunctive verb
state([i, V | S]) :- subv(V), state(S).

% subv is true if the verb in consideration is used in subjunctive clauses like "I think", "I suppose"
subv(think). subv(believe). subv(suppose). subv(guess). subv(say).


% askers
% askis(NP, ADJ) is true if object O of noun phrase NP has some property of value V of adjective phrase ADJ
askis(NP, ADJ) :- noun(O, NP), adjective(V, ADJ), prop(O, _, V), 
    print_list(["Yes", O, "is", V, "."]), nl.
% askis(NP, ADJ) is true if object O of noun phrase NP is of type T of adjective phrase ADJ
askis(NP, ADJ) :- noun(O, NP), noun(T, ADJ), istype(O, T), 
    print_list(["Yes", O, "is", T, "."]), nl.
askis(NP, ADJ) :- noun(O, NP), noun(T, ADJ), istype(O, TT), dif(T, TT), 
    print_list(["No, ", NP, "is not a", T, ". It's a ", TT, "."]), nl.
askis(NP, ADJ) :- noun(O, NP), noun(V, ADJ), \+prop(O, _, V), 
    print_list(["I can't tell whether ", NP, "is", ADJ, "."]), nl.