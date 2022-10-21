% ------------- LABORATORIO 2 - SIMULACION EDITOR DE IMAGENES EN PROLOG -------------
% Nombre: Diego Armijo Palominos (20.223.138-1). 
% Seccion: 13204-0 | B-2. 
% Profesor: Victor Flores. 

%TDA pixbit
%Dominio: X(int) x Y(int) x Bit (0 | 1) x Depth(int). 
% X e Y representan la posicion del pixel, bit su color y Depth la profundidad del pixel. 

pixbit(X,Y,Bit,Depth,[[X,Y],Bit,Depth,"pixbit", 1]):-
    integer(X), 
    X >= 0, 
    integer(Y),
    Y >= 0,
    integer(Bit),
    Bit = 0,
    integer(Depth), 
    Depth >= 0,!.

pixbit(X,Y,Bit,Depth,[[X,Y],Bit,Depth,"pixbit", 0]):-
    integer(X), 
    X >= 0, 
    integer(Y),
    Y >= 0,
    integer(Bit),
    Bit = 1,
    integer(Depth), 
    Depth >= 0,!.

transform1(Num, Num1):-
    NumAux is floor(Num/16),
    NumAux >= 0,
    NumAux < 10, 
    Num1 is NumAux,!. 

transform1(Num, Num1):-
    NumAux is floor(Num/16),
    NumAux = 10,
    Num1 is "A",!. 

transform1(Num, Num1):-
    NumAux is floor(Num/16),
    NumAux = 11,
    Num1 is "B",!. 

transform1(Num, Num1):-
    NumAux is floor(Num/16),
    NumAux = 12,
    Num1 is "C",!. 

transform1(Num, Num1):-
    NumAux is floor(Num/16),
    NumAux = 13,
    Num1 is "D",!.

transform1(Num, Num1):-
    NumAux is floor(Num/16),
    NumAux = 14,
    Num1 is "E",!. 

transform1(Num, Num1):-
    NumAux is floor(Num/16),
    NumAux = 15,
    Num1 is "F",!. 

transform2(Num, Num1):-
    Num1 is ((Num/16) - floor(Num/16))*16.
    

%TDA pixrgb
%Dominio: X(int) x Y(int) x R(C) x G(C) x B(C) x Depth(int). 
% X e Y representan la posicion del pixel, R G B los colores y Depth la profundidad.

pixrgb(X,Y,R,G,B,Depth,[[X,Y],[R,G,B],Depth,"pixrgb"]):-
    integer(X),
    X >= 0, 
    integer(Y),
    Y >= 0,
    integer(R),
    integer(G), 
    integer(B),
    R >= 0,
    R < 256,
    G >= 0,
    G < 256,
    B >= 0, 
    B < 256,
    integer(Depth),
    Depth >= 0.
    %transform1(R, R1), 
    %transform2(R, R2),
    %transform1(G, G1), 
    %transform2(G, G2),
    %transform1(B, B1), 
    %transform2(B, B2).
    
%Funcion que determina si un elemento pertenece a una lista. 
member(X,[X|_]),!.
member(X,[_|Xs]) :- member(X,Xs),!.

%Funcion que determina si un numero es hexadecimal.
esHex([]).
esHex([Cabeza|Cola]):-
    member(Cabeza, [35,48,49,50,51,52,53,54,55,56,57,65,66,67,68,69,70]),!,
    esHex(Cola).    

%TDA pixhex
%Dominio: X(int) x Y(int) x Hex(string) x D(int). S
% X e Y representan las coordenadas del pixel, Hex su color en hexadecimal y D la profundidad. 

pixhex(X,Y,Hex,D,[[X,Y],Hex,D,"pixhex"]):-
    integer(X), 
    X >= 0,
    integer(Y), 
    Y >= 0,
    string(Hex), 
    string_length(Hex, L), 
    L = 7, 
    string_to_list(Hex, Aux1),
    esHex(Aux1).

%TDAimage 
image(Width,Height,Pix,[Width,Height,Pix,L]):-
    integer(Width), 
    Width >= 0,
    integer(Height),
    Height >= 0,
    is_list(Pix),
    WxH is Width*Height,
    length(Pix, WxH),
    maplist(getTypePix, Pix, L).
    %pack(L,L2),!.


getTypePix([_,_,_,Type|_], Type).

getListOfListPix([_,_,_,TypePix|_], TypePix).

%Funcion que verifica si la lista contiene solo "pixbit".
verificarPixbit([]).
verificarPixbit([Cabeza|Cola]):-
    equals(Cabeza, "pixbit"),!,
    verificarPixbit(Cola).

%Funcion que verifica si una imagen es bitmap.
imageIsBitmap(Image):-
    getListOfListPix(Image, L1),
    %getFirstElement(L1, L), 
    verificarPixbit(L1),!, 
    write("true").

%Funcion que verifica si la lista contiene solo "pixrgb".
verificarPixmap([]).
verificarPixmap([Cabeza|Cola]):-
    equals(Cabeza, "pixrgb"),!,
    verificarPixmap(Cola).

%Verifica si la imagen es un Pixmap.
imageIsPixmap(Image):-
    getListOfListPix(Image, L1), 
    getFirstElement(L1, L),
    verificarPixmap(L), 
    write("true").

%Funcion que verifica si la lista contienen solo "pixhex".
verificarHexmap([]).
verificarHexmap([Cabeza|Cola]):-
    equals(Cabeza, "pixhex"),!,
    verificarHexmap(Cola).

%Verifica si la imagen es un Hexmap.
imageIsHexmap(Image):-
    getListOfListPix(Image, L1), 
    getFirstElement(L1, L),
    verificarHexmap(L), 
    write("true").

%Obtiene el primer elemento de una lista.
getFirstElement([Cabeza|_], Cabeza). 
    
%Compara que dos numeros sean iguales.
equals(X, Y):- X = Y.

%Flip-H
getListOfPix([_,_,Pix|_],Pix).
%maplist(getFirstElement, ListPix, L).


%Funciones test.

is_list([]).
is_list([_|T]) :-
        is_list(T).

getPix([Cabeza|_], Cabeza).


succ(X, Y) :- Y is X + 1.
maplist(_,[],[]).
maplist(P,[A|As],[B|Bs]) :- call(P,A,B),
maplist(P,As,Bs),!.


are_identical(X, Y) :-
    X == Y.

%Funcion que filtra una lista. (No se puede utilizar).
filterList(A, In, Out) :-
    exclude(are_identical(A), In, Out).
 
%No Funciona    
slice([X|_],1,1,[X]).
slice([X|Xs],1,K,[X|Ys]) :- K > 1, K1 is K - 1, slice(Xs,1,K1,Ys). 
slice([_|Xs],I,K,Ys) :- I > 1, I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).

remover( _, [], []).
remover( R, [R|T], T).
remover( R, [H|T], [H|T2]) :- H \= R, remover(R, T, T2).


verifyX(X, Y):-
    getFirstElement(X, X1),
    getFirstElement(Y, Y1),
     X1 \= Y1.

pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- getFirstElement(X, L1), getFirstElement(Y, L2), L1 \= L2.
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).

getSecondElement([_,Element|_], Element). 

split(L,0,[],L).
split([X|Xs],N,[X|Ys],Zs) :- N > 0, N1 is N - 1, split(Xs,N1,Ys,Zs).





%Image de prueba. 
%pixrgb( 0, 0, 1, 30, 40, 10, P1), pixrgb( 0, 1, 1, 10, 10, 10, P2), pixrgb( 1, 0, 1, 20, 20, 20, P3), pixrgb( 1, 1, 1, 70, 40, 10, P4), image(2,2,[P1,P2,P3,P4],CS), imageIsBitmap(CS).
%pixbit(0, 0, 1,10, P1), pixbit(0, 1, 0,20, P2),pixbit(1, 0, 1,30, P3),pixbit(1, 1, 1,40, P4), image(2,2, [P1,P2,P3,P4], CS).
