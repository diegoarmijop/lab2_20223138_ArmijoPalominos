%TDA pixhex
%Dominio: X(int) x Y(int) x Hex(string) x D(int). 
% X e Y representan las coordenadas del pixel, Hex su color en hexadecimal y D la profundidad. 

%Constructor de pixhex.
pixhex(X,Y,Hex,D,[X,Y,Hex,D,"pixhex"]).

%Predicado que determina si un numero es hexadecimal.
%DOM: Una lista de elementos.
esHex([]).
esHex([Cabeza|Cola]):-
    member(Cabeza, [35,48,49,50,51,52,53,54,55,56,57,65,66,67,68,69,70]),!,
    esHex(Cola). 

%Predicado de pertenencia de pixhex.
%DOM: Pixel. 
verifyPixhex(Pix):-
    pixhex(X,Y,Hex,D,Pix),
    integer(X), 
    X >= 0,
    integer(Y), 
    Y >= 0,
    string(Hex), 
    string_length(Hex, L), 
    L = 7, 
    string_to_list(Hex, L1),
    esHex(L1),
    integer(D),
    D >= 0.

