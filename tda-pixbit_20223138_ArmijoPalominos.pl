%TDA pixbit
%Dominio: X(int) x Y(int) x Bit (0 | 1) x Depth(int). 
% X e Y representan la posicion del pixel, bit su color y Depth la profundidad del pixel. 

%Constructor pixbit.
pixbit(X,Y,Bit,Depth,[X,Y,Bit,Depth,"pixbit"]).

%Predicado de pertenencia de pixbit.
%DOM: Pixel. 
verifyPixbit(Pix):-
    pixbit(X,Y,Bit,Depth,Pix),
    integer(X), 
    X >= 0, 
    integer(Y),
    Y >= 0,
    integer(Bit),
    Bit >= 0,
    Bit < 2,
    integer(Depth), 
    Depth >= 0,!.

