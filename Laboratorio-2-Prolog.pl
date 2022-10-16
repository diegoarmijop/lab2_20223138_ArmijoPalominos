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
    Depth >= 0,!.
    


