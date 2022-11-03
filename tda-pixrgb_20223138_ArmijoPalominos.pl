%TDA pixrgb
%Dominio: X(int) x Y(int) x R(C) x G(C) x B(C) x Depth(int). 
% X e Y representan la posicion del pixel, R G B los colores y Depth la profundidad.

%Constructor de pixrgb.
pixrgb(X,Y,R,G,B,Depth,[X,Y,[R,G,B],Depth,"pixrgb"]).

%Predicado de pertenencia de pixrgb.
%DOM: Pixel.
verifyPixrgb(Pix):-
    pixrgb(X,Y,R,G,B,Depth,Pix),
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

%Predicado que obtiene el color R de un pixel.
%DOM: pixrgb
getR([R,_,_], R).

%Predicado que obtiene el color R de un pixel.
%DOM: pixrgb
getG([_,G,_], G).

%Predicado que obtiene el color R de un pixel.
%DOM: pixrgb
getB([_,_,B], B).
