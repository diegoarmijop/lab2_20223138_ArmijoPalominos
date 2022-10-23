% ------------- LABORATORIO 2 - SIMULACION EDITOR DE IMAGENES EN PROLOG -------------
% Nombre: Diego Armijo Palominos (20.223.138-1). 
% Seccion: 13204-0 | B-2. 
% Profesor: Victor Flores. 

%TDA pixbit
%Dominio: X(int) x Y(int) x Bit (0 | 1) x Depth(int). 
% X e Y representan la posicion del pixel, bit su color y Depth la profundidad del pixel. 

%Constructor pixbit.
pixbit(X,Y,Bit,Depth,[X,Y,Bit,Depth,"pixbit"]).

%Funcion de pertenencia de pixbit.
verifyPixbit(Pix):-
    pixbit(X,Y,Bit, Depth, Pix),
    integer(X), 
    X >= 0, 
    integer(Y),
    Y >= 0,
    integer(Bit),
    Bit >= 0,
    Bit < 2,
    integer(Depth), 
    Depth >= 0,!.

%TDA pixrgb
%Dominio: X(int) x Y(int) x R(C) x G(C) x B(C) x Depth(int). 
% X e Y representan la posicion del pixel, R G B los colores y Depth la profundidad.

%Constructor de pixrgb.
pixrgb(X,Y,R,G,B,Depth,[X,Y,[R,G,B],Depth,"pixrgb"]).

%Funcion de pertenencia de pixrgb.
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

%TDA pixhex
%Dominio: X(int) x Y(int) x Hex(string) x D(int). S
% X e Y representan las coordenadas del pixel, Hex su color en hexadecimal y D la profundidad. 

%Constructor de pixhex.
pixhex(X,Y,Hex,D,[X,Y,Hex,D,"pixhex"]).

%Funcion que determina si un numero es hexadecimal.
esHex([]).
esHex([Cabeza|Cola]):-
    member(Cabeza, [35,48,49,50,51,52,53,54,55,56,57,65,66,67,68,69,70]),!,
    esHex(Cola). 

%Funcion de pertenencia de pixhex.
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

%TDAimage 
%Dominio: Width(int) x Height(int) x Pixeles(string) x D(int). S
% Width x Height representan el ancho y alto de una fotografia... 

%Constructor de image.
image(Width,Height,Pixels,[Width,Height,Pixels,TypePixels]):-
    maplist(getTypePixels, Pixels, TypePixels).

%Funcion de pertenencia de image.
verifyImage(Image):-
    image(Width,Height,Pixels,Image),
    integer(Width), 
    Width >= 0,
    integer(Height),
    Height >= 0,
    is_list(Pixels),
    WidthXHeight is Width*Height,
    length(Pixels, WidthXHeight).

%Funcion que verifica si la lista contiene solo "pixbit".
isPixbit([]).
isPixbit([Cabeza|Cola]):-
    equals(Cabeza, "pixbit"),!,
    isPixbit(Cola).

%Funcion que verifica si una imagen es bitmap.
imageIsBitmap(Image):-
    verifyImage(Image),
    getListPixels(Image, Aux),
    isPixbit(Aux),!.

%Funcion que verifica si la lista contiene solo "pixrgb".
isPixmap([]).
isPixmap([Cabeza|Cola]):-
    equals(Cabeza, "pixrgb"),!,
    isPixmap(Cola).

%Verifica si la imagen es un Pixmap.
imageIsPixmap(Image):-
    verifyImage(Image),
    getListPixels(Image, Aux),
    isPixmap(Aux). 

%Funcion que verifica si la lista contienen solo "pixhex".
isHexmap([]).
isHexmap([Cabeza|Cola]):-
    equals(Cabeza, "pixhex"),!,
    isHexmap(Cola).

%Verifica si la imagen es un Hexmap.
imageIsHexmap(Image):-
    verifyImage(Image),
    getListPixels(Image, Aux),
    isHexmap(Aux).

%Funcion que encuentra el numero de elementos de una lista.
listLength([],0).
listLength([_|L],N) :- listLength(L,N1), N is N1 + 1.

%Funcion que verifica si una imagen esta comprimida.
%imageIsCompressed
imageIsCompressed(Image):-
    image(Width,Height,Pixels,Image),
    WidthXHeight is Width*Height,
    listLength(Pixels, Count),
    Count \= WidthXHeight.

addElement(Element, [], [Element]).
addElement(Element, List, [Element|List]).

%Funcion que invierte una imágen horizontalmente.
imageFlipH(Image, NewImage):-
    image(Width,Height,Pixels, Image),
    pixelIsFlipH(Pixels, Height, NewPixels),
    sort(NewPixels, NewPixelsO),
    image(Width, Height, NewPixelsO, NewImage).
    
%Funcion que modifica la coordenada Y en una lista de pixeles dependiendo del ancho de una imagen.
pixelIsFlipH([],_,[]).
pixelIsFlipH([Pixel|Resto], Ancho, NewPixeles):-
    pixelIsFlipH(Resto, Ancho, AuxNewPixeles),
    (   verifyPixbit(Pixel) 
    ->  pixbit(X,Y,Bit,Depth, Pixel);
    verifyPixrgb(Pixel)
    -> pixrgb(X,Y,R,G,B,Depth,Pixel);
    verifyPixhex(Pixel)
    ->  pixhex(X,Y,Hex,D, Pixel)),
    NewY is abs(Y - (Ancho-1)),
    (   verifyPixbit(Pixel) 
    ->  pixbit(X,NewY,Bit,Depth, NewPixel);
    verifyPixrgb(Pixel)
    -> pixrgb(X,NewY,R,G,B,Depth,NewPixel);
    verifyPixhex(Pixel)
    ->  pixhex(X,NewY,Hex,D, NewPixel)),
    addElement(NewPixel, AuxNewPixeles, NewPixeles).

%Funcion que invierte una imágen verticalmente.
imageFlipV(Image, NewImage):-
    image(Width,Height,Pixeles, Image),
    pixelIsFlipV(Pixeles, Width, NewPixels),
    sort(NewPixels, NewPixelesO),
    image(Width, Height, NewPixelesO, NewImage).

%Funcion que modifica la coordenada X en una lista de pixeles dependiendo del alto de una imagen.
pixelIsFlipV([],_,[]).
pixelIsFlipV([Pixel|Resto], Alto, NewPixeles):-
    pixelIsFlipV(Resto, Alto, AuxNewPixeles),
    (   verifyPixbit(Pixel) 
    ->  pixbit(X,Y,Bit,Depth, Pixel);
    verifyPixrgb(Pixel)
    -> pixrgb(X,Y,R,G,B,Depth,Pixel);
    verifyPixhex(Pixel)
    ->  pixhex(X,Y,Hex,D, Pixel)),
    NewX is abs(X - (Alto-1)),
    (   verifyPixbit(Pixel) 
    ->  pixbit(NewX,Y,Bit,Depth,NewPixel);
    verifyPixrgb(Pixel)
    -> pixrgb(NewX,Y,R,G,B,Depth,NewPixel);
    verifyPixhex(Pixel)
    ->  pixhex(NewX,Y,Hex,D, NewPixel)),
    addElement(NewPixel, AuxNewPixeles, NewPixeles).
    

%Funcion que recorta una imágen a partir de un cuadrante.
%Falta cambiar Newcoords.
imageCrop(Image,X1,Y1,X2,Y2, NewImage):-
    image(_,_,Pixeles,Image),
    crop(Pixeles,X1,Y1,X2,Y2,NewPixels),
    maxElement(X1,X2,X3),
    maxElement(Y1,Y2,Y3),
    NewWidth is Y3 + 1,
    NewHeight is X3 + 1,
    image(NewWidth, NewHeight, NewPixels, NewImage).

%Funcion que dada una lista de pixeles, esta va agregando los pixeles que 
%se encuentren dentro de un rango dado.
crop([],_,_,_,_,[]).
crop([Pixel|Resto],X1,Y1,X2,Y2,NewPixeles):-
    crop(Resto,X1,Y1,X2,Y2,AuxNewPixeles),
    (   verifyPixbit(Pixel) 
    ->  pixbit(X,Y,_,_, Pixel);
    verifyPixrgb(Pixel)
    -> pixrgb(X,Y,_,_,_,_,Pixel);
    verifyPixhex(Pixel)
    ->  pixhex(X,Y,_,_,Pixel)),
    %ordenar x1 y1
    (   filterCropX(X,X1,X2), filterCropY(Y,Y1,Y2)
    ->  addElement(Pixel, AuxNewPixeles, NewPixeles)
    ;   NewPixeles = AuxNewPixeles).

%Filtro para verificar que la coord X se encuentre dentro de un rango.
filterCropX(X,X1,X2):-
    (X>=X1, X =< X2).

%Filtro para verificar que la coord Y se encuentre dentro de un rango.
filterCropY(Y,Y1,Y2):-
    (Y >=Y1, Y =< Y2).

%Funcion que me devuelve el numero mayor entre dos numeros.
maxElement(X1,X2,X3):-
    (X1 >= X2
    ->  X3 is X1;
    X3 is X2
    ).

%Funcion que determina si un elemento pertenece a una lista. 
member(X,[X|_]).
member(X,[_|Xs]) :- member(X,Xs),!.


getTypePixels([_,_,_,_,Type|_], Type).
getListPixels1([_,_,Pixels|_], Pixels).
getListPixels([_,_,_,TypePix|_], TypePix).

maplist(_,[],[]).
maplist(P,[A|As],[B|Bs]) :- call(P,A,B),
maplist(P,As,Bs),!.

equals(X, Y):- X = Y.



%pixrgb( 0, 0, 1, 30, 40, 10, P1), pixrgb( 0, 1, 1, 10, 10, 10, P2), pixrgb( 1, 0, 1, 20, 20, 20, P3), pixrgb( 1, 1, 1, 70, 40, 10, P4), image(2,2,[P1,P2,P3,P4],CS), imageIsBitmap(CS).
%pixbit(0, 0, 1,10, P1), pixbit(0, 1, 0,20, P2),pixbit(1, 0, 1,30, P3),pixbit(1, 1, 1,40, P4), image(2,2, [P1,P2,P3,P4], CS).
%pixrgb( 0, 0, 1, 30, 40, 10, P1).

/** <examples>
?- 
pixbit(0, 0, 1,10, P1), pixbit(0, 1, 0,20, P2),pixbit(1, 0, 1,30, P3),pixbit(1, 1, 1,40, P4), image(2,2, [P1,P2,P3,P4], CS), verifyImage(CS), imageIsBitmap(CS).

*/
