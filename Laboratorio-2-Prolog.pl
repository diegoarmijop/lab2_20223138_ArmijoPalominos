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

%-----------------------------------------------------------------------------------------

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

%-----------------------------------------------------------------------------------------

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

%-----------------------------------------------------------------------------------------

%TDAimage 
%Dominio: Width(int) x Height(int) x Pixeles(string) x D(int). S
% Width x Height representan el ancho y alto de una fotografia... 

%Constructor de image.
image(Width,Height,Pixels,[Width,Height,Pixels,TypePixels]):-
    maplist(getTypePixel, Pixels, TypePixels).

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

%-----------------------------------------------------------------------------------------

%Funcion que verifica si la lista contiene solo "pixbit".
isPixbit([]).
isPixbit([Cabeza|Cola]):-
    equals(Cabeza, "pixbit"),!,
    isPixbit(Cola).

%Funcion que verifica si una imagen es bitmap.
imageIsBitmap(Image):-
    verifyImage(Image),
    getListTypePixels(Image, Aux),
    isPixbit(Aux),!.

%-----------------------------------------------------------------------------------------

%Funcion que verifica si la lista contiene solo "pixrgb".
isPixmap([]).
isPixmap([Cabeza|Cola]):-
    equals(Cabeza, "pixrgb"),!,
    isPixmap(Cola).

%Verifica si la imagen es un Pixmap.
imageIsPixmap(Image):-
    verifyImage(Image),
    getListTypePixels(Image, Aux),
    isPixmap(Aux). 

%-----------------------------------------------------------------------------------------

%Funcion que verifica si la lista contienen solo "pixhex".
isHexmap([]).
isHexmap([Cabeza|Cola]):-
    equals(Cabeza, "pixhex"),!,
    isHexmap(Cola).

%Verifica si la imagen es un Hexmap.
imageIsHexmap(Image):-
    verifyImage(Image),
    getListTypePixels(Image, Aux),
    isHexmap(Aux).

%-----------------------------------------------------------------------------------------

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

%-----------------------------------------------------------------------------------------

%Funcion que agrega un elemento a una lista.
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

%-----------------------------------------------------------------------------------------

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

%-----------------------------------------------------------------------------------------
    
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
%-----------------------------------------------------------------------------------------

%Funcion que transforma una imagen desde una representación RGB a una representación HEX.
imageRGBToHex(Image, NewImage):-
    image(Width,Height,Pixeles, Image),
    rgbTohex(Pixeles, NewPixeles),
    image(Width,Height,NewPixeles, NewImage).

%Funcion que dada una lista de pixeles rgb esta transforma los colores a Hex.
rgbTohex([],[]).
rgbTohex([Pixel|Resto],NewPixeles):-
    rgbTohex(Resto, AuxNewPixeles),
 	pixrgb(X,Y,R,G,B,Depth,Pixel),
   	hex_bytes(L, [R,G,B]),
    pixhex(X,Y,L,Depth, NewPixel),
    addElement(NewPixel, AuxNewPixeles, NewPixeles).   

%-------------------------------------------------------------------------------------------
%Histograma(pixbit)

preBit(L, L2):-
    msort(L, L1),
    encode(L1, L2).

%-----------------------------------------------------------------------------------------
%Histogram(pixrgb).

getR([R,_,_], R).
getG([_,G,_], G).
getB([_,_,B], B).

extractRGB(List,L1,L2,L3):-
    maplist(getR,List, P1),
    msort(P1, L1),
    maplist(getG,List, P2),
    msort(P2, L2),
    maplist(getB,List, P3),
    msort(P3, L3).

preRGB(R,G,B,[L1,L2,L3]):-
	encode(R, L1),
  	encode(G, L2),
    encode(B, L3).

%-----------------------------------------------------------------------------------------

preHex(Hex, L2):-
    msort(Hex, L1), 
    encode(L1, L2).

%-----------------------------------------------------------------------------------------

%Funcion que genera un histograma, sirve para pixbit, pixrgb y pixhex.
imageToHistogram(Image, Histogram):-
  	image(_,_,Pixels,Image),
    (getFirstElement(Pixels, Pix), verifyPixbit(Pix)
    -> extractcAllColors(Pixels,L), preBit(L, Histogram);
    getFirstElement(Pixels, Pix), verifyPixrgb(Pix)
    -> extractcAllColors(Pixels,L), extractRGB(L,R,G,B), preRGB(R,G,B, Histogram);
    getFirstElement(Pixels, Pix), verifyPixhex(Pix)
    -> extractcAllColors(Pixels,L), preHex(L, Histogram)).

%Funcion que extrae en una lista los colores de una lista de pixeles. 
extractcAllColors([],[]).
extractcAllColors([Pixel|Rest], Colors):-
    extractcAllColors(Rest, AuxNewColors),
    getPixelColor(Pixel, Color),
 	addElement(Color, AuxNewColors, Colors).

%Funcion que obtiene el color de un pixel.
getPixelColor([_,_,Color|_], Color).

%-----------------------------------------------------------------------------------------


%Funcion que determina si un elemento pertenece a una lista. 
member(X,[X|_]).
member(X,[_|Xs]) :- member(X,Xs),!.

%Funcion que retorna el primer elemento de una lista.
getFirstElement([Element|_], Element).


getTypePixel([_,_,_,_,Type|_], Type).
getListTypePixels([_,_,_,TypePix|_], TypePix).
getListPixels([_,_,Pixels|_], Pixels).


%Funcion que recibe una lista y hace que los duplicados consecutivos se
%agrupanen en terminos [NumeroDeDuplicados, Elemento].
encode(L1,L2) :- pack(L1,L), transform(L,L2).

transform([],[]).
transform([[X|Xs]|Ys],[[N,X]|Zs]) :- length([X|Xs],N), transform(Ys,Zs).

%Funcion que recibe una lista y separa en sublistas los elementos que esten repetidos.       
pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- X \= Y.
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).


%Funcion que modifica una lista en base a una funcion. 
maplist(_,[],[]).
maplist(P,[A|As],[B|Bs]) :- call(P,A,B),
maplist(P,As,Bs),!.

%Verifica que dos "algo" sean iguales.
equals(X, Y):- X = Y.



%pixrgb( 0, 0, 1, 30, 40, 10, P1), pixrgb( 0, 1, 1, 10, 10, 10, P2), pixrgb( 1, 0, 1, 20, 20, 20, P3), pixrgb( 1, 1, 1, 70, 40, 10, P4), image(2,2,[P1,P2,P3,P4],CS), imageIsBitmap(CS).
%pixbit(0, 0, 1,10, P1), pixbit(0, 1, 0,20, P2),pixbit(1, 0, 1,30, P3),pixbit(1, 1, 1,40, P4), image(2,2, [P1,P2,P3,P4], CS).
%pixrgb( 0, 0, 1, 30, 40, 10, P1).

/** <examples>
?- 
pixbit(0, 0, 1,10, P1), pixbit(0, 1, 0,20, P2),pixbit(1, 0, 1,30, P3),pixbit(1, 1, 1,40, P4), image(2,2, [P1,P2,P3,P4], CS), verifyImage(CS), imageIsBitmap(CS).

?- pixrgb( 0, 0, 10, 20, 180, 10, P1), pixrgb( 0, 1, 24, 22, 20, 20, P2), pixrgb( 1, 0, 30, 30, 70, 32, P3), pixrgb( 1, 1, 100, 45, 45, 40, P4), image( 2, 2,[ P1, P2, P3, P4], I1), getListPixels1(I1, I2), extraerColoresT(I2, I3), extraerRGB(I3, L1,L2,L3).
?-  pixhex(0,0,"#FF0011",10, P1), pixhex(0,1,"#AABBCC",20, P2), pixhex(1,0,"#A5F2C2",30,P3), pixhex(1,1,"#FFFFFF",40,P4), image(2,2,[P1,P2,P3,P4], CS), imageToHistogram(CS,CS2). 
*/
