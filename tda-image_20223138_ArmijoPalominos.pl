% ------------- LABORATORIO 2 - SIMULACION EDITOR DE IMAGENES EN PROLOG -------------
% Nombre: Diego Armijo Palominos (20.223.138-1). 
% Seccion: 13204-0 | B-2. 
% Profesor: Victor Flores. 


%Se importan los archivos que utilizaremos.
:-include("tda-pixbit_20223138_ArmijoPalominos").
:-include("tda-pixrgb_20223138_ArmijoPalominos").
:-include("tda-pixhex_20223138_ArmijoPalominos").

/* Lista de predicados

verifyImage(Image)
getListTypePixels(List, List)
getListPixels(List, List)
isPixbit(List)
imageIsBitmap(Image)
isPixmap(List)
imageIsPixmap(Image)
isHexmap(List)
imageIsHexmap(Image)
listLenght(List, Num)
imageIsCompressed(Image)
addElement(Element, List)
imageFlipH(Image, NewImage)
pixelIsFlipH(List,Num, List)
imageFlipV(Image, NewImage)
pixelIsFLipV(List,Num, List)
imageCrop(Image,X1,Y1,X2,Y2,NewImage)
crop(List,X1,Y1,X2,Y2,List)
filterCropX(X,X1,X2)
filterCropY(Y,Y1,Y2)
maxElement(Num1, Num2, Num)
imageRGBToHex(Image, NewImage)
rgbTohex(List, List)
preBit(List, List)
extractRGB(L,L1, L2,L3)
preRGB(R,G,B,List)
preHex(Hex, L2)
imageToHistogram(Image, Histogram)
extractAllColors(List, List)
imageRotate90(Image, NewImage)
pixelIsRotate90Aux(List, Num, List)
imageChangePixel(Image, PixelModificado, NewImage)
preImageChangePixelBit(List, PM, List)
preImageChangePixelRGB(List, PM, List)
preImageChangePixelHex(List, PM, List)
imageInvertColorRGB(Pix, PixMod)

*/

%Metas:
%Crear una imagen valida y poder hacerle modificaciones.


%TDAimage 
%Dominio: Width(int) x Height(int) x Pixeles(string) x D(int). S
% Width x Height representan el ancho y alto de una fotografia... 

%Constructor de image.
image(Width,Height,Pixels,[Width,Height,Pixels,TypePixels]):-
    maplist(getTypePixel, Pixels, TypePixels).

%Predicado de pertenencia de image.
%DOM: Image
verifyImage(Image):-
    image(Width,Height,Pixels,Image),
    integer(Width), 
    Width >= 0,
    integer(Height),
    Height >= 0,
    is_list(Pixels),
    WidthXHeight is Width*Height,
    length(Pixels, WidthXHeight), 
    verifyCoords(Pixels, Width, Height).

verifyCoords(Pixels, Ancho, Alto):-
    getXYCoords(Pixels, Aux),
    sort(Aux, Aux1),
    LengthImage is Ancho * Alto,
    listLength(Aux1, LengthImage),
    msort(Pixels, PixelsAux), 
    maplist(getX, PixelsAux, X), 
    maplist(getY, PixelsAux, YAux), 
    msort(YAux, Y),
    pack(X, X1),
    pack(Y, Y1),
    listLength(X1, X2), 
    listLength(Y1, Y2), 
    Y2 = Ancho, 
    X2 = Alto.

getXYCoords([],[]).
getXYCoords([Pixel|Resto], NewCoords):-
    getXYCoords(Resto, AuxNewPixeles),
   (   verifyPixbit(Pixel) 
    ->  pixbit(X,Y,_,_, Pixel);
    verifyPixrgb(Pixel)
    -> pixrgb(X,Y,_,_,_,_,Pixel);
    verifyPixhex(Pixel)
    ->  pixhex(X,Y,_,_, Pixel)),
    addElement([X,Y], AuxNewPixeles, NewCoords).  

%-----------------------------------------------------------------------------------------

%Predicado que verifica si la lista contiene solo "pixbit".
%DOM: list
isPixbit([]).
isPixbit([Cabeza|Cola]):-
    equals(Cabeza, "pixbit"),!,
    isPixbit(Cola).

%Predicado que verifica si una imagen es bitmap.
%DOM: Image
imageIsBitmap(Image):-
    verifyImage(Image),
    getListTypePixels(Image, Aux),
    isPixbit(Aux),!.

%-----------------------------------------------------------------------------------------

%Predicado que verifica si la lista contiene solo "pixrgb".
%DOM: list
isPixmap([]).
isPixmap([Cabeza|Cola]):-
    equals(Cabeza, "pixrgb"),!,
    isPixmap(Cola).

%Predicado si la imagen es un Pixmap.
%DOM: Image
imageIsPixmap(Image):-
    verifyImage(Image),
    getListTypePixels(Image, Aux),
    isPixmap(Aux). 

%-----------------------------------------------------------------------------------------

%Predicado que verifica si la lista contienen solo "pixhex".
%DOM: list
isHexmap([]).
isHexmap([Cabeza|Cola]):-
    equals(Cabeza, "pixhex"),!,
    isHexmap(Cola).

%Predicado que verifica si la imagen es un Hexmap.
%DOM: Image
imageIsHexmap(Image):-
    verifyImage(Image),
    getListTypePixels(Image, Aux),
    isHexmap(Aux).

%-----------------------------------------------------------------------------------------
%Predicado que encuentra el numero de elementos de una lista.
%DOM: list X num
listLength([],0).
listLength([_|L],N) :- listLength(L,N1), N is N1 + 1.

%Predicado que verifica si una imagen esta comprimida.
%DOM: Image
imageIsCompressed(Image):-
    image(Width,Height,Pixels,Image),
    WidthXHeight is Width*Height,
    listLength(Pixels, Count),
    Count \= WidthXHeight.

%-----------------------------------------------------------------------------------------

%Predicado que agrega un elemento a una lista.
%DOM: element X list
addElement(Element, [], [Element]).
addElement(Element, List, [Element|List]).

%Predicado que invierte una imágen horizontalmente.
%DOM: Image X NewImage
imageFlipH(Image, NewImage):-
    image(Width,Height,Pixels, Image),
    pixelIsFlipH(Pixels, Height, NewPixels),
    sort(NewPixels, NewPixelsO),
    image(Width, Height, NewPixelsO, NewImage),!.
    
%Predicado que modifica la coordenada Y en una lista de pixeles dependiendo del ancho de una imagen.
%DOM: list X num X list
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

%Predicado que invierte una imágen verticalmente.
%DOM: Image X NewImage
imageFlipV(Image, NewImage):-
    image(Width,Height,Pixeles, Image),
    pixelIsFlipV(Pixeles, Width, NewPixels),
    sort(NewPixels, NewPixelesO),
    image(Width, Height, NewPixelesO, NewImage),!.

%Predicado que modifica la coordenada X en una lista de pixeles dependiendo del alto de una imagen.
%DOM: list X num X list
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
    
%Predicado que recorta una imágen a partir de un cuadrante.
%DOM: Image x Num x Num x Num x Num x NewImage
imageCrop(Image,X1,Y1,X2,Y2, NewImage):-
    image(_,_,Pixeles,Image),
    crop(Pixeles,X1,Y1,X2,Y2,NewPixels),
    X2 >= X1, 
    Y2 >= Y1,
    maplist(getX, NewPixels, Aux1), 
    maplist(getY, NewPixels, Aux2),
    sort(Aux1, XAux), 
    sort(Aux2, YAux),
    listLength(XAux, H),
    listLength(YAux, W),
    NewWidth is W,
    NewHeight is H,
    image(NewWidth, NewHeight, NewPixels, NewImage).

%Predicado que dada una lista de pixeles, esta va agregando los pixeles que 
%se encuentren dentro de un rango dado.
%DOM: List x Num x Num x Num x Num x List
crop([],_,_,_,_,[]).
crop([Pixel|Resto],X1,Y1,X2,Y2,NewPixeles):-
    crop(Resto,X1,Y1,X2,Y2,AuxNewPixeles),
    (   verifyPixbit(Pixel) 
    ->  pixbit(X,Y,_,_, Pixel);
    verifyPixrgb(Pixel)
    -> pixrgb(X,Y,_,_,_,_,Pixel);
    verifyPixhex(Pixel)
    ->  pixhex(X,Y,_,_,Pixel)),
    (   filterCropX(X,X1,X2), filterCropY(Y,Y1,Y2)
    ->  addElement(Pixel, AuxNewPixeles, NewPixeles)
    ;   NewPixeles = AuxNewPixeles).

%Filtro para verificar que la coord X se encuentre dentro de un rango.
%DOM: Num x Num x Num
filterCropX(X,X1,X2):-
    (X>=X1, X =< X2).

%Predicado para verificar que la coord Y se encuentre dentro de un rango.
%DOM: Num x Num x Num
filterCropY(Y,Y1,Y2):-
    (Y >=Y1, Y =< Y2).

%Predicado que me devuelve el numero mayor entre dos numeros.
%DOM: Num x Num x Num 
maxElement(X1,X2,X3):-
    (X1 >= X2
    ->  X3 is X1;
    X3 is X2
    ).
%-----------------------------------------------------------------------------------------

%Predicado que transforma una imagen desde una representación RGB a una representación HEX.
%DOM: Image x NewImage
imageRGBToHex(Image, NewImage):-
    image(Width,Height,Pixeles, Image),
    rgbTohex(Pixeles, NewPixeles),
    image(Width,Height,NewPixeles, NewImage),!.

%Predicado que dada una lista de pixeles rgb esta transforma los colores a Hex.
%DOM: List x List
rgbTohex([],[]).
rgbTohex([Pixel|Resto],NewPixeles):-
    rgbTohex(Resto, AuxNewPixeles),
 	pixrgb(X,Y,R,G,B,Depth,Pixel),
   	hex_bytes(L, [R,G,B]),
    string_upper(L, L1),
    string_concat("#", L1, L2),
    pixhex(X,Y,L2,Depth, NewPixel),
    addElement(NewPixel, AuxNewPixeles, NewPixeles).   

%-------------------------------------------------------------------------------------------
%Histograma(pixbit)
%Predicado que ordena y hace encode a una lista de píxeles tipo bit.
%DOM: List x List
preBit(L, L2):-
    msort(L, L1),
    encode(L1, L2).

%-----------------------------------------------------------------------------------------
%Histogram(pixrgb).
%Predicado que extrae los colores R G B y los ordena.
%DOM: List x List x List x List
extractRGB(List,L1,L2,L3):-
    maplist(getR,List, P1),
    msort(P1, L1),
    maplist(getG,List, P2),
    msort(P2, L2),
    maplist(getB,List, P3),
    msort(P3, L3).

%Predicado que hace encode a R G B.
%DOM: R x G x B x List
preRGB(R,G,B,[L1,L2,L3]):-
	encode(R, L1),
  	encode(G, L2),
    encode(B, L3).

%-----------------------------------------------------------------------------------------
%Predicado que ordena y hace encode a una lista de píxeles tipo hex
%DOM: List x List
preHex(Hex, L2):-
    msort(Hex, L1), 
    encode(L1, L2).

%-----------------------------------------------------------------------------------------

%Predicado que genera un histograma, sirve para pixbit, pixrgb y pixhex.
%DOM: Image x NewImage
imageToHistogram(Image, Histogram):-
  	image(_,_,Pixels,Image),
    (getFirstElement(Pixels, Pix), verifyPixbit(Pix)
    -> extractcAllColors(Pixels,L), preBit(L, Histogram);
    getFirstElement(Pixels, Pix), verifyPixrgb(Pix)
    -> extractcAllColors(Pixels,L), extractRGB(L,R,G,B), preRGB(R,G,B, Histogram);
    getFirstElement(Pixels, Pix), verifyPixhex(Pix)
    -> extractcAllColors(Pixels,L), preHex(L, Histogram)).

%Predicado que extrae en una lista los colores de una lista de pixeles. 
%DOM: List x List
extractcAllColors([],[]).
extractcAllColors([Pixel|Rest], Colors):-
    extractcAllColors(Rest, AuxNewColors),
    getPixelColor(Pixel, Color),
 	addElement(Color, AuxNewColors, Colors).

%Predicado que recibe una lista y hace que los duplicados consecutivos se
%agrupanen en terminos [NumeroDeDuplicados, Elemento].
%DOM: List x List
encode(L1,L2) :- pack(L1,L), transform(L,L2).

transform([],[]).
transform([[X|Xs]|Ys],[[N,X]|Zs]) :- length([X|Xs],N), transform(Ys,Zs).

%Predicado que recibe una lista y separa en sublistas los elementos que esten repetidos.  
%DOM: List x List     
pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- X \= Y.
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).

%Predicado que obtiene el color de un pixel.
%DOM: List x Color
getPixelColor([_,_,Color|_], Color).

%-----------------------------------------------------------------------------------------
%Predicado que permite rotar en 90 grados una imagen.
%DOM: Image x NewImage
imageRotate90(Image, NewImage):-
  	image(Width,Height,Pixeles, Image),
    pixelIsRotate90Aux(Pixeles, Width, NewPixelsAux), 
    sort(NewPixelsAux, NewPixels), 
    image(Height, Width, NewPixels, NewImage),!.


%Predicado que rota en 90 grados una lista de píxeles.
%DOM: List x Num x List
pixelIsRotate90Aux([],_,[]).
pixelIsRotate90Aux([Pixel|Resto],Ancho,NewPixeles):-
    pixelIsRotate90Aux(Resto,Ancho,AuxNewPixeles),
    (   verifyPixbit(Pixel) 
    ->  pixbit(X,Y,Bit,Depth, Pixel);
    verifyPixrgb(Pixel)
    -> pixrgb(X,Y,R,G,B,Depth,Pixel);
    verifyPixhex(Pixel)
    ->  pixhex(X,Y,Hex,D, Pixel)),
    NewY is X,
    NewX is abs(Y-(Ancho-1)),
    (   verifyPixbit(Pixel) 
    ->  pixbit(NewX,NewY,Bit,Depth,NewPixel);
    verifyPixrgb(Pixel)
    -> pixrgb(NewX,NewY,R,G,B,Depth,NewPixel);
    verifyPixhex(Pixel)
    ->  pixhex(NewX,NewY,Hex,D, NewPixel)),
    addElement(NewPixel, AuxNewPixeles, NewPixeles).

%-----------------------------------------------------------------------------------------
%changePixel
%Predicado que permite reemplazar un pixel en una imagen.
%DOM: Image x PixelMod x NewImage
imageChangePixel(Image,PixelModificado,NewImage):-
    image(Width,Height,Pixels, Image),
    (getFirstElement(Pixels, Pix), verifyPixbit(Pix), verifyPixbit(PixelModificado)
    -> preImageChangePixelBit(Pixels,PixelModificado, NewPixeles), image(Width,Height,NewPixeles, NewImage);
    getFirstElement(Pixels, Pix), verifyPixrgb(Pix), verifyPixrgb(PixelModificado)
    -> preImageChangePixelRGB(Pixels,PixelModificado, NewPixeles), image(Width,Height,NewPixeles, NewImage);
    getFirstElement(Pixels, Pix), verifyPixhex(Pix), verifyPixhex(PixelModificado)
    -> preImageChangePixelHex(Pixels,PixelModificado, NewPixeles), image(Width,Height,NewPixeles, NewImage)).

%Predicado que cambia un pixel tipo bit en una lista de píxeles tipo bit.
%DOM: List x PixelMod x List
preImageChangePixelBit([],_,[]).
preImageChangePixelBit([Pixel|Resto], PixelModificado, NewPixeles):-
    preImageChangePixelBit(Resto, PixelModificado, AuxNewPixeles),
    getX(PixelModificado, X1),
    getY(PixelModificado, Y1),
    getPixelColor(PixelModificado, BitAux),
    getDepth(PixelModificado, DepthAux),
   	pixbit(X,Y,Bit,Depth, Pixel),
    (   X = X1, Y = Y1
    ->  NewBit is BitAux, NewDepth is DepthAux,pixbit(X,Y,NewBit,NewDepth,NewPixel),addElement(NewPixel, AuxNewPixeles, NewPixeles);
    pixbit(X,Y,Bit,Depth,NewPixel),addElement(NewPixel, AuxNewPixeles, NewPixeles)
    ).


%Predicado que cambia un pixel tipo rgb en una lista de píxeles tipo rgb.
%DOM: List x PixelMod x List
preImageChangePixelRGB([],_,[]).
preImageChangePixelRGB([Pixel|Resto], PixelModificado, NewPixeles):-
    preImageChangePixelRGB(Resto, PixelModificado, AuxNewPixeles),
    getX(PixelModificado, X1),
    getY(PixelModificado, Y1),
    getPixelColor(PixelModificado, List),
    getR(List, R1),
	getG(List, G1),
	getB(List, B1),
    getDepth(PixelModificado, DepthAux),
   	pixrgb(X,Y,R,G,B,Depth, Pixel),
    (   X = X1, Y = Y1
    ->  NewR is R1, NewG is G1, NewB is B1, NewDepth is DepthAux,pixrgb(X,Y,NewR,NewG,NewB,NewDepth, NewPixel),addElement(NewPixel, AuxNewPixeles, NewPixeles);
    pixrgb(X,Y,R,G,B,Depth,NewPixel),addElement(NewPixel, AuxNewPixeles, NewPixeles)
    ).
    
%Predicado que cambia un pixel tipo hex en una lista de píxeles tipo hex.    
%DOM: List x PixelMod x List
preImageChangePixelHex([],_,[]).
preImageChangePixelHex([Pixel|Resto], PixelModificado, NewPixeles):-
    preImageChangePixelHex(Resto, PixelModificado, AuxNewPixeles),
    getX(PixelModificado, X1),
    getY(PixelModificado, Y1),
    getPixelColor(PixelModificado, HexAux),
    getDepth(PixelModificado, DepthAux),
   	pixhex(X,Y,Hex,Depth, Pixel),
    (   X = X1, Y = Y1
    ->  NewHex is HexAux, NewDepth is DepthAux,pixhex(X,Y,NewHex,NewDepth,NewPixel),addElement(NewPixel, AuxNewPixeles, NewPixeles);
    pixhex(X,Y,Hex,Depth,NewPixel),addElement(NewPixel, AuxNewPixeles, NewPixeles)
    ).

%-----------------------------------------------------------------------------------------
%Predicado que permite obtener el color simétricamente opuesto en cada canal dentro de un píxel.
%DOM: PixelRgb x PixelRgbModify
imageInvertColorRGB(PixelRGB, PixelRGBModify):-
    verifyPixrgb(PixelRGB),
    pixrgb(X,Y,R,G,B,Depth, PixelRGB),
    NewR is abs(R-255),
    NewG is abs(G-255),
    NewB is abs(B-255),
    pixrgb(X,Y,NewR,NewG,NewB,Depth, PixelRGBModify).
    
 
%-----------------------------------------------------------------------------------------

%Predicado que comprime una imagen.
%DOM: Image x Compressed(Imagen comprimida)
imageCompressed(Image, Compressed):-
    image(_,_,Pixels,Image),
     (    maplist(getPixelColor, Pixels, Aux), encode(Aux, Aux2), listLength(Aux2, 1)
    -> maplist(getPixelColor, Pixels, Aux), sort(Aux, L), getFirstElement(L, L1), string_concat("Toda la imagen es del mismo color ", L1, Compressed) ;
     maplist(getPixelColor, Pixels, Aux), encode(Aux, Compressed)
    ).

%-----------------------------------------------------------------------------------------

%predicado que modifica una lista en base a una predicado. 
%DOM: Predicado x List x List
maplist(_,[],[]).
maplist(P,[A|As],[B|Bs]) :- call(P,A,B),
maplist(P,As,Bs),!.

%Verifica que dos "algo" sean iguales.
%DOM: Num x Num | String x String
equals(X, Y):- X = Y.

%predicado que retorna el primer elemento de una lista.
%DOM: List x Element
getFirstElement([Element|_], Element).

%predicado que obtiene la coordenada X de un pixel. 
%DOM: List x X
getX([X|_], X).

%predicado que obtiene la coordenada Y de un pixel. 
%DOM: List x Y
getY([_,Y|_], Y).

%predicado que obtiene la profundidad de un pixel. 
%DOM: List x Depth 
getDepth([_,_,_,Depth|_], Depth).

%predicado que obtiene el type de un pixel. 
%DOM: List x Type
getTypePixel([_,_,_,_,Type|_], Type).

%predicado que obtiene la lista con el type de cada pixel de una image. 
%DOM: List x TypePix
getListTypePixels([_,_,_,TypePix|_], TypePix).

%predicado que obtiene la lista de pixeles de una image. 
%DOM: List x Pixels
getListPixels([_,_,Pixels|_], Pixels).
