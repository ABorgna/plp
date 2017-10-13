%------------------Predicados predefinidos:------------------%

%fliplength(?Longitud, ?Lista)
fliplength(N, L) :- length(L, N).

%matriz(?Matriz, ?Filas, ?Columnas)
matriz(M, F, C) :- length(M, F), maplist(fliplength(C), M).

%dif1(+N1, ?N2)
dif1(N1, N2) :- N2 is N1 + 1.
dif1(N1, N2) :- N2 is N1 - 1.

%adyacente(+F1, +C1, ?F2, ?C2)
adyacente(F1,C1,F1,C2) :- dif1(C1,C2).
adyacente(F1,C1,F2,C1) :- dif1(F1,F2).
adyacente(F1,C1,F2,C2) :- dif1(C1,C2), dif1(F1,F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :- F > 0, C > 0, length([Fila|Filas], FMax), F =< FMax, length(Fila, CMax), C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T,F1,C1,F2,C2) :- adyacente(F1,C1,F2,C2), enRango(T,F2,C2).

%------------------Predicados extra:------------------%

%free(+Tablero, +Fila, +Columna)
free(Tablero, Fila, Columna) :- nth1(Fila, Tablero, Row), nth1(Columna, Row, X), var(X).

%instanciarCasillero(?X)
instanciarCasillero(X) :- var(X), X = ~ .
instanciarCasillero(X) :- nonvar(X).

%------------------Predicados a definir:------------------%

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)

%disponible(+Tablero, ?Fila, ?Columna)
disponible(Tablero, Fila, Columna) :- free(Tablero, Fila, Columna),
    forall(adyacenteEnRango(Tablero, Fila, Columna, F, C), free(Tablero, F, C)).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)

%ubicarBarcos(+Barcos, +?Tablero)

%completarConAgua(+?Tablero)
completarConAgua(Tablero) :- maplist(maplist(instanciarCasillero), Tablero).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)

% Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).

test(3) :- T = [[~, _, _],[o, _, _]], contenido(T,1,1,~), contenido(T,2,1,o).
test(4) :- T = [[~, _, _],[o, _, _]], contenido(T,1,1,X), contenido(T,2,1,Y), X = ~, Y = o.
test(5) :- T = [[o, ~],[o, ~]], setof((Y,X,C), contenido(T,Y,X,C), [(1,1,o), (1,2,~), (2,1,o), (2,2,~)]).

test(6) :- T = [[o, _, _],[~, _, _]], disponible(T,2,3), not(disponible(T,1,2)).
test(7) :- T = [[o, _, _],[~, _, _]], setof((Y,X), disponible(T,Y,X), [(1,3), (2,3)]).

test(8) :- matriz(M,2,3), contenido(M,2,1,o), puedoColocar(2,vertical,M,1,3).
test(9) :- matriz(M,2,3), setof((Dir,F,C), puedoColocar(3,Dir,M,F,C),
                              [(horizontal,1,1), (horizontal,1,2), (horizontal,2,1), (horizontal,2,2)]).
test(10) :- matriz(M,2,2), setof((Dir,F,C), puedoColocar(2,Dir,M,F,C),
                              [(horizontal,1,1), (horizontal,2,1), (vertical,1,1), (vertical,1,2)]).

test(11) :- matriz(M,3,2), setof(M, ubicarBarcos([2,1],M), [ [[o,o], [_,_], [o,_]], [[o,o], [_,_], [_,o]],
                                                                 [[o,_], [_,_], [o,o]], [[_,o], [_,_], [o,o]] ]).

test(12) :- matriz(M,3,2), completarConAgua(M), not(free(M,_,_)).
test(13) :- T = [[o, _, _],[~, _, o]], completarConAgua(T), T = [[o, ~, ~],[~, ~, o]].

test(14) :- T = [[o, _, _],[~, _, o]], golpear(T,2,3,R), R = [[o, _, _],[~, _, ~]].
test(15) :- T = [[o, _, _],[~, _, o]], golpear(T,2,2,R), R = [[o, _, _],[~, ~, o]].

test(16) :- T = [[o,o], [~,~], [~,o]], atacar(T,1,1,Res,R), Res = tocado, R = [[~,o], [~,~], [~,o]].
test(17) :- T = [[o,o], [~,~], [~,o]], atacar(T,3,1,Res,R), Res = agua, R = T.
test(18) :- T = [[o,o], [~,~], [~,o]], atacar(T,3,2,Res,R), Res = hundido, R = [[~,o], [~,~], [~,~]].

tests :- forall(between(1,18,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.

