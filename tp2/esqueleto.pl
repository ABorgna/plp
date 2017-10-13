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
disponible(Tablero, Fila, Columna) :-
    matriz(Tablero, M, N),
    between(1, M, Fila), between(1, N, Columna), free(Tablero, Fila, Columna),
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
tests :- forall(between(1,2,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.