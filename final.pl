% 2025-I

% Desarrolle un programa en PROLOG que en base a la imagen considere;
% -	Un viajero sale de un país a conocer diferentes países del mundo con la idea de retornar al país de donde partió.
% -	Cada vuelo (digamos Perú- Brasil) tiene un mismo costo y duración para todos los clientes, cada vuelo entre países diferentes tiene diferente costo y duración. 
% -	Los viajeros pueden visitar de manera consecutiva tantos países como deseen y si desean retornar a su país.
% -	En base a dichas condiciones responda las siguientes preguntas;

% 1.	¿Quiénes han salido de viaje y aun no regresan?
% 2.	¿Cuánto han gastado los que han salido de Perú?
% 3.	¿Qué países han visitado los que han salido de Perú?
% 4.	¿Qué porcentaje de los viajeros conocen La India?
% 5.	¿Cuántas horas ha viajado Ana?
% 6.	Agregar una o varias condiciones y plantee y responda una pregunta

% filepath: d:\WORKSPACE\PP\EXAMEN FINAL\PP\final.pl
% --- Datos de vuelos: vuelo(Origen, Destino, Costo, DuracionHoras).
vuelo(peru, brasil, 500, 5).
vuelo(brasil, india, 900, 12).
vuelo(india, francia, 800, 10).
vuelo(francia, peru, 700, 11).
vuelo(peru, chile, 200, 2).
vuelo(chile, india, 950, 13).
vuelo(peru, india, 1200, 18).
vuelo(peru, eeuu, 1000, 8).
vuelo(eeuu, francia, 1100, 9).
vuelo(francia, china, 1200, 11).
vuelo(china, australia, 1300, 12).
vuelo(australia, peru, 1400, 15).
vuelo(india, china, 700, 6).
vuelo(brasil, eeuu, 900, 8).

% --- Datos de viajes: viaje(Viajero, PaisOrigen, [ListaDePaisesVisitadosIncluyendoOrigen]).
viaje(ana, peru, [peru, brasil, india, francia]). % No regresa
viaje(juan, peru, [peru, chile, india, francia, peru]). % Regresa
viaje(luis, brasil, [brasil, india, francia, china, australia, brasil]). % Regresa
viaje(maria, peru, [peru, eeuu, francia, china, india, australia, peru]). % Regresa
viaje(carla, chile, [chile, brasil, eeuu, francia, india]). % No regresa

% --- Nuevos hechos: género(Viajero, Genero).
genero(ana, femenino).
genero(juan, masculino).
genero(luis, masculino).
genero(maria, femenino).
genero(carla, femenino).

% --- Nuevos hechos: continente(Pais, Continente).
continente(peru, america_sur).
continente(brasil, america_sur).
continente(india, asia).
continente(francia, europa).
continente(chile, america_sur).
continente(eeuu, america_norte).
continente(china, asia).
continente(australia, oceania).

listar_paises(PaisesUnicos) :-
   findall(Pais, (vuelo(Pais, _, _, _) ; vuelo(_, Pais, _, _)), TodosLosPaises),
   write('Lista completa de TodosLosPaises: '), write_term(TodosLosPaises, [quoted(true), max_depth(0)]), nl, % <--- Línea añadida
   list_to_set(TodosLosPaises, PaisesUnicos),
   write('Lista completa de PaisesUnicos: '), write_term(PaisesUnicos, [quoted(true), max_depth(0)]), nl. % <--- Línea añadida para PaisesUnicos

% 1. ¿Quiénes han salido de viaje y aun no regresan? (con detalle de país actual)
no_regresan_detalle(Viajero, PaisActual) :-
    viaje(Viajero, PaisOrigen, Lista),
    last(Lista, PaisActual),
    PaisOrigen \= PaisActual.

% 2. ¿Cuánto han gastado los que han salido de Perú? (detalle por tramo y total)
gasto_peru_detallado(Viajero, Tramos, GastoTotal) :-
    viaje(Viajero, peru, Lista),
    tramos_y_gastos(Lista, Tramos, GastoTotal).

tramos_y_gastos([_], [], 0).
tramos_y_gastos([A,B|R], [(A,B,Costo)|Tramos], Total) :-
    (vuelo(A,B,Costo,_) ; vuelo(B,A,Costo,_)),
    tramos_y_gastos([B|R], Tramos, Subtotal),
    Total is Subtotal + Costo.

% 3. ¿Qué países han visitado los que han salido de Perú? (únicos, sin repetir)
paises_visitados_peru(Viajero, PaisesUnicos) :-
    viaje(Viajero, peru, Lista),
    list_to_set(Lista, PaisesUnicos).

% 4. ¿Qué porcentaje de los viajeros conocen La India? (con lista de nombres)
porcentaje_india(Porcentaje, Lista) :-
    findall(V, viaje(V, _, _), Todos),
    findall(V, (viaje(V, _, L), member(india, L)), ConocenIndia),
    length(Todos, Total),
    length(ConocenIndia, Conocen),
    (Total > 0 -> Porcentaje is (Conocen * 100) / Total ; Porcentaje = 0),
    Lista = ConocenIndia.

% 5. ¿Cuántas horas ha viajado Ana? (detalle por tramo y total)
horas_viajadas_detallado(Viajero, Tramos, HorasTotal) :-
    viaje(Viajero, _, Lista),
    tramos_y_horas(Lista, Tramos, HorasTotal).

tramos_y_horas([_], [], 0).
tramos_y_horas([A,B|R], [(A,B,Horas)|Tramos], Total) :-
    (vuelo(A,B,_,Horas) ; vuelo(B,A,_,Horas)),
    tramos_y_horas([B|R], Tramos, Subtotal),
    Total is Subtotal + Horas.

% 6. Nueva condición: ¿Quiénes han visitado China y Australia en el mismo viaje?
china_y_australia(Viajero) :-
    viaje(Viajero, _, Lista),
    member(china, Lista),
    member(australia, Lista).

% Respuesta dinámica a la 2, 3, 4, y 6

% --- Predicados dinámicos generales para las preguntas ---
:- dynamic gasto_detallado/4.
:- dynamic paises_visitados_por_origen/3.
:- dynamic porcentaje_viajeros_conocen_pais/3.
:- dynamic viajeros_visitaron_paises_especificos/3.

% 2. ¿Cuánto han gastado los que han salido de un PaísOrigen específico? (detalle por tramo y total)
gasto_detallado(Viajero, PaisOrigen, Tramos, GastoTotal) :-
    viaje(Viajero, PaisOrigen, Lista),
    tramos_y_gastos(Lista, Tramos, GastoTotal).

% 3. ¿Qué países han visitado los que han salido de un PaísOrigen específico? (únicos, sin repetir)
paises_visitados_por_origen(Viajero, PaisOrigen, PaisesUnicos) :-
    viaje(Viajero, PaisOrigen, Lista),
    list_to_set(Lista, PaisesUnicos).

% 4. ¿Qué porcentaje de los viajeros conocen un País específico? (con lista de nombres)
porcentaje_viajeros_conocen_pais(Pais, Porcentaje, Lista) :-
    findall(V, viaje(V, _, _), Todos),
    findall(V, (viaje(V, _, L), member(Pais, L)), ConocenPais),
    length(Todos, Total),
    length(ConocenPais, Conocen),
    (Total > 0 -> Porcentaje is (Conocen * 100) / Total ; Porcentaje = 0),
    Lista = ConocenPais.

% Predicado auxiliar para obtener los continentes visitados por un viajero
continentes_visitados(Viajero, ContinentesUnicos) :-
    viaje(Viajero, _, ListaPaises),
    findall(C, (member(P, ListaPaises), continente(P, C)), Continentes),
    list_to_set(Continentes, ContinentesUnicos).

% Predicado principal para la pregunta 6
viajeros_multi_continente(Viajero) :-
    continentes_visitados(Viajero, Continentes),
    length(Continentes, NumContinentes),
    NumContinentes > 1.

% --- Nueva funcionalidad: Porcentaje de género que llegaron a un país ---

% Predicado auxiliar para encontrar todos los viajeros que pasaron por un país
viajeros_que_visitaron_pais(Pais, Viajeros) :-
    findall(V, (viaje(V, _, ListaPaises), member(Pais, ListaPaises)), Viajeros).

% Predicado auxiliar para contar géneros y listar viajeros por género
contar_generos_en_lista([], 0, 0, [], []).
contar_generos_en_lista([Viajero|Resto], CountFemenino, CountMasculino, ListaFemenino, ListaMasculino) :-
    % Primero, realiza la llamada recursiva para obtener los valores previos
    contar_generos_en_lista(Resto, PrevFemenino, PrevMasculino, PrevListaFemenino, PrevListaMasculino),
    genero(Viajero, femenino),
    CountFemenino is PrevFemenino + 1,
    CountMasculino = PrevMasculino, % El conteo masculino no cambia
    ListaFemenino = [Viajero|PrevListaFemenino],
    ListaMasculino = PrevListaMasculino.
contar_generos_en_lista([Viajero|Resto], CountFemenino, CountMasculino, ListaFemenino, ListaMasculino) :-
    % Primero, realiza la llamada recursiva para obtener los valores previos
    contar_generos_en_lista(Resto, PrevFemenino, PrevMasculino, PrevListaFemenino, PrevListaMasculino),
    genero(Viajero, masculino),
    CountMasculino is PrevMasculino + 1,
    CountFemenino = PrevFemenino, % El conteo femenino no cambia
    ListaMasculino = [Viajero|PrevListaMasculino],
    ListaFemenino = PrevListaFemenino.


% Predicado principal para calcular el porcentaje de género que llegaron a un país
% porcentaje_genero_llegan_pais(Pais, PorcentajeFemenino, PorcentajeMasculino, ListaFemenino, ListaMasculino)
porcentaje_genero_llegan_pais(Pais, PorcentajeFemenino, PorcentajeMasculino, ListaFemenino, ListaMasculino) :-
    viajeros_que_visitaron_pais(Pais, Viajeros),
    list_to_set(Viajeros, ViajerosUnicos), % Asegurarse de contar cada viajero una vez
    contar_generos_en_lista(ViajerosUnicos, CountFemenino, CountMasculino, ListaFemenino, ListaMasculino),
    TotalViajeros is CountFemenino + CountMasculino,
    (TotalViajeros > 0 ->
        PorcentajeFemenino is (CountFemenino * 100) / TotalViajeros,
        PorcentajeMasculino is (CountMasculino * 100) / TotalViajeros
    ;
        PorcentajeFemenino = 0,
        PorcentajeMasculino = 0
    ).

% --- Permitir agregar vuelos y puntos de viaje dinámicamente ---

:- dynamic vuelo/4.
:- dynamic viaje/3.
:- dynamic genero/2. % Hacer dinámico el predicado genero
:- dynamic continente/2. % Hacer dinámico el predicado continente

% Agregar un nuevo vuelo entre dos países
agregar_vuelo(Origen, Destino, Costo, Duracion) :-
    \+ vuelo(Origen, Destino, _, _),
    assertz(vuelo(Origen, Destino, Costo, Duracion)).



% Agregar un nuevo punto de viaje a un viajero existente (al final de la lista)
% Solo si NuevoPais existe como origen o destino en algún vuelo
agregar_punto_viaje(Viajero, NuevoPais) :-
    viaje(Viajero, Origen, ListaVieja),
    last(ListaVieja, Ultimo),
    (vuelo(Ultimo, NuevoPais, _, _) ; vuelo(NuevoPais, Ultimo, _, _)),
    (vuelo(NuevoPais, _, _, _) ; vuelo(_, NuevoPais, _, _)), % NuevoPais debe ser origen o destino de algún vuelo
    append(ListaVieja, [NuevoPais], ListaNueva),
    retract(viaje(Viajero, Origen, ListaVieja)),
    assertz(viaje(Viajero, Origen, ListaNueva)),
    write('Punto de viaje agregado exitosamente.'), nl.

agregar_punto_viaje(Viajero, NuevoPais) :-
    viaje(Viajero, _, ListaVieja),
    last(ListaVieja, Ultimo),
    (\+ vuelo(Ultimo, NuevoPais, _, _), \+ vuelo(NuevoPais, Ultimo, _, _)),
    write('No existe un vuelo entre '), write(Ultimo), write(' y '), write(NuevoPais), nl, !, fail.
agregar_punto_viaje(Viajero, NuevoPais) :-
    viaje(Viajero, _, ListaVieja),
    last(ListaVieja, _),
    (\+ vuelo(NuevoPais, _, _, _), \+ vuelo(_, NuevoPais, _, _)),
    write('El país '), write(NuevoPais), write(' no está registrado en vuelos.'), nl, !, fail.


% Agregar un nuevo viaje: solo persona y lugar de origen
agregar_viajero(Viajero, Origen) :-
    \+ viaje(Viajero, _, _),
    assertz(viaje(Viajero, Origen, [Origen])).

% Agregar un nuevo género para un viajero
agregar_genero(Viajero, Genero) :-
    \+ genero(Viajero, _),
    assertz(genero(Viajero, Genero)).

% Agregar un nuevo continente para un país
agregar_continente(Pais, Continente) :-
    \+ continente(Pais, _),
    assertz(continente(Pais, Continente)).


% Ejemplo de uso:
% ?- agregar_vuelo(peru, mexico, 800, 7).
% ?- agregar_punto_viaje(ana, china).
% ?- agregar_viajero(sofia, peru).
% ?- agregar_genero(sofia, femenino).
% ?- agregar_continente(mexico, america_norte).

% Más consultas de ejemplo:
% ¿Quiénes no han regresado y en qué país están?
% ?- no_regresan_detalle(Viajero, PaisActual).

% ¿Cuánto gastó cada viajero que salió de Perú? (detalle de tramos y total)
% ?- gasto_peru_detallado(Viajero, Tramos, GastoTotal).

% ¿Qué países únicos visitó cada viajero que salió de Perú?
% ?- paises_visitados_peru(Viajero, PaisesUnicos).

% ¿Qué porcentaje de los viajeros conocen La India y quiénes son?
% ?- porcentaje_india(Porcentaje, Lista).

% ¿Cuántas horas ha viajado Ana? (detalle de tramos y total)
% ?- horas_viajadas_detallado(ana, Tramos, HorasTotal).

% ¿Quiénes han visitado China y Australia en el mismo viaje?
% ?- china_y_australia(Viajero).

% ¿Cuántos viajeros han visitado más de 3 países?
% ?- viaje(Viajero, _, Lista), list_to_set(Lista, Set), length(Set, N), N > 3.

% ¿Cuál es el país final de cada viajero?
% ?- viaje(Viajero, _, Lista), last(Lista, PaisFinal).

% ¿Cuántos vuelos ha tomado cada viajero?
% ?- viaje(Viajero, _, Lista), length(Lista, N), Vuelos is N-1.

% ¿Quiénes han pasado por Francia?
% ?- viaje(Viajero, _, Lista), member(francia, Lista).



% =====================
% CONSULTAS GENERALES
% =====================

% ¿Qué viajeros han pasado por un país específico?
viajero_paso_por(Viajero, Pais) :-
    viaje(Viajero, _, Lista),
    member(Pais, Lista).

% ¿Qué viajeros han pasado por dos países específicos en el mismo viaje?
viajero_paso_por(Viajero, Pais1, Pais2) :-
    viaje(Viajero, _, Lista),
    member(Pais1, Lista),
    member(Pais2, Lista).

% Verifica si un viajero regresó a su punto de origen
viajero_regreso_origen(Viajero) :-
    viaje(Viajero, Origen, Lista),
    last(Lista, Origen).

% Devuelve la lista de viajeros que hicieron un viaje circular (salieron y regresaron al país de origen)
viajeros_viaje_circular(Lista) :-
    findall(Viajero, viajero_regreso_origen(Viajero), Lista).

% Consulta para la nueva pregunta 6: ¿Qué viajeros han visitado países en más de un continente?
% ?- viajeros_multi_continente(Viajero).

% Consulta para la nueva funcionalidad: Porcentaje de género que llegaron a un país
% Ejemplo: ¿Qué porcentaje de viajeros femeninos y masculinos han visitado India?
% ?- porcentaje_genero_llegan_pais(india, PorcentajeFemenino, PorcentajeMasculino, ListaFemenino, ListaMasculino).

% ¿Quiénes han visitado todos los continentes?
% Primero, obtén la lista de todos los continentes:
% ?- findall(C, continente(_, C), L), list_to_set(L, Todos), continentes_visitados(V, CV), sort(CV, CVS), sort(Todos, TS), CVS = TS.

% ¿Quién visitó más países?
% ?- findall(N-V, (viaje(V, _, L), list_to_set(L, S), length(S, N)), P), sort(P, SP), reverse(SP, [Max-Viajero|_]).

% ¿Quién ha pasado más de una vez por un país?
% ?- viaje(V, _, L), select(P, L, R), member(P, R).

% ¿Qué países nunca han sido visitados?
% ?- listar_paises(Todos), findall(P, (viaje(_, _, L), member(P, L)), Visitados), list_to_set(Visitados, SetV), subtract(Todos, SetV, NoVisitados).

% ¿Cuál es el país más visitado?
% ?- findall(P, (viaje(_, _, L), member(P, L)), Todos), msort(Todos, S), clumped(S, Clumps), sort(2, @>=, Clumps, [Max-Count|_]).

% ¿Quién hizo viaje circular y cuántos países distintos visitó?
% ?- viaje(V, O, L), last(L, O), list_to_set(L, S), length(S, N).

% ¿Cuántos viajeros femeninos y masculinos hay?
% ?- findall(V, genero(V, femenino), Fems), length(Fems, NF), findall(V, genero(V, masculino), Macs), length(Macs, NM).

