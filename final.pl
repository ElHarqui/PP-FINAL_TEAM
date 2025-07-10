% =============================================================
% PROLOG - PROGRAMA GENERAL DE VIAJES Y GASTOS ENTRE PAÍSES
% =============================================================
% Modela viajeros, vuelos, gastos, recorridos, idiomas, géneros, continentes y consultas generales.
% Autor: Sebastian Peralta, Andres Ordoñez
% Fecha: 2025-I
% -------------------------------------------------------------

% =====================
% HECHOS BÁSICOS
% =====================

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
vuelo(australia, brasil, 1500, 15).
vuelo(australia, peru, 1400, 15).
vuelo(india, china, 700, 6).
vuelo(francia, india, 900, 10).
vuelo(brasil, eeuu, 900, 8).
vuelo(chile, brasil, 600, 6).

% --- Datos de viajes: viaje(Viajero, PaisOrigen, [ListaDePaisesVisitadosIncluyendoOrigen]).
viaje(ana, peru, [peru, brasil, india, francia]). % No regresa
viaje(juan, peru, [peru, chile, india, francia, peru]). % Regresa
viaje(luis, brasil, [brasil, india, francia, china, australia, brasil]). % Regresa
viaje(maria, peru, [peru, eeuu, francia, china, india, peru]). % Regresa
viaje(carla, chile, [chile, brasil, eeuu, francia, india]). % No regresa

% --- Género de los viajeros
genero(ana, femenino).
genero(juan, masculino).
genero(luis, masculino).
genero(maria, femenino).
genero(carla, femenino).

% --- Progenitores
progenitor(luis, juan).
progenitor(ana, juan).
progenitor(luis, maria).
progenitor(carla, maria).

% --- País natal de los viajeros
pais(ana, peru).
pais(juan, peru).
pais(luis, brasil).
pais(maria, peru).
pais(carla, chile).

% --- Idioma nativo de cada país
idioma_nativo(peru, espanol).
idioma_nativo(brasil, portugues).
idioma_nativo(india, hindi).
idioma_nativo(francia, frances).
idioma_nativo(chile, espanol).
idioma_nativo(eeuu, ingles).
idioma_nativo(china, chino).
idioma_nativo(australia, ingles).

% --- Continente de cada país
continente(peru, america_sur).
continente(brasil, america_sur).
continente(india, asia).
continente(francia, europa).
continente(chile, america_sur).
continente(eeuu, america_norte).
continente(china, asia).
continente(australia, oceania).

% =====================
% PREDICADOS AUXILIARES Y REGLAS GENERALES
% =====================

% --- Predicado auxiliar para vuelos bidireccionales ---
vuelo_bidireccional(A, B, Costo, Duracion) :-
    vuelo(A, B, Costo, Duracion).
vuelo_bidireccional(A, B, Costo, Duracion) :-
    vuelo(B, A, Costo, Duracion).

% --- Idiomas ---
idioma_natal(Persona, Idioma):-
    pais(Persona, Pais),
    idioma_nativo(Pais, Idioma).

hereda_idioma_natal(Hijo, Idioma):-
    progenitor(Progenitor, Hijo),
    idioma_natal(Progenitor, Idioma).

idiomas_totales(Persona, IdiomasFinales):-
    idioma_natal(Persona, IdiomaNatal),
    findall(IdiomaHeredado, hereda_idioma_natal(Persona, IdiomaHeredado), IdiomasHeredados),
    IdiomasAprendidos = [],
    append(IdiomasAprendidos, IdiomasHeredados, OtrosIdiomas),
    ListaConDuplicados = [IdiomaNatal | OtrosIdiomas],
    list_to_set(ListaConDuplicados, IdiomasFinales).

sabe_idioma(Persona, Idioma):-
    idiomas_totales(Persona, ListaDeIdiomas),
    member(Idioma, ListaDeIdiomas).

sabe_algun_idioma(Persona, [Idioma|_]) :-
    sabe_idioma(Persona, Idioma), !.
sabe_algun_idioma(Persona, [_|Resto]) :-
    sabe_algun_idioma(Persona, Resto).

sabe_todos_los_idiomas(_, []).
sabe_todos_los_idiomas(Persona, [Idioma|Resto]) :-
    sabe_idioma(Persona, Idioma),
    sabe_todos_los_idiomas(Persona, Resto).

idiomas_tramo(Viajero, IdiomasUnicos) :-
    viaje(Viajero, _, PaisesVisitados),
    findall(Idioma, (member(Pais, PaisesVisitados), idioma_nativo(Pais, Idioma)), TodosLosIdiomas),
    list_to_set(TodosLosIdiomas, IdiomasUnicos).

idiomas_traductores_requiere(Viajero, IdiomasPendientes) :-
    viaje(Viajero, _, PaisesVisitados),
    findall(IdiomaPais, (member(Pais, PaisesVisitados), idioma_nativo(Pais, IdiomaPais)), TodosLosIdiomasVisitados),
    list_to_set(TodosLosIdiomasVisitados, IdiomasUnicosVisitados),
    idiomas_totales(Viajero, IdiomasQueSabe),
    subtract(IdiomasUnicosVisitados, IdiomasQueSabe, IdiomasPendientes).

listar_paises(PaisesUnicos) :-
    findall(Pais, (vuelo(Pais, _, _, _) ; vuelo(_, Pais, _, _)), TodosLosPaises),
    list_to_set(TodosLosPaises, PaisesUnicos).

% =====================
% CONSULTAS PRINCIPALES
% =====================

% 1. ¿Quiénes han salido de viaje y aun no regresan? (con detalle de país actual)
no_regresan_detalle(Viajero, PaisActual) :-
    viaje(Viajero, PaisOrigen, Lista),
    last(Lista, PaisActual),
    PaisOrigen \= PaisActual.

no_regresan_lista(Lista) :-
    findall(Viajero-PaisActual, no_regresan_detalle(Viajero, PaisActual), Lista).

% 2. ¿Cuánto han gastado los que han salido de un PaísOrigen específico?
tramos_y_gastos([_], [], 0).
tramos_y_gastos([A,B|R], [(A,B,Costo)|Tramos], Total) :-
    vuelo_bidireccional(A, B, Costo, _),
    tramos_y_gastos([B|R], Tramos, Subtotal),
    Total is Subtotal + Costo.

gasto_detallado_lista(PaisOrigen) :-
    gasto_detallado_lista(PaisOrigen, Lista, GastoTotalTodos),
    write('==== Gasto detallado de viajeros que salieron de '), write(PaisOrigen), writeln(' ===='), nl,
    forall(member(Viajero-Tramos-Gasto, Lista), (
        write('Viajero: '), write(Viajero), nl,
        write('  Tramos: '), write(Tramos), nl,
        write('  Gasto total: '), write(Gasto), nl,
        writeln('---------------------------------')
    )),
    write('Gasto total de todos los viajeros: '), writeln(GastoTotalTodos).

gasto_detallado_lista(PaisOrigen, Lista, GastoTotalTodos) :-
    findall(
        Viajero-Tramos-GastoViajero,
        (viaje(Viajero, PaisOrigen, L), tramos_y_gastos(L, Tramos, GastoViajero)),
        Lista
    ),
    findall(GastoViajero,
        (viaje(Viajero, PaisOrigen, L), tramos_y_gastos(L, _, GastoViajero)),
        Gastos
    ),
    sumlist(Gastos, GastoTotalTodos).

gasto_detallado(Viajero, PaisOrigen, Tramos, GastoTotal) :-
    viaje(Viajero, PaisOrigen, Lista),
    tramos_y_gastos(Lista, Tramos, GastoTotal).

% 3. ¿Qué países han visitado los que han salido de un PaísOrigen específico?
paises_visitados_por_origen(Viajero, PaisOrigen, PaisesUnicos) :-
    viaje(Viajero, PaisOrigen, Lista),
    list_to_set(Lista, PaisesUnicos).

% 4. ¿Qué porcentaje de los viajeros conocen un País específico?
porcentaje_viajeros_conocen_pais(Pais, Porcentaje, ListaUnica) :-
    findall(V, viaje(V, _, _), Todos),
    findall(V, (viaje(V, _, L), member(Pais, L)), ConocenPaisConDuplicados),
    list_to_set(ConocenPaisConDuplicados, ListaUnica),
    length(Todos, Total),
    length(ListaUnica, Conocen),
    (Total > 0 -> Porcentaje is (Conocen * 100) / Total ; Porcentaje = 0).

% 5. ¿Cuántas horas ha viajado un viajero específico?
horas_viajadas_detallado(Viajero, Tramos, HorasTotal) :-
    viaje(Viajero, _, Lista),
    tramos_y_horas(Lista, Tramos, HorasTotal).

tramos_y_horas([_], [], 0).
tramos_y_horas([A,B|R], [(A,B,Horas)|Tramos], Total) :-
    vuelo_bidireccional(A, B, _, Horas),
    tramos_y_horas([B|R], Tramos, Subtotal),
    Total is Subtotal + Horas.

% 6. ¿Quiénes han visitado dos países específicos en el mismo viaje?
viajeros_visitaron_paises_especificos(Viajero, Pais1, Pais2) :-
    viaje(Viajero, _, Lista),
    member(Pais1, Lista),
    member(Pais2, Lista).

% =====================
% FUNCIONES GENERALES Y DINÁMICAS
% =====================

:- dynamic vuelo/4.
:- dynamic viaje/3.
:- dynamic genero/2.
:- dynamic continente/2.
:- dynamic gasto_detallado/4.
:- dynamic paises_visitados_por_origen/3.
:- dynamic porcentaje_viajeros_conocen_pais/3.
:- dynamic viajeros_visitaron_paises_especificos/3.

% --- Agregar hechos dinámicamente ---
agregar_vuelo(Origen, Destino, Costo, Duracion) :-
    \+ vuelo(Origen, Destino, _, _),
    assertz(vuelo(Origen, Destino, Costo, Duracion)).

agregar_punto_viaje(Viajero, NuevoPais) :-
    viaje(Viajero, Origen, ListaVieja),
    last(ListaVieja, Ultimo),
    vuelo_bidireccional(Ultimo, NuevoPais, _, _),
    (vuelo(NuevoPais, _, _, _) ; vuelo(_, NuevoPais, _, _)),
    append(ListaVieja, [NuevoPais], ListaNueva),
    retract(viaje(Viajero, Origen, ListaVieja)),
    assertz(viaje(Viajero, Origen, ListaNueva)),
    write('Punto de viaje agregado exitosamente.'), nl.

agregar_punto_viaje(Viajero, NuevoPais) :-
    viaje(Viajero, _, ListaVieja),
    last(ListaVieja, Ultimo),
    \+ vuelo_bidireccional(Ultimo, NuevoPais, _, _),
    write('No existe un vuelo entre '), write(Ultimo), write(' y '), write(NuevoPais), nl, !, fail.

agregar_punto_viaje(Viajero, NuevoPais) :-
    viaje(Viajero, _, ListaVieja),
    last(ListaVieja, _),
    (\+ vuelo(NuevoPais, _, _, _), \+ vuelo(_, NuevoPais, _, _)),
    write('El país '), write(NuevoPais), write(' no está registrado en vuelos.'), nl, !, fail.

agregar_viajero(Viajero, Origen) :-
    \+ viaje(Viajero, _, _),
    assertz(viaje(Viajero, Origen, [Origen])).

agregar_genero(Viajero, Genero) :-
    \+ genero(Viajero, _),
    assertz(genero(Viajero, Genero)).

agregar_continente(Pais, Continente) :-
    \+ continente(Pais, _),
    assertz(continente(Pais, Continente)).

% =====================
% CONSULTAS GENERALES
% =====================

viajero_paso_por(Viajero, Pais) :-
    viaje(Viajero, _, Lista),
    member(Pais, Lista).

viajero_paso_por(Viajero, Pais1, Pais2) :-
    viaje(Viajero, _, Lista),
    member(Pais1, Lista),
    member(Pais2, Lista).

viajero_regreso_origen(Viajero) :-
    viaje(Viajero, Origen, Lista),
    last(Lista, Origen).

% --- Continentes visitados ---
continentes_visitados(Viajero, ContinentesUnicos) :-
    viaje(Viajero, _, ListaPaises),
    findall(C, (member(P, ListaPaises), continente(P, C)), Continentes),
    list_to_set(Continentes, ContinentesUnicos).

viajeros_multi_continente(Viajero) :-
    continentes_visitados(Viajero, Continentes),
    length(Continentes, NumContinentes),
    NumContinentes > 1.

% --- Porcentaje de género que llegaron a un país ---
viajeros_que_visitaron_pais(Pais, Viajeros) :-
    findall(V, (viaje(V, _, ListaPaises), member(Pais, ListaPaises)), Viajeros).

contar_generos_en_lista([], 0, 0, [], []).
contar_generos_en_lista([Viajero|Resto], CountFemenino, CountMasculino, ListaFemenino, ListaMasculino) :-
    contar_generos_en_lista(Resto, PrevFemenino, PrevMasculino, PrevListaFemenino, PrevListaMasculino),
    genero(Viajero, femenino),
    CountFemenino is PrevFemenino + 1,
    CountMasculino = PrevMasculino,
    ListaFemenino = [Viajero|PrevListaFemenino],
    ListaMasculino = PrevListaMasculino.
contar_generos_en_lista([Viajero|Resto], CountFemenino, CountMasculino, ListaFemenino, ListaMasculino) :-
    contar_generos_en_lista(Resto, PrevFemenino, PrevMasculino, PrevListaFemenino, PrevListaMasculino),
    genero(Viajero, masculino),
    CountMasculino is PrevMasculino + 1,
    CountFemenino = PrevFemenino,
    ListaMasculino = [Viajero|PrevListaMasculino],
    ListaFemenino = PrevListaFemenino.

porcentaje_genero_llegan_pais(Pais, PorcentajeFemenino, PorcentajeMasculino, ListaFemenino, ListaMasculino) :-
    viajeros_que_visitaron_pais(Pais, Viajeros),
    list_to_set(Viajeros, ViajerosUnicos),
    contar_generos_en_lista(ViajerosUnicos, CountFemenino, CountMasculino, ListaFemenino, ListaMasculino),
    TotalViajeros is CountFemenino + CountMasculino,
    (TotalViajeros > 0 ->
        PorcentajeFemenino is (CountFemenino * 100) / TotalViajeros,
        PorcentajeMasculino is (CountMasculino * 100) / TotalViajeros
    ;
        PorcentajeFemenino = 0,
        PorcentajeMasculino = 0
    ).

/*
=====================
PREGUNTAS Y CONSULTAS DE EJEMPLO
=====================

% ¿Quiénes han salido de viaje y aún no regresan?
% ?- no_regresan_detalle(Viajero, PaisActual).
% Presionar “;” para que siga mostrando el siguiente resultado.

% ¿Cuánto han gastado los que han salido de un país específico?
% ?- gasto_detallado_lista(brasil).
% ?- gasto_detallado_lista(PaisOrigen).
% Presionar “;” para que siga mostrando el siguiente resultado.

% ¿Qué países han visitado los que han salido de Perú?
% ?- paises_visitados_por_origen(Viajero, peru, PaisUnico).
% Presionar “;” para que siga mostrando el siguiente resultado.

% ¿Qué porcentaje de los viajeros conocen La India?
% ?- porcentaje_viajeros_conocen_pais(india, Porcentaje, Lista).

% ¿Cuántas horas ha viajado Ana?
% ?- horas_viajadas_detallado(ana, Tramos, HorasTotal).
*/

