% Árbol Genealógico - 4 Generaciones (Sin duplicados)
% Sistema optimizado que evita backtracking multiple

% Género
hombre(alberto).    % Bisabuelo
hombre(carlos).     % Abuelo  
hombre(fernando).   % Padre
hombre(ivan).       % Hijo
hombre(javier).     % Hijo
hombre(diego).      % Bisabuelo
hombre(eduardo).    % Abuelo
hombre(gabriel).    % Padre
hombre(luis).       % Hijo
hombre(marcos).     % Hijo
hombre(hugo).       % Padre
hombre(nicolas).    % Hijo
hombre(oscar).      % Hijo
hombre(pablo).      % Nieto
hombre(antonio).    % Nieto
hombre(andres).     % Nieto

mujer(beatriz).     % Bisabuela
mujer(diana).       % Abuela
mujer(gloria).      % Madre
mujer(julia).       % Hija
mujer(laura).       % Hija
mujer(carmen).      % Bisabuela
mujer(francisca).   % Abuela
mujer(helena).      % Madre
mujer(maria).       % Hija
mujer(natalia).     % Hija
mujer(isabel).      % Madre
mujer(olivia).      % Hija
mujer(patricia).    % Hija
mujer(sofia).       % Nieta
mujer(camila).      % Nieta
mujer(valentina).   % Nieta

% Matrimonios (esposo, esposa)
casado(alberto, beatriz).
casado(carlos, diana).
casado(fernando, gloria).
casado(ivan, julia).
casado(javier, laura).
casado(diego, carmen).
casado(eduardo, francisca).
casado(gabriel, helena).
casado(luis, maria).
casado(marcos, natalia).
casado(hugo, isabel).
casado(nicolas, olivia).
casado(oscar, patricia).

% Relaciones padre-hijo (una sola línea de descendencia por familia)
padre(alberto, carlos).
padre(carlos, fernando).
padre(fernando, ivan).
padre(fernando, javier).
padre(ivan, pablo).
padre(javier, antonio).

padre(diego, eduardo).
padre(eduardo, gabriel).
padre(gabriel, luis).
padre(gabriel, marcos).
padre(luis, sofia).
padre(marcos, camila).

padre(eduardo, hugo).
padre(hugo, nicolas).
padre(hugo, oscar).
padre(nicolas, andres).
padre(oscar, valentina).

% Relaciones madre-hijo
madre(beatriz, carlos).
madre(diana, fernando).
madre(gloria, ivan).
madre(gloria, javier).
madre(julia, pablo).
madre(laura, antonio).

madre(carmen, eduardo).
madre(francisca, gabriel).
madre(helena, luis).
madre(helena, marcos).
madre(maria, sofia).
madre(natalia, camila).

madre(francisca, hugo).
madre(isabel, nicolas).
madre(isabel, oscar).
madre(olivia, andres).
madre(patricia, valentina).

% REGLAS OPTIMIZADAS (sin duplicados)

% Reglas básicas
padre_de(X, Y) :- padre(X, Y).
madre_de(X, Y) :- madre(X, Y).
progenitor(X, Y) :- padre(X, Y) ; madre(X, Y).

% Hijos
hijo(X, Y) :- padre(Y, X), hombre(X).
hijo(X, Y) :- madre(Y, X), hombre(X).
hija(X, Y) :- padre(Y, X), mujer(X).
hija(X, Y) :- madre(Y, X), mujer(X).

% Esposos (reglas simples)
esposo(X, Y) :- casado(X, Y), hombre(X).
esposa(X, Y) :- casado(Y, X), mujer(X).

% Hermanos - sin cut
hermano(X, Y) :- 
    padre(Z, X), 
    padre(Z, Y), 
    X \= Y, 
    hombre(X).

hermana(X, Y) :- 
    padre(Z, X), 
    padre(Z, Y), 
    X \= Y, 
    mujer(X).

hermanos(X, Y) :- 
    padre(Z, X), 
    padre(Z, Y), 
    X \= Y.

% Abuelos - sin cut, estructura mejorada
abuelo(X, Y) :- 
    padre(X, Z), 
    progenitor(Z, Y), 
    hombre(X).

abuela(X, Y) :- 
    madre(X, Z), 
    progenitor(Z, Y), 
    mujer(X).

% Nietos
nieto(X, Y) :- abuelo(Y, X), hombre(X).
nieta(X, Y) :- abuela(Y, X), mujer(X).

% Bisabuelos - sin cut, pero con lógica que evita duplicados naturalmente
bisabuelo(X, Y) :- 
    padre(X, Z), 
    abuelo_interno(Z, Y), 
    hombre(X).

bisabuela(X, Y) :- 
    madre(X, Z), 
    abuelo_interno(Z, Y), 
    mujer(X).

% Predicado auxiliar para abuelos (evita duplicados internos)
abuelo_interno(X, Y) :- 
    padre(X, Z), 
    progenitor(Z, Y).

% Bisnietos
bisnieto(X, Y) :- bisabuelo(Y, X), hombre(X).
bisnieta(X, Y) :- bisabuela(Y, X), mujer(X).

% Tíos - sin cut
tio(X, Y) :- 
    progenitor(Z, Y), 
    hermano(X, Z), 
    hombre(X).

tia(X, Y) :- 
    progenitor(Z, Y), 
    hermana(X, Z), 
    mujer(X).

% Sobrinos
sobrino(X, Y) :- tio(Y, X), hombre(X).
sobrina(X, Y) :- tia(Y, X), mujer(X).

% Primos - sin cut
primo(X, Y) :- 
    padre(Z, X), 
    padre(W, Y), 
    hermanos(Z, W), 
    X \= Y, 
    hombre(X).

prima(X, Y) :- 
    padre(Z, X), 
    padre(W, Y), 
    hermanos(Z, W), 
    X \= Y, 
    mujer(X).

primos(X, Y) :- 
    padre(Z, X), 
    padre(W, Y), 
    hermanos(Z, W), 
    X \= Y.

% Cuñados - sin cut
cunado(X, Y) :- 
    casado(Y, Z), 
    hermano(X, Z), 
    hombre(X).

cunado(X, Y) :- 
    casado(X, Z), 
    hermano(Y, Z), 
    hombre(X).

cunada(X, Y) :- 
    casado(Y, Z), 
    hermana(X, Z), 
    mujer(X).

cunada(X, Y) :- 
    casado(Z, X), 
    hermana(Y, Z), 
    mujer(X).

% Suegros - sin cut
suegro(X, Y) :- 
    casado(Y, Z), 
    padre(X, Z), 
    hombre(X).

suegra(X, Y) :- 
    casado(Y, Z), 
    madre(X, Z), 
    mujer(X).

% Yernos y nueras - sin cut
yerno(X, Y) :- 
    casado(X, Z), 
    progenitor(Y, Z), 
    hombre(X).

nuera(X, Y) :- 
    casado(Z, X), 
    progenitor(Y, Z), 
    mujer(X).

% FUNCIONES ADICIONALES DE DESCENDENCIA

% 21. Descendientes directos (hijos)
descendientes_directos(Persona, Descendiente) :-
    progenitor(Persona, Descendiente).

% 22. Todos los descendientes (hijos, nietos, bisnietos, etc.)
descendiente(Persona, Descendiente) :-
    progenitor(Persona, Descendiente).
descendiente(Persona, Descendiente) :-
    progenitor(Persona, Hijo),
    descendiente(Hijo, Descendiente).

% 23. Solo descendientes de una persona específica
solo_descendientes_de(Persona, ListaDescendientes) :-
    findall(D, descendiente(Persona, D), ListaDescendientes).

% 24. Contar descendientes totales
contar_descendientes(Persona, Cantidad) :-
    findall(D, descendiente(Persona, D), Lista),
    length(Lista, Cantidad).

% 25. Descendientes por generación
descendientes_generacion_1(Persona, Hijo) :-
    progenitor(Persona, Hijo).

descendientes_generacion_2(Persona, Nieto) :-
    progenitor(Persona, Hijo),
    progenitor(Hijo, Nieto).

descendientes_generacion_3(Persona, Bisnieto) :-
    progenitor(Persona, Hijo),
    progenitor(Hijo, Nieto),
    progenitor(Nieto, Bisnieto).

% 26. Descendientes masculinos y femeninos
descendientes_masculinos(Persona, DescendienteMasculino) :-
    descendiente(Persona, DescendienteMasculino),
    hombre(DescendienteMasculino).

descendientes_femeninos(Persona, DescendienteFemenino) :-
    descendiente(Persona, DescendienteFemenino),
    mujer(DescendienteFemenino).

% 27. Linaje completo (ancestros hacia arriba)
ancestro(Ancestro, Persona) :-
    progenitor(Ancestro, Persona).
ancestro(Ancestro, Persona) :-
    progenitor(Padre, Persona),
    ancestro(Ancestro, Padre).

% 28. Árbol genealógico completo de una persona
arbol_genealogico(Persona, Ancestros, Descendientes) :-
    findall(A, ancestro(A, Persona), Ancestros),
    findall(D, descendiente(Persona, D), Descendientes).

% Consultas de ejemplo genealógicas:
% ?- bisabuelo(alberto, X).
% ?- hermanos(ivan, javier).
% ?- primo(luis, nicolas).
% ?- abuelo(carlos, ivan).
% ?- cunado(javier, ivan).
% ?- tio(gabriel, ivan).
% ?- solo_descendientes_de(alberto, X).
% ?- descendientes_masculinos(carlos, X).
% ?- contar_descendientes(alberto, N).

%
%------------------------------------------------------------
%


% ===================== SECCIÓN DE IDIOMAS (CORREGIDA Y COMPLETA) =====================

% Definir idiomas base para todas las personas sin padres (generación base)
% Puedes agregar más hechos habla/2 si hay más personas base
habla(carlos, aleman).
habla(carlos, espanol).
habla(maria, aleman).
habla(maria, ingles).
habla(estefany, ingles).
habla(estefany, espanol).
habla(roberta, frances).
habla(daniel, espanol).
habla(daniel, italiano).
habla(daniel, ruso).
habla(sofia, italiano).
habla(ana, espanol).
habla(ana, portugues).
habla(ana, japones).
habla(carmen, aleman).
habla(carmen, chino).

% Niveles de dificultad (originales + nuevos idiomas)
dificultad(espanol, 1).    % Más fácil
dificultad(italiano, 2).
dificultad(portugues, 2).
dificultad(japones, 3).
dificultad(ingles, 3).
dificultad(frances, 4).
dificultad(ruso, 5).
dificultad(aleman, 5).
dificultad(chino, 6).     % Más difícil

% Predicado principal que determina los idiomas de una persona (sin duplicados)
idiomas_persona(Persona, IdiomasUnicos) :-
    findall(Idioma, idioma_heredado_robusto(Persona, Idioma), Idiomas),
    sort(Idiomas, IdiomasUnicos).

% Herencia robusta: si la persona tiene hechos habla/2, se usan; si no, se hereda de los padres (aunque solo haya uno)
idioma_heredado_robusto(Persona, Idioma) :-
    habla(Persona, Idioma).
idioma_heredado_robusto(Persona, Idioma) :-
    padre(Padre, Persona),
    madre(Madre, Persona),
    idioma_hijo_robusto(Persona, Idioma).
idioma_heredado_robusto(Persona, Idioma) :-
    padre(Padre, Persona),
    \+ madre(_, Persona),
    idiomas_persona(Padre, IdiomasPadre),
    member(Idioma, IdiomasPadre).
idioma_heredado_robusto(Persona, Idioma) :-
    madre(Madre, Persona),
    \+ padre(_, Persona),
    idiomas_persona(Madre, IdiomasMadre),
    member(Idioma, IdiomasMadre).
% Si no tiene padre ni madre ni idioma base, asignar espanol
idioma_heredado_robusto(Persona, espanol) :-
    \+ padre(_, Persona),
    \+ madre(_, Persona),
    \+ habla(Persona, _).

% Herencia robusta (ambos padres):
idioma_hijo_robusto(Hijo, Idioma) :-
    padre(Padre, Hijo),
    madre(Madre, Hijo),
    idiomas_persona(Padre, IdiomasPadre),
    idiomas_persona(Madre, IdiomasMadre),
    member(Idioma, IdiomasPadre),
    member(Idioma, IdiomasMadre).
idioma_hijo_robusto(Hijo, Idioma) :-
    padre(Padre, Hijo),
    madre(Madre, Hijo),
    idiomas_persona(Padre, IdiomasPadre),
    idiomas_persona(Madre, IdiomasMadre),
    subtract(IdiomasPadre, IdiomasMadre, SoloPadre),
    subtract(IdiomasMadre, IdiomasPadre, SoloMadre),
    append(SoloPadre, SoloMadre, IdiomasNoComunes),
    IdiomasNoComunes \= [],
    findall(Nivel-Id, (member(Id, IdiomasNoComunes), dificultad(Id, Nivel)), Niveles),
    keysort(Niveles, [NivelMin-Idioma|_]),
    member(Idioma, IdiomasNoComunes),
    dificultad(Idioma, NivelMin).

% Consulta de idiomas en un paradero (funciona para todos)
idiomas_en_paradero(Paradero, Idioma, Personas) :-
    pasajeros_en(Paradero, Pasajeros),
    findall(P, (
        member(P, Pasajeros),
        idiomas_persona(P, Idiomas),
        member(Idioma, Idiomas)
    ), PersonasSinFiltrar),
    sort(PersonasSinFiltrar, Personas).

% Mostrar hablantes de un idioma en el bus (todas las personas)
mostrar_hablantes(Idioma) :-
    format('\nHABLANTES DE ~w EN EL BUS:~n', [Idioma]),
    forall(paradero(P, _), (
        idiomas_en_paradero(P, Idioma, Personas),
        Personas \= [],
        format('Paradero ~w:~n', [P]),
        forall(member(Persona, Personas), (
            idiomas_persona(Persona, Idiomas),
            format(' - ~w (idiomas: ~w)~n', [Persona, Idiomas])
        )),
        nl
    )).

% Mostrar una tabla con al menos 7 personas y todos los idiomas que saben
mostrar_tabla_idiomas :-
    findall(P, persona(P), PersonasTodas),
    sort(PersonasTodas, PersonasUnicas),
    length(PersonasUnicas, N),
    (N >= 7 -> take(7, PersonasUnicas, PersonasMostrar) ; PersonasMostrar = PersonasUnicas),
    format('~`-t~40|~n'),
    format('~w~t~20+| ~w~n', ['Persona', 'Idiomas']),
    format('~`-t~40|~n'),
    forall(member(P, PersonasMostrar), (
        idiomas_persona(P, Idiomas),
        atomic_list_concat(Idiomas, ', ', IdiomasStr),
        format('~w~t~20+| ~w~n', [P, IdiomasStr])
    )),
    format('~`-t~40|~n').

mostrar_tabla_idiomas(Cantidad) :-
    findall(P, persona(P), PersonasTodas),
    sort(PersonasTodas, PersonasUnicas),
    length(PersonasUnicas, N),
    (N >= Cantidad -> take(Cantidad, PersonasUnicas, PersonasMostrar) ; PersonasMostrar = PersonasUnicas),
    format('~`-t~40|~n'),
    format('~w~t~20+| ~w~n', ['Persona', 'Idiomas']),
    format('~`-t~40|~n'),
    forall(member(P, PersonasMostrar), (
        idiomas_persona(P, Idiomas),
        atomic_list_concat(Idiomas, ', ', IdiomasStr),
        format('~w~t~20+| ~w~n', [P, IdiomasStr])
    )),
    format('~`-t~40|~n').

% -------------------------------------------------------------------
% Obtener todas las personas conocidas en la base de datos
persona(P) :- hombre(P).
persona(P) :- mujer(P).
persona(P) :- habla(P, _).
persona(P) :- sube(P, _).
persona(P) :- baja(P, _).

% Personas que hablan dos idiomas específicos (en cualquier orden, robusto)
personas_hablan_dos_idiomas(Idioma1, Idioma2, Personas) :-
    setof(Persona, Idiomas^(persona(Persona), idiomas_persona(Persona, Idiomas), member(Idioma1, Idiomas), member(Idioma2, Idiomas)), Personas), !.
personas_hablan_dos_idiomas(_, _, []).
% -------------------------------------------------------------------

% ===================== FIN SECCIÓN DE IDIOMAS =====================
 
% Definición de la ruta (orden de paraderos)
paradero(sjl, 1).
paradero('15_de_enero', 2).
paradero(mariategui, 3).
paradero(grau, 4).
paradero(san_marcos, 5).

sube(alberto, sjl).
sube(carlos, sjl).
sube(fernando, sjl).
sube(diego, sjl).

sube(beatriz, '15_de_enero').
sube(diana, '15_de_enero').
sube(gloria, '15_de_enero').
sube(eduardo, '15_de_enero').
sube(francisca, '15_de_enero').

sube(ivan, mariategui).
sube(javier, mariategui).
sube(gabriel, mariategui).
sube(helena, mariategui).
sube(luis, mariategui).
sube(hugo, mariategui).

sube(julia, grau).
sube(laura, grau).
sube(maria, grau).
sube(marcos, grau).
sube(natalia, grau).
sube(isabel, grau).
sube(nicolas, grau).
sube(olivia, grau).
sube(oscar, grau).
sube(patricia, grau).

% baja(Persona, Paradero) - donde baja cada persona
baja(beatriz, '15_de_enero').
baja(eduardo, '15_de_enero').

baja(alberto, mariategui).
baja(diana, mariategui).
baja(ivan, mariategui).
baja(gabriel, mariategui).

baja(carlos, grau).
baja(gloria, grau).
baja(javier, grau).
baja(helena, grau).
baja(julia, grau).
baja(marcos, grau).
baja(isabel, grau).

baja(fernando, san_marcos).
baja(diego, san_marcos).
baja(francisca, san_marcos).
baja(luis, san_marcos).
baja(hugo, san_marcos).
baja(laura, san_marcos).
baja(maria, san_marcos).
baja(natalia, san_marcos).
baja(nicolas, san_marcos).
baja(olivia, san_marcos).
baja(oscar, san_marcos).
baja(patricia, san_marcos).

% FUNCIONES DEL SISTEMA DE TRANSPORTE

% 1. Estaciones después de una estación dada
estacion_despues_de(EstacionBase, EstacionPosterior) :-
    paradero(EstacionBase, Pos1),
    paradero(EstacionPosterior, Pos2),
    Pos2 > Pos1.

% 2. Estaciones antes de una estación dada
estacion_antes_de(EstacionBase, EstacionAnterior) :-
    paradero(EstacionBase, Pos1),
    paradero(EstacionAnterior, Pos2),
    Pos2 < Pos1.

% 3. Estaciones entre dos estaciones
estacion_entre(EstacionInicio, EstacionFin, EstacionIntermedia) :-
    paradero(EstacionInicio, Pos1),
    paradero(EstacionFin, Pos3),
    paradero(EstacionIntermedia, Pos2),
    Pos1 < Pos2,
    Pos2 < Pos3.

% 4. Próxima estación
proxima_estacion(EstacionActual, ProximaEstacion) :-
    paradero(EstacionActual, Pos),
    PosProxima is Pos + 1,
    paradero(ProximaEstacion, PosProxima).

% 5. Estación anterior
estacion_anterior(EstacionActual, EstacionPrevia) :-
    paradero(EstacionActual, Pos),
    PosPrevia is Pos - 1,
    paradero(EstacionPrevia, PosPrevia).

% 6. Primera y ultima estacion
primera_estacion(sjl).
ultima_estacion(san_marcos).

% 7. Pasajeros que suben en una estación
pasajeros_suben_en(Estacion, Persona) :-
    sube(Persona, Estacion).

% 8. Pasajeros que bajan en una estación
pasajeros_bajan_en(Estacion, Persona) :-
    baja(Persona, Estacion).

% 9. Personas que viajan entre dos estaciones específicas
viaja_entre(Persona, EstacionSubida, EstacionBajada) :-
    sube(Persona, EstacionSubida),
    baja(Persona, EstacionBajada),
    paradero(EstacionSubida, Pos1),
    paradero(EstacionBajada, Pos2),
    Pos1 < Pos2.

% 10. Personas que pasan por una estación (suben antes y bajan después)
pasa_por_estacion(Persona, EstacionPaso) :-
    sube(Persona, EstacionSubida),
    baja(Persona, EstacionBajada),
    paradero(EstacionSubida, PosSubida),
    paradero(EstacionPaso, PosPaso),
    paradero(EstacionBajada, PosBajada),
    PosSubida < PosPaso,
    PosPaso < PosBajada.

% 11. Contar pasajeros en el bus en una estación específica
% (los que ya subieron y aún no han bajado)
en_bus_en_estacion(Persona, Estacion) :-
    sube(Persona, EstacionSubida),
    baja(Persona, EstacionBajada),
    paradero(EstacionSubida, PosSubida),
    paradero(Estacion, PosEstacion),
    paradero(EstacionBajada, PosBajada),
    PosSubida =< PosEstacion,
    PosEstacion < PosBajada.

% 12. Distancia entre estaciones (número de paraderos)
distancia_estaciones(Estacion1, Estacion2, Distancia) :-
    paradero(Estacion1, Pos1),
    paradero(Estacion2, Pos2),
    Distancia is abs(Pos2 - Pos1).

% 13. Ruta completa que hace una persona
ruta_persona(Persona, EstacionSubida, EstacionBajada, Distancia) :-
    sube(Persona, EstacionSubida),
    baja(Persona, EstacionBajada),
    distancia_estaciones(EstacionSubida, EstacionBajada, Distancia).

% 14. Personas que hacen viajes largos (mas de 2 estaciones)
viaje_largo(Persona) :-
    ruta_persona(Persona, _, _, Distancia),
    Distancia > 2.

% 15. Personas que hacen viajes cortos (1 estación)
viaje_corto(Persona) :-
    ruta_persona(Persona, _, _, 1).

% 16. Estación mas concurrida (donde mas gente sube)
estacion_concurrida_subida(Estacion) :-
    paradero(Estacion, _),
    findall(P, sube(P, Estacion), Lista),
    length(Lista, Cantidad),
    \+ (paradero(OtraEstacion, _), 
        Estacion \= OtraEstacion,
        findall(P2, sube(P2, OtraEstacion), Lista2),
        length(Lista2, OtraCantidad),
        OtraCantidad > Cantidad).

% 17. Compañeros de viaje (personas que están en el bus al mismo tiempo)
companeros_viaje(Persona1, Persona2) :-
    Persona1 \= Persona2,
    paradero(Estacion, _),
    en_bus_en_estacion(Persona1, Estacion),
    en_bus_en_estacion(Persona2, Estacion).

% 18. Recorrido completo de la ruta
recorrido_completo(sjl, '15_de_enero', mariategui, grau, san_marcos).

% 19. Validar si una persona hace un recorrido válido
recorrido_valido(Persona) :-
    sube(Persona, EstacionSubida),
    baja(Persona, EstacionBajada),
    paradero(EstacionSubida, PosSubida),
    paradero(EstacionBajada, PosBajada),
    PosSubida < PosBajada.

% SISTEMA DE TARIFAS DE TRANSPORTE

% Tarifas por género
tarifa(Persona, 2) :- hombre(Persona).  % Hombres pagan 2 soles
tarifa(Persona, 1) :- mujer(Persona).   % Mujeres pagan 1 sol

% 21. Calcular tarifa individual
costo_viaje(Persona, Costo) :-
    tarifa(Persona, Costo),
    recorrido_valido(Persona).

% 22. Calcular ingresos por estación (donde suben)
ingresos_estacion_subida(Estacion, Total) :-
    findall(Costo, (sube(Persona, Estacion), tarifa(Persona, Costo)), Costos),
    sum_list(Costos, Total).

% 23. Calcular ingresos totales del recorrido
ingresos_totales(Total) :-
    findall(Costo, (sube(Persona, _), tarifa(Persona, Costo)), Costos),
    sum_list(Costos, Total).

% 24. Pasajeros que pagan tarifa específica en una estación
pasajeros_tarifa_estacion(Estacion, TarifaPago, Persona) :-
    sube(Persona, Estacion),
    tarifa(Persona, TarifaPago).

% 25. Contar pasajeros por tipo de tarifa
contar_hombres_estacion(Estacion, Cantidad) :-
    findall(P, (sube(P, Estacion), hombre(P)), Lista),
    length(Lista, Cantidad).

contar_mujeres_estacion(Estacion, Cantidad) :-
    findall(P, (sube(P, Estacion), mujer(P)), Lista),
    length(Lista, Cantidad).

% 26. Distribución de ingresos por género
ingresos_hombres(Total) :-
    findall(2, (sube(Persona, _), hombre(Persona)), Tarifas),
    sum_list(Tarifas, Total).

ingresos_mujeres(Total) :-
    findall(1, (sube(Persona, _), mujer(Persona)), Tarifas),
    sum_list(Tarifas, Total).

% 27. Estación con mayores ingresos
estacion_mayor_ingreso(Estacion, Ingresos) :-
    ingresos_estacion_subida(Estacion, Ingresos),
    \+ (ingresos_estacion_subida(OtraEstacion, OtrosIngresos),
        OtraEstacion \= Estacion,
        OtrosIngresos > Ingresos).

% 28. Promedio de tarifa por estación
promedio_tarifa_estacion(Estacion, Promedio) :-
    findall(Tarifa, (sube(P, Estacion), tarifa(P, Tarifa)), Tarifas),
    sum_list(Tarifas, Total),
    length(Tarifas, Cantidad),
    Cantidad > 0,
    Promedio is Total / Cantidad.

% 29. Compañeros de viaje con misma tarifa
companeros_misma_tarifa(Persona1, Persona2, Tarifa) :-
    companeros_viaje(Persona1, Persona2),
    tarifa(Persona1, Tarifa),
    tarifa(Persona2, Tarifa).

% 30. Ingresos por tramo de viaje
ingresos_tramo(EstacionInicio, EstacionFin, Total) :-
    findall(Costo, viaja_entre_con_costo(_, EstacionInicio, EstacionFin, Costo), Costos),
    sum_list(Costos, Total).

% Predicado auxiliar para viajes con costo
viaja_entre_con_costo(Persona, EstacionSubida, EstacionBajada, Costo) :-
    viaja_entre(Persona, EstacionSubida, EstacionBajada),
    tarifa(Persona, Costo).

% 31. Análisis financiero por estación
analisis_financiero_estacion(Estacion, Hombres, Mujeres, IngresosHombres, IngresosMujeres, TotalIngresos) :-
    contar_hombres_estacion(Estacion, Hombres),
    contar_mujeres_estacion(Estacion, Mujeres),
    IngresosHombres is Hombres * 2,
    IngresosMujeres is Mujeres * 1,
    TotalIngresos is IngresosHombres + IngresosMujeres.

% 32. Pasajeros mas rentables (hombres que hacen viajes largos)
pasajero_rentable(Persona) :-
    hombre(Persona),
    viaje_largo(Persona).

% 33. Optimización de ingresos - viajeros por distancia y género
ingreso_por_distancia_genero(Distancia, Genero, TotalIngresos) :-
    findall(Costo, (ruta_persona(P, _, _, Distancia), 
                    call(Genero, P), 
                    tarifa(P, Costo)), Costos),
    sum_list(Costos, TotalIngresos).

% FUNCIONES ADICIONALES DE DESCENDENCIA

% 34. Descendientes directos (hijos)
descendientes_directos(Persona, Descendiente) :-
    progenitor(Persona, Descendiente).

% 35. Todos los descendientes (hijos, nietos, bisnietos, etc.)
descendiente(Persona, Descendiente) :-
    progenitor(Persona, Descendiente).
descendiente(Persona, Descendiente) :-
    progenitor(Persona, Hijo),
    descendiente(Hijo, Descendiente).

% 36. Solo descendientes de una persona específica
solo_descendientes_de(Persona, ListaDescendientes) :-
    findall(D, descendiente(Persona, D), ListaDescendientes).

% 37. Contar descendientes totales
contar_descendientes(Persona, Cantidad) :-
    findall(D, descendiente(Persona, D), Lista),
    length(Lista, Cantidad).

% 38. Descendientes por generación
descendientes_generacion_1(Persona, Hijo) :-
    progenitor(Persona, Hijo).

descendientes_generacion_2(Persona, Nieto) :-
    progenitor(Persona, Hijo),
    progenitor(Hijo, Nieto).

descendientes_generacion_3(Persona, Bisnieto) :-
    progenitor(Persona, Hijo),
    progenitor(Hijo, Nieto),
    progenitor(Nieto, Bisnieto).

% 39. Descendientes masculinos y femeninos
descendientes_masculinos(Persona, DescendienteMasculino) :-
    descendiente(Persona, DescendienteMasculino),
    hombre(DescendienteMasculino).

descendientes_femeninos(Persona, DescendienteFemenino) :-
    descendiente(Persona, DescendienteFemenino),
    mujer(DescendienteFemenino).

% 40. Linaje completo (ancestros hacia arriba)
ancestro(Ancestro, Persona) :-
    progenitor(Ancestro, Persona).
ancestro(Ancestro, Persona) :-
    progenitor(Padre, Persona),
    ancestro(Ancestro, Padre).

% 41. Árbol genealógico completo de una persona
arbol_genealogico(Persona, Ancestros, Descendientes) :-
    findall(A, ancestro(A, Persona), Ancestros),
    findall(D, descendiente(Persona, D), Descendientes).

% 42. Descendientes que viajan en transporte
descendientes_viajeros(Persona, DescendienteViajero) :-
    descendiente(Persona, DescendienteViajero),
    sube(DescendienteViajero, _).

% 43. Ingresos generados por descendientes
ingresos_descendientes(Persona, Total) :-
    findall(Costo, (descendiente(Persona, D), 
                    sube(D, _), 
                    tarifa(D, Costo)), Costos),
    sum_list(Costos, Total).

% FUNCIONES DE DESCENDIENTES POR ESTACIONES

% 44. Descendientes que suben después de una estación específica
descendientes_suben_despues(Persona, EstacionBase, Descendiente) :-
    descendiente(Persona, Descendiente),
    sube(Descendiente, EstacionSubida),
    estacion_despues_de(EstacionBase, EstacionSubida).

% 45. Descendientes que suben antes de una estación específica
descendientes_suben_antes(Persona, EstacionBase, Descendiente) :-
    descendiente(Persona, Descendiente),
    sube(Descendiente, EstacionSubida),
    estacion_antes_de(EstacionBase, EstacionSubida).

% 46. Descendientes que suben entre dos estaciones
descendientes_suben_entre(Persona, EstacionInicio, EstacionFin, Descendiente) :-
    descendiente(Persona, Descendiente),
    sube(Descendiente, EstacionSubida),
    estacion_entre(EstacionInicio, EstacionFin, EstacionSubida).

% 47. Descendientes que bajan después de una estación específica
descendientes_bajan_despues(Persona, EstacionBase, Descendiente) :-
    descendiente(Persona, Descendiente),
    baja(Descendiente, EstacionBajada),
    estacion_despues_de(EstacionBase, EstacionBajada).

% 48. Descendientes que bajan antes de una estación específica
descendientes_bajan_antes(Persona, EstacionBase, Descendiente) :-
    descendiente(Persona, Descendiente),
    baja(Descendiente, EstacionBajada),
    estacion_antes_de(EstacionBase, EstacionBajada).

% 49. Descendientes que bajan entre dos estaciones
descendientes_bajan_entre(Persona, EstacionInicio, EstacionFin, Descendiente) :-
    descendiente(Persona, Descendiente),
    baja(Descendiente, EstacionBajada),
    estacion_entre(EstacionInicio, EstacionFin, EstacionBajada).

% 50. Descendientes que ingresan (suben) en una estación específica
descendientes_ingresan_en(Persona, Estacion, Descendiente) :-
    descendiente(Persona, Descendiente),
    sube(Descendiente, Estacion).

% 51. Descendientes que salen (bajan) en una estación específica
descendientes_salen_en(Persona, Estacion, Descendiente) :-
    descendiente(Persona, Descendiente),
    baja(Descendiente, Estacion).

% 52. Descendientes que viajan en un tramo específico (suben en una estación y bajan en otra)
descendientes_viajan_tramo(Persona, EstacionSubida, EstacionBajada, Descendiente) :-
    descendiente(Persona, Descendiente),
    viaja_entre(Descendiente, EstacionSubida, EstacionBajada).

% 53. Descendientes que pasan por una estación específica
descendientes_pasan_por(Persona, EstacionPaso, Descendiente) :-
    descendiente(Persona, Descendiente),
    pasa_por_estacion(Descendiente, EstacionPaso).

% 54. Contar descendientes por rangos de estaciones
contar_descendientes_suben_despues(Persona, EstacionBase, Cantidad) :-
    findall(D, descendientes_suben_despues(Persona, EstacionBase, D), Lista),
    length(Lista, Cantidad).

contar_descendientes_suben_antes(Persona, EstacionBase, Cantidad) :-
    findall(D, descendientes_suben_antes(Persona, EstacionBase, D), Lista),
    length(Lista, Cantidad).

contar_descendientes_suben_entre(Persona, EstacionInicio, EstacionFin, Cantidad) :-
    findall(D, descendientes_suben_entre(Persona, EstacionInicio, EstacionFin, D), Lista),
    length(Lista, Cantidad).

% 55. Ingresos de descendientes por rangos de estaciones
ingresos_descendientes_suben_despues(Persona, EstacionBase, Total) :-
    findall(Costo, (descendientes_suben_despues(Persona, EstacionBase, D), 
                    tarifa(D, Costo)), Costos),
    sum_list(Costos, Total).

ingresos_descendientes_suben_antes(Persona, EstacionBase, Total) :-
    findall(Costo, (descendientes_suben_antes(Persona, EstacionBase, D), 
                    tarifa(D, Costo)), Costos),
    sum_list(Costos, Total).

ingresos_descendientes_suben_entre(Persona, EstacionInicio, EstacionFin, Total) :-
    findall(Costo, (descendientes_suben_entre(Persona, EstacionInicio, EstacionFin, D), 
                    tarifa(D, Costo)), Costos),
    sum_list(Costos, Total).

% 56. Descendientes que hacen viajes completos en rangos específicos
descendientes_viaje_completo_despues(Persona, EstacionBase, Descendiente) :-
    descendiente(Persona, Descendiente),
    sube(Descendiente, EstacionSubida),
    baja(Descendiente, EstacionBajada),
    estacion_despues_de(EstacionBase, EstacionSubida),
    estacion_despues_de(EstacionBase, EstacionBajada).

descendientes_viaje_completo_antes(Persona, EstacionBase, Descendiente) :-
    descendiente(Persona, Descendiente),
    sube(Descendiente, EstacionSubida),
    baja(Descendiente, EstacionBajada),
    estacion_antes_de(EstacionBase, EstacionSubida),
    estacion_antes_de(EstacionBase, EstacionBajada).

% 57. Análisis de patrones de descendientes por estaciones
patron_descendientes_estacion(Persona, Estacion, Suben, Bajan, Pasan) :-
    findall(D, descendientes_ingresan_en(Persona, Estacion, D), ListaSuben),
    length(ListaSuben, Suben),
    findall(D, descendientes_salen_en(Persona, Estacion, D), ListaBajan),
    length(ListaBajan, Bajan),
    findall(D, descendientes_pasan_por(Persona, Estacion, D), ListaPasan),
    length(ListaPasan, Pasan).

% 58. Descendientes con viajes largos/cortos por estaciones
descendientes_viaje_largo_desde(Persona, EstacionBase, Descendiente) :-
    descendiente(Persona, Descendiente),
    sube(Descendiente, EstacionSubida),
    estacion_despues_de(EstacionBase, EstacionSubida),
    viaje_largo(Descendiente).

descendientes_viaje_corto_desde(Persona, EstacionBase, Descendiente) :-
    descendiente(Persona, Descendiente),
    sube(Descendiente, EstacionSubida),
    estacion_despues_de(EstacionBase, EstacionSubida),
    viaje_corto(Descendiente).



% ========================================================================
% SISTEMA DE RECOMENDACIÓN DE CARRERAS PROFESIONALES
% ========================================================================

% Definición de carreras disponibles
carrera(ingenieria_sistemas).
carrera(medicina).
carrera(psicologia).
carrera(diseno_grafico).
carrera(administracion).
carrera(arquitectura).
carrera(derecho).
carrera(educacion).
carrera(arte).
carrera(ingenieria_civil).
carrera(biologia).
carrera(comunicaciones).

% Definición de gustos/intereses
gusto(ciencia).
gusto(tecnologia).
gusto(arte).
gusto(trabajo_equipo).
gusto(resolucion_problemas).
gusto(creatividad).
gusto(liderazgo).
gusto(ayuda_social).
gusto(investigacion).
gusto(construccion).
gusto(comunicacion).
gusto(matematicas).
gusto(salud).
gusto(justicia).
gusto(ensenanza).

% Perfiles de gustos para cada persona de la familia (simulación)
% Bisabuelos (orientación tradicional)
perfil_gustos(alberto, [liderazgo, construccion, resolucion_problemas]).
perfil_gustos(beatriz, [ayuda_social, ensenanza, trabajo_equipo]).
perfil_gustos(diego, [ciencia, investigacion, matematicas]).
perfil_gustos(carmen, [arte, creatividad, comunicacion]).

% Abuelos (transición generacional)
perfil_gustos(carlos, [liderazgo, administracion, trabajo_equipo]).
perfil_gustos(diana, [salud, ayuda_social, comunicacion]).
perfil_gustos(eduardo, [ciencia, tecnologia, investigacion]).
perfil_gustos(francisca, [arte, creatividad, diseno]).

% Padres (generación moderna)
perfil_gustos(fernando, [tecnologia, resolucion_problemas, liderazgo]).
perfil_gustos(gloria, [salud, ayuda_social, trabajo_equipo]).
perfil_gustos(gabriel, [ciencia, investigacion, matematicas]).
perfil_gustos(helena, [arte, creatividad, ensenanza]).
perfil_gustos(hugo, [construccion, resolucion_problemas, liderazgo]).
perfil_gustos(isabel, [comunicacion, ayuda_social, trabajo_equipo]).

% Hijos (generación joven)
perfil_gustos(ivan, [tecnologia, ciencia, resolucion_problemas]).
perfil_gustos(julia, [salud, ayuda_social, investigacion]).
perfil_gustos(javier, [arte, creatividad, comunicacion]).
perfil_gustos(laura, [psicologia, ayuda_social, trabajo_equipo]).
perfil_gustos(luis, [ciencia, matematicas, investigacion]).
perfil_gustos(maria, [educacion, ensenanza, ayuda_social]).
perfil_gustos(marcos, [tecnologia, resolucion_problemas, liderazgo]).
perfil_gustos(natalia, [arte, diseno, creatividad]).
perfil_gustos(nicolas, [construccion, matematicas, resolucion_problemas]).
perfil_gustos(olivia, [comunicacion, arte, trabajo_equipo]).
perfil_gustos(oscar, [justicia, liderazgo, ayuda_social]).
perfil_gustos(patricia, [salud, ciencia, investigacion]).

% Nietos (nueva generación)
perfil_gustos(pablo, [tecnologia, ciencia, innovacion]).
perfil_gustos(antonio, [arte, creatividad, comunicacion]).
perfil_gustos(andres, [construccion, resolucion_problemas, liderazgo]).
perfil_gustos(sofia, [salud, ayuda_social, ciencia]).
perfil_gustos(camila, [arte, diseno, creatividad]).
perfil_gustos(valentina, [comunicacion, ayuda_social, trabajo_equipo]).

% Matriz de compatibilidad: carrera-gustos con pesos
% Formato: compatibilidad(Carrera, Gusto, Peso)
compatibilidad(ingenieria_sistemas, tecnologia, 90).
compatibilidad(ingenieria_sistemas, ciencia, 80).
compatibilidad(ingenieria_sistemas, resolucion_problemas, 85).
compatibilidad(ingenieria_sistemas, matematicas, 80).

compatibilidad(medicina, salud, 95).
compatibilidad(medicina, ciencia, 85).
compatibilidad(medicina, ayuda_social, 80).
compatibilidad(medicina, investigacion, 70).

compatibilidad(psicologia, ayuda_social, 90).
compatibilidad(psicologia, comunicacion, 85).
compatibilidad(psicologia, investigacion, 75).
compatibilidad(psicologia, trabajo_equipo, 80).

compatibilidad(diseno_grafico, arte, 95).
compatibilidad(diseno_grafico, creatividad, 90).
compatibilidad(diseno_grafico, tecnologia, 70).
compatibilidad(diseno_grafico, comunicacion, 75).

compatibilidad(administracion, liderazgo, 90).
compatibilidad(administracion, trabajo_equipo, 85).
compatibilidad(administracion, resolucion_problemas, 80).
compatibilidad(administracion, comunicacion, 75).

compatibilidad(arquitectura, arte, 85).
compatibilidad(arquitectura, construccion, 90).
compatibilidad(arquitectura, creatividad, 80).
compatibilidad(arquitectura, matematicas, 75).

compatibilidad(derecho, justicia, 95).
compatibilidad(derecho, comunicacion, 85).
compatibilidad(derecho, liderazgo, 80).
compatibilidad(derecho, ayuda_social, 75).

compatibilidad(educacion, ensenanza, 95).
compatibilidad(educacion, ayuda_social, 85).
compatibilidad(educacion, comunicacion, 80).
compatibilidad(educacion, trabajo_equipo, 75).

compatibilidad(arte, creatividad, 95).
compatibilidad(arte, arte, 90).
compatibilidad(arte, comunicacion, 70).

compatibilidad(ingenieria_civil, construccion, 95).
compatibilidad(ingenieria_civil, matematicas, 85).
compatibilidad(ingenieria_civil, resolucion_problemas, 80).
compatibilidad(ingenieria_civil, ciencia, 75).

compatibilidad(biologia, ciencia, 95).
compatibilidad(biologia, investigacion, 90).
compatibilidad(biologia, salud, 80).
compatibilidad(biologia, matematicas, 70).

compatibilidad(comunicaciones, comunicacion, 95).
compatibilidad(comunicaciones, creatividad, 80).
compatibilidad(comunicaciones, trabajo_equipo, 75).
compatibilidad(comunicaciones, arte, 70).

% Calcular compatibilidad de una persona con una carrera
calcular_compatibilidad(Persona, Carrera, Porcentaje) :-
    perfil_gustos(Persona, Gustos),
    carrera(Carrera),
    calcular_puntuacion_total(Gustos, Carrera, PuntuacionTotal),
    length(Gustos, NumGustos),
    (NumGustos > 0 -> 
        Porcentaje is min(100, PuntuacionTotal / NumGustos) 
    ; 
        Porcentaje = 0
    ).

% Calcular puntuación total de una lista de gustos para una carrera
calcular_puntuacion_total([], _, 0).
calcular_puntuacion_total([Gusto|Resto], Carrera, PuntuacionTotal) :-
    (compatibilidad(Carrera, Gusto, Peso) -> 
        PuntuacionGusto = Peso 
    ; 
        PuntuacionGusto = 0
    ),
    calcular_puntuacion_total(Resto, Carrera, PuntuacionResto),
    PuntuacionTotal is PuntuacionGusto + PuntuacionResto.

% Recomendar carrera con mayor compatibilidad
recomendar_carrera(Persona, CarreraRecomendada, Porcentaje) :-
    findall(Porcentaje-Carrera, calcular_compatibilidad(Persona, Carrera, Porcentaje), Lista),
    sort(Lista, ListaOrdenada),
    reverse(ListaOrdenada, [Porcentaje-CarreraRecomendada|_]).

% Recomendar top N carreras
recomendar_top_carreras(Persona, N, TopCarreras) :-
    findall(Porcentaje-Carrera, calcular_compatibilidad(Persona, Carrera, Porcentaje), Lista),
    sort(Lista, ListaOrdenada),
    reverse(ListaOrdenada, ListaDesc),
    take(N, ListaDesc, TopCarreras).

% Auxiliar para tomar N elementos de una lista
take(0, _, []).
take(_, [], []).
take(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, R).

% Comparar compatibilidad entre familiares
comparar_familias_carrera(Carrera, ListaCompatibilidad) :-
    findall(Porcentaje-Persona, 
            (perfil_gustos(Persona, _), calcular_compatibilidad(Persona, Carrera, Porcentaje)), 
            Lista),
    sort(Lista, ListaOrdenada),
    reverse(ListaOrdenada, ListaCompatibilidad).

% Encontrar la carrera mas popular en la familia
carrera_popular_familia(CarreraPopular, PromedioCompatibilidad) :-
    findall(Carrera-Promedio, 
            (carrera(Carrera), promedio_carrera_familia(Carrera, Promedio)), 
            Lista),
    sort(Lista, ListaOrdenada),
    reverse(ListaOrdenada, [PromedioCompatibilidad-CarreraPopular|_]).

% Calcular promedio de compatibilidad de una carrera en toda la familia
promedio_carrera_familia(Carrera, Promedio) :-
    findall(Porcentaje, 
            (perfil_gustos(Persona, _), calcular_compatibilidad(Persona, Carrera, Porcentaje)), 
            Lista),
    sum_list(Lista, Suma),
    length(Lista, Cantidad),
    (Cantidad > 0 -> Promedio is Suma / Cantidad ; Promedio = 0).

% Recomendar carreras por generación
recomendar_por_generacion(Generacion, ListaRecomendaciones) :-
    findall(Persona-CarreraRecomendada-Porcentaje,
            (miembro_generacion(Persona, Generacion), 
             recomendar_carrera(Persona, CarreraRecomendada, Porcentaje)),
            ListaRecomendaciones).

% Definir miembros por generación
miembro_generacion(Persona, bisabuelos) :-
    member(Persona, [alberto, beatriz, diego, carmen]).

miembro_generacion(Persona, abuelos) :-
    member(Persona, [carlos, diana, eduardo, francisca]).

miembro_generacion(Persona, padres) :-
    member(Persona, [fernando, gloria, gabriel, helena, hugo, isabel]).

miembro_generacion(Persona, hijos) :-
    member(Persona, [ivan, julia, javier, laura, luis, maria, marcos, natalia, nicolas, olivia, oscar, patricia]).

miembro_generacion(Persona, nietos) :-
    member(Persona, [pablo, antonio, andres, sofia, camila, valentina]).

% Análisis de herencia vocacional
analizar_herencia_vocacional(Persona, AnalisisHerencia) :-
    findall(Ancestro-CarreraAncestro-PorcentajeAncestro,
            (ancestro(Ancestro, Persona), 
             recomendar_carrera(Ancestro, CarreraAncestro, PorcentajeAncestro)),
            AnalisisHerencia).

% Buscar personas con gustos similares
personas_gustos_similares(Persona, PersonasSimilares) :-
    perfil_gustos(Persona, GustosPersona),
    findall(Similitud-OtraPersona,
            (perfil_gustos(OtraPersona, GustosOtra), 
             OtraPersona \= Persona,
             calcular_similitud_gustos(GustosPersona, GustosOtra, Similitud)),
            Lista),
    sort(Lista, ListaOrdenada),
    reverse(ListaOrdenada, PersonasSimilares).

% Calcular similitud entre dos listas de gustos
calcular_similitud_gustos(Gustos1, Gustos2, Similitud) :-
    intersection(Gustos1, Gustos2, Comunes),
    union(Gustos1, Gustos2, Union),
    length(Comunes, NumComunes),
    length(Union, NumUnion),
    (NumUnion > 0 -> 
        Similitud is (NumComunes * 100) / NumUnion 
    ; 
        Similitud = 0
    ).


% CONSULTAS DE EJEMPLO COMPLETAS:
%----------------------------------
% === GENEALÓGICAS ===
% --------------------------------
% ?- bisabuelo(alberto, X).
% ?- hermanos(ivan, javier).
% ?- primo(luis, nicolas).
% ?- abuelo(carlos, ivan).
% ?- cunado(javier, ivan).
% ?- tio(gabriel, ivan).
% ?- solo_descendientes_de(alberto, X).
% ?- descendientes_masculinos(carlos, X).
% ?- contar_descendientes(alberto, N).
% ?- ancestro(alberto, X).
% ?- arbol_genealogico(ivan, Ancestros, Descendientes).

%---------------------------------
% === TRANSPORTE BÁSICO ===
% ---------------------------------
% ?- estacion_despues_de(mariategui, X).
% ?- estacion_entre(sjl, san_marcos, X).
% ?- pasajeros_suben_en(mariategui, X).
% ?- viaja_entre(alberto, sjl, mariategui).
% ?- en_bus_en_estacion(X, grau).
% ?- companeros_viaje(fernando, diego).
% ?- viaje_largo(X).
% ?- estacion_concurrida_subida(X).
% ?- pasa_por_estacion(fernando, mariategui).

% === TARIFAS Y ECONOMÍA ===
% ?- costo_viaje(alberto, Costo).
% ?- ingresos_estacion_subida(sjl, Total).
% ?- ingresos_totales(Total).
% ?- contar_hombres_estacion(mariategui, N).
% ?- ingresos_hombres(Total).
% ?- ingresos_mujeres(Total).
% ?- estacion_mayor_ingreso(Estacion, Ingresos).
% ?- promedio_tarifa_estacion(grau, Promedio).
% ?- companeros_misma_tarifa(P1, P2, Tarifa).
% ?- analisis_financiero_estacion(sjl, H, M, IH, IM, Total).
% ?- pasajero_rentable(X).
% ?- ingresos_descendientes(alberto, Total).

% === DESCENDIENTES POR ESTACIONES ===
% ?- descendientes_suben_despues(alberto, mariategui, X).
% ?- descendientes_suben_antes(carlos, grau, X).
% ?- descendientes_suben_entre(alberto, sjl, grau, X).
% ?- descendientes_bajan_despues(diego, mariategui, X).
% ?- descendientes_bajan_antes(carlos, san_marcos, X).
% ?- descendientes_bajan_entre(alberto, mariategui, san_marcos, X).
% ?- descendientes_ingresan_en(alberto, sjl, X).
% ?- descendientes_salen_en(carlos, grau, X).
% ?- descendientes_viajan_tramo(alberto, sjl, mariategui, X).
% ?- descendientes_pasan_por(carlos, mariategui, X).
% ?- contar_descendientes_suben_despues(alberto, mariategui, N).
% ?- ingresos_descendientes_suben_entre(carlos, sjl, grau, Total).
% ?- patron_descendientes_estacion(alberto, mariategui, Suben, Bajan, Pasan).
% ?- descendientes_viaje_largo_desde(alberto, sjl, X).
% ?- descendientes_viaje_corto_desde(carlos, mariategui, X).

% === COMBINADAS (GENEALOGÍA + TRANSPORTE) ===
% ?- descendientes_viajeros(alberto, X).
% ?- ingresos_descendientes(carlos, Total).


%----------------------------------------------------
% CONSULTAS DE RECOMENDACIÓN DE CARRERAS:
% ---------------------------------------------------
% ?- recomendar_carrera(ivan, Carrera, Porcentaje).
% ?- recomendar_top_carreras(sofia, 3, Top).
% ?- calcular_compatibilidad(pablo, ingenieria_sistemas, Porcentaje).
% ?- comparar_familias_carrera(medicina, Lista).
% ?- carrera_popular_familia(Carrera, Promedio).
% ?- recomendar_por_generacion(nietos, Lista).
% ?- analizar_herencia_vocacional(pablo, Herencia).
% ?- personas_gustos_similares(ivan, Similares).

% CONSULTAS INTEGRADAS (GENEALOGÍA + CARRERAS):
% ?- hermanos(X, Y), recomendar_carrera(X, C1, P1), recomendar_carrera(Y, C2, P2).
% ?- padre(P, H), recomendar_carrera(P, CP, PP), recomendar_carrera(H, CH, PH).
% ?- primo(X, Y), personas_gustos_similares(X, Lista), member(_-Y, Lista).
% ?- bisabuelo(B, N), recomendar_carrera(B, CB, PB), recomendar_carrera(N, CN, PN).
% ?- descendientes_masculinos(carlos, D), recomendar_carrera(D, Carrera, Porcentaje).
% Permitir hechos habla/2 dinámicos para aprendizaje de idiomas
:- dynamic(habla/2).

% Predicado para que una persona aprenda un idioma nuevo (si no lo sabe ya)
aprender_idioma(Persona, Idioma) :-
    (   idiomas_persona(Persona, Idiomas),
        member(Idioma, Idiomas)
    ->  format('~w ya sabe ~w.~n', [Persona, Idioma]), fail
    ;   assertz(habla(Persona, Idioma)),
        format('~w ha aprendido ~w.~n', [Persona, Idioma])
    ).
