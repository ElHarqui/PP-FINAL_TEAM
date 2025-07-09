2025-I

Desarrolle un programa en PROLOG que en base a la imagen considere;
-	Un viajero sale de un país a conocer diferentes países del mundo con la idea de retornar al país de donde partió.
-	Cada vuelo (digamos Perú- Brasil) tiene un mismo costo y duración para todos los clientes, cada vuelo entre países diferentes tiene diferente costo y duración. 
-	Los viajeros pueden visitar de manera consecutiva tantos países como deseen y si desean retornar a su país.
-	En base a dichas condiciones responda las siguientes preguntas;

1.	¿Quiénes han salido de viaje y aun no regresan?
2.	¿Cuánto han gastado los que han salido de Perú?
3.	¿Qué países han visitado los que han salido de Perú?
4.	¿Qué porcentaje de los viajeros conocen La India?
5.	¿Cuántas horas ha viajado Ana?
6.	Agregar una o varias condiciones y plantee y responda una pregunta

% filepath: d:\WORKSPACE\PP\EXAMEN FINAL\final.pl
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
viaje(maria, peru, [peru, eeuu, francia, china, australia, peru]). % Regresa
viaje(carla, chile, [chile, brasil, eeuu, francia, india]). % No regresa

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
    Porcentaje is (Conocen * 100) / Total,
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

% Consultas ejemplo:
% ?- no_regresan_detalle(V, P).
% ?- gasto_peru_detallado(V, Tramos, Total).
% ?- paises_visitados_peru(V, Paises).
% ?- porcentaje_india(Porcentaje, Lista).
% ?- horas_viajadas_detallado(ana, Tramos, Total).
% ?- china_y_australia(V).
