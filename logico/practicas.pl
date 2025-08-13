% piano(Nombre, CantidadPiezas, TonalidadesDistintas)
piano(ana, 8, 2).
piano(luis, 3, 4).
piano(sofia, 2, 1).

% guitarra(Nombre, CantAcordes, ErroresEnRecitales)
guitarra(ana, 20, 5).
guitarra(pedro, 10, 12).
guitarra(luis, 5, 3).

% bateria(Nombre, Estilo)
bateria(maria, jazz).
bateria(pedro, rock).
bateria(sofia, pop).

% Queremos saber si alguien es buen músico según estas reglas:

% En piano → es buen pianista si sabe más de 5 piezas o toca en más de 3 tonalidades distintas.

% En guitarra → es buen guitarrista si conoce más acordes que errores cometidos en recitales 

% En batería → es buen baterista si toca estilo jazz o rock.

% Una persona es buen músico si es buen pianista o buen guitarrista o buen baterista.


buenPianista(Persona) :- piano(Persona, Piezas, _), Piezas > 5.
buenPianista(Persona) :- piano(Persona, _, TonalidadesDistintas), TonalidadesDistintas > 3.
buenGuitarrista(Persona) :- guitarra(Persona, CantAcordes, ErroresEnRecitales), CantAcordes > ErroresEnRecitales.
buenBaterista(Persona) :- bateria(Persona, jazz).
buenBaterista(Persona) :- bateria(Persona, rock).
buenMusico(Persona) :- buenPianista(Persona).
buenMusico(Persona) :- buenGuitarrista(Persona).
buenMusico(Persona) :- buenBaterista(Persona).


%tests (hechos por mi tambien)
:- begin_tests(musica).
    test(buen_pianista, set(Persona ==[ana, luis])) :- buenPianista(Persona).
    test(buen_guitarrista, set(Persona == [ana, luis])) :- buenGuitarrista(Persona).
    test(buen_baterista, set(Persona == [maria, pedro])) :- buenBaterista(Persona).
    test(buen_musico, set(Persona == [ana, luis, maria, pedro])) :- buenMusico(Persona).
:- end_tests(musica).
