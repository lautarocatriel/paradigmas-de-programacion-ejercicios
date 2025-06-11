{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use first" #-}
module Library where
import PdePreludat

-- Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3, p.ej: 
-- Main> esMultiploDeTres 9 
-- True 

esMultiploDeTres :: Number -> Bool
esMultiploDeTres numero = mod numero 3 == 0;


-- Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero, p.ej. 
-- Main> esMultiploDe 3 12
-- True

esMultiploDe :: Number -> Number -> Bool
esMultiploDe otro numero = mod numero otro == 0

-- Definir la función cubo/1, devuelve el cubo de un número.

cubo :: Number -> Number
cubo numero = numero ^ 3

-- Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.

area :: Number -> Number -> Number
area base altura = base * altura

-- Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) Nota: Resolverlo reutilizando la función esMultiploDe/2

esBisiesto :: Number -> Bool
esBisiesto numero = esMultiploDe 400 numero || esMultiploDe 4 numero && (not . esMultiploDe 100) numero

-- Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.

celsiusToFahr :: Number -> Number
celsiusToFahr numero = (numero * 9) / 5 + 32

-- Definir la función fahrToCelsius/1, la inversa de la anterior.

fahrToCelsius :: Number -> Number
fahrToCelsius numero = (numero - 32) * 5 / 9

-- Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius. 

haceFrio :: Number -> Bool
haceFrio numero = fahrToCelsius numero < 8

-- Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula. 
-- m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)} 


mcm :: Number -> Number -> Number
mcm numero otro = abs numero * otro / gcd numero otro

-- Dispersión
-- Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días consecutivos; cada medición es un entero que representa una cantidad de cm. 
-- P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm. 
-- A partir de estos tres números, podemos obtener algunas conclusiones. 
-- Definir estas funciones: 
-- dispersion, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las versiones de dos elementos. De esa forma se puede definir dispersión sin escribir ninguna guarda (las guardas están en max y min, que estamos usando). 

-- diasParejos, diasLocos y diasNormales reciben los valores de los tres días. Se dice que son días parejos si la dispersión es chica, que son días locos si la dispersión es grande, y que son días normales si no son ni parejos ni locos. Una dispersión se considera chica si es de menos de 30 cm, y grande si es de más de un metro. 
-- Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas. 

dispersion :: Number -> Number -> Number -> Number
dispersion medicion1 medicion2 medicion3 = (max medicion2 . max medicion3) medicion1 - (min medicion2 . min medicion3) medicion1


diasParejos :: Number-> Number -> Number -> Bool
diasParejos medicion1 medicion2 medicion3 = dispersion medicion1 medicion2 medicion3 < 30

diasLocos :: Number-> Number -> Number -> Bool
diasLocos medicion1 medicion2 medicion3 = dispersion medicion1 medicion2 medicion3 > 100

diasNormales :: Number-> Number -> Number -> Bool
diasNormales medicion1 medicion2 medicion3 = ((diasParejos medicion1 medicion2 medicion3 /=) .not) (diasLocos medicion1 medicion2 medicion3)


-- En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. El peso de un pino se puede calcular a partir de la altura así: 3 kg x cm hasta 3 metros, 2 kg x cm arriba de los 3 metros. P.ej. 2 metros ⇒  600 kg, 5 metros ⇒  1300 kg. 
-- Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un pino fuera de este rango no le sirve a la fábrica. Para esta situación: 
-- Definir la función pesoPino, recibe la altura de un pino y devuelve su peso. 
-- Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. 
-- Definir la función sirvePino, recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. Usar composición en la definición. 

pesoPino :: Number -> Number
pesoPino altura
 | altura <= 300 = altura * 3
 | otherwise = 900 + (altura - 300) * 2

esPesoUtil :: Number -> Bool
esPesoUtil peso = peso <= 1000 && peso >=400

sirvePino:: Number -> Bool
sirvePino = esPesoUtil . pesoPino


-- Este ejercicio alguna vez se planteó como un Desafío Café con Leche: Implementar la función esCuadradoPerfecto/1, sin hacer operaciones con punto flotante. Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. Pensar que el primer cuadrado perfecto es 0, para llegar al 2do (1) sumo 1, para llegar al 3ro (4) sumo 3, para llegar al siguiente (9) sumo 5, después sumo 7, 9, 11 etc.. También algo de recursividad van a tener que usar. 

--APLICACION PARCIAL

-- Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1. 

siguiente :: Number -> Number
siguiente = (+1)

-- Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número, ej: 
mitad :: Number -> Number
mitad = (/2)

-- Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa. 
-- Main> inversa 4
-- 0.25
-- Main> inversa 0.5
-- 2.0

inversa :: Number -> Number
inversa = (1/)

-- Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo.

triple :: Number -> Number
triple = (*3)

-- Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el número es positivo y false en caso contrario. 
-- Main> esNumeroPositivo (-5)
-- False
-- Main> esNumeroPositivo 0.99
-- True 

esNumeroPositivo :: Number -> Bool
esNumeroPositivo = (>0)

--COMPOSICION

-- Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2, utilizando aplicación parcial y composición.

esMultiploDeC :: Number -> Number -> Bool
esMultiploDeC numero otro = ((==0) . mod otro) numero

-- Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1, utilizando aplicación parcial y composición.

-- esBisiesto :: Number -> Bool
-- esBisiesto numero = esMultiploDe 400 numero || esMultiploDe 4 numero && (not . esMultiploDe 100) numero

esBisiestoC :: Number -> Bool
esBisiestoC numero = esMultiploDe 400 numero || ((==esMultiploDe 100 numero) . not . esMultiploDe 4) numero

-- Resolver la función inversaRaizCuadrada/1, que da un número n devolver la inversa su raíz cuadrada. 
-- Main> inversaRaizCuadrada 4 
-- 0.5 
-- Nota: Resolverlo utilizando la función inversa Ej. 2.3, sqrt y composición.

inversaRaizCuadrada :: Number -> Number
inversaRaizCuadrada = inversa . sqrt

-- Definir una función incrementMCuadradoN, que invocándola con 2 números m y n, incrementa un valor m al cuadrado de n por Ej: 
-- Main> incrementMCuadradoN 3 2 
-- 11 
-- Incrementa 2 al cuadrado de 3, da como resultado 11. Nota: Resolverlo utilizando aplicación parcial y composición. 

incrementMcuadradoN :: Number -> Number -> Number
incrementMcuadradoN numero incremento = ((+ incremento) . (^2)) numero

-- Definir una función esResultadoPar/2, que invocándola con número n y otro m, devuelve true si el resultado de elevar n a m es par. 
-- Main> esResultadoPar 2 5 
-- True 
-- Main> esResultadoPar 3 2
-- False 
-- Nota Obvia: Resolverlo utilizando aplicación parcial y composición.

esResultadoPar :: Number -> Number -> Bool
esResultadoPar numero potencia = (even . (^potencia)) numero



--TUPLAS

-- Definir las funciones fst3, snd3, trd3, que dada una tupla de 3 elementos devuelva el elemento correspondiente, p.ej. 
-- Main> snd3 (4,5,6) 
-- 5
-- Main> trd3(4,5,6)
-- 6

numeros :: (Number, Number, Number)
numeros = (4,5,6)

fst3 :: (Number, Number, Number) -> Number
fst3 (numero, numero2, numero3) = numero

snd3 :: (Number, Number, Number) -> Number
snd3 (numero, numero2, numero3) = numero2


trd3 :: (Number, Number, Number) -> Number
trd3 (numero, numero2, numero3) = numero3

-- Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones y un entero, me devuelve como resultado una tupla con el resultado de aplicar el elemento a cada una de la funciones, ej: 
-- Main> aplicar (doble,triple) 8 
-- (16,24) 
-- Main> aplicar ((3+),(2*)) 8 
-- (11,16)

aplicar :: (Number -> Number, Number-> Number) -> Number -> (Number, Number)
aplicar (funcion, funcion2) numero = (funcion numero, funcion2 numero)

-- Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es mayor al segundo devuelve la suma, si el segundo le lleva más de 10 al primero devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega a llevarle 10, devuelve el producto. Ej: 
-- Main> cuentaBizarra (5,8)
-- 40
-- Main> cuentaBizarra (8,5)
-- 13
-- Main> cuentaBizarra (5,29)
-- 24

cuentaBizarra :: (Number, Number) -> Number
cuentaBizarra (numero, otro)
 | numero > otro = numero + otro
 | (otro - numero) >= 10 = otro - numero
 | (((otro - numero < 10)==) . (>numero)) otro = numero * otro


--  Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1,nota2), p.ej. un patito en el 1ro y un 7 en el 2do se representan mediante el par (2,7). 
-- A partir de esto: 
-- Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en caso contrario. No vale usar guardas. 
-- Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo. 
-- Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 15 y además haberse sacado al menos 7 en cada parcial. 
-- Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo y composición. La consulta tiene que tener esta forma (p.ej. para el par de notas (5,8)) 
-- Main> (... algo ...) (5,8)

type Notas = (Number,Number)

esNotaBochazo :: Number -> Bool
esNotaBochazo numero = numero < 6

aprobo :: Notas -> Bool
aprobo (nota1, nota2) = (not . esNotaBochazo) nota1 && (not . esNotaBochazo) nota2

promociono :: Notas -> Bool
promociono (nota1, nota2) = (((nota1 + nota2 >= 15) &&) . ((nota1 >= 7)==)) (nota2 >=7)

-- Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios, lo representamos mediante un par de pares ((parc1,parc2),(recup1,recup2)). 
-- Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente. 
-- Observamos que con la codificación elegida, siempre la mejor nota es el máximo entre nota del parcial y nota del recuperatorio. 
-- Considerar que vale recuperar para promocionar. En este ejercicio vale usar las funciones que se definieron para el anterior. 
-- Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a las notas finales del alumno para el 1er y el 2do parcial. P.ej. 
-- Main> notasFinales ((2,7),(6,-1)) 
-- (6,7) 
-- Main> notasFinales ((2,2),(6,2)) 
-- (6,2) 
-- Main> notasFinales ((8,7),(-1,-1)) 
-- (8,7) 
-- Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recursa o no. O sea, la respuesta debe ser True si recursa, y False si no recursa. Usar las funciones definidas en este punto y el anterior, y composición. La consulta debe tener esta forma:
-- Main> (... algo ...) ((2,7),(6,-1)) 
-- Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recuperó el primer parcial. Usar composición. La consulta debe tener esta forma:
-- Main> (... algo ...) ((2,7),(6,-1)) 
-- Definir la función recuperoDeGusto que dado el par de pares que representa a un alumno, devuelve True si el alumno, pudiendo promocionar con los parciales (o sea sin recup.), igual rindió al menos un recup. Vale definir funciones auxiliares. Hacer una definición que no use pattern matching, en las eventuales funciones auxiliares tampoco; o sea, manejarse siempre con fst y snd.

notasFinales :: ((Number, Number), (Number, Number)) -> (Number, Number)
notasFinales ((parc1, parc2), (recup1, recup2)) = (max parc1 recup1, max parc2 recup2)

-- (not . aprobo . notasFinales)((2,7),(6,-1))

--((>(-1)) . fst . snd) ((2,7),(6,-1))

recupero :: (Number, Number) -> Bool
-- recupero (recup1, recup2) = (((recup1>(-1))||) . (>(-1))) recup2
recupero notas = fst notas > (-1) || snd notas > (-1)

recuperoDeGusto :: ((Number, Number), (Number, Number)) -> Bool
recuperoDeGusto notas = (((promociono . fst )notas==) . (recupero . snd)) notas

-- Definir la función esMayorDeEdad, que dada una tupla de 2 elementos (persona, edad) me devuelva True si es mayor de 21 años y False en caso contrario. Por Ej:.
-- Main> esMayorDeEdad (juan,18) 
-- False 
-- Nota: Definir la función utilizando aplicación parcial y composición.

esMayorDeEdad :: (String, Number) -> Bool
esMayorDeEdad  =  (>21) . snd

-- Definir la función calcular, que recibe una tupla de 2 elementos, si el primer elemento es par lo duplica, sino lo deja como está y con el segundo elemento en caso de ser impar le suma 1 y si no deja esté último como esta. 
-- Main> calcular (4,5)
-- (8,6) 
-- Main> calcular (3,7)
-- (3,8) 
-- Nota: Resolverlo utilizando aplicación parcial y composición. 
-- esParCalcular :: Number -> Number
-- esParCalcular numero
--  | even numero = numero * 2
--  |otherwise = numero
 
-- esImparCalcular :: Number -> Number
-- esImparCalcular numero 
--  |odd numero = numero + 1
--  |otherwise = numero

calcular :: (Number, Number) -> (Number, Number)
-- calcular (numero1, numero2) = (esParCalcular numero1, esImparCalcular numero2)
calcular notas
 |(even . fst) notas && (odd . snd) notas = (fst notas *2, snd notas + 1)
 |(even . fst) notas = (((*2) . fst) notas, snd notas)
 |(odd. snd) notas = (fst notas, ((+1) . snd) notas)


