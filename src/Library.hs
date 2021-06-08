module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Gimnasta = Gimnasta String Number Number Number deriving(Show)
pancho = Gimnasta "Francisco" 40.0 120.0 1.0
andres = Gimnasta "Andy" 22.0 80.0 6.0

--PUNTO 1

saludable :: Gimnasta -> Bool
saludable (Gimnasta nombre edad peso tonificacion) = peso <= 100 && tonificacion > 5

--PUNTO 2

type Calorias = Number

quemarCalorias :: Gimnasta -> Calorias -> Gimnasta
quemarCalorias (Gimnasta nombre edad peso tonificacion) calorias
                | peso > 100 = (Gimnasta nombre edad (peso - calorias/150) tonificacion)
                | edad > 30 && calorias > 200 = (Gimnasta nombre edad (peso - 1) tonificacion)
                | otherwise = (Gimnasta nombre edad (peso - calorias/(peso*edad)) tonificacion)

-- PUNTO 3

type Ejercicio = Gimnasta -> Gimnasta

--a
type Minutos = Number

caminataEnCinta :: Minutos -> Ejercicio
caminataEnCinta minutos gimnasta = quemarCalorias gimnasta (1*5*minutos)

entrenamientoEnCinta :: Minutos -> Ejercicio
entrenamientoEnCinta minutos gimnasta = quemarCalorias gimnasta (1*((6+ 6 + div minutos 5)/2)*5*minutos)

--b
type Kilos = Number

pesas :: Kilos -> Minutos -> Ejercicio
pesas kilos minutos (Gimnasta nombre edad peso tonificacion)
                    | minutos > 10 = (Gimnasta nombre edad peso (tonificacion + kilos/10))
                    | otherwise = (Gimnasta nombre edad peso tonificacion)

--c
type Inclinacion = Number

colina :: Inclinacion -> Minutos -> Ejercicio
colina inclinacion minutos gimnasta = quemarCalorias gimnasta (2*minutos*inclinacion)

--d 

montania :: Inclinacion -> Minutos -> Ejercicio
montania inclinacion minutos = incrementarTonificacion.(colina (inclinacion+3) (minutos/2)).(colina inclinacion (minutos/2))

incrementarTonificacion :: Ejercicio
incrementarTonificacion (Gimnasta nombre edad peso tonificacion) = (Gimnasta nombre edad peso (tonificacion + 1))

--PUNTO 4

--a

data Rutina = UnaRutina{
    nombre :: String,
    duracion :: Number,
    ejercicios :: [Ejercicio]
}

completo = UnaRutina{
    nombre = "Completo",
    duracion = 50,
    ejercicios = [colina 5 40,pesas 50 15]
}

--i

--ii

realizarRutina :: Rutina -> Ejercicio
realizarRutina rutina = realizarEjercicios (ejercicios rutina)

realizarEjercicios :: [Ejercicio] -> Gimnasta -> Gimnasta
realizarEjercicios [] gimnasta = gimnasta 
realizarEjercicios (ejercicio : ejercicios) gimnasta = realizarEjercicios ejercicios (ejercicio gimnasta)


--iii

realizarRutina' :: Rutina -> Ejercicio
realizarRutina' rutina gimnasta = foldl realizarEjercicio' gimnasta (ejercicios rutina)

realizarEjercicio' :: Gimnasta -> Ejercicio -> Gimnasta
realizarEjercicio' gimnasta ejercicio = ejercicio gimnasta

--b

resumenDeRutina :: Rutina -> Gimnasta -> (String,Number,Number)
resumenDeRutina rutina gimnasta = (nombre rutina , obtenerKilosPerdidos (realizarRutina rutina gimnasta) gimnasta , obtenerTonificacionGanada (realizarRutina rutina gimnasta) gimnasta )

obtenerKilosPerdidos :: Gimnasta -> Gimnasta -> Number
obtenerKilosPerdidos (Gimnasta nombre edad peso tonificacion) (Gimnasta nombreOriginal edadOriginal pesoOriginal tonificacionOriginal) = pesoOriginal - peso

obtenerTonificacionGanada :: Gimnasta -> Gimnasta -> Number
obtenerTonificacionGanada (Gimnasta nombre edad peso tonificacion) (Gimnasta nombreOriginal edadOriginal pesoOriginal tonificacionOriginal) = tonificacion - tonificacionOriginal

--PUNTO 5

rutinasSaludable :: [Rutina] -> Gimnasta -> [Rutina]
rutinasSaludable rutinas gimnasta = filter (esRutinaSaludable gimnasta) rutinas

esRutinaSaludable :: Gimnasta -> Rutina -> Bool
esRutinaSaludable gimnasta rutina = saludable(realizarRutina rutina gimnasta)