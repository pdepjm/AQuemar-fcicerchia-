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
                | peso > 100 = (Gimnasta nombre edad (peso - div calorias 150) tonificacion)
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

--montania :: Inclinacion -> Minutos -> Ejercicio
--montania inclinacion minutos gimnasia = 