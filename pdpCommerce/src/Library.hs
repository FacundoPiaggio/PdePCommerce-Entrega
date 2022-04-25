module Library where
--import PdePreludat
import Prelude

--doble :: Number -> Number
doble numero = numero + numero



------------- PDPCOMMERCE TAREA 1
{-
precioTotal:: Fractional c => c -> c -> c -> c -> c
precioTotal precioUnitario cantidad descuento costoEnvio = (((flip aplicarCostoDeEnvio costoEnvio)).(*cantidad).(aplicarDescuento precioUnitario)) descuento                               
--aplicarCostoDeEnvio (((*cantidad).(aplicarDescuento precioUnitario)) descuento) costoEnvio


productoDeElite:: String -> Bool
productoDeElite producto = productoDeLujo producto && productoCodiciado producto && (not.productoCorriente) producto

productoDeLujo:: String ->Bool
productoDeLujo producto = elem 'x' producto || elem 'z' producto

productoCodiciado:: String -> Bool
productoCodiciado = ((>10).length)

productoCorriente:: String -> Bool
productoCorriente producto = head producto == 'a' || head producto == 'e' || head producto == 'i' || head producto == 'o' || head producto == 'u'

productoXL:: String -> String
productoXL = (++ "XL")

descodiciarProducto:: String -> String
descodiciarProducto = (take 10)

versionBarata:: String -> String 
versionBarata = (reverse.descodiciarProducto)

aplicarDescuento:: Fractional c => c -> c -> c
aplicarDescuento precio descuento = precio - (((/100).(*descuento)) precio)

aplicarCostoDeEnvio:: Num a => a -> a -> a
aplicarCostoDeEnvio precio = (+ precio) 

entregaSencilla:: String -> Bool
entregaSencilla = (even.length)

-- 
--take:: Int -> [a] -> [a]
--drop:: Int -> [a] -> [a]
--head:: [a] -> a
--elem:: Eq a => a -> [a] -> Bool
--reverse:: [a] -> [a]
-}

---------------- PDPCOMMERCE con tuplas. producto = (nombre,precio)

producto1 = ("ProductoDePrueba",100)

precioTotal:: Fractional c =>(String,c) -> c -> c -> c -> (String,c)
precioTotal (nombre, precioUnitario) cantidad descuento costoEnvio = (nombre, ((+ costoEnvio).(*cantidad).snd.aplicarDescuento (nombre,precioUnitario)) descuento     )
    --(nombre,(((flip aplicarCostoDeEnvio costoEnvio)).(*cantidad).snd.aplicarDescuento (nombre,precioUnitario)) descuento)

productoDeElite:: Num a => (String, a) -> Bool
productoDeElite (nombre,precio) = productoDeLujo (nombre,precio) && productoCodiciado (nombre,precio) && (not.productoCorriente) (nombre,precio)

productoDeLujo:: Num a => (String, a) -> Bool
productoDeLujo (nombre, _) = elem 'x' nombre || elem 'z' nombre

productoCodiciado::  Num a => (String, a) -> Bool
productoCodiciado (nombre, precio) = ((>10).length) nombre

productoCorriente:: Num a => (String, a) -> Bool
productoCorriente (nombre, _) = ((=='a').head) nombre || ((=='e').head) nombre || ((=='i').head) nombre || ((=='o').head) nombre || ((=='u').head) nombre

productoXL:: Num a => (String, a) -> (String, a)
productoXL (nombre, precio) = ((++ "XL") nombre, precio)

descodiciarProducto:: Num a => (String, a) -> (String, a)
descodiciarProducto (nombre,precio) = ((take 10) nombre, precio)

versionBarata:: Num a => (String, a) -> (String, a)
versionBarata (nombre, precio) = ((reverse.fst.descodiciarProducto) (nombre,precio), precio)

aplicarDescuento:: Fractional c => (String,c) -> c -> (String,c)
aplicarDescuento (nombre,precio) descuento = (nombre, precio - (((/100).(* descuento)) precio))

aplicarCostoDeEnvio:: Num a => (String, a) -> a -> (String, a)
aplicarCostoDeEnvio (nombre,precio) costoEnvio = (nombre, (+ precio) costoEnvio)

entregaSencilla:: Num a => (String,a) -> Bool
entregaSencilla (nombre,_) = (even.length) nombre












