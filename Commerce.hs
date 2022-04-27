type Producto = (String,Int) 

preciototal :: Producto->Int->Int->Int->Int
preciototal producto cantidad descuento envio = ((aplicarDescuento producto descuento) * cantidad) + descuento


productoDeElite :: Producto -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && not (productoCorriente unProducto)


productoDeLujo :: Producto -> Bool
productoDeLujo unProducto = any ('z' ==) (fst unProducto) || any ('x'==) (fst unProducto)


productoCodiciado :: Producto -> Bool
productoCodiciado unProducto = length (fst unProducto) > 10


productoCorriente :: Producto -> Bool
productoCorriente unProducto = elem (head (fst unProducto)) ['a','e','i','o','u']


aplicarDescuento :: Producto -> Int ->Int
aplicarDescuento unProducto unDescuento = (snd unProducto) - (( div (snd unProducto) 100) * unDescuento)

entregaSencilla :: String -> Bool
entregaSencilla unDia = (mod (length unDia) 2) == 0

descodiciarProducto :: Producto -> Producto
descodiciarProducto unProducto = (reverse (drop 10 (reverse (fst unProducto))), snd unProducto)
                               
aplicarCostoDeEnvio::Producto -> Int -> Int
aplicarCostoDeEnvio unProducto costoDeEnvio = costoDeEnvio + snd unProducto


productoXL::Producto->Producto
productoXL unProducto = ((fst unProducto)++ "XL", snd unProducto)

versionBarata::Producto->Producto
versionBarata unProducto = (reverse (fst (descodiciarProducto unProducto)),snd unProducto) 
