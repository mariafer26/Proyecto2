import Data.Time.Clock
import Data.Time.Format
import System.IO
import Control.Exception --usamos estas importaciones, para manejar excepciones y leer todo el archivo
import Control.DeepSeq (deepseq)
import Data.List

-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime -- Usamos Maybe para representar que el vehículo aún está en el parqueadero o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    Vehiculo placaVehiculo tiempo Nothing : parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero
    where
        isNothing Nothing = True
        isNothing _       = False


-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    diffUTCTime tiempoActual (entrada vehiculo)

-- Función para mostrar un vehículo en formato legible
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo (Vehiculo placa entrada (Just salida)) =
    placa ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" entrada ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" salida --hace cadena de texto legible
mostrarVehiculo (Vehiculo placa entrada Nothing) =
    placa ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" entrada ++ ","

-- Función para guardar la información de los vehículos en un archivo de texto
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    withFile "parqueadero.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarVehiculo parqueadero))
    putStrLn "Parqueadero guardado en el archivo parqueadero.txt."

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    resultado <- try (readFile "parqueadero.txt") :: IO (Either IOException String) -- se usa un try que captura excepciones
    case resultado of
        Left ex -> do
            putStrLn $ "Advertencia: No se pudo leer el archivo parqueadero.txt: " ++ show ex
            return []
        Right contenido -> do
            contenido `deepseq` return ()  -- Forzar la evaluación completa del contenido
            let parqueadero = map leerVehiculo (lines contenido)
            return parqueadero

-- Función para leer un vehículo desde una línea de texto
leerVehiculo :: String -> Vehiculo
leerVehiculo linea =
    let [placa, entrada, salida] = split ',' linea
    in Vehiculo placa (read entrada) (if salida == "" then Nothing else Just (read salida))

-- Función auxiliar para dividir una cadena de texto, separa los argumentos
split :: Char -> String -> [String]
split _ "" = [""]
split delimiter str =
    foldr (\c acc -> if c == delimiter then "":acc else (c:head acc):tail acc) [""] str
-- Función para eliminar un vehículo por su placa del parqueadero
eliminarVehiculo :: String -> [Vehiculo] -> [Vehiculo]
eliminarVehiculo placaVehiculo = filter (\v -> placa v /= placaVehiculo)

--crear formato de las listas
formatoVehiculo :: Vehiculo -> String
formatoVehiculo vehiculo =
    "Vehiculo {PLACA: " ++ mostrarVehiculo vehiculo ++ "}"


main :: IO ()
main = do
    parqueaderoInicial <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"
    cicloPrincipal parqueaderoInicial

-- Función principal
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. mostrar vehiculos"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    tiempoActual <- getCurrentTime
                    let parqueaderoActualizado = eliminarVehiculo placaVehiculo (registrarSalida placaVehiculo tiempoActual parqueadero)
                    putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ha salido del parqueadero."
                    guardarParqueadero parqueaderoActualizado
                    cicloPrincipal parqueaderoActualizado
                Nothing -> do
                    putStrLn "Vehículo no encontrado en el parqueadero o ya ha salido."
                    cicloPrincipal parqueadero

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    let tiempoTotal = tiempoEnParqueadero vehiculo tiempoActual
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero

        "4" -> do
            contenido <- readFile "parqueadero.txt"
            let lineas  = lines contenido
                vehiculos = map leerVehiculo lineas
                vehiculosFormateados = map formatoVehiculo vehiculos
            mapM_ putStrLn vehiculosFormateados -- ejecuta la acción putStrLn para cada elemento de la lista
            cicloPrincipal parqueadero

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero
