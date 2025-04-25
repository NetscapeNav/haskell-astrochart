{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Effects
import Data.Astro.Time.Epoch
import Data.Astro.CelestialObject.RiseSet
import Data.Astro.Planet
import Data.Astro.Planet.PlanetMechanics
import Data.Astro.Moon
import Data.Astro.Moon.MoonDetails
import Data.Astro.Sun
import Data.Fixed (mod')
import System.IO
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.List (isInfixOf, lookup)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad (when)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Win32.Console (setConsoleCP, getConsoleCP, setConsoleOutputCP)

constellationsData :: BS.ByteString
constellationsData = $(embedFile "Созвездия.csv")

housesData :: BS.ByteString
housesData = $(embedFile "Дома.csv")

retrogradeData :: BS.ByteString
retrogradeData = $(embedFile "Ретроградность.csv")

aspectsData :: BS.ByteString
aspectsData = $(embedFile "Аспекты.csv")

parseEmbeddedCSV :: BS.ByteString -> [[Text]]
parseEmbeddedCSV bs = 
    map (T.split (== ';')) . 
    T.lines . 
    TE.decodeUtf8 $ bs

-- Устанавливаем кодировку консоли
setConsoleEncoding :: IO ()
setConsoleEncoding = do
  setLocaleEncoding utf8
  _ <- setConsoleCP 65001
  _ <- setConsoleOutputCP 65001
  return ()

-- 1. Функции зодиакальных знаков и вспомогательные функции
zodiacSign :: Double -> Text
zodiacSign lon
    | lon >= 0   && lon < 30   = "Овен"
    | lon >= 30  && lon < 60   = "Телец"
    | lon >= 60  && lon < 90   = "Близнецы"
    | lon >= 90  && lon < 120  = "Рак"
    | lon >= 120 && lon < 150  = "Лев"
    | lon >= 150 && lon < 180  = "Дева"
    | lon >= 180 && lon < 210  = "Весы"
    | lon >= 210 && lon < 240  = "Скорпион"
    | lon >= 240 && lon < 270  = "Стрелец"
    | lon >= 270 && lon < 300  = "Козерог"
    | lon >= 300 && lon < 330  = "Водолей"
    | otherwise = "Рыбы"

arcsecs = fromDMS 0 0

data NonPlanet = Pluto deriving (Show, Eq)

j2000NonPlanetDetails :: NonPlanet -> PlanetDetails
j2000NonPlanetDetails Pluto = PlanetDetails Neptune j2000 245.73 240.4311 223.654 0.24448 39.23107 17.165 110.249 (arcsecs 0.08)

-- 2. Основные функции расчета
getLongitudeAsDouble :: EclipticCoordinates -> Double
getLongitudeAsDouble (EcC _ (DD lambda)) = lambda

getHouse :: Double -> Double -> Int
getHouse asc lon =
  let house = 1 + floor ((lon - asc) / 30) `mod` 12
  in if house == 0 then 12 else house

addByModule :: Double -> Double -> Double
addByModule number add =
  let result = number + add
  in if result > 360 then result - 360 else result

angleBetween :: Double -> Double -> Double
angleBetween lon1 lon2 =
  let diff = abs (lon1 - lon2)
  in min diff (360 - diff)

-- 3. Функции для работы с координатами
riseToHorizon :: RiseSetMB -> HorizonCoordinates
riseToHorizon (RiseSet (Just (_, azRise)) _) = HC 0 azRise
riseToHorizon _ = error "No rise data"

setToHorizon :: RiseSetMB -> HorizonCoordinates
setToHorizon (RiseSet _ (Just (_, azSet))) = HC 0 azSet
setToHorizon _ = error "No set data"

horizonToEcliptic :: GeographicCoordinates -> JulianDate -> HorizonCoordinates -> EclipticCoordinates
horizonToEcliptic ro jd hc =
  let eq2 = horizonToEquatorial (geoLatitude ro) hc
      eq1 = EC1 (e2Declination eq2) (haToRA (e2HoursAngle eq2) (geoLongitude ro) jd)
  in equatorialToEcliptic eq1 jd

-- 4. Функции для анализа аспектов и ретроградности
aspects :: [(Text, Double)]
aspects = [("Соединение", 0), ("Секстиль", 60), ("Квадрат", 90), ("Трин", 120), ("Оппозиция", 180)]

orbitalMargins :: [(Text, Double)]
orbitalMargins = [("Солнце", 8), ("Луна", 8), ("Меркурий", 5), ("Венера", 5), 
                 ("Марс", 5), ("Юпитер", 3), ("Сатурн", 3), ("Уран", 3), 
                 ("Нептун", 3), ("Плутон", 3), ("Асцендент", 3), ("МС", 3)]

findAspect :: Text -> Double -> Text -> Double -> Maybe Text
findAspect name1 lon1 name2 lon2 =
  let angle = angleBetween lon1 lon2
      margin1 = Prelude.lookup name1 orbitalMargins
      margin2 = Prelude.lookup name2 orbitalMargins
      margin = case (margin1, margin2) of
                 (Just m1, Just m2) -> (m1 + m2) / 2
                 _ -> 0
  in case filter (\(_, exact) -> abs (angle - exact) <= margin) aspects of
       ((aspect, _):_) -> Just (name1 <> " и " <> name2 <> " образуют " <> aspect)
       [] -> Nothing

isRetrograde :: (JulianDate -> EquatorialCoordinates1) -> LocalCivilTime -> JulianDate -> Bool
isRetrograde positionFunc dt jd =
  let lon1 = getLongitudeAsDouble (equatorialToEcliptic (positionFunc jd) jd)
      prevDate (LCT tz jd') = LCT tz (jd' - JD 1)
      prevJD = lctUniversalTime (prevDate dt)
      pos2 = positionFunc prevJD
      ecl2 = equatorialToEcliptic pos2 prevJD
      lon2 = getLongitudeAsDouble ecl2
      deltaLon = (lon2 - lon1 + 360) `mod'` 360
  in deltaLon < 180

isCombust :: Text -> Double -> Double -> Maybe Text
isCombust name lonSun lonPlanet =
  let angle = angleBetween lonSun lonPlanet
  in if name /= "Солнце" && angle <= 4 then Just (name <> " сожжен Солнцем") else Nothing

-- 5. Функции для работы с CSV
type CSV = [[Text]]

readCSV :: FilePath -> IO [[Text]]
readCSV path = do
  contents <- TIO.readFile path
  return $ map (T.split (== ';')) (T.lines contents)

getCell :: CSV -> Int -> Int -> IO Text
getCell csv rowIndex colIndex =
  if rowIndex >= 0 && rowIndex < length csv then
    let row = csv !! rowIndex
    in if colIndex >= 0 && colIndex < length row then
         return (row !! colIndex)
       else
         return "Ошибка: индекс столбца выходит за границы"
  else
    return "Ошибка: индекс строки выходит за границы"

-- 6. Порядки и индексы для поиска в CSV
planetOrderGalaxy :: [(Text, Int)]
planetOrderGalaxy = [("Солнце", 1), ("Луна", 2), ("Меркурий", 3), ("Венера", 4), 
                    ("Марс", 5), ("Юпитер", 6), ("Сатурн", 7), ("Уран", 8), 
                    ("Нептун", 9), ("Плутон", 10), ("Асцендент", 11), ("МС", 12)]

planetOrderHouse :: [(Text, Int)]
planetOrderHouse = [("Солнце", 1), ("Луна", 2), ("Меркурий", 3), ("Венера", 4),
                   ("Марс", 5), ("Юпитер", 6), ("Сатурн", 7), ("Уран", 8),
                   ("Нептун", 9), ("Плутон", 10), ("Асцендент", 11), ("МС", 12)]

planetOrderR :: [(Text, Int)]
planetOrderR = [("Меркурий", 1), ("Венера", 2), ("Марс", 3), ("Юпитер", 4),
               ("Сатурн", 5), ("Уран", 6), ("Нептун", 7), ("Плутон", 8)]

aspectsOrder :: [(Text, Int)]
aspectsOrder = [("Соединение", 1), ("Секстиль", 2), ("Квадрат", 3), 
                ("Трин", 4), ("Оппозиция", 5)]

planetsAspectNumber :: [(Text, Int)]
planetsAspectNumber = [("Солнце", 1), ("Луна", 56), ("Меркурий", 106), 
                      ("Венера", 151), ("Марс", 191), ("Юпитер", 226),
                      ("Сатурн", 256), ("Уран", 281), ("Нептун", 301), 
                      ("Плутон", 316)]

constellationsOrder :: [(Text, Int)]
constellationsOrder = [("Овен", 1), ("Телец", 2), ("Близнецы", 3), ("Рак", 4),
                      ("Лев", 5), ("Дева", 6), ("Весы", 7), ("Скорпион", 8),
                      ("Стрелец", 9), ("Козерог", 10), ("Водолей", 11), ("Рыбы", 12)]

-- 7. Функции поиска в CSV
findConstellationValue :: CSV -> Text -> Text -> Int -> IO ()
findConstellationValue csv constellation planet colIndex = do
  let planetNum = fromMaybe 0 (lookup planet planetOrderGalaxy)
      constNum = fromMaybe 0 (lookup constellation constellationsOrder)
      rowIndex = (constNum - 1) * 12 + planetNum
  cellValue <- getCell csv rowIndex colIndex
  TIO.putStrLn $ "Значение в CSV: " <> cellValue

findHouseValue :: CSV -> Int -> Text -> Int -> IO ()
findHouseValue csv houseNumber planet colIndex = do
  let planetNum = fromMaybe 0 (lookup planet planetOrderHouse)
      rowIndex = (houseNumber - 1) * 10 + planetNum
  cellValue <- getCell csv rowIndex colIndex
  TIO.putStrLn $ "Значение в CSV: " <> cellValue

findRetrogradeValue :: CSV -> Text -> Int -> IO ()
findRetrogradeValue csv planet colIndex = do
  let planetNum = fromMaybe 0 (lookup planet planetOrderR)
      rowIndex = planetNum - 1
  cellValue <- getCell csv rowIndex colIndex
  TIO.putStrLn $ "Значение в CSV: " <> cellValue

findAspectValueInCSV :: CSV -> Text -> Text -> Text -> Int -> IO ()
findAspectValueInCSV csv planet1 planet2 aspect colIndex = do
  case calculateAspectIndex planet1 planet2 aspect of
    Just rowIndex -> do
      cellValue <- getCell csv (rowIndex - 1) colIndex
      TIO.putStrLn $ "Значение в CSV: " <> cellValue
    Nothing -> TIO.putStrLn "Ошибка: одна из планет или аспект не найдены!"

calculateAspectIndex :: Text -> Text -> Text -> Maybe Int
calculateAspectIndex planet1 planet2 aspect = do
  p1Num <- lookup planet1 planetsAspectNumber
  p1House <- lookup planet1 planetOrderHouse
  p2House <- lookup planet2 planetOrderHouse
  aspectNum <- lookup aspect aspectsOrder
  return $ p1Num + (p2House - p1House - 1) * 5 + aspectNum

extractAspectName :: Text -> Text
extractAspectName msg =
  head $ filter (`T.isInfixOf` msg) ["Соединение", "Секстиль", "Квадрат", "Трин", "Оппозиция"]

isSpecialPair :: Text -> Text -> Bool
isSpecialPair name1 name2 =
  (name1 == "Асцендент" && name2 == "МС") || (name1 == "МС" && name2 == "Асцендент")

fromDMS'' :: Double -> Double -> Double -> DecimalDegrees
fromDMS'' d m s = DD (d + m/60 + s/3600)

-- 8. Главная функция
main :: IO ()
main = do
  setConsoleEncoding
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8

  let constellations = parseEmbeddedCSV constellationsData
      houses = parseEmbeddedCSV housesData
      retrograde = parseEmbeddedCSV retrogradeData
      aspects = parseEmbeddedCSV aspectsData
  
  -- Ввод данных
  TIO.putStrLn "=== Введите данные рождения ==="
  
  TIO.putStrLn "Год (например, 2006):"
  year <- read <$> getLine :: IO Integer
  
  TIO.putStrLn "Месяц (1-12):"
  month <- read <$> getLine :: IO Int
  
  TIO.putStrLn "День (1-31):"
  day <- read <$> getLine :: IO Int
  
  TIO.putStrLn "Час (0-23):"
  hour <- read <$> getLine :: IO Int
  
  TIO.putStrLn "Минуты (0-59):"
  minute <- read <$> getLine :: IO Int
  
  TIO.putStrLn "Секунды (0-59):"
  second <- read <$> getLine :: IO Double
  
  TIO.putStrLn "Часовой пояс (например, 7 для UTC+7):"
  tz <- read <$> getLine :: IO Double
  
  TIO.putStrLn "Широта (градусы минуты секунды через пробел, например: 53 20 50):"
  latStr <- getLine
  let [latD, latM, latS] = map read (words latStr) :: [Double]
      latitude = fromDMS'' latD latM latS
  
  TIO.putStrLn "Долгота (градусы минуты секунды через пробел, например: 83 46 42):"
  lonStr <- getLine
  let [lonD, lonM, lonS] = map read (words lonStr) :: [Double]
      longitude = fromDMS'' lonD lonM lonS

  --TIO.putStrLn "Введите номер столбца для интерпретации (2 - Характер, 3 - Цели и амбиции, 4 - Эмоции, 5 - Дружба, 6 - Любовь, 7 - Семья, 8 - Финансовое благополучие, 9 - Карьера, 10 - Интеллект, 11 - Умения, 12 - Здоровье, 13 - События прошлого, 14 - Потенциал развития, 15 - Творчество, 16 - Риски, 17 - Миссия):"
  --colIndexStr <- getLine
  --let colIndex = read colIndexStr :: Int
  
  -- Расчетные данные
  let dt = lctFromYMDHMS (DH tz) year month day hour minute second
      jd = lctUniversalTime dt
      ro = GeoC latitude longitude
      today = lcdFromYMD (DH tz) year month day
      riseSetSun = sunRiseAndSet ro 0.833333 today

      -- Данные для Майами:
      --Широта (градусы минуты секунды через пробел, например: 53 20 50):
      --25 46 0
      --Долгота (градусы минуты секунды через пробел, например: 83 46 42):
      --(-80) 11 31

      ascEcliptic = horizonToEcliptic ro jd (riseToHorizon riseSetSun)
      ascLongitude = getLongitudeAsDouble ascEcliptic
      mcLongitude = (ascLongitude + 270) `mod'` 360
      
      -- Планетные данные
      earthDetails = j2010PlanetDetails Earth
      mercuryDetails = j2010PlanetDetails Mercury
      venusDetails = j2010PlanetDetails Venus
      marsDetails = j2010PlanetDetails Mars
      jupiterDetails = j2010PlanetDetails Jupiter
      saturnDetails = j2010PlanetDetails Saturn
      uranusDetails = j2010PlanetDetails Uranus
      neptuneDetails = j2010PlanetDetails Neptune
      plutoDetails = j2000NonPlanetDetails Pluto
      
      -- Позиции планет
      mercuryPos = planetPosition planetTrueAnomaly1 mercuryDetails earthDetails jd
      venusPos = planetPosition planetTrueAnomaly1 venusDetails earthDetails jd
      marsPos = planetPosition planetTrueAnomaly1 marsDetails earthDetails jd
      jupiterPos = planetPosition planetTrueAnomaly1 jupiterDetails earthDetails jd
      saturnPos = planetPosition planetTrueAnomaly1 saturnDetails earthDetails jd
      uranusPos = planetPosition planetTrueAnomaly1 uranusDetails earthDetails jd
      neptunePos = planetPosition planetTrueAnomaly1 neptuneDetails earthDetails jd
      plutoPos = planetPosition planetTrueAnomaly1 plutoDetails earthDetails jd
      moonPos = moonPosition1 j2010MoonDetails jd
      sunPos = sunPosition2 jd
      
      -- Эклиптические координаты
      mercuryEcl = equatorialToEcliptic mercuryPos jd
      venusEcl = equatorialToEcliptic venusPos jd
      marsEcl = equatorialToEcliptic marsPos jd
      jupiterEcl = equatorialToEcliptic jupiterPos jd
      saturnEcl = equatorialToEcliptic saturnPos jd
      uranusEcl = equatorialToEcliptic uranusPos jd
      neptuneEcl = equatorialToEcliptic neptunePos jd
      plutoEcl = equatorialToEcliptic plutoPos jd
      moonEcl = equatorialToEcliptic moonPos jd
      sunEcl = equatorialToEcliptic sunPos jd
      
      -- Список целей
      targets = [("Солнце", getLongitudeAsDouble sunEcl),
                ("Луна", getLongitudeAsDouble moonEcl),
                ("Меркурий", getLongitudeAsDouble mercuryEcl),
                ("Венера", getLongitudeAsDouble venusEcl),
                ("Марс", getLongitudeAsDouble marsEcl),
                ("Юпитер", getLongitudeAsDouble jupiterEcl),
                ("Сатурн", getLongitudeAsDouble saturnEcl),
                ("Уран", getLongitudeAsDouble uranusEcl),
                ("Нептун", getLongitudeAsDouble neptuneEcl),
                ("Плутон", getLongitudeAsDouble plutoEcl),
                ("Асцендент", ascLongitude),
                ("МС", addByModule ascLongitude 270)]

  let processColumn colIndex = do
        clearScreen
        setCursorPosition 0 0
        TIO.putStrLn "\n=== Результаты расчета ==="
        
        -- Вывод информации о созвездиях
        TIO.putStrLn "\n--- Позиции в созвездиях ---"
        let printConstellation planetName ecliptic = do
              let sign = zodiacSign (getLongitudeAsDouble ecliptic)
              TIO.putStrLn $ planetName <> ": " <> sign
              findConstellationValue constellations sign planetName colIndex
        
        printConstellation "Солнце" sunEcl
        printConstellation "Луна" moonEcl
        printConstellation "Меркурий" mercuryEcl
        printConstellation "Венера" venusEcl
        printConstellation "Марс" marsEcl
        printConstellation "Юпитер" jupiterEcl
        printConstellation "Сатурн" saturnEcl
        printConstellation "Уран" uranusEcl
        printConstellation "Нептун" neptuneEcl
        printConstellation "Плутон" plutoEcl
        let ascSign = zodiacSign ascLongitude
        TIO.putStrLn $ "Асцендент: " <> ascSign
        findConstellationValue constellations ascSign "Асцендент" colIndex
        let mcSign = zodiacSign mcLongitude
        TIO.putStrLn $ "MC: " <> mcSign
        findConstellationValue constellations mcSign "МС" colIndex
        
        -- Вывод информации о домах
        TIO.putStrLn "\n--- Позиции в домах ---"
        let printHouse planetName ecliptic = do
              let house = getHouse ascLongitude (getLongitudeAsDouble ecliptic)
              TIO.putStrLn $ planetName <> ": дом " <> T.pack (show house)
              findHouseValue houses house planetName colIndex
        
        printHouse "Солнце" sunEcl
        printHouse "Луна" moonEcl
        printHouse "Меркурий" mercuryEcl
        printHouse "Венера" venusEcl
        printHouse "Марс" marsEcl
        printHouse "Юпитер" jupiterEcl
        printHouse "Сатурн" saturnEcl
        printHouse "Уран" uranusEcl
        printHouse "Нептун" neptuneEcl
        printHouse "Плутон" plutoEcl
        
        -- Проверка ретроградности
        TIO.putStrLn "\n--- Ретроградность ---"
        let checkRetrograde planetName details = do
              let retro = isRetrograde (\jd -> planetPosition planetTrueAnomaly1 details earthDetails jd) dt jd
              TIO.putStrLn $ planetName <> ": " <> if retro then "Ретроградный" else "Директный"
              when retro $ findRetrogradeValue retrograde planetName colIndex
        
        checkRetrograde "Меркурий" mercuryDetails
        checkRetrograde "Венера" venusDetails
        checkRetrograde "Марс" marsDetails
        checkRetrograde "Юпитер" jupiterDetails
        checkRetrograde "Сатурн" saturnDetails
        checkRetrograde "Уран" uranusDetails
        checkRetrograde "Нептун" neptuneDetails
        checkRetrograde "Плутон" plutoDetails
        
        -- Проверка сожжения
        TIO.putStrLn "\n--- Сожжение планет ---"
        let sunLon = getLongitudeAsDouble sunEcl
            checkCombust (name, lon) = 
              case isCombust name sunLon lon of
                Just msg -> TIO.putStrLn msg
                Nothing -> return ()
        
        mapM_ checkCombust [ (name, lon) | (name, lon) <- targets, name /= "Солнце" ]
        
        -- Анализ аспектов
        TIO.putStrLn "\n--- Аспекты между планетами ---"
        let checkAspects (i, (name1, lon1)) = 
              mapM_ (\(name2, lon2) -> 
                when (name1 /= name2 && not (isSpecialPair name1 name2)) $ do
                  case findAspect name1 lon1 name2 lon2 of
                    Just aspectMsg -> do
                      TIO.putStrLn aspectMsg
                      findAspectValueInCSV aspects name1 name2 (extractAspectName aspectMsg) (colIndex+1)
                    Nothing -> return ()
                ) (drop (i + 1) targets)
        
        mapM_ checkAspects (zip [0..] targets)
  
  -- Основной цикл запросов
  let loop = do
        TIO.putStrLn "\nВведите номер столбца для интерпретации (2 - Характер, 3 - Цели и амбиции, 4 - Эмоции, 5 - Дружба, 6 - Любовь, 7 - Семья, 8 - Финансовое благополучие, 9 - Карьера, 10 - Интеллект, 11 - Умения, 12 - Здоровье, 13 - События прошлого, 14 - Потенциал развития, 15 - Творчество, 16 - Риски, 17 - Миссия, q - Выход):"
        input <- getLine
        if input == "q"
          then return ()
          else do
            case readMaybe input of
              Just colIndex | colIndex >= 2 && colIndex <= 17 -> do
                processColumn colIndex
                loop
              _ -> do
                TIO.putStrLn "Некорректный ввод. Пожалуйста, введите число от 2 до 17 или 'q' для выхода."
                loop
  
  -- Запускаем цикл
  loop