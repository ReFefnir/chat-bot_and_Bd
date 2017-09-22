module Db where

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.Types
import Data.Int
import Data.String (words)
import Data.List 
import Data.List.Split (splitOn)
import System.IO
import Control.Applicative
import Prelude

type SqlQuery a = Connection -> IO a
type SqlCommand = Connection -> IO Int64
--Обобщенное описание функция для отправки sql-запрсоов с параметрами и без.
-- На них строятся все функции запрсов.

--Возвращает результат, имеет параметры запуска. Select, например.
sqlQuery :: (QueryParams q, QueryResults r) => Query -> q -> Connection -> IO [r]
sqlQuery q vs conn = query conn q vs

--Возвращает результат, работате без параметров. Select, например.
sqlQuery_ :: QueryResults r => Query -> Connection -> IO [r]
sqlQuery_ q conn = query_ conn q

--Исполняет команду с входными данными без ответа. Insert, например.
sqlCmd :: QueryParams q => Query -> q -> Connection -> IO Int64
sqlCmd q vs conn = execute conn q vs

--Исполняет команду без входных данных и без ответа. Insert, например.
sqlCmd_ :: Query -> Connection -> IO Int64
sqlCmd_ q conn = execute_ conn q

--Позволяет запускать несколько запросов последовательно с одним указанием
--параметров БД.
(>>>) :: SqlQuery a -> SqlQuery b -> SqlQuery b
(>>>) q1 q2 conn = do
  q1 conn
  q2 conn

--Сами функции, посылающие sql-запросы к бд. 

--Возвращает названия одного или нескольких городов в порядке их появления в
--строке в формате CityNames.  
selectTwoCities :: String -> SqlQuery [CityNames]
selectTwoCities messageString = sqlQuery "Select s.name from (Select instr  \
\ (BINARY (?),t.name) as pos, t.name from cities t) s where s.pos>0 order by  \
\ s.pos;" [messageString]

--Возвращает запись из таблицы cities по соответствующему имени города.
selectCityName :: String -> SqlQuery [Cities]
selectCityName name = sqlQuery "SELECT id, Name, Continent, Country,  \
\ Latitude, Longitude FROM cities WHERE Name = (?) ;" [name]

--Возвращает запись из таблицы cities по соответствующему id города.
selectCityId :: Int -> SqlQuery [Cities]
selectCityId id = sqlQuery "SELECT id, Name, Continent, Country,  \
\ Latitude, Longitude FROM cities WHERE id = (?) ;" [id]

--Выгружает всю коллекцию маршрутов.
selectCollection :: SqlQuery [Collection]
selectCollection = sqlQuery_ "SELECT id, `From`, `To` FROM collection;"

--Возвращает запись из таблицы flights по соответствующим id городов в ней.
selectFlight :: Int -> Int -> SqlQuery [Flights]
selectFlight toId fromId = sqlQuery " SELECT id, Flight, `From`, `To` FROM  \
\ flights WHERE `From`=? AND `To`=? LIMIT 1;" [toId,fromId]

--Возвращает записи из таблицы collection_has_flights по соответствующему
--id коллекции.
selectCollectionByFlight :: Int -> SqlQuery [CollectionByFlight]
selectCollectionByFlight  collectionId = sqlQuery " SELECT Collection_id,  \
\ Flights_id FROM collection_has_flights WHERE `Collection_id`=? order by  \
\ Collection_id;" [collectionId]

--Вставляет город в таблицу cities по заданным данным. id записи создается 
--автоматически (autoincrement).
insertCity :: [String] -> SqlCommand
insertCity [name, continent, country, latitude, longitude] = sqlCmd "insert  \
\ into cities (Name, Continent, Country, Latitude, Longitude)  \
\ values (?, ?, ?, ?, ?)" ((intercalate " " (splitOn "_" name)),
 continent, country, (read latitude::Double), (read longitude::Double))

--простая функция для отбрасывания всего кроме имен городов. 
getNames :: (CityNames) -> String
getNames (CityNames {cityName=cityName}) = cityName

--простая функция для отбрасывания всего кроме id города. 
getId :: (Cities) -> Int
getId (Cities {cityId=cityId}) = cityId

--функции создания некоторых наших типов данных. Их использует модуль Graph для
--модификации базы данных. 
cities :: (Int, String, String, String, Double, Double) -> Cities
cities (cityId, name, continent, country, latitude, longitude) = 
  Cities { cityId = cityId, name = name, continent = continent, country = 
  country, latitude = latitude, longitude = longitude }

collection :: (Int, Int, Int) -> Collection
collection (collectionId, fromId, toId) = 
  Collection {collectionId = collectionId, fromId = fromId, toId = toId}

flights :: (Int, String, Int, Int) -> Flights
flights (flightId, flight, idFrom, idTo) = 
  Flights {flightId = flightId, flight = flight, idFrom = idFrom, idTo = idTo}

collectionByFlight :: (Int, Int) -> CollectionByFlight
collectionByFlight (collId, flId) = 
  CollectionByFlight {collId = collId, flId = flId}

--Далее идет описание типов данных и способов их создания из типа QueryResults,
--который возвращают функции обращения к бд.
data Cities = Cities {cityId :: Int, name:: String, continent :: String,
  country :: String, latitude :: Double, longitude :: Double } deriving Show

instance QueryResults Cities where
  convertResults [fa,fb,fc,fd,fe,fg] [va,vb,vc,vd,ve,vg] = 
    Cities {cityId = a, name = b, continent = c, country = d, latitude = e, 
    longitude = g } 
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd
          e = convert fe ve
          g = convert fg vg      
  convertResults fs vs  = convertError fs vs 2

data CityNames = CityNames {cityName:: String} deriving Show

instance QueryResults CityNames where
  convertResults [fa] [va] = CityNames {cityName = a} 
    where a = convert fa va   
  convertResults fs vs  = convertError fs vs 2

data Collection = Collection {collectionId :: Int, fromId :: Int, toId :: Int}
  deriving Show

instance QueryResults Collection where
  convertResults [fa,fb,fc] [va,vb,vc] = 
    Collection {collectionId = a, fromId = b, toId = c } 
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc     
  convertResults fs vs  = convertError fs vs 2

data Flights = 
  Flights {flightId :: Int, flight :: String, idFrom :: Int, idTo :: Int} 
  deriving Show

instance QueryResults Flights where
  convertResults [fa,fb,fc,fd] [va,vb,vc,vd] =
    Flights {flightId = a, flight = b, idFrom = c, idTo = d } 
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd     
  convertResults fs vs  = convertError fs vs 2

data CollectionByFlight = CollectionByFlight {collId :: Int, flId :: Int}
  deriving Show

instance QueryResults CollectionByFlight where
  convertResults [fa,fb] [va,vb] = CollectionByFlight {collId = a, flId = b} 
    where a = convert fa va
          b = convert fb vb 
  convertResults fs vs  = convertError fs vs 2




--Данные для поключения к моей базе данных. При желании их можно изменить.
--Описание базы данных есть в readme.
connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost = "localhost",
                            connectPort = 3306,
                            connectUser = "artem", --имя пользователя
                        connectPassword = "", --пароль пользователя
                        connectDatabase = "cbr", --имя базы данных
                         connectOptions = [],
                            connectPath = "",
                             connectSSL = Nothing }

--Берет строку для разбора, возвращает названия двух городов из таблицы.
findTwoCities :: String -> SqlQuery [CityNames]
findTwoCities x = selectTwoCities x

--Берет имя города, возвращает запись из таблицы о нем.
findCityByName:: String -> SqlQuery [Cities]
findCityByName x = selectCityName x

--Функция указана грязной так как в теории она должна была быть таковой в
--модуле Graph (из-за обращений к бд). Эта заглушка просто возвращает два 
--города в том же порядке через " -> "
path:: String -> String -> IO String
path from to=return(from++" -> "++ to)

--Функция добавления города.
cityAdd:: String ->String -> IO String
cityAdd from body = do
  let separatedString = words body
  if ((length separatedString) == 6) 
  then do
    conn  <- connect connectInfo
    insertCity (tail separatedString) conn
    gr <- findCityByName (intercalate " " 
      (splitOn "_" (head (tail separatedString)))) conn
    return ("Hello, "++from++". New city id= "++(show (getId (head gr)))++".")
  else 
    return (errorCity from body separatedString)

--Разбор ошибок добавления нового города
errorCity:: String -> String -> [String] -> String
errorCity from body separatedString = if ((length separatedString) < 6) 
then 
  "Too little params, "++from++". Use spaces to separate them." 
else "Too many params, "++from++". If your city name consists of multiple"++
  " words, use _ to separate them. It will then be changed to normal space."

--Функция, которая нужна была для проекта. Она может добавить новый город
--(если введена команда "/AddCity"), иначе вычленяет названия городов начала и
-- конца из строки ввода,подаваемой ей из модуля Server и вызывает с ними
--функцию path. Так как сама функция path из модуля graph не работает, вместо
--этого вызывается функция-заглушка, которая просто возвращает эти города. 
--Действует посредством sql-запроса к базе по поиску вхождения городов в 
--строку, а затем выводит все эти города в порядке их наождения в строке 
--(мое допущение в том, что сначала будет введен начальный город, а потом
--уже город-цель; немного это правится проверкой на наличие "from" перед 
--вторым городом и сменой порядка городов в таком случае). Если их число 
--не равно 2 (дубликаты считаются отдельно), вызывается функцию разбора 
--ошибок wrongInput. Иначе запускает path.
roadGet :: String -> String -> IO String
roadGet from body = if ((isPrefixOf "/AddCity" body)||
  (isPrefixOf "/Addcity" body)||(isPrefixOf "/addCity" body)||
  (isPrefixOf "/addcity" body)||(isPrefixOf "AddCity" body)||
  (isPrefixOf "Addcity" body)||(isPrefixOf "addCity" body)||
  (isPrefixOf "addcity" body)||(isPrefixOf "/Add City" body)||
  (isPrefixOf "/Add city" body)||(isPrefixOf "/add City" body)||
  (isPrefixOf "/add city" body)||(isPrefixOf "Add City" body)||
  (isPrefixOf "Add city" body)||(isPrefixOf "add City" body)||
  (isPrefixOf "add city" body)||(isPrefixOf "/Add_City" body)||
  (isPrefixOf "/Add_city" body)||(isPrefixOf "/add_City" body)||
  (isPrefixOf "/add_city" body)||(isPrefixOf "Add_City" body)||
  (isPrefixOf "Add_city" body)||(isPrefixOf "add_City" body)||
  (isPrefixOf "add_city" body)) 
  then do 
    cityAdd from body 
  else do
    conn  <- connect connectInfo
    unclearedCities <- findTwoCities body conn
    let cities = clearCities body (map getNames unclearedCities)
    if ( ((length cities) == 2) &&
      ((length (splitOn (head cities) body)) < 3  + 
        (boolToInt (nameDetection(head cities) (head (tail cities))))) &&
      ((length (splitOn (head (tail cities)) body)) < 3 + 
        (boolToInt (nameDetection (head (tail cities)) (head cities)))) ) 
    then
      if ((isInfixOf ("from "++(head (tail cities))) body)||
        (isInfixOf ("From "++(head (tail cities))) body))|| 
        (secondFirst cities body) 
      then do
        gr <- path (head (tail cities)) 
          (head cities)
        return ("Hello, "++from++". Here's your request: "++gr++".")
      else do
        gr <- path (head cities) 
          (head (tail cities))
        return ("Hello, "++from++". Here's your request: "++gr++".")
    else 
      return (wrongInput from body cities)

--Индикатор того, что searchCity на самом деле ложный результат и его надо
--удалить.
falseCityDetection:: String -> String -> String -> Bool
falseCityDetection body searchCity city = (isInfixOf searchCity city) &&
  ((length (splitOn searchCity body)) == (length (splitOn city body)))

--True=1, False=0. Нужно для прибавления единицы допустимому числу вхождений 
--названия города при двойном вхождении за счет включения в другое название.
boolToInt:: Bool -> Int
boolToInt bool = if bool then 1 else 0

--Проверяет, входит ли первый город во второй (названия).
nameDetection:: String -> String -> Bool
nameDetection searchCity city = isInfixOf searchCity city 

--Удаляет ложные города, оставляя толькко нужные.
clearCities:: String -> [String] -> [String]
clearCities body cities = case (length cities) of
  (0) -> cities
  (1) -> cities
  (_) -> if (or (map (falseCityDetection body (head cities)) (tail cities))) 
  then
    clearCities body (tail cities)
  else (head cities):(clearCities body (tail cities))

--Проверяет, является ли одно из названий городов включенным в название другого
--и, если да, проверяет, действительно ли оно встречается первым в программе
--или это ошибка и на самом деле это слово идет вторым.
secondFirst:: [String] -> String -> Bool 
secondFirst cities body = if (nameDetection(head cities) (head (tail cities))) 
then 
  isInfixOf (head cities) (head (tail (splitOn (head (tail cities)) body)))
else False


--Функция более точного разбора ошибок ввода при работе с городами.
wrongInput:: String -> String -> [String] -> String
wrongInput from body cities = case (length cities) of
  (2) ->if ((length (splitOn (head (tail cities)) body)) < 3 + 
    (boolToInt (nameDetection(head cities) (head (tail cities))))) 
    then
      "Hey, "++from++". You asked me twice about city "++
      (head cities)++". If you want to go to "++
      (head (tail cities))++
      " and back, ask me twice (separately)." 
    else
      "I don't know what to say, "++from++". You asked me about "++
      (head (tail cities))++" multiple times. "++
      "What's the meaning of this?"
  (1) ->if ((length (splitOn (head cities) body)) > 2) 
    then
      "Hello, "++from++". You wanted to trick me and stay in "++
      (head cities)++", didn't you?" 
    else 
      from++", I need two cities to build route. Maybe I don't"++
      " know one of the cities you specified. If so, you can add it with "++
      "/AddCity command. City "++(head cities)++
      " is known to me."
  (0) -> "Sorry, "++from++
    ", I don't know any of the cities you specified. If you didn't specify"++
    " any, it's just a wrong input."
  (_) -> "Sorry, "++from++
    ", I can work with two cities only. You asked of cities: "++
    (intercalate ", " cities)++"."

--Тестовый запуск программы, который считывает с ввода данные, которые должны
--были приходить в функцию рзбора сообщений, а именно Имя пользователя и 
--строку-сообщение для разбора.
run :: IO ()
run = do
  print "Enter your name:"
  from <- getLine
  print "Enter /AddCity command with params (Name, Continent, Country,  \
\ latitude, longitude) or from/to:"
  line <- getLine
  value <- roadGet from line
  print (value)
  return ()
