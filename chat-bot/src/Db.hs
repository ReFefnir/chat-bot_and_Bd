module Db where

{-# LANGUAGE OverloadedStrings #-}

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
 --Обобщенное описание функция для отправки sql-запрсоов с параметрами и без. На них строятся все функции
--запрсоов.
sqlQuery :: (QueryParams q, QueryResults r) => Query -> q -> Connection -> IO [r]
sqlQuery q vs conn = query conn q vs

sqlQuery_ :: QueryResults r => Query -> Connection -> IO [r]
sqlQuery_ q conn = query_ conn q

sqlCmd :: QueryParams q => Query -> q -> Connection -> IO Int64
sqlCmd q vs conn = execute conn q vs

sqlCmd_ :: Query -> Connection -> IO Int64
sqlCmd_ q conn = execute_ conn q

(>>>) :: SqlQuery a -> SqlQuery b -> SqlQuery b
(>>>) q1 q2 conn = do
  q1 conn
  q2 conn
--Сами функции, посылающие sql запросы. 
selecttwocities :: String -> SqlQuery [Citynames]
selecttwocities spstring = sqlQuery "Select s.name from (Select instr (BINARY (?),t.name) as pos, t.name from cities t) s where s.pos>0 order by s.pos;" [spstring]

selectcityname :: String -> SqlQuery [Cities]
selectcityname name = sqlQuery "SELECT id, Name, Continent, Country, Latitude, Longitude FROM cities WHERE Name = (?) ;" [name]

selectcityid :: Int -> SqlQuery [Cities]
selectcityid id = sqlQuery "SELECT id, Name, Continent, Country, Latitude, Longitude FROM cities WHERE id = (?) ;" [id]

selectcollection :: SqlQuery [Collection]
selectcollection = sqlQuery_ "SELECT id, `From`, `To` FROM collection;"


selectflight :: Int -> Int -> SqlQuery [Flights]
selectflight toid fromid = sqlQuery " SELECT id, Flight, `From`, `To` FROM flights WHERE `From`=? AND `To`=? LIMIT 1;" [toid,fromid]

selectcollfl :: Int -> SqlQuery [Collfl]
selectcollfl  collid = sqlQuery " SELECT Collection_id, Flights_id FROM collection_has_flights WHERE `Collection_id`=? order by Collection_id;" [collid]

insertcity :: [String] -> SqlCommand
insertcity [b, c, d, e, f] = sqlCmd "insert into cities (Name, Continent, Country, Latitude, Longitude) values (?, ?, ?, ?, ?)" (b, c, d, (read e::Double), (read f::Double))


--простая функция для отбрасывания лишнего, нужная в roadget.
getnames :: (Citynames) -> String
getnames (Citynames {cname=cname}) = cname

getid :: (Cities) -> Int
getid (Cities {cityid=cityid}) = cityid

--функции создания некоторых наших типов данных. Их использует модуль Graph для модификации базы данных. 
cities :: (Int, String, String, String, Double, Double) -> Cities
cities (cityid, name, continent, country, latitude, longitude) = Cities { cityid = cityid, name = name, continent = continent, country = country, latitude = latitude, longitude = longitude }

collection :: (Int, Int, Int) -> Collection
collection (collectionid, fromid, toid) = Collection { collectionid = collectionid, fromid = fromid, toid = toid }

flights :: (Int, String, Int, Int) -> Flights
flights (flightid, flight, idfrom, idto) = Flights { flightid = flightid, flight = flight, idfrom = idfrom, idto = idto }

collfl :: (Int, Int) -> Collfl
collfl (collid, flid) = Collfl { collid = collid, flid = flid }

--Далее идет описание типов данных и способов их создания из типа QueryResults, который возвращают функции обращения к бд.
data Cities = Cities {cityid :: Int, name:: String, continent :: String, country :: String, latitude :: Double, longitude :: Double } deriving Show

instance QueryResults Cities where
  convertResults [fa,fb,fc,fd,fe,fg] [va,vb,vc,vd,ve,vg] = Cities {cityid = a, name = b, continent = c, country = d, latitude = e, longitude = g } 
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd
          e = convert fe ve
          g = convert fg vg      
  convertResults fs vs  = convertError fs vs 2

data Citynames = Citynames {cname:: String} deriving Show

instance QueryResults Citynames where
  convertResults [fa] [va] = Citynames {cname = a} 
    where a = convert fa va   
  convertResults fs vs  = convertError fs vs 2

data Collection = Collection {collectionid :: Int, fromid :: Int, toid :: Int} deriving Show

instance QueryResults Collection where
  convertResults [fa,fb,fc] [va,vb,vc] = Collection {collectionid = a, fromid = b, toid = c } 
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc     
  convertResults fs vs  = convertError fs vs 2

data Flights = Flights {flightid :: Int, flight :: String, idfrom :: Int, idto :: Int} deriving Show

instance QueryResults Flights where
  convertResults [fa,fb,fc,fd] [va,vb,vc,vd] = Flights {flightid = a, flight = b, idfrom = c, idto = d } 
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd     
  convertResults fs vs  = convertError fs vs 2

data Collfl = Collfl {collid :: Int, flid :: Int} deriving Show

instance QueryResults Collfl where
  convertResults [fa,fb] [va,vb] = Collfl {collid = a, flid = b} 
    where a = convert fa va
          b = convert fb vb 
  convertResults fs vs  = convertError fs vs 2




--Данные для поключения к моей базе данных. При желании их можно изменить. Описание базы данных есть в readme.
connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost = "localhost",
                            connectPort = 3306,
                            connectUser = "artem", --имя пользователя
                        connectPassword = "", --пароль пользователя
                        connectDatabase = "cbr", --имя базы данных
                         connectOptions = [],
                            connectPath = "",
                             connectSSL = Nothing }

res :: String -> SqlQuery [Citynames]
res x = selecttwocities x

namec:: String -> SqlQuery [Cities]
namec x = selectcityname x

--Функция указана грязной так как в теории она должна была быть таковой в модуле Graph (из-за обращений к бд)
path:: String -> String -> IO String
path x y=return(x++" -> "++ y)
--Функция добавления города.
cityadd:: String ->String -> IO String
cityadd from body = do
  let cr = words body
  if ((length cr) == 6) then do
    conn  <- connect connectInfo
    insertcity (tail cr) conn
    gr <- namec (head (tail cr)) conn
    return ("Hello, "++from++". New city id= "++(show (getid (head gr)))++".")
  else return ("Wrong input.")
--Функция, которая нужна была для проекта. Она может добавить новый город (если введена команда "/AddCity"), 
--иначе вычленяет названия городов начала и конца из строки ввода,подаваемой ей из модуля Server и вызывает 
--с ними функцию path. Так как сама функция path из модуля graph не работает, вместо этого вызывается 
--функция-заглушка, которая просто возвращает эти города. Действует посредством sql-запроса к базе по поиску
--вхождения городов в строку, а затем выводит все эти города в порядке их наождения в строке (мое допущение 
--в том, что сначала будет введен начальный город, а потом уже город-цель; немного это правится проверкой на
--наличие "from" перед вторым городом и сменой порядка городов в таком случае). Если их число не равно 3, 
--выдает ошибку ввода. Иначе запускает path.
roadget :: String -> String -> IO String
roadget from body = if (isPrefixOf "/AddCity" body) then do cityadd from body else do
  conn  <- connect connectInfo
  cities <- res body conn
  if ( ((length cities) == 2) && ((length (splitOn (head (map getnames cities)) body)) < 3) && ((length (splitOn (head (tail (map getnames cities))) body)) < 3) ) then if ((isInfixOf ("from "++(head (tail (map getnames cities)))) body)|| (isInfixOf ("From "++(head (tail (map getnames cities)))) body)) then do
        gr <- path (head (tail (map getnames cities))) (head (map getnames cities))
        return ("Hello, "++from++". Here's your request: "++gr++".")
      else do
        gr <- path (head (map getnames cities)) (head (tail (map getnames cities)))
        return ("Hello, "++from++". Here's your request: "++gr++".")
    else return ("Wrong Input. "++ from++", correct it, please, and try again.")
--Тестовый запуск программы, который считывает с ввода данные, которые должны были приходить в функцию рзбора 
--сообщений, а именно Имя пользователя и строку-сообщение для разбора.
run :: IO ()
run = do
  print "Enter your name:"
  name <- getLine
  print "Enter /AddCity command with params (Name, Continent, Country, latitude, longitude) or from/to:"
  line <- getLine
  value <- roadget name line
  print (value)
  return ()
