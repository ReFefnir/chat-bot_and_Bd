module Db where

{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.Types
import Data.Int
import Data.List
import System.IO
import Control.Applicative

import Prelude
 
type SqlQuery a = Connection -> IO a
type SqlCommand = Connection -> IO Int64
 
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

selecttwocities :: String -> SqlQuery [Citynames]
selecttwocities spstring = sqlQuery "Select Name from cities where instr ((?),Name) >0 order by instr ((?),Name);" [spstring, spstring]

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

--простые функции для вычленения некоторых данных, нужных в функции roadget и graph
citynames :: (String) -> Citynames
citynames (cname) = Citynames {cname = cname }

getnames :: (Citynames) -> String
getnames (Citynames {cname=cname}) = cname

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


graph:: String -> String -> IO String
graph x y=return(x++" -> "++ y)

--Функция, которая нужна была для проекта. Она вычленяет названия городов начала и конца из строки ввода,
--подаваемой ей из модуля Server и вызывает с ними функцию path. Так как сама функция path из модуля graph
--не работает, вместо этого вызывается функция-заглушка, которая просто возвращает эти города. 
--Действует посредством sql-запроса к базе по поиску вхождения городов в строку, а затем выводит все эти
--города в порядке их наождения в строке (мое допущение в том, что сначала будет введен начальный город,
--а потом уже город-цель). Если их число не равно 3, выдает ошибку ввода. Иначе запускает path.
roadget :: String -> String -> IO String
roadget from body = do
  conn  <- connect connectInfo
  cities <- res body conn
  if ((length cities) == 2) then do
    gr <- graph (head (map getnames cities)) (head (tail(map getnames cities)))
    return ("Hello,"++from++". Here's your request:"++gr++".") else return ("Wrong Input."++ from++", correct it, please, and try again.")

run :: IO ()
run = do
  print "Enter your name:"
  name <- getLine
  print "Enter from/to:"
  line <- getLine
  value <- roadget name line
  print (value)
  return ()

