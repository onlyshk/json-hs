{------------------------------------------------------------------------------
    Tools.Json.Data.hs
    
    Description: General json data

    Contact: t: @onlyshk
             m: <"kuleshovmail@gmail.com">    
------------------------------------------------------------------------------}


module Tools.Json.Data where

-- Main json data
data Json a = Json [Pair a] deriving (Show)

-- All json data in Key | Value
--data Pair a = Pair [(Key, Value a)]
data Pair a = Pair (Key, Value) deriving (Show)

-- Json key
type Key = String

-- Json value
--data Value v = Value v

data Value = JString String               -- json string
			 | JInt Int                   -- json int
			 | JDouble Double             -- json double
			 | JBool Bool                 -- json bool
			 | JNull                      -- json null
			 | JStartArray                -- json start array - '[': {"", JStartArray}
			 | JEndArray                  -- json start array - '[': {"", JEndArray}
			 | JEmpty                     -- {
			 | JEndEmpty                  -- }
			 | JEmptyVal                  -- Empty val: {"some_key", JEmptyVal}
			 deriving (Show)

--
-- Get pairs list from json
-- TODO maybe make with record
getPairs :: Json a -> [Pair a]
getPairs (Json p) = 
	case p of 
		[] -> []
		_ -> p

--
-- add new pair to Json
add_new_pair :: Json a -> Pair a -> Json a
add_new_pair (Json p) newPair =
	case p of
		[] -> Json [newPair] 
		_ -> Json (concat [p ++ [newPair]])

--
-- Get last pair from json
getLastPair :: Json a -> Pair a
getLastPair (Json p) = last p

--
-- Get Json without last pair
getAllWithoutLast :: Json a -> Json a
getAllWithoutLast (Json p) = let removeLast = init p
								in Json removeLast

								--
-- Get Json without head pair
getAllWithoutHead :: Json a -> Json a
getAllWithoutHead (Json p) = let (phead : pairs) = p
								in Json pairs