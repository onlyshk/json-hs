{------------------------------------------------------------------------------
    Tools.Json.Parser.hs
    
    Description: Json parser

    Contact: t: @onlyshk
             m: <"kuleshovmail@gmail.com">    
------------------------------------------------------------------------------}

module Tools.Json.Parser where

import Data.Int
import Data.Char

import Tools.IO
import Tools.Json.Data

--
-- Simple json test:
-- Json [Pair ("Test", JString "ASD"), Pair ("Test", JInt 3)]
-- { 
--   "Test" : "Asd",
--   "Test" : 3
-- }
-- 

--
-- Simple parsing:
-- Read all json data by symbol and match it
-- @String - json data
--
genParse :: String -> Json [Pair a]
-- TODO make more shorter
genParse (json_symb : json_rest) = startParsing ( filter (\x -> x /= '\n') (json_symb : json_rest)) (Json [])

--
-- Parse json
-- not effective for not big json data
--
startParsing :: String -> Json [Pair a] -> Json [Pair a]

-- stop parsing
startParsing [] st = st

-- general json elements parsing
startParsing (json : json_rest) st = 
	case json of
		--
		-- if we met '{' symbol start new json leaf:
	    -- Pair ("", JEmpty) - it will be empty leaf
        --
		'{' -> startParsing json_rest (add_new_pair st (Pair ("", JEmpty)))

        --
		-- if we met '}' symbol start new json leaf:
	    -- Pair ("", JEndEmpty) - it will be empty leaf
        --
		'}' -> startParsing json_rest (add_new_pair st (Pair ("", JEndEmpty)))

		-- Start array
		'[' -> startParsing json_rest (add_new_pair st (Pair ("", JStartArray)))

		-- End array
		']' -> startParsing json_rest (add_new_pair st (Pair ("", JEndArray)))

        --
        -- Here match " symbol and data after it
        --
		'\"' -> case head json_rest of
			        -- Some punctuation
					':' -> let  
								-- remove all space to first symbol
								next_object = head $ concat $ words json_rest
					 		in case next_object of
					 			-- new value String
					 			'\"' -> let (newJson, restVal) = updateJson 0 (json : json_rest) st
					 						in startParsing restVal newJson
					 			-- bool true parsing
					 			't' -> 	let (newJson, restVal) = updateJson 2 (json:json_rest) st
					  						in startParsing restVal newJson
					 			-- if space go next
					 			' ' -> startParsing json_rest st
					-- Some punctuation
					',' -> startParsing json_rest st
					-- Here maybe Key or Value
					_ ->
					    let (newJson, restVal) = updateJson 0 (json : json_rest) st
					 		in startParsing restVal newJson
 		-- if space go next
		' ' -> startParsing json_rest st
		-- if ',' go next
		',' -> startParsing json_rest st
		-- bool true parsing
		't' -> 	let (newJson, restVal) = updateJson 2 (json:json_rest) st
				  in startParsing restVal newJson
		-- bool false parsing
		'f' -> 	let (newJson, restVal) = updateJson 3 (json:json_rest) st
				  in startParsing restVal newJson
		-- null parsing
		'n' -> let (newJson, restVal) = updateJson 4 (json:json_rest) st 
				in startParsing restVal newJson	
		-- if \t go next
		'\t' -> startParsing json_rest st
		-- if \r go next
		'\r' -> startParsing json_rest st
		_ -> 
		    --
		    -- Here we parse digits values
		    --
			case isDigit json of
				-- if 'json' is digit int or double
				True ->
				    -- make new pair and parse next
					let
			    		(newJson, restVal) = updateJson 1 (json:json_rest) st
					  in startParsing restVal newJson
				-- other symbols parse next
				False -> startParsing json_rest st
--
-- We get last of Json [Pair (_, _), ...] and check it:
--
-- Json [] -
-- Json [Pair (key, JEmptyValue)] - insert new value
-- Json [Pair (key, val), (key, val)] - create new pair 
--
make_new_key :: Key -> Json a -> Pair a
make_new_key new_val (Json []) = Pair (new_val, JEmptyVal)

make_new_key new_val (Json pairs) = 
	case last pairs of
		-- Add new val to key
		Pair (_, JEmptyVal) -> let Pair (key, _) = last pairs
		                       	in Pair (key, JEmptyVal)
		-- Create new pair
		_ -> Pair (new_val, JEmptyVal)

--
-- Create new json value and return new Pair
--
make_new_val :: Value -> Json a -> Pair a
make_new_val new_val (Json pairs) =
	let Pair (key, _) = last pairs
		in Pair (key, new_val)

--
-- Update All json tree
-- Create new pair and add to json storage
-- @flag - flag type:
--    0 - JString
--    1 - JInt
--    2 - JDouble
-- @json - string json data
-- @json_rest - string json data
-- @st - Json storage :: Json [Pair a]
--
updateJson :: Int8 -> String -> Json [Pair a] -> (Json [Pair a], String)
updateJson flag (json : json_rest) st =
	case flag of
		-- string
		0 ->
			let 
				-- take all symbols up to "\"" or maybe , or } symbol
				getVal = takeWhile (\c -> not $ isPunctuation c) json_rest
        		-- get last pair of json
				lastPair = getLastPair(st)
				-- make new pair
				(newPair, updated_json) = case lastPair of                  
								  			-- pair wthi key, but without value
								  			Pair (_, JEmptyVal) ->
			            		            			         updateJsonHelper flag getVal st
						  		  			_ -> 
							 					(make_new_key getVal st, st)
				-- get rest json without end "\""
				restVal = tail $ dropWhile (\c -> not $ isPunctuation c) json_rest
				-- update json tree
				newJson = add_new_pair updated_json newPair
				-- return newJson and rest of json string
	  		  in (newJson, restVal)
	   -- Number parsing
		1 ->
			updateJsonHelper2 flag (json : json_rest) st
	  	-- True
	  	2 ->
			updateJsonHelper2 flag (json : json_rest) st
	  	-- False
	  	3 ->
	  		updateJsonHelper2 flag (json : json_rest) st
	  	-- null parsing
	  	4 ->
	  		updateJsonHelper2 flag (json : json_rest) st

--
-- Helpers
--

-- Check is s integer return True else False
isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

-- Check is s double return True else False
isDouble :: String -> Bool 
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

-- make json helper
-- make (new pair, Json without last pair)  
updateJsonHelper :: Int8 -> String -> Json a -> (Pair a, Json a)
updateJsonHelper flag new_val st =
	case flag of
		0 -> (make_new_val (JString new_val) st, getAllWithoutLast st)
		1 ->
			case isInteger new_val of
				True -> (make_new_val (JInt $ read new_val) st, getAllWithoutLast st)
				False -> (make_new_val (JDouble $ read new_val) st, getAllWithoutLast st)
		2 -> (make_new_val (JBool $ read "True") st, getAllWithoutLast st)
		3 -> (make_new_val (JBool $ read "False") st, getAllWithoutLast st)
		4 -> (make_new_val (JNull) st, getAllWithoutLast st)

-- it's herlper for num parsing (int and double, null, and boolean values)
updateJsonHelper2 :: Int8 -> String -> Json a -> (Json a, String)
updateJsonHelper2 flag (json : json_rest) st =
	let 
	    -- take all symbols up to } or ,
		getVal = takeWhile (\c -> not ((c == ',')  || (c == '}'))) (json : json_rest)
    	-- get last pair of json
		lastPair = getLastPair(st)
		-- make new pair
		(newPair, updated_json) = updateJsonHelper flag getVal st
		-- get rest json data after , or }
		restVal = dropWhile (\c -> not ((c == ',')  || (c == '}'))) (json : json_rest)
		-- update json tree
		newJson = add_new_pair updated_json newPair
		-- return newJson and rest of json string
	  in (newJson, restVal)

