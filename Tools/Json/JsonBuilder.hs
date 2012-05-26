{------------------------------------------------------------------------------
    Tools.Json.JsonBuilder.hs
    
    Description: Json builder

    Contact: t: @onlyshk
             m: <"kuleshovmail@gmail.com">    
------------------------------------------------------------------------------}

module Tools.Json.JsonBuilder where

import Tools.Json.Data

--
-- json generator by Json data from Data.hs
--
buildJson :: Json [a] -> String -> String

buildJson (Json []) st = st

buildJson (Json p) st = 
	let 
        -- start parse Json Tree
		(pair : _) = p 
	-- match first pair
	in case pair of
		-- '{' parse
		Pair (_, JEmpty) ->  buildJson (getAllWithoutHead (Json p)) (st ++ "{ ")
		-- '}' parse
		Pair (_, JEndEmpty) -> buildJson (getAllWithoutHead (Json p)) (st ++ "}" ++ check_next_end p)
		-- "[" parse
		Pair (_, JStartArray) -> buildJson (getAllWithoutHead (Json p)) (st ++ "[ ")
		-- "]" parse
		Pair (_, JEndArray) -> buildJson (getAllWithoutHead (Json p)) (st ++ "]")
		-- key parse
		Pair (key, JString str) -> 
			-- make \"key\" : \"some_string\", expression
			let expr = "\"" ++ key ++ "\":\"" ++ str ++ "\"" ++ check_next p
				in buildJson (getAllWithoutHead (Json p)) (st ++ expr)
		-- parse int value
		Pair (key, JInt int) -> 
			-- make \"key\" : some_int, expression
			let expr = "\"" ++ key ++ "\":" ++ show int ++ check_next p
				in buildJson (getAllWithoutHead (Json p)) (st ++ expr)
		-- parse double value
		Pair (key, JDouble double) -> 
			-- make \"key\" : some_double' expression
			let expr = "\"" ++ key ++ "\":" ++ show double ++ check_next p
				in buildJson (getAllWithoutHead (Json p)) (st ++ expr)
		-- parse bool value
		Pair (key, JBool bool) ->
		    case bool of
	    		False ->
					let expr = "\"" ++ key ++ "\": false" ++ check_next p
					  in buildJson (getAllWithoutHead (Json p)) (st ++ expr)
		    	True ->
			        -- make \"key\" : true, expression
					let expr = "\"" ++ key ++ "\": true" ++ check_next p
						in buildJson (getAllWithoutHead (Json p)) (st ++ expr)
		-- parse null value
		Pair (key, JNull) -> 
			-- make \"key\" : true, expression
			let expr = "\"" ++ key ++ "\": null" ++ check_next p
				in buildJson (getAllWithoutHead (Json p)) (st ++ expr)
		-- start of array or object
		Pair (key, JEmptyVal) ->
			-- make \"key\" :  expression
			let expr = "\"" ++ key ++ "\": "
				in buildJson (getAllWithoutHead (Json p)) (st ++ expr)

check_next_end :: [Pair a] -> String
check_next_end p = if length p == 1
				   then ""
				   else let (_ : p2 : _) = p
				   		in case p2 of
				   			Pair (_, JEndEmpty) -> ""
				   			Pair (_, JEmpty) -> ""
				   			Pair (_, JEndArray) -> ""
				   			_ -> ","

check_next :: [Pair a] -> String
check_next p = 
	let (p0 : p1 : pairs) = p
		in case (p1) of
			Pair (_, JEndEmpty) -> ""
			_ -> ","