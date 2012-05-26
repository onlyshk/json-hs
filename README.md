### json-hs - json tools library

Json tools:

  * Json parser
  * Json builder

Json builder:

```haskell
test = Json [Pair ("", JEmpty),
				Pair ("key1", JString "key2"),
				Pair ("key4", JString "key3"),
			Pair ("", JEndEmpty)
			]

--
-- output:
--
-- { 
--   "key1" : "key2",
--   "key4" : "key3"
-- }
--
```

Build:

```
cabal configure && cabal build
```

Dependencies:

  * base
  * directory

Bugs:

  https://github.com/onlyshk/json-hs/issues

About:

If you have a question file an issue or find me on the twitter @onlyshk.