mysql-haskell-nem
=================

Provides a simpler interface for retrieving results when using the [mysql-haskell](http://hackage.haskell.org/package/mysql-haskell) package.

Guide
-----

The `Database.MySQL.Base` and `Database.MySQL.Nem` modules provides everything you need to start making queries:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Base
import Database.MySQL.Base.Nem
import Data.Text (unpack)
import qualified System.IO.Streams as Streams

main :: IO () 
main = do
    conn <- connect
        defaultConnectInfo {ciUser = "username", ciPassword = "password", ciDatabase = "dbname"}

	results <- queryResults conn "SELECT email, name FROM users" >>=
	_ <-
		Streams.mapM_
			(\(email, name) -> print $ (Text.unpack email) ++ ":" ++ (name :: String) ) results >>=
		Streams.toList
```

It's recommended to use prepared statement to improve query speed:

```haskell
    ...
    s <- prepareStmt conn "SELECT * FROM some_table where person_age > ?"
    ...
    results <- queryStmtResutls s [MySQLInt32U 18]
    ...
```
