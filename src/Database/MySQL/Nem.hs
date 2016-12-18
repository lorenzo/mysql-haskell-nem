module Database.MySQL.Nem
  ( module Database.MySQL.Nem.QueryResults
  , module Database.MySQL.Nem.Result
   -- * Extracting results
   -- $result
  , queryResults
  , queryStmtResults
  ) where

import Database.MySQL.Base
import Database.MySQL.Nem.QueryResults
import Database.MySQL.Nem.Result
import qualified System.IO.Streams as Streams

-- | Execute a MySQL query which return a result-set converted to the specified
-- haskell type
--
-- Note that you must fully consumed the result-set before start a new query on
-- the same 'MySQLConn', or an 'UnconsumedResultSet' will be thrown.
-- if you want to skip the result-set, use 'Stream.skipToEof'.
--
queryResults
  :: (QueryResults r)
  => MySQLConn -> Query -> IO (Streams.InputStream r)
queryResults conn qry = do
  (defs, results) <- query_ conn qry
  Streams.map (convertResults defs) results

-- | Execute prepared query statement with parameters, expecting resultset.
--
--
-- Note that you must fully consumed the result-set before start a new query on
-- the same 'MySQLConn', or an 'UnconsumedResultSet' will be thrown.
-- if you want to skip the result-set, use 'Stream.skipToEof'.
--
queryStmtResults
  :: (QueryResults r)
  => MySQLConn -> StmtID -> [MySQLValue] -> IO (Streams.InputStream r)
queryStmtResults conn qry params = do
  (defs, results) <- queryStmt conn qry params
  Streams.map (convertResults defs) results
--
-- $result
--
-- The 'queryResults' and 'queryStmtResults' functions return a list of values in the
-- 'QueryResults' typeclass. This class performs automatic extraction
-- and type conversion of rows from a query result.
--
-- Here is a simple example of how to extract results:
--
-- > import qualified Data.Text as Text
-- > import Database.MySQL.Nem
-- > import qualified System.IO.Streams as Streams
-- >
-- > xs <- queryResults conn "select name,age from users"
-- > Streams.mapM_
-- >    \(name,age) ->
-- >       putStrLn $ Text.unpack name ++ " is " ++ show (age :: Int)
-- >    xs
--
-- Notice two important details about this code:
--
-- * The number of columns we ask for in the query template must
--   exactly match the number of elements we specify in a row of the
--   result tuple.  If they do not match, a 'ResultError' exception
--   will be thrown.
--
-- * Sometimes, the compiler needs our help in specifying types. It
--   can infer that @name@ must be a 'Text', due to our use of the
--   @unpack@ function. However, we have to tell it the type of @age@,
--   as it has no other information to determine the exact type.
--
--   For converting to your custom data types. Check the documentation for
--   the QueryResults package.
