{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import Control.Monad (forM)
import Control.Exception (IOException, try)
import Pipes
import Pipes.HTTP
import Data.Either (rights)
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PB
import Pipes.Csv (decode, HasHeader(..)) 
import Data.Time
import Data.Char (toUpper)
import qualified Data.Vector as V
import qualified Data.Csv as CSV
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Types


-----------------------------------------------------------------------------------
-- Retrieves historical price data from server; currently only supports Yahoo data

quoteDataFromProvider :: QuoteProvider 
                      -> Ticker
                      -- ^ The ticker symbol (eg Ticker "AAPL")
                      -> Integer
                      -- ^ How many days of data
                      -> QuoteType
                      -- ^ Daily, Weekly or Monthly
                      -> IO (V.Vector Quote)
quoteDataFromProvider qp t nd p = do
  ct <- getCurrentTime
  let tr = mkTimeRange ct nd
  let (ProviderURL url) = mkURL qp t p tr
  req <- parseUrl url 
  withManager tlsManagerSettings $ \m ->
    withHTTP req m $ \resp -> do
      vs <- PP.toListM $ quoteProducer resp
      return $ V.fromList $ rights $ vs
      where
        quoteProducer resp = decode HasHeader (responseBody resp) -- :: Producer (Either String Quote) IO ()

-----------------------------------------------------------------------------------
-- Loads historical price data from file; all data is loaded

quoteDataFromFile :: FilePath -> IO (V.Vector Quote) 
quoteDataFromFile f = do
  e <- try (withFile f ReadMode $ \hIn -> do
               qp <- PP.toListM $ quoteProducer hIn
               return $ V.fromList . rights $ qp)
  case e of
   Left (e :: IOException) -> return V.empty
   Right v -> return v --case v of
  where
    quoteProducer hIn = (decode HasHeader (PB.fromHandle hIn) :: Producer (Either String Quote) IO ())
    
-----------------------------------------------------------------------------------
-- Download historical price data and save to file; currently only supports Yahoo data
     
downloadDataFromServer :: QuoteProvider  -- todo validate filepath ? directoryExists
                       -> Ticker 
                       -> QuoteType 
                       -> Integer 
                       -> FilePath
                       -> IO ()
downloadDataFromServer qp t@(Ticker s) p nds dir = do
  ct <- getCurrentTime
  let tr = mkTimeRange ct nds
  let (ProviderURL url) = mkURL qp t p tr 
  req <- parseUrl $ url
  withManager defaultManagerSettings $ \m ->
    withHTTP req m $ \resp ->
      withFile (dir ++ (map toUpper s) ++ ".csv") WriteMode $ \hOut -> do
      runEffect $ for (responseBody resp) (liftIO . BS.hPutStr hOut)

mkTimeRange :: UTCTime -> Integer -> TimeRange
mkTimeRange et numDays = TimeRange st et
  where
    st = UTCTime (addDays (negate $ abs numDays) (utctDay et)) (utctDayTime et)


mkURL :: QuoteProvider -> Ticker -> QuoteType -> TimeRange -> ProviderURL
mkURL Yahoo (Ticker s) qType tRange = ProviderURL $ concat ["http://ichart.finance.yahoo.com/table.csv?s="
                                                   , map toUpper s
                                                   , yahooURLParams tRange qType
                                                   , "&ignore=.csv"
                                                   ]
  where
    yahooURLParams :: TimeRange -> QuoteType -> String
    yahooURLParams tr p = schars tr ++ echars tr ++ (pchar p)
    schars tr = concat $ zipWith (++) ["&a=","&b=","&c="] $ clist $ toGregorian (utctDay $ startT tr)
    echars tr = concat $ zipWith (++) ["&d=","&e=","&f="] $ clist $ toGregorian (utctDay $ endT tr)
    clist (y,m,d) = [show (m-1), show d, show y]
    pchar Daily   = "&g=d"
    pchar Weekly  = "&g=w"
    pchar Monthly = "&g=m"
mkURL Google _ _ _ = undefined


-----------------------------------------------
--Convenience functions
        
opens :: V.Vector Quote -> V.Vector Double
opens = V.map openPrice
        
highs :: V.Vector Quote -> V.Vector Double
highs = V.map highPrice
        
lows :: V.Vector Quote -> V.Vector Double
lows = V.map lowPrice
        
closes :: V.Vector Quote -> V.Vector Double
closes = V.map closePrice
        
volumes :: V.Vector Quote -> V.Vector Integer
volumes = V.map volume
        
dates :: V.Vector Quote -> V.Vector UTCTime
dates = V.map date


-------------------------------------------------
-- Filter Analysis

filterAnalysis :: FilePath -> [(V.Vector Quote -> Bool)] -> IO Bool
filterAnalysis fp fns = do
  dataSet <- quoteDataFromFile fp
  return $ and $ map (\fn -> fn dataSet) fns





{--
quoteFilter :: Monad m => Pipe (Either String Quote) Quote m r
quoteFilter = go
  where go = do
          x <- await
          case x of
           Left _ -> go
Right v -> yield v >> go
        
quoteDataFromFile :: FilePath -> IO (V.Vector Quote)
quoteDataFromFile f = do
  e <- try (withFile f ReadMode $ \hIn -> do
               hSetBinaryMode hIn True
               bs <- BL.hGetContents hIn
               let qs = CSV.decode HasHeader bs :: Either String (V.Vector Quote)
               return qs)
  case e of
   Left (e :: IOException) -> return V.empty
   Right v -> case v of
     Right vs -> return vs
     Left _ -> return V.empty


--}
