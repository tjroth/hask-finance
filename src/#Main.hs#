{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Pipes
import Pipes.HTTP
import Data.Either (rights)
import qualified Pipes.ByteString as PB  -- from `pipes-bytestring`
import qualified Pipes.Prelude as PP
import Pipes.Csv (decode, HasHeader(..), FromRecord(..), (.:), (.!))
import Data.Time
import Data.Char (toUpper)
import Data.Either (rights)
import Data.List (tails)
import qualified Data.Vector as V
import qualified Data.Csv as CSV
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Types
import GHC.IO.Handle (hClose)

import Control.Foldl (Fold(Fold), purely)
import qualified Control.Foldl as L
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S

quoteDataFromProvider :: QuoteProvider  -- todo validate filepath ? directoryExists
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


quoteDataFromFile :: FilePath -> IO (V.Vector Quote)
quoteDataFromFile f = do
  withFile f ReadMode $ \hIn -> do
    bs <- BL.hGetContents hIn
    let qs = CSV.decode HasHeader bs :: Either String (V.Vector Quote)
    case qs of
     Right vs -> return vs 
     Left e -> return V.empty 

  
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
      

s :: Monad m => Pipe Quote [Quote] m ()
s = PP.scan (\acc v -> v:acc) [] id

quoteFilter :: Monad m => Pipe (Either String Quote) Quote m r
quoteFilter = go
  where go = do
          x <- await
          case x of
           Left e -> go
           Right v -> yield v >> go

mkTimeRange :: UTCTime -> Integer -> TimeRange
mkTimeRange et numDays = TimeRange st et
  where
    st = UTCTime (addDays (negate $ abs numDays) (utctDay et)) (utctDayTime et)
    negate i = (-1) * i


mkURL :: QuoteProvider -> Ticker -> QuoteType -> TimeRange -> ProviderURL
mkURL Yahoo (Ticker s) p tr = ProviderURL $ concat ["http://ichart.finance.yahoo.com/table.csv?s="
                                                   , map toUpper s
                                                   , yahooURLParams tr p
                                                   , "&ignore=.csv"
                                                   ]
  where
    yahooURLParams :: TimeRange -> QuoteType -> String
    yahooURLParams tr p = schars ++ echars ++ (pchar p)
      where
        schars = concat $ zipWith (++) ["&a=","&b=","&c="] $ clist $ toGregorian (utctDay $ startT tr)
        echars = concat $ zipWith (++) ["&d=","&e=","&f="] $ clist $ toGregorian (utctDay $ endT tr)
        clist (y,m,d) = [show (m-1), show d, show y]
        pchar Daily   = "&g=d"
        pchar Weekly  = "&g=w"
        pchar Monthly = "&g=m"





{--
withFileData :: Show b => String -> Pipe [Quote] b IO () -> IO [Either String Quote] --()
withFileData f study= do
  withFile f ReadMode $ \hIn -> do
    qp <- PP.toListM $ quoteProducer hIn
    return qp

    {--let dataProducer = each . reverse . tails . rights $ qp
    runEffect $ dataProducer >-> study >-> PP.print --}
    where
      quoteProducer hIn = (decode HasHeader (PB.fromHandle hIn) :: Producer (Either String Quote) IO ()) 
--}
