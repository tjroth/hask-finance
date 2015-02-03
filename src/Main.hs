{-# LANGUAGE OverloadedStrings #-}

module Main where


import Pipes
import Pipes.HTTP
import Data.Either (rights)
import qualified Pipes.ByteString as PB  -- from `pipes-bytestring`
import qualified Pipes.Prelude as PP
import Pipes.Csv (decode, HasHeader(..), FromRecord(..), (.:), (.!))
import Data.Time
import Data.Char (toUpper)
import Types


withYahooData :: Show b => Ticker
                 -- ^ The ticker symbol (eg Ticker "AAPL")
                 -> Integer
                 -- ^ How many days of data
                 -> QuoteType
                 -- ^ Daily, Weekly or Monthly
                 -> Pipe Quote b IO ()
                 -- ^ Study to run on the data
                 -> IO ()
withYahooData t nd p study = do
  ct <- getCurrentTime
  let tr = mkTimeRange ct nd
  let (ProviderURL url) = mkURL Yahoo t p tr
  req <- parseUrl url 
  withManager tlsManagerSettings $ \m ->
    withHTTP req m $ \resp ->
      runEffect $ quoteProducer resp >-> quoteFilter >-> study >-> PP.print
      where
        quoteProducer resp = decode HasHeader (responseBody resp) :: Producer (Either String Quote) IO ()

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


-------------------------------------------------------
-- Convenience Pipes for Quote elements
        
dates :: Monad m => Pipe Quote UTCTime m r
dates = PP.map date

opens :: Monad m => Pipe Quote Double m r
opens = PP.map openPrice

highs :: Monad m => Pipe Quote Double m r
highs = PP.map highPrice
        
lows :: Monad m => Pipe Quote Double m r
lows = PP.map lowPrice
        
closes :: Monad m => Pipe Quote Double m r
closes = PP.map closePrice        

volumes :: Monad m => Pipe Quote Integer m r
volumes = PP.map volume


--mkQuote d o h l c v ac = Quote d o h l c v ac

