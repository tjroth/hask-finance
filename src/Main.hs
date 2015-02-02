{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB  -- from `pipes-bytestring`
--import Data.Csv
import Data.Time
import Data.Char (toUpper)
--import Types

yahooURL = "http://real-chart.finance.yahoo.com/table.csv?s=FB&d=1&e=2&f=2015&g=d&a=4&b=18&c=2012&ignore=.csv"
{--
withYahooData (ProviderURL url) = do
    req <- parseUrl url 
    withManager tlsManagerSettings $ \m ->
        withHTTP req m $ \resp ->
            runEffect $ responseBody resp >-> PB.stdout
--}

withYahooData :: Ticker -> Integer -> QuoteType -> IO ()
withYahooData t nd p = do
  ct <- getCurrentTime
  let tr = mkTimeRange ct nd
  let (ProviderURL url) = mkURL Yahoo t p tr
  req <- parseUrl url 
  withManager tlsManagerSettings $ \m ->
    withHTTP req m $ \resp ->
      runEffect $ responseBody resp >-> PB.stdout




newtype Ticker = Ticker String deriving (Show, Read, Eq)

newtype ProviderURL = ProviderURL String deriving (Show, Read, Eq)



data TimeRange = TimeRange { startT :: UTCTime
                           , endT   :: UTCTime }
                           deriving (Show, Eq, Ord)

data QuoteType = Daily | Weekly | Monthly

data QuoteProvider = Yahoo | Google


data Quote = Quote { date :: !String
                   , openPrice :: !Double
                   , highPrice :: !Double
                   , lowPrice :: !Double
                   , closePrice :: !Double
                   , volume :: !Integer
                   , adjClose :: !Double } deriving (Show, Eq, Ord, Read)



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
instance FromRecord Quote where
     parseRecord v
         | V.length v == 7 = Quote <$>
                           v .! 0 <*>
                           v .! 1 <*>
                           v .! 2 <*>
                           v .! 3 <*>
                           v .! 4 <*>
                           v .! 5 <*>
                           v .! 6
         | otherwise     = mzero

mkQuote d o h l c v ac = Quote d o h l c v ac


-- Convenience Functions
-- ---------------------

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

highestHigh :: V.Vector Quote -> Double
highestHigh vs = V.foldl1 max $ highs vs

lowestLow :: V.Vector Quote -> Double
lowestLow vs = V.foldl1 min $ lows vs
--}
