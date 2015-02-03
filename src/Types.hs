module Types where

import Data.Time (UTCTime(..))



newtype Ticker = Ticker String
               deriving (Show, Read, Eq)

newtype ProviderURL = ProviderURL String
                    deriving (Show, Read, Eq)

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
