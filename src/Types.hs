{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Time (UTCTime(..))
import Data.Time.Format (readTime)
import Data.ByteString.Char8 (unpack)
import System.Locale
import Pipes.Csv (FromField(..), FromRecord(..), (.:), (.!))
import Data.Csv (Parser)
import qualified Data.Vector as V
import Data.Vector (length)

newtype Ticker = Ticker String
               deriving (Show, Read, Eq)

newtype ProviderURL = ProviderURL String
                    deriving (Show, Read, Eq)

data TimeRange = TimeRange { startT :: UTCTime
                           , endT   :: UTCTime }
               deriving (Show, Eq, Ord)

data QuoteType = Daily | Weekly | Monthly

data QuoteProvider = Yahoo | Google

data Quote = Quote { date :: !UTCTime
                   , openPrice :: !Double
                   , highPrice :: !Double
                   , lowPrice :: !Double
                   , closePrice :: !Double
                   , volume :: !Integer
                   , adjClose :: !Double } deriving (Show, Eq, Ord, Read)

instance FromField UTCTime where
  parseField s = pure $ readTime defaultTimeLocale "%Y-%m-%d" $ unpack s :: Parser UTCTime

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
