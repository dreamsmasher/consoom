{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Req (

    ) where

import Network.HTTP.Conduit
import Data.Aeson
import Data.Time.ISO8601
import Data.Text (Text)
import qualified Data.Text as T
import Data.List

data XAmzTarget = XAT {
                    prodAPINameSpace :: String,
                    apiVersion :: String,
                    operation :: String
                      }

data Auth = Auth {
                algorithm :: String,
                credential :: String,
                headers :: [String],
                signature :: String
                 } deriving (Show)

newtype Ct = Ct Int

mkBounds :: Int -> Int -> (Int -> b) -> (Int -> Maybe b)
mkBounds l r f = \n -> if (l <= n && n <= r) then Just $ f n else Nothing

mkCt :: Int -> Maybe Ct
mkCt = mkBounds 1 10 Ct

newtype Rating = Rating Int
mkRating :: Int -> Maybe Rating
mkRating = mkBounds 0 5 Rating

newtype Percent = Percent Int
mkPercent :: Int -> Maybe Percent
mkPercent = mkBounds 0 100 Percent


data Avail = Available | IncludeOutOfStock deriving (Eq, Ord, Enum, Show)
data Condition = Any | New | Used | Collectible | Refurbished deriving (Eq, Ord, Enum, Show)
data DeliveryFlags = AmazonGlobal | FreeShipping | FulfilledByAmazon | Prime deriving (Eq, Ord, Enum, Show)
data Merchant = All | Amazon deriving (Eq, Ord, Enum, Show)

data SortBy = AvgCustomerReviews | Featured | NewestArrivals | Price PriceSort | Relevant deriving (Eq, Ord)
instance Show SortBy where
    show = \case
        AvgCustomerReviews -> "AvgCustomerReviews"
        Featured -> "Featured"
        NewestArrivals -> "NewestArrivals"
        Price p -> "Price:" <> show p
        Relevant  -> "Relevant"

data PriceSort = HighToLow | LowToHigh  deriving (Eq, Ord, Enum, Show)


data SearchParam = Actor String 
                 | Artist String 
                 | Author String
                 | Avail Avail
                 | Brand String
                 | BrowseNodeId String
                 | Condition Condition
                 | CurrencyOfPerference String -- CurrencyOfPreference
                 | DeliveryFlags [DeliveryFlags]
                 | ItemCount Ct
                 | ItemPage Ct
                 | Keywords [String]
                 | LanguageOfPreference [String]
                 | MarketPlace String
                 | MaxPrice Integer
                 | Merchant Merchant
                 | MinPrice Integer
                 | MinReviewsRating Rating
                 | MinSavingPercent Percent
                 | OfferCount Int -- has to be 1
                 | PartnerTag Int
                 | PartnerType String
                 | Properties [(String, String)]
                 | Resources [String]
                 | SearchIndex String
                 | SortBy SortBy
                 | Title String

                  

instance Show XAmzTarget where
    show (XAT p v o) = intercalate (".") [p, v, "ProductAdvertisingAPI", v, o]

data ProdOperation = GetBrowseNode | GetItems | GetVariations | SearchItems deriving (Eq, Ord, Enum)

data Payload = Pay {
                host :: Text,
                xAmzDate :: String, --ISO8601 time
                xAmzTarget :: XAmzTarget
                   }

searchItem :: xAmzTarget
searchItem = undefined

