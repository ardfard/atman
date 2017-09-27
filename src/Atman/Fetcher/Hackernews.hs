
module Atman.Fetcher.Hackernews where

import Atman.Prelude
import Atman.Model
import GHC.Base (String)
import Text.XML.HXT.Core 
import Text.XML.HXT.Curl
import Text.HandsomeSoup
import Network.HTTP.Simple 
import Crypto.Hash (hash, Digest, MD5)
import Text.Parsec (many1, digit, spaces, string, parse)


hackernewsUrl :: String
hackernewsUrl = "https://news.ycombinator.com"

getHNContents :: IO [Item]
getHNContents = do
  scores <- runX $ table >>> getScores
  lt <- runX $ table >>> getLinksTexts
  let contents = zip lt scores
      sortedContents = sortBy (\c1 c2 -> compare (snd c1) (snd c2)) contents
  return $ reverse . fmap (\((link, desc),_) -> Item (toS link) (toS desc) (show . getDigest $ link)) $ sortedContents
  where
    doc = readDocument [ withValidate no
                       , withParseHTML yes
                       , withCurl []
                       ] hackernewsUrl
    table = doc >>> css "table.itemlist"
    getLinksTexts =  css "a.storylink" >>> (getAttrValue "href"  &&& deep getText)
    getScores = css "td.subtext" >>> css "span:first-child" //> getText >>> arr (fromMaybe 0 . parseScore)
    getDigest :: String -> Digest MD5
    getDigest l =  hash (toS l :: ByteString)

parseScore :: String -> Maybe Int
parseScore s = readMaybe =<< rightToMaybe (parse parser "" s)
  where 
    parser = many1 digit <* spaces <* string "point"
