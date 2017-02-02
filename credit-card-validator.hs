-- Amex bin list: Card numbers start with a 37. 
-- Visa bin list: Card numbers start with a 4.
-- MasterCard bin list: Card numbers start with the numbers 51 through 55.
-- Diners Club bin list: Card numbers begin with 300 through 305, 36 or 38. 
-- Discover bin list: Card numbers begin with 6011 or 65.
-- JCB bin list: Card numbers begin with 35.
import Data.Char
import Data.List

data CreditCardIssuer =
      Visa 
    | Mastercard 
    | Discover 
    | Amex 
    | Diners 
    | JCB deriving (Show)

data CreditCardError =
      UnknownIssuer 
    | InvalidNumber
    | Expired deriving (Show)

data MajorIndustryIdentifier = 
      Airlines 
    | Travel
    | Banking 
    | Petroleum 
    | Telecom 
    | NationalAssigment deriving (Show)

doubleEveryOther (index,value) 
    | even index = sumDigits (2 * value)
    | otherwise  = value

sumDigits 0 = 0
sumDigits x
    | modulo == 0 = 9
    | otherwise   = modulo
    where  modulo = x `mod` 9

chunkIndexed = reverse . (map doubleEveryOther) . (zip [1..]) . reverse
checksum = sum . chunkIndexed
luhn xs = (checksum xs) `mod` 10 == 0


type CreditCardNumber = String
type Prefix = String
type PrefixDigit = Char
-- Concat prefix with each prefix digit
(+|+) :: Prefix -> [PrefixDigit] -> [Prefix]
(+|+) prefix digits = map ((prefix ++) . (:[])) digits
--(+|+) prefix digits = map (\d ->  prefix ++ [d]) digits

hasAnyPrefixIn :: CreditCardNumber -> [Prefix] -> Bool
hasAnyPrefixIn cc prefixes = any (`isPrefixOf` cc) prefixes

cardIssuer :: CreditCardNumber -> Maybe CreditCardIssuer
cardIssuer cc
    | cc `hasAnyPrefixIn` ["4"]                = Just Visa
    | cc `hasAnyPrefixIn` ("5" +|+ ['1'..'5']) = Just Mastercard
    | cc `hasAnyPrefixIn` ("3" +|+ ['4', '7']) = Just Amex
    | cc `hasAnyPrefixIn` ("3" +|+ ['6', '8']) = Just Diners
    | cc `hasAnyPrefixIn` ["6011", "65"]       = Just Discover
    | otherwise                                = Nothing

--cardIssuer' :: String -> Maybe CreditCardIssuer
--cardIssuer' = cardIssuer . (map digitToInt)

--validCreditCard :: [a] -> Either CreditCardError (CreditCardIssuer,MajorIndustryIdentifier)
--validCreditCard xs
--    | luhn xs = 
--    | otherwise = Left InvalidNumber