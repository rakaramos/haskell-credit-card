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

-- Type Synonyms
type CreditCardNumber = String
type CreditCardPrefix = String
type CreditCardDigit = Char


doubleEveryOther :: (Integral a, Integral b) => (a,b) -> b
doubleEveryOther (index,value) 
    | even index = sumDigits (2 * value)
    | otherwise  = value

sumDigits :: Integral a => a -> a
sumDigits 0 = 0
sumDigits x
    | modulo == 0 = 9
    | otherwise   = modulo
    where  modulo = x `mod` 9

getChecksum :: Integral a => [a] -> a
getChecksum = sum . reverse . (map doubleEveryOther) . (zip [1..]) . reverse

isValidSum :: Integral a => a -> Bool
isValidSum sum = sum `mod` 10 == 0

validateCreditCardNumber :: CreditCardNumber -> Bool
validateCreditCardNumber = isValidSum . getChecksum . (map digitToInt)


-- Concat prefix with each prefix digit
(+|+) :: CreditCardPrefix -> [CreditCardDigit] -> [CreditCardPrefix]
(+|+) prefix digits = map ((prefix ++) . (:[])) digits
--(+|+) prefix digits = map (\d ->  prefix ++ [d]) digits

hasAnyPrefixIn :: CreditCardNumber -> [CreditCardPrefix] -> Bool
hasAnyPrefixIn cc prefixes = any (`isPrefixOf` cc) prefixes

getCardIssuer :: CreditCardNumber -> Maybe CreditCardIssuer
getCardIssuer cc
    | cc `hasAnyPrefixIn` ["4"]                = Just Visa
    | cc `hasAnyPrefixIn` ("5" +|+ ['1'..'5']) = Just Mastercard
    | cc `hasAnyPrefixIn` ("3" +|+ ['4', '7']) = Just Amex
    | cc `hasAnyPrefixIn` ("3" +|+ ['6', '8']) = Just Diners
    | cc `hasAnyPrefixIn` ["6011", "65"]       = Just Discover
    | otherwise                                = Nothing

--validCreditCard :: [a] -> Either CreditCardError (CreditCardIssuer,MajorIndustryIdentifier)
--validCreditCard xs
--    | luhn xs = 
--    | otherwise = Left InvalidNumber