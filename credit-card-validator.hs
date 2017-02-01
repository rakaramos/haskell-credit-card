-- Amex bin list: Card numbers start with a 37. 
-- Visa bin list: Card numbers start with a 4.
-- MasterCard bin list: Card numbers start with the numbers 51 through 55.
-- Diners Club bin list: Card numbers begin with 300 through 305, 36 or 38. 
-- Discover bin list: Card numbers begin with 6011 or 65.
-- JCB bin list: Card numbers begin with 35.
import Data.Char

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
    | otherwhise  = modulo
    where  modulo = x `mod` 9

chunkIndexed = reverse . (map doubleEveryOther) . (zip [1..]) . reverse
checksum = sum . chunkIndexed
luhn xs = (checksum xs) `mod` 10 == 0

cardIssuer :: Integral a => [a] -> Maybe CreditCardIssuer
cardIssuer []    = Nothing
cardIssuer (4:_) = Just Visa
cardIssuer (5:x:_) | x `elem` [1..5] = Just Mastercard
cardIssuer (3:x:_) | x `elem` [4,7]  = Just Amex
cardIssuer (3:x:_) | x `elem` [6,8]  = Just Diners
cardIssuer (6:0:1:1:_) = Just Discover
cardIssuer (6:5:_) = Just Discover
cardIssuer xs = Nothing

cardIssuer' = cardIssuer . (map digitToInt)

--validCreditCard :: [a] -> Either CreditCardError (CreditCardIssuer,MajorIndustryIdentifier)
--validCreditCard xs
--    | luhn xs = 
--    | otherwise = Left InvalidNumber