-- Amex bin list: Card numbers start with a 37. 
-- Visa bin list: Card numbers start with a 4.
-- MasterCard bin list: Card numbers start with the numbers 51 through 55.
-- Diners Club bin list: Card numbers begin with 300 through 305, 36 or 38. 
-- Discover bin list: Card numbers begin with 6011 or 65.
-- JCB bin list: Card numbers begin with 35.

data CreditCardIssuer = Visa | Mastercard | Discover | Amex | Diners | JCB deriving (Show)
data CreditCardError = UnknownIssuer | InvalidNumber | Expired deriving (Show)
data MajorIndustryIdentifier = Airlines | Travel | Banking | Petroleum | Telecom | NationalAssigment deriving (Show)

lastButOne xs = xs!!((length xs) - 2)

slice xs = if (length xs) == 0 then xs else slice (head (tail xs)):xs