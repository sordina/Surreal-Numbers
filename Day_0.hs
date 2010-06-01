{- In the beginning, everything was void, and J. H. W. H.
 - Conway began to create numbers. Conway said, "Let
 - there be two rules which bring forth all numbers large
 - and small. This shall be the first rule: Every number
 - corresponds to two sets of previously created numbers,
 - such that no member of the left set is greater than or
 - equal to any member of the right set. And the second rule
 - shall be this: One number is less than or equal to another
 - number if and only if no member of the first number's left
 - set is greater than or equal to the second number, and no
 - member of the second number's right set is less than or
 - equal to the first number." And Conway examined these
 - two rules he had made, and behold! They were very good.
 - And the first number was created from the void left set
 - and the void right set. Conway called this number "zero,"
 - and said that it shall be a sign to seperate positive num-
 - bers from negative numbers. Conway proved that zero was
 - less than or equal to zero, and he saw that it was good.
 - And the evening and the morning were the day of zero.
 -}

module Day_0 where

-- Every number corresponds to two sets of previously created numbers.
data Number = N ([Number],[Number]) deriving (Show)

-- No member of the left set is greater than or equal to any member of the right set.
valid :: Number -> Bool
valid (N (l, r)) = none (\x -> any (x `geq`) r) l

-- The first number was created from the void left set
-- and the void right set. Conway called this number "zero".
zero :: Number
zero = N ([],[])

-- Conway proved that zero was less than or equal to zero.
prop_zero_leq_zero = zero `leq` zero

geq :: Number -> Number -> Bool
geq = undefined

leq :: Number -> Number -> Bool
l@(N (ls,_)) `leq` r@(N (_,rs)) = none (`geq` r) ls  && none (`leq` l) rs

gt :: Number -> Number -> Bool
gt a = not . leq a

-- Helper functions

none :: (a -> Bool) -> [a] -> Bool
none f = not . any f
