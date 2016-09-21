{-#LANGUAGE GADTs, InstanceSigs, KindSignatures#-}

type Pizza = ([Topping], Size, ThinCrust)

type Size = Int

type Topping = String

type ThinCrust = Bool

toppingCal :: Topping -> Int
toppingCal "Pinapple" = 5
toppingCal "Bacon" = 20
toppingCal "Olives" = 10
toppingCal "Mushrooms" = 15

toppingsCal :: [Topping] -> Int
toppingsCal [] = 0
toppingsCal (a:toppings) = (toppingCal a) + (toppingsCal toppings)

crustCal :: ThinCrust -> Int
crustCal True = 10
crustCal False = 20

pizzaArea :: Size -> Int
pizzaArea a = a * a * 3

calories :: Pizza -> Int
calories (a, b, c) = ( (toppingsCal a) + (crustCal c) ) * (pizzaArea b)

sizePrice :: Size -> Int
sizePrice a
    | a < 12 = 5
    | a < 16 = 7
    | a < 24 = 10
    | otherwise = 100 

price :: Pizza -> Int
price (a, b, c) = (length a) * 2 + (sizePrice b) 

pizzaA :: Pizza
pizzaA = (["Bacon"], 6, True)

data Pizza' = Pizza' {  toppings :: [Topping']
                        , size :: Size'
                        , crust :: Crust'} 
                        deriving (Show)

data Topping'   = Pinapple
                | Bacon
                | Olives
                | Mushrooms

instance Show Topping' where
    show::Topping' -> String
    show Pinapple = "pinapple"
    show Bacon = "bacon"
    show Olives = "olives"
    show Mushrooms = "mushrooms"

data Size'      = Small
                | Medium
                | Large

instance Show Size' where
    show::Size' -> String
    show Small = "small"
    show Medium = "medium"
    show Large = "large"                

data Crust'     = Thin
                | Thick

instance Show Crust' where
    show::Crust' -> String
    show Thin = "thin"
    show Thick = "thick"

toppingCal' :: Topping' -> Int
toppingCal' Pinapple = 5
toppingCal' Bacon = 20
toppingCal' Olives = 10
toppingCal' Mushrooms = 15

toppingsCal' :: [Topping'] -> Int
toppingsCal' [] = 0
toppingsCal' (a:toppings) = (toppingCal' a) + (toppingsCal' toppings)

crustCal' :: Crust' -> Int
crustCal' Thin = 10
crustCal' Thick = 20

circleArea' :: Size' -> Int
circleArea' Small = 6 * 6 * 3
circleArea' Medium = 12 * 12 * 3
circleArea' Large = 16 * 16 * 3

calories' :: Pizza' -> Int
calories' Pizza'{toppings = a, size = b, crust = c} = ( (toppingsCal' a) + (crustCal' c) ) * (circleArea' b)

sizePrice' :: Size' -> Int
sizePrice' Small = 5
sizePrice' Medium = 7
sizePrice' Large = 10

price' :: Pizza' -> Int
price' Pizza'{toppings = a, size = b, crust = c} = (length a) * 2 + (sizePrice' b) 

pizzaA' :: Pizza'
pizzaA' = Pizza' { toppings = [Bacon], size = Small, crust = Thin }