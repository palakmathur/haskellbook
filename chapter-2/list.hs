data List a = Empty | Cons a (List a) deriving Show

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

