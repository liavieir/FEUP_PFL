module Proj where  

import Data.List
import Data.Char

--Chosen representation of a monomial
type Term = (Int,[(Char,Int)])

--Chosen representation of a polynomial
type Pol =  [Term]

-- Function that creates a monomial
createTerm:: Int -> [(Char,Int)] -> Term
createTerm   x ys = (x,ys)

-- Funcion that creates a polynomial
createPolynomial::Term -> Pol -> Pol
createPolynomial a [] = [a]
createPolynomial a s = s ++ [a]


-- Auxiliar function to sum the index of 2 tuples
joinTuple::(Char,Int) -> (Char,Int) -> (Char,Int)
joinTuple (a,b) (c,d) = (a,b+d)


-- Auxiliar function to concatenate 2 variables if they have the same base
joinBases::(Char,Int) -> [(Char,Int)] -> [(Char,Int)]
joinBases  x [] = [x]
joinBases (a,b) ((x,y):xs) 
                | (a == x) = [(joinTuple (a,b) (x,y))] ++ xs 
                | otherwise =  (x,y):(joinBases (a,b) xs)


-- Function that compares monomials in order to organize a polynomial
--First organizes by the monomial degree and then organizes by alphabetical order of the variables
compareTerm::Term -> Term -> Ordering
compareTerm (n,[]) (n2,((c,d):yx)) = GT 
compareTerm (n,((c,d):yx)) (n2,[]) = LT 
compareTerm (n,((a,b):xs)) (n2,((c,d):yx)) | b < d = GT
                                           | b > d = LT
                                           | a < c = LT
                                           | a > c = GT
                                           | n < n2 = LT
                                           | n > n2 = GT
                                           |otherwise = EQ


-- Function that organizes the variables of each individual monomial
mycompare::(Char,Int) -> (Char,Int) ->Ordering
mycompare (a,b) (c,d) | b < d = GT
                      | b > d = LT
                      | a < c = GT
                      | a > c = LT
                      |otherwise = EQ


-- Auxiliar function  called by simplifyTerm to iteratively compute the simplifications
iter:: Int ->[(Char,Int)] -> [(Char,Int)]--funcao chamada por simplify term
iter 0 l = sortBy mycompare l
iter  n (x:xs) = iter(n-1) (joinBases x xs)
             

-- Function that simplifies each monomial
simplifyTerm:: Term -> Term 
simplifyTerm (b,s) = (b,iter (length (s)) s)


-- Function that simplifies polynomials
simplifyPol:: Pol -> Pol
simplifyPol [] = []
simplifyPol(x:[]) = [simplifyTerm x]
simplifyPol (x:xs) = [simplifyTerm x] ++ (simplifyPol xs)


-- Function that compares if 2 arrays are equal (this is used in the simplification of polynomials)
compareArray:: [(Char, Int)] -> [(Char, Int)] -> Bool
compareArray a b = (a == b)


--Function that joins 2 monomials, adding the indeces and joining the arrays of correspondent variables
joinTerms::Term -> Term -> Term
joinTerms (n,[]) (n2,[]) = (n+n2,[]) 
joinTerms (n,(x:xs)) (n2,(y:ys)) = (n+n2,(x:xs))

--Auxiliar function called by normalize, it is used to iterate over a polynomial joining terms that have equal variables
iter2::Int -> Pol -> Pol
iter2 0 p = p
iter2 n (x:xs) = iter2(n-1) (joinPol x xs)


--Auxiliar function called by iter2 to join terms that have equal variables
joinPol::Term -> Pol -> Pol
joinPol x [] = [x]
joinPol (n,l) ((n2,lista):xs) 
                |compareArray l lista = [(joinTerms (n,l) (n2,lista))] ++ xs
                |otherwise = (n2,lista):(joinPol (n,l) xs)

--Main function to normalize a polynomial
normalize:: Pol -> Pol
normalize a = iter2 (length (simplifyPol a)) (simplifyPol a)


-- Function that adds 2 polynomials
addPol::Pol->Pol->Pol
addPol [(a,[])] [(b,[])] = cleanPol [(a+b,[])]
addPol a [(b,[])] = cleanPol(a ++ [(b,[])])
addPol a b = cleanPol (normalize (a++b))


-- Auxiliar function( called by multPols) that multiplies 2 monomials
multTerms::Term -> Term -> Term
multTerms (n,lista) (n2,[]) = simplifyTerm (n*n2,lista)
multTerms (n,lista) (n2,lista2) = simplifyTerm (n*n2,lista++lista2)

-- Function that multiplies 2 polynomials
multPols::Pol->Pol->Pol
multPols [] pol2 = pol2
multPols [(0,_)] pol2 = []
multPols pol1 pol2 = cleanPol (normalize [multTerms x y | x <- pol1, y <-pol2])

--Auxiliar function that executes the mathematical differentiation
exediffTerm::Char->Term->Term
exediffTerm c (n,((v,y):xs)) | c == v = (n*y,(v,y-1):xs)
                             | otherwise = addExp (v,y) (exediffTerm c (n,xs))

--Auxiliar funtion that determines if a monomial can be differentiated by a variable given
canbeDiff::Char->Term->Bool
canbeDiff c (n,[]) = False
canbeDiff c (n,((v,y):xs)) | c == v = True
                           |otherwise = canbeDiff c (n,(xs))

--Auxiliar function that differentiates a monomial if possible
diffTerm::Char->Term->Term
diffTerm c t = if canbeDiff c t then exediffTerm c t 
            else (0,[('0',0)])                      


addExp::(Char,Int) -> Term->Term
addExp (c,i) (n,lista) = simplifyTerm (n,lista ++ [(c,i)])

--Function that differentiates a polynomial
diffPol::Char->Pol->Pol
diffPol c [] = []
diffPol c (x:xs) = cleanPol (normalize (([diffTerm c x]) ++ diffPol c xs))


--Function that elimate terms with 0 on the index and terms multiplied by 0
cleanPol:: Pol->Pol
cleanPol [] = []                  
cleanPol ((n,[]):xs)| n /= 0 = cleanPol xs ++ [(n,[])]
                    | otherwise = cleanPol xs
cleanPol [(n,((a,b):x))] | n==0 = []
                         | otherwise = [cleanTerm  (length (x)+1) (n,((a,b):x))]                   
cleanPol ((n,((a,b):x)):xs) | n == 0 = [] ++ cleanPol xs
                            | otherwise  = [cleanTerm (length(x)+1) (n,((a,b):x))] ++ cleanPol xs

--Auxiliar function (called by cleanPol) that handles each monomial
cleanTerm:: Int -> Term -> Term
cleanTerm 0 a = a
cleanTerm m (n,((a,b):xs))| b == 0 = cleanTerm  (m-1) (n,xs)
                          | otherwise = cleanTerm (m-1) (n,(xs++[(a,b)]))


                        

--Functions to output, to console, polynomials in the string format
outputTerm:: [(Char, Int)] -> String
outputTerm [] = []
outputTerm [(a,b)] = a:'^':(show b)
outputTerm ((a,b):xs) = a:'^':((show b)++(outputTerm xs))

outputPol:: Pol -> String
outputPol [] = []
outputPol ((n,[]):[])  = (show n)
outputPol ((n, []):xy) = (show n)++" + "++outputPol(xy)
outputPol [(n, l)] = (show n)++outputTerm l
outputPol ((n, l):xy) = (show n)++outputTerm l++ outputNext xy

outputNext :: Pol -> String
outputNext [] = []
outputNext ((n, l):xy) | n < 0 = " - " ++ outputPol((n*(-1),l): xy)
                       | otherwise = " + " ++ outputPol((n,l): xy)



-----------------------------------------------STRINGS-------------------------------------------------------------------------------


-- Definition of the function words from Prelude
split :: String -> [String]
split pol | (take 1 pol) == "-" = [take 1 pol] ++ split (drop 1 pol)
                | otherwise = case dropWhile (\x -> x == '+' || x == '-') pol of
                                    "" -> []
                                    s' -> w : split s''
                                        where (w, s'') = break (\x -> x == '+' || x == '-') s'

-- Function that removes all spaces
removeSpaces :: [String] -> [String]
removeSpaces x = [filter (\xs -> (xs /=' ')) y | y <- x]


-- Function that creates a monomial
createMono :: [String] -> [String]
createMono [] = []
createMono (x:[]) = [x]                          
createMono (x:y:z) | x == "+" = [y] ++ createMono z
                      | x == "-" = [x ++ y] ++ createMono z
                      | otherwise = [x] ++ createMono ([y] ++ z)


--Function that handles the polynomial variable
createLetter :: String -> [(Char, Int)]
createLetter (x:y:z) | isLetter y = [(x, 1)] ++ createLetter ([y] ++ z) --caso xy^2 - (x,1)
                        | y == '^' = [(x, read (takeWhile (isDigit) z) :: Int)] ++ createLetter (dropWhile (isDigit) z) --caso x^2y
createLetter (x:[]) = [(x, 1)] --caso xy
createLetter [] = []

--Auxiliar function that handles the index 
createCoeficient :: String -> Term
createCoeficient (x:xs)
    | isDigit x = ((read ([x] ++ (takeWhile (isDigit) xs)) :: Int), createLetter (dropWhile (isDigit) xs))
    | x == '-' = if isDigit (head xs) then ((read ([x] ++ (takeWhile (isDigit) xs)) :: Int), createLetter (dropWhile (isDigit) xs))
              else (-1, createLetter xs)
    | isLetter x = (1, createLetter ([x]++xs))


--Function that transformes the string into the internal representation
createPol :: String -> Pol
createPol x = [z | z <- pol]
            where pol = [createCoeficient m | m <- createMono (removeSpaces (split x))]


--Functions that should be called to compute the operations of normalize, add, multiply and differentiate

normalizeP :: String -> String
normalizeP s = outputPol (sortBy compareTerm (cleanPol (normalize (createPol (s)))))

sumP :: String -> String -> String
sumP s1 s2 = outputPol (sortBy compareTerm (cleanPol (normalize (addPol (createPol s1) (createPol s2)))))

multP :: String -> String -> String
multP s1 s2 = outputPol (sortBy compareTerm (sortBy compareTerm (cleanPol (normalize(multPols (createPol s1) (createPol s2))))))

deriveP :: Char -> String -> String
deriveP c s = outputPol (sortBy compareTerm (cleanPol (normalize (diffPol c (createPol s)))))






