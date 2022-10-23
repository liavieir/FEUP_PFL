import Proj


main :: IO()
main = do
    putStrLn "\nTest 1: Normalize polynomial 3x^2x^3 + 3y + 5y + 2z^2"
    putStrLn "Expected: 3x^5 + 2z^2 + 8y^1"
    putStrLn "Result: "
    putStrLn (normalizeP "3x^2x^3 + 3y + 5y + 2z^2")

    putStrLn "\nTest 2: Normalize polynomial 2x^2y^3 + 3"
    putStrLn "Expected: 2y^3x^2 + 3"
    putStrLn "Result: "
    putStrLn (normalizeP "2x^2y^3 + 3")

    putStrLn "\nTest 3: Sum two polynomial \"3x^2 + 3\" \"4y + 2x^2\""
    putStrLn "Expected: 5x^2 + 4y^1 + 3"
    putStrLn "Result: "
    putStrLn (sumP "3x^2 + 3" "4y + 2x^2")

    putStrLn "\nTest 4: Sum two polynomial \"3z^2 + 4x + 2z^2\" \"-4x + 4\""
    putStrLn "Expected: 5z^2 + 4"
    putStrLn "Result: "
    putStrLn (sumP "3z^2 + 4x + 2z^2" "-4x + 4")

    putStrLn "\nTest 5: Sum two polynomial \"4x + 2y^2\" \"\""
    putStrLn "Expected: 2y^2 + 4x^1"
    putStrLn "Result: "
    putStrLn (sumP "4x + 2y^2" "")

    putStrLn "\nTest 6: Associative property of addition: p1 + p2 = p2 + p1"
    putStrLn "\nSum of \"3x + 2y^2\" and \"5y^2 + 4\""
    putStrLn "Expected: 7y^2 + 3x^1 + 4"
    putStrLn "Result: "
    putStrLn (sumP "3x + 2y^2" "5y^2 + 4")
    putStrLn (sumP "5y^2 + 4" "3x + 2y^2")

    putStrLn "\nTest 7: Multiply two polynomial \"10z + 2y\" \"4y + 2x^2\""
    putStrLn "Expected: 4x^2y^1 + 20x^2z^1 + 8y^2 + 40y^1z^1"
    putStrLn "Result: "
    putStrLn (multP "10z + 2y" "4y + 2x^2")

    putStrLn "\nTest 8: Multiply polynomial with constant \"3z^2 + 4x + 2z^2\" \"-4\""
    putStrLn "Expected: -20z^2 - 16x^1"
    putStrLn "Result: "
    putStrLn (multP "3z^2 + 4x + 2z^2" "-4")

    putStrLn "\nTest 9: Null element property \"4x + 2y^2\" \"0\""
    putStrLn "Expected: \"\""
    putStrLn "Result: "
    putStrLn (multP "4x + 2y^2" "0")

    putStrLn "\nTest 10: Associative property of multiplication: p1 * p2 = p2 * p1"
    putStrLn "\nMultiplication of \"3x + 2y^2\" and \"5y^2 + 4\""
    putStrLn "Expected: 10y^4 + 8y^2 + 15y^2x^1 + 12x^1"
    putStrLn "Result: "
    putStrLn (multP "3x + 2y^2" "5y^2 + 4")
    putStrLn (multP "5y^2 + 4" "3x + 2y^2")


    putStrLn "\nTest 11: Derivatives of a polynomial \'x\' \"4x + 2x^2 + 3y\""
    putStrLn "Expected: 4x^1 + 4"
    putStrLn "Result: "
    putStrLn (deriveP 'x' "4x + 2x^2 + 3y")

    putStrLn "\nTest 12: Derivatives of a polynomial \'z\' \"4x + 2x^2 + 3y\""
    putStrLn "Expected: "
    putStrLn "Result: "
    putStrLn (deriveP 'z' "4x + 2x^2 + 3y")

    putStrLn "\nTest 13: Derivatives of a polynomial \'x\' \"3x^3 + 4y - 2x\""
    putStrLn "Expected: \"9x^2 - 2\""
    putStrLn "Result: "
    putStrLn (deriveP 'x' "3x^3 + 4y - 2x")

    putStrLn "\nTest 14: Composition of fuctions: (multP (sumP \"3x + 2y^2\" \"3x^3 + 4y - 2x\") \"4x\")"
    putStrLn "Expected: \"12x^4 + 4x^2 + 8y^2x^1 + 16y^1x^1\""
    putStrLn "Result: "
    putStrLn (multP (sumP "3x + 2y^2" "3x^3 + 4y - 2x") "4x")




























