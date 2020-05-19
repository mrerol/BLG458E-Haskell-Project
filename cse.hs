import System.Environment
import System.IO


-- definition of Ninja data type
data Ninja = Ninja {name:: String, country:: Char,
                    status:: String, exam1:: Float,
                    exam2:: Float, ability1:: String,
                    ability2:: String, r:: Int} 
-- name is the name of the ninja.
-- county is the county of the ninja. Character choice for each country is given below.
-- status is the current status of the ninja. It is initialized as junior. 
-- exam1 is the score of the first examination. 
-- exam2 is the score of the second examination. 
-- ability1 is the first ability of the ninja.
-- ability2 is the second ability of the ninja. 
-- r is the number of rounds that the ninja took place. It is initialized as 0.

fire :: [Ninja] -- add the junior ninjas of Land of Fire to that list
fire = []
lightning :: [Ninja] -- add the junior ninjas of Land of Lightning to that list
lightning = []
water :: [Ninja] -- add the junior ninjas of Land of Water to that list
water = []
wind :: [Ninja] -- add the junior ninjas of Land of Wind to that list
wind = []
earth :: [Ninja] -- add the junior ninjas of Land of Earth to that list
earth = []

main :: IO()
main = do

    args <- getArgs
    let fileName = head args
    print fileName
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle 
    let lineContent = lines contents
    print (length lineContent)
    let x = (length lineContent) / 5
    

    return ()




mainLoop :: IO()
mainLoop = do
    putStrLn "a) View a Country's Ninja Information"
    putStrLn "b) View All Countries' Ninja Information"
    putStrLn "c) Make a Round Between Ninjas"
    putStrLn "b) Make a Round Between Countries"
    putStrLn "e) Exit"
    putStrLn "Enter the action:"
    action <- getLine
    if action == "a" || action == "A"
        then viewCountrysNinjaInformation
        else if action == "b" || action == "B"
            then viewAllCountrysNinjaInformation
            else if action == "c" || action == "C"
                then makeRoundBetweenNinjas
                else if action == "d" || action == "D"
                    then makeRoundBetweenCountries
                    else if action == "e" || action == "E"
                        then return ()
                        else
                            do
                                putStrLn "Error! Proper inputs [a/A, b/B, c/C, d/D, e/E]"
                                mainLoop
                
viewCountrysNinjaInformation :: IO()
viewCountrysNinjaInformation = undefined

viewAllCountrysNinjaInformation :: IO()
viewAllCountrysNinjaInformation = undefined

makeRoundBetweenNinjas :: IO()
makeRoundBetweenNinjas = undefined

makeRoundBetweenCountries :: IO()
makeRoundBetweenCountries = undefined




 

    
        


