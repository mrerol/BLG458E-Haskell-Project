module Demo where

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



-- data Country = Fire | Lightning | Water | Wind | Earth
--     deriving (Eq,Show)

-- data Ability = Clone | Hit | Lightning | Vision | Sand | Fire | Water | Blade | Summon | Storm | Rock
--     deriving (Eq,Show)




fillNinjas :: [String] -> [Ninja]
fillNinjas []           = []
fillNinjas [n]          = [fillNinjaHelper n]
fillNinjas (n:ns)       = fillNinjaHelper n : fillNinjas ns

fillNinjaHelper :: String -> Ninja
fillNinjaHelper n = Ninja (ninjaName) (ninjaCountry) "junior" (ninjaExam1) (ninjaExam2) (ninjaAbility1) (ninjaAbility2) 0
    where
        items = words n
        ninjaName = items !! 0
        county = items !! 1
        ninjaCountry = case county of
            "Earth"     -> 'E'
            "Lightning" -> 'L'
            "Water"     -> 'W'
            "Wind"      -> 'N'
            "Fire"      -> 'F'
            _           -> error "Unknown Country Name!"

             
        
        ninjaExam1 = read (items !! 2) :: Float
        ninjaExam2 = read (items !! 3) :: Float

        ninjaAbility1 = items !! 4
        ninjaAbility2 = items !! 5


type Ninjas = ([Ninja], [Ninja], [Ninja], [Ninja], [Ninja])



mainLoop :: Ninjas -> IO()
mainLoop ninjas = do
    putStrLn "a) View a Country's Ninja Information"
    putStrLn "b) View All Countries' Ninja Information"
    putStrLn "c) Make a Round Between Ninjas"
    putStrLn "b) Make a Round Between Countries"
    putStrLn "e) Exit"
    putStrLn "Enter the action:"
    action <- getLine
    if action == "a" || action == "A"
        then do
            viewCountrysNinjaInformation ninjas
            mainLoop ninjas
        else if action == "b" || action == "B"
            then do
                viewAllCountrysNinjaInformation
                mainLoop ninjas
            else if action == "c" || action == "C"
                then do
                    makeRoundBetweenNinjas
                    mainLoop ninjas
                else if action == "d" || action == "D"
                    then do 
                        makeRoundBetweenCountries
                        mainLoop ninjas
                    else if action == "e" || action == "E"
                        then return ()
                        else
                            do
                                putStrLn "Error! Proper inputs [a/A, b/B, c/C, d/D, e/E]"
                                mainLoop ninjas


                
viewCountrysNinjaInformation :: Ninjas -> IO()
viewCountrysNinjaInformation ninjas = do
    putStrLn "Enter the country code: "
    country <- getChar
    let (fire, lightning, water, wind, earth) = ninjas
    if elem country ['F', 'f']
        then printCountrysNinjaInformation fire
        else if elem country ['L', 'l']
            then printCountrysNinjaInformation lightning
            else if  elem country ['W', 'w']
                then printCountrysNinjaInformation water
                else if  elem country ['N', 'n']
                    then printCountrysNinjaInformation wind
                    else if elem country ['E', 'e']
                        then printCountrysNinjaInformation earth
                        else error "Unknown Country Code"


        where    
    

            printCountrysNinjaInformation :: [Ninja] -> IO()
            printCountrysNinjaInformation ninjas = do
                let ordered = iSort' ordering ninjas
                putStrLn name (ordered !! 0)
                putStrLn "aloo"


            ordering :: Ninja -> Ninja -> Bool
            ordering n0 n1 = result
                where
                    n0Score = getScore n0
                    n1Score = getScore n1
                    
                    roundFlag = r n0 < r n1
                    scoreFlag = n0Score > n1Score

                    result 
                        | roundFlag == True     = True
                        | scoreFlag == True     = True
                        | scoreFlag == False    = False
                        | otherwise             = False


            ins' :: (Ninja -> Ninja -> Bool) -> Ninja -> [Ninja] -> [Ninja]
            ins' p n []             = [n]
            ins' p n xs@(x':xs')    
                | p n x'            = n : xs
                | otherwise         = x' : ins' p n xs'


            iSort' :: (Ninja -> Ninja -> Bool) -> [Ninja] -> [Ninja]
            iSort' p [] = []
            iSort' p (x:xs) = ins' p x (iSort' p xs)



getScore :: Ninja -> Float
getScore ninja = 0.5 * (exam1 ninja) + 0.3 * (exam2 ninja) + ability1' + ability2' + round'
    where 
        ability1' = abilityToScore (ability1 ninja)
        ability2' = abilityToScore (ability2 ninja)
        round'    = 10.0 * fromInteger (toInteger (r ninja))::Float


abilityToScore :: String -> Float
abilityToScore ability = case ability of
    "Clone"     -> 20.0
    "Hit"       -> 10.0
    "Lightning" -> 50.0
    "Vision"    -> 30.0
    "Sand"      -> 50.0
    "Fire"      -> 40.0
    "Water"     -> 30.0
    "Blade"     -> 20.0
    "Summon"    -> 10.0
    "Rock"      -> 20.0
    _           -> error "Unknown Ability"


viewAllCountrysNinjaInformation :: IO()
viewAllCountrysNinjaInformation = return ()

makeRoundBetweenNinjas :: IO()
makeRoundBetweenNinjas = return ()

makeRoundBetweenCountries :: IO()
makeRoundBetweenCountries = return ()



main :: IO()
main = do

    args <- getArgs
    let fileName = head args
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle 
    let lineContent = lines contents
    let ninjas = fillNinjas lineContent
    
    

    let fire = filter (\a -> country a == 'F') ninjas
    let lightning = filter (\a -> country a == 'L') ninjas
    let water = filter (\a -> country a == 'W') ninjas
    let wind = filter (\a -> country a == 'N') ninjas
    let earth = filter (\a -> country a == 'E') ninjas

    mainLoop (fire, lightning, water, wind, earth)

    return ()
 

        



    
        







 

    
        





