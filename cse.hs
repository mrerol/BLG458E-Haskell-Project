
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


type Ninjas = ([Ninja], [Ninja], [Ninja], [Ninja], [Ninja])



fillNinjas :: [String] -> [Ninja]
fillNinjas []           = []
-- fillNinjas [n]          = [fillNinjaHelper n]
fillNinjas (n:ns)       = fillNinjaHelper n : fillNinjas ns
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

        fillNinjaHelper :: String -> Ninja
        fillNinjaHelper n = Ninja (ninjaName) (ninjaCountry) "Junior" (ninjaExam1) (ninjaExam2) (ninjaAbility1) (ninjaAbility2) 0
        


countryToString :: Char -> String
countryToString c
    | c == 'e' || c == 'E' = "Earth"
    | c == 'l' || c == 'L' = "Lightning"
    | c == 'w' || c == 'W' = "Water"
    | c == 'n' || c == 'N' = "Wind"
    | c == 'f' || c == 'F' = "Fire"
    | otherwise            = error "Invalid Country name with " ++ [c]

getCountryNinjasFromChar :: Ninjas -> Char -> [Ninja]
getCountryNinjasFromChar (fire, lightning, water, wind, earth) c
    | elem c ['e', 'E'] = earth
    | elem c ['l', 'L'] = lightning
    | elem c ['w', 'W'] = water
    | elem c ['n', 'N'] = wind
    | elem c ['f', 'F'] = fire
    | otherwise         = error "Invalid Country Name"



getNinjaFromNinjaListWithName :: [Ninja] -> String -> Ninja
getNinjaFromNinjaListWithName [] search     = error ("Invalid Ninja Name with " ++ search)
getNinjaFromNinjaListWithName (n:ns) search = if name n == search
                                                then n
                                                else getNinjaFromNinjaListWithName ns search


getNinjaFromNameAndCountry :: [Ninja] -> String -> Char -> Ninja
getNinjaFromNameAndCountry [] _ _                          = error ("Invalid Ninja Name with ")
getNinjaFromNameAndCountry (n:ns) searchName searchCountry = if ((name n) == searchName) && ((country n) == searchCountry)
                                                                then n
                                                                else getNinjaFromNameAndCountry ns searchName searchCountry


getOneNinja :: [Ninja] -> Ninja
getOneNinja   []                              = error ("Empty List")
getOneNinja   (x:xs)                          = x


getJourneymansOfList :: [Ninja] -> [Ninja]
getJourneymansOfList = filter (\a -> status a == "Journeyman")


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
    "Summon"    -> 50.0
    "Storm"     -> 10.0
    "Rock"      -> 20.0
    _           -> error "Unknown Ability"



updateNinjaLists :: [Ninja] -> Ninja -> Ninja -> Ninjas
updateNinjaLists ninjaList winner loser = ninjas
    where
        updateAfterWin = winner : (filter (\a -> not ((name a) == (name winner) && (country a) == (country winner))) ninjaList)
        updateAfterLost = (filter (\a -> not ((name a) == (name loser) && (country a) == (country loser))) updateAfterWin)
        fire = sortNinjas (filter (\a -> country a == 'F') updateAfterLost)
        lightning = sortNinjas (filter (\a -> country a == 'L') updateAfterLost)
        water = sortNinjas (filter (\a -> country a == 'W') updateAfterLost)
        wind = sortNinjas (filter (\a -> country a == 'N') updateAfterLost)
        earth = sortNinjas (filter (\a -> country a == 'E') updateAfterLost)

        ninjas = (fire, lightning, water, wind, earth)



winRound :: Ninja -> Ninja
winRound ninja = Ninja (name ninja) (country ninja) (status) (exam1 ninja) (exam2 ninja) (ability1 ninja) (ability2 ninja) (round)
    where
        status = if (r ninja) == 2
            then "Journeyman"
            else "Junior"
        round = (r ninja) + 1


sortNinjas :: [Ninja] -> [Ninja]
sortNinjas = iSort' ordering
    where
        ins' :: (Ninja -> Ninja -> Bool) -> Ninja -> [Ninja] -> [Ninja]
        ins' p n []             = [n]
        ins' p n xs@(x':xs')    
            | p n x'            = n : xs
            | otherwise         = x' : ins' p n xs'


        iSort' :: (Ninja -> Ninja -> Bool) -> [Ninja] -> [Ninja]
        iSort' p [] = []
        iSort' p (x:xs) = ins' p x (iSort' p xs)


        ordering :: Ninja -> Ninja -> Bool
        ordering n0 n1 = result
            where
                n0Score = getScore n0
                n1Score = getScore n1
                
                result = if r n0 < r n1
                            then True
                            else if r n0 == r n1
                                then if n0Score > n1Score
                                    then True
                                    else False
                            else 
                                False


ninjasToNinjaList :: Ninjas -> [Ninja]
ninjasToNinjaList = concat . tupleToListOfList
    where
        tupleToListOfList :: Ninjas -> [[Ninja]]
        tupleToListOfList (a, b, c, d, e) = [a, b, c, d, e]


sortAllNinjas :: Ninjas -> [Ninja]
sortAllNinjas = sortNinjas . ninjasToNinjaList

winnerPrinter :: Ninja -> IO()
winnerPrinter ninja = do
    putStrLn ("Winner: " ++ (name ninja) ++ ", Round: " ++ show (r ninja) ++ ", Status: " ++ (status ninja))


makeRound :: Ninjas -> Ninja -> Ninja -> IO()
makeRound ninjas ninja1 ninja2 = do
    let ninja1_score = getScore ninja1
    let ninja2_score = getScore ninja2
    let allNinjas = ninjasToNinjaList ninjas
    if ninja1_score > ninja2_score
        then do
            let ninja1' = winRound ninja1
            let ninjas' = (updateNinjaLists allNinjas ninja1' ninja2)
            winnerPrinter ninja1'
            mainLoop ninjas'
        else if ninja2_score > ninja1_score
            then do
                let ninja2' = winRound ninja2
                let ninjas' = (updateNinjaLists allNinjas ninja2' ninja1)
                winnerPrinter ninja2'
                mainLoop ninjas'
            else do
                let sumAbilities1 =  abilityToScore (ability1 ninja1) + abilityToScore (ability2 ninja1)
                let sumAbilities2 = abilityToScore (ability1 ninja2) + abilityToScore (ability2 ninja2)
                if sumAbilities1 > sumAbilities2
                    then do
                        let ninja1' = winRound ninja1
                        let ninjas' = (updateNinjaLists allNinjas ninja1' ninja2)
                        winnerPrinter ninja1'
                        mainLoop ninjas'
                    else do
                        let ninja2' = winRound ninja2
                        let ninjas' = (updateNinjaLists allNinjas ninja2' ninja1)
                        winnerPrinter ninja2'
                        mainLoop ninjas'

-- a                
viewCountrysNinjaInformation :: Ninjas -> IO()
viewCountrysNinjaInformation ninjas = do
    putStrLn "Enter the country code: "
    input <- getLine
    let country = input !! 0
    printCountrysNinjaInformation (getCountryNinjasFromChar ninjas country)
    mainLoop ninjas

        where    
            printCountrysNinjaInformation :: [Ninja] -> IO()
            printCountrysNinjaInformation ninjas = do
                putStrLn (shower ninjas)
                    where
                        shower :: [Ninja] -> String
                        shower []       = ""
                        -- shower [ninja]  = showerHelper ninja
                        shower (n:ns)   = showerHelper n ++ shower ns
                            
                        showerHelper :: Ninja -> String
                        showerHelper ninja = (name ninja) ++ ", Score: " ++  show (getScore ninja) ++ ", Status: " ++ (status ninja) ++ ", Round: " ++ show (r ninja) ++ "\n" ++ toAppend
                            where
                                toAppend = if (status ninja) == "Journeyman"
                                            then (countryToString (country ninja)) ++ " country cannot be included in a fight\n"
                                            else ""


-- b
viewAllCountrysNinjaInformation :: Ninjas -> IO()
viewAllCountrysNinjaInformation ninjas = do
    let ordered = sortAllNinjas ninjas
    putStrLn (shower ordered)
    mainLoop ninjas
        where
            shower :: [Ninja] -> String
            shower []       = ""
            -- shower [ninja]  = showerHelper ninja
            shower (n:ns)   = showerHelper n ++ shower ns
                
            showerHelper :: Ninja -> String
            showerHelper ninja = (name ninja) ++ ", Score: " ++  show (getScore ninja) ++ ", Status: " ++ (status ninja) ++ ", Round: " ++ show (r ninja) ++ "\n"


-- c
makeRoundBetweenNinjas :: Ninjas -> IO()
makeRoundBetweenNinjas ninjas = do
    putStrLn "Enter the name of the first ninja: "
    ninja1_name <- getLine
    putStrLn "Enter the country code: "
    ninja1_c <- getLine
    let ninja1 = getNinjaFromNinjaListWithName (getCountryNinjasFromChar ninjas (ninja1_c !! 0)) ninja1_name
    putStrLn "Enter the name of the second ninja: "
    ninja2_name <- getLine
    putStrLn "Enter the country code: "
    ninja2_c <- getLine
    let ninja2 = getNinjaFromNinjaListWithName (getCountryNinjasFromChar ninjas (ninja2_c !! 0)) ninja2_name
    makeRound ninjas ninja1 ninja2


-- d
makeRoundBetweenCountries :: Ninjas -> IO()
makeRoundBetweenCountries ninjas = do
    putStrLn "Enter the first country code: "
    ninja1_c <- getLine
    putStrLn "Enter the country code: "
    ninja2_c <- getLine
    let ninja1 = getOneNinja (getCountryNinjasFromChar ninjas (ninja1_c !! 0))
    let ninja2 = getOneNinja (getCountryNinjasFromChar ninjas (ninja2_c !! 0))
    makeRound ninjas ninja1 ninja2

-- e
exit :: Ninjas -> IO()
exit ninjas = do
    let (fire, lightning, water, wind, earth) = ninjas
    let allNinjas = ninjasToNinjaList ninjas
    let allJourneyMans = getJourneymansOfList allNinjas
    let toPrint = (showJournaymans' (sortNinjas allJourneyMans))
    putStr toPrint
    return()
    where
        showJournaymans' :: [Ninja] -> String
        showJournaymans' []     = ""
        showJournaymans' (n:ns) = ((name n) ++ ", Score: " ++ show (getScore n) ++ ", Status: " ++ (status n) ++ ", Round: " ++ show (r n) ++ "\n") ++ showJournaymans' ns 

-- otherwise
errorMessage :: Ninjas -> IO()
errorMessage ninjas = do
                        putStrLn "Error! Proper inputs [a/A, b/B, c/C, d/D, e/E]"
                        mainLoop ninjas

mainLoopHandler :: Char -> (Ninjas -> IO())
mainLoopHandler c
    | elem c ['a', 'A'] = viewCountrysNinjaInformation
    | elem c ['b', 'B'] = viewAllCountrysNinjaInformation
    | elem c ['c', 'C'] = makeRoundBetweenNinjas
    | elem c ['d', 'D'] = makeRoundBetweenCountries
    | elem c ['e', 'E'] = exit
    | otherwise         = errorMessage
    

mainLoop :: Ninjas -> IO()
mainLoop ninjas = do
    putStrLn "a) View a Country's Ninja Information"
    putStrLn "b) View All Countries' Ninja Information"
    putStrLn "c) Make a Round Between Ninjas"
    putStrLn "d) Make a Round Between Countries"
    putStrLn "e) Exit"
    putStrLn "Enter the action:"
    action <- getLine
    (mainLoopHandler (action !! 0)) ninjas




main :: IO()
main = do
    args <- getArgs
    let fileName = head args
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle 
    let lineContent = lines contents
    let ninjas = fillNinjas lineContent
    
    

    let fire = sortNinjas (filter (\a -> country a == 'F') ninjas)
    let lightning = sortNinjas (filter (\a -> country a == 'L') ninjas)
    let water = sortNinjas (filter (\a -> country a == 'W') ninjas)
    let wind = sortNinjas (filter (\a -> country a == 'N') ninjas)
    let earth = sortNinjas (filter (\a -> country a == 'E') ninjas)

    

    mainLoop (fire, lightning, water, wind, earth)

    return ()
 
