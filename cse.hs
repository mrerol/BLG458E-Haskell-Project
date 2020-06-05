
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
        
{-
Function: countryToString
    The function takes first letter of country as a char and returns the full country name as a string

Input:
    c(char): First letter of country as a char

Return:
    String: The full country name
-}

countryToString :: Char -> String
countryToString c
    | c == 'e' || c == 'E' = "Earth"
    | c == 'l' || c == 'L' = "Lightning"
    | c == 'w' || c == 'W' = "Water"
    | c == 'n' || c == 'N' = "Wind"
    | c == 'f' || c == 'F' = "Fire"
    | otherwise            = error "Invalid Country name with " ++ [c]

{-
Function: getCountryNinjasFromChar
    The function takes Ninjas and first letter of country as a char and returns list of ninjas that belongs to a country

Input:
    Ninjas: Tuple that contains all ninjas according to the their countr
    c(char):First letter of country as a char

Return:
    returns: List of ninjas 
-}
getCountryNinjasFromChar :: Ninjas -> Char -> [Ninja]
getCountryNinjasFromChar (fire, lightning, water, wind, earth) c
    | elem c ['e', 'E'] = earth
    | elem c ['l', 'L'] = lightning
    | elem c ['w', 'W'] = water
    | elem c ['n', 'N'] = wind
    | elem c ['f', 'F'] = fire
    | otherwise         = error "Invalid Country Name"


{-
Function: getNinjaFromNinjaListWithName
    The function takes list of ninja, the ninja name and finds the searched ninja's name in the list and returns it in type Ninja

Input:
    Ninja(List): List of ninjas that belongs a country
    ninja name(string):Ninja name as a string

Return:
    returns: searched ninja in type Ninja
-}
getNinjaFromNinjaListWithName :: [Ninja] -> String -> Ninja
getNinjaFromNinjaListWithName [] search     = error ("Invalid Ninja Name with " ++ search)
getNinjaFromNinjaListWithName (n:ns) search = if name n == search
                                                then n
                                                else getNinjaFromNinjaListWithName ns search

{-
Function: getNinjaFromNameAndCountry
    The function takes list of ninja, the ninja name, first letter of country and finds the ninja in the list according to its name and country and returns it in type Ninja

Input:
    Ninja(List): List of ninjas that belongs a country
    ninja name(string):Ninja name as a string
    country(char):First letter of country as a char

Return:
    returns: searched ninja in type Ninja
-}
getNinjaFromNameAndCountry :: [Ninja] -> String -> Char -> Ninja
getNinjaFromNameAndCountry [] _ _                          = error ("Invalid Ninja Name with ")
getNinjaFromNameAndCountry (n:ns) searchName searchCountry = if ((name n) == searchName) && ((country n) == searchCountry)
                                                                then n
                                                                else getNinjaFromNameAndCountry ns searchName searchCountry


{-
Function: getOneNinja
    The function takes list of ninja and returns the first ninja in the list

Input:
    Ninja(List): List of ninjas that belongs to the country

Return:
    returns: ninja in type Ninja
-}
getOneNinja :: [Ninja] -> Ninja
getOneNinja   []                              = error "Empty List"
getOneNinja   (x:xs)                          = x


{-
Function: getJourneymansOfList
    The function takes list of ninja and finds the journeymans in the given list and returns them

Input:
    Ninja(List): List of ninjas that belongs a country

Return:
    returns: List of Ninja
-}
getJourneymansOfList :: [Ninja] -> [Ninja]
getJourneymansOfList = filter (\a -> status a == "Journeyman")


{-
Function: getScore
    The function takes a ninja and calculate the score of the ninja

Input:
    Ninja(Ninja): A ninja in type Ninja

Return:
    returns: Ninja's score
-}
getScore :: Ninja -> Float
getScore ninja = 0.5 * (exam1 ninja) + 0.3 * (exam2 ninja) + ability1' + ability2' + round'
    where 
        ability1' = abilityToScore (ability1 ninja)
        ability2' = abilityToScore (ability2 ninja)
        round'    = 10.0 * fromInteger (toInteger (r ninja))::Float


{-
Function: abilityToScore
    The function takes ability and returns impact of the ability

Input:
    Ability(String): Ability as a string

Return:
    returns: impact of the ability
-}
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


{-
Function: updateNinjaLists
    The function takes list of ninja, two opponents and returns the updated Ninjas according to result of round between ninjas

Input:
    Ninjas: Tuple that contains all ninjas according to the their country
    ninja1(Ninja):Ninja that won the round
    ninja2(Ninja):Ninja that lost the round

Return:
    returns: updated Ninjas
-}
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


{-
Function: winRound
    The function takes a ninja and returns the updates ninja with status and rank

Input:
    ninja(Ninja):Ninja that won the round

Return:
    returns: Updated ninja with status and rank.
-}
winRound :: Ninja -> Ninja
winRound ninja = Ninja (name ninja) (country ninja) (status) (exam1 ninja) (exam2 ninja) (ability1 ninja) (ability2 ninja) (round)
    where
        status = if (r ninja) == 2
            then "Journeyman"
            else "Junior"
        round = (r ninja) + 1

{-
Function: sortNinjas
    The function takes list of ninja and returns sorted list of ninja

Input:
    List of Ninja: List of Ninja

Return:
    returns: Sorted list of Ninja.
-}
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

{-
Function: ninjasToNinjaList
    The function takes Ninjas and returns a list that contains all ninjas

Input:
   Ninjas Tuple that contains all ninjas according to the their country

Return:
    returns: List that contains all ninjas.
-}
ninjasToNinjaList :: Ninjas -> [Ninja]
ninjasToNinjaList = concat . tupleToListOfList
    where
        tupleToListOfList :: Ninjas -> [[Ninja]]
        tupleToListOfList (a, b, c, d, e) = [a, b, c, d, e]

{-
Function: sortAllNinjas
    The function takes Ninjas and returns sorted list that contains all ninjas

Input:
   Ninjas: Tuple that contains all ninjas according to the their country

Return:
    returns: Sorted list that contains all ninjas.
-}
sortAllNinjas :: Ninjas -> [Ninja]
sortAllNinjas = sortNinjas . ninjasToNinjaList


{-
Function: winnerPrinter
    The function takes a ninja and prints the winner ninja's information

Input:
   Ninja(Ninja): Tuple that contains all ninjas according to the their country

Return:
    returns: It prints ninja's information.
-}
winnerPrinter :: Ninja -> IO()
winnerPrinter ninja = do
    putStrLn ("Winner: " ++ (name ninja) ++ ", Round: " ++ show (r ninja) ++ ", Status: " ++ (status ninja))


{-
Function: prepareRound
    The function takes Ninjas, two opponents and prints a message if there is a obstacle to making round otherwise it calls the function that makes round.

Input:
    Ninjas: Tuple that contains all ninjas according to the their country
    ninja1(Ninja): information of first ninja
    ninja2(Ninja): information of second ninja

Return:
    returns: It prints error message if it is needed
-}
prepareRound :: Ninjas -> Ninja -> Ninja -> IO()
prepareRound ninjas ninja1 ninja2 = do
    
    let j1_flag = length (getJourneymansOfList (getCountryNinjasFromChar ninjas (country ninja1))) > 0
    let j2_flag = length (getJourneymansOfList (getCountryNinjasFromChar ninjas (country ninja2))) > 0

    if j1_flag && j2_flag
        then do
            putStrLn ("Country of " ++ (countryToString (country ninja1)) ++ " and " ++ (countryToString (country ninja2)) ++ " cannot be included in a fight")
            mainLoop ninjas
        else if j1_flag
            then do
                putStrLn ("Country of " ++ (countryToString (country ninja1)) ++ " cannot be included in a fight")
                mainLoop ninjas
        else if j2_flag
            then do
                putStrLn ("Country of " ++ (countryToString (country ninja2)) ++ " cannot be included in a fight")
                mainLoop ninjas
        else do
            makeRound ninjas ninja1 ninja2

        where
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


{-
Function: viewCountrysNinjaInformation
    The function takes Ninjas, input from user for selecting the country and shows all ninjas of that country in the CSE, according to their scores in descending order.

Input:
    Ninjas: Tuple that contains all ninjas according to the their country

Return:
    returns: It prints all ninjas of that country
-}
-- a                
viewCountrysNinjaInformation :: Ninjas -> IO()
viewCountrysNinjaInformation ninjas = do
    putStrLn "Enter the country code: "
    input <- getLine
    if not (length input == 1 && elem (input !! 0) ['F', 'f', 'E', 'e', 'W', 'w', 'N', 'n', 'L', 'l'])
        then do 
            putStrLn "Invalid Country code!"
            mainLoop ninjas
        else do
            printCountrysNinjaInformation (getCountryNinjasFromChar ninjas (input !! 0))
            mainLoop ninjas
        where    
            printCountrysNinjaInformation :: [Ninja] -> IO()
            printCountrysNinjaInformation ninjas = do
                putStr (shower ninjas)
                    where
                        shower :: [Ninja] -> String
                        shower = concat . map (showerHelper)
                            
                        showerHelper :: Ninja -> String
                        showerHelper ninja = (name ninja) ++ ", Score: " ++  show (getScore ninja) ++ ", Status: " ++ (status ninja) ++ ", Round: " ++ show (r ninja) ++ "\n" ++ toAppend
                            where
                                toAppend = if (status ninja) == "Journeyman"
                                            then (countryToString (country ninja)) ++ " country cannot be included in a fight\n"
                                            else ""


{-
Function: viewAllCountrysNinjaInformation
    The function takes Ninjas and shows all ninjas according to ninja's scores in descending order and their number of rounds in ascending order

Input:
    Ninjas: Tuple that contains all ninjas according to the their country

Return:
    returns: It prints all ninjas
-}
-- b
viewAllCountrysNinjaInformation :: Ninjas -> IO()
viewAllCountrysNinjaInformation ninjas = do
    let ordered = sortAllNinjas ninjas
    putStr (shower ordered)
    mainLoop ninjas
        where
            shower :: [Ninja] -> String
            shower = concat . map (\a -> (name a) ++ ", Score: " ++  show (getScore a) ++ ", Status: " ++ (status a) ++ ", Round: " ++ show (r a) ++ "\n")


{-
Function: makeRoundBetweenNinjas
    The function takes Ninjas, input from user for selecting the ninjas and makes round between them. After making round It shows the result

Input:
    Ninjas: Tuple that contains all ninjas according to the their country

Return:
    returns: It prints the result of round
-}
-- c
makeRoundBetweenNinjas :: Ninjas -> IO()
makeRoundBetweenNinjas ninjas = do
    putStrLn "Enter the name of the first ninja: "
    ninja1_name <- getLine
    putStrLn "Enter the country code: "
    ninja1_c <- getLine

    if not (length ninja1_c == 1 && elem (ninja1_c !! 0) ['F', 'f', 'E', 'e', 'W', 'w', 'N', 'n', 'L', 'l'])
        then do 
            putStrLn "Invalid Country code!"
            mainLoop ninjas
        else do
            if length (filter (\a -> (name a) == ninja1_name) (getCountryNinjasFromChar ninjas (ninja1_c !! 0))) < 1
                then do
                    putStrLn ("Ninja named " ++ (ninja1_name) ++ " with country " ++ (countryToString (ninja1_c !! 0)) ++ " is not in the list")
                    mainLoop ninjas
                else do

                    putStrLn "Enter the name of the second ninja: "
                    ninja2_name <- getLine
                    putStrLn "Enter the country code: "
                    ninja2_c <- getLine

                    if not (length ninja2_c == 1 && elem (ninja2_c !! 0) ['F', 'f', 'E', 'e', 'W', 'w', 'N', 'n', 'L', 'l'])
                        then do 
                            putStrLn "Invalid Country code!"
                            mainLoop ninjas
                        else do

                            if length (filter (\a -> (name a) == ninja2_name) (getCountryNinjasFromChar ninjas (ninja2_c !! 0))) < 1
                                then do
                                    putStrLn ("Ninja named " ++ (ninja2_name) ++ " with country " ++ (countryToString (ninja2_c !! 0)) ++ " is not in the list")
                                    mainLoop ninjas
                                else do

                                    let c1_flag = length (getCountryNinjasFromChar ninjas (ninja1_c !! 0)) < 1
                                    let c2_flag = length (getCountryNinjasFromChar ninjas (ninja2_c !! 0)) < 1

                                    if c1_flag && c2_flag
                                        then do
                                            putStrLn ("Country of " ++ (countryToString (ninja1_c !! 0)) ++ " and " ++ (countryToString (ninja2_c !! 0)) ++ " is empty so cannot be included in a fight")
                                            mainLoop ninjas
                                        else if c1_flag
                                            then do
                                                putStrLn ("Country of " ++ (countryToString (ninja1_c !! 0)) ++ " is empty so cannot be included in a fight")
                                                mainLoop ninjas
                                        else if c2_flag
                                            then do
                                                putStrLn ("Country of " ++ (countryToString (ninja2_c !! 0)) ++ " is empty so cannot be included in a fight")
                                                mainLoop ninjas
                                        else do
                                            let ninja1 = getNinjaFromNinjaListWithName (getCountryNinjasFromChar ninjas (ninja1_c !! 0)) ninja1_name
                                            let ninja2 = getNinjaFromNinjaListWithName (getCountryNinjasFromChar ninjas (ninja2_c !! 0)) ninja2_name
                                            prepareRound ninjas ninja1 ninja2


{-
Function: makeRoundBetweenNinjas
    The function takes Ninjas, input from user for selecting countries and makes round between ninjas which are in the first place in their country list.
    After making round It shows the result

Input:
    Ninjas Tuple that contains all ninjas according to the their country

Return:
    returns: It prints the result of round
-}
-- d
makeRoundBetweenCountries :: Ninjas -> IO()
makeRoundBetweenCountries ninjas = do
    putStrLn "Enter the first country code: "
    ninja1_c <- getLine
    if not (length ninja1_c == 1 && elem (ninja1_c !! 0) ['F', 'f', 'E', 'e', 'W', 'w', 'N', 'n', 'L', 'l'])
        then do 
            putStrLn "Invalid Country code!"
            mainLoop ninjas
        else do
        putStrLn "Enter the country code: "
        ninja2_c <- getLine

        if not (length ninja2_c == 1 && elem (ninja2_c !! 0) ['F', 'f', 'E', 'e', 'W', 'w', 'N', 'n', 'L', 'l'])
        then do 
            putStrLn "Invalid Country code!"
            mainLoop ninjas
        else do
            let c1_flag = length (getCountryNinjasFromChar ninjas (ninja1_c !! 0)) < 1
            let c2_flag = length (getCountryNinjasFromChar ninjas (ninja2_c !! 0)) < 1

            if c1_flag && c2_flag
                then do
                    putStrLn ("Country of " ++ (countryToString (ninja1_c !! 0)) ++ " and " ++ (countryToString (ninja2_c !! 0)) ++ " is empty so cannot be included in a fight")
                    mainLoop ninjas
                else if c1_flag
                    then do
                        putStrLn ("Country of " ++ (countryToString (ninja1_c !! 0)) ++ " is empty so cannot be included in a fight")
                        mainLoop ninjas
                else if c2_flag
                    then do
                        putStrLn ("Country of " ++ (countryToString (ninja2_c !! 0)) ++ " is empty so cannot be included in a fight")
                        mainLoop ninjas
                else do
                    let ninja1 = getOneNinja (getCountryNinjasFromChar ninjas (ninja1_c !! 0))
                    let ninja2 = getOneNinja (getCountryNinjasFromChar ninjas (ninja2_c !! 0))
                    prepareRound ninjas ninja1 ninja2
    


{-
Function: makeRoundBetweenNinjas
    The function takes Ninjas and shows all journeymen after that terminate the program

Input:
    Ninjas: Tuple that contains all ninjas according to the their country

Return:
    returns: It prints all journeymen
-}
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

{-
Function: errorMessage
    Prints error message  
-}
-- otherwise
errorMessage :: Ninjas -> IO()
errorMessage ninjas = do
                        putStrLn "Error! Proper inputs [a/A, b/B, c/C, d/D, e/E]"
                        mainLoop ninjas

{-
Function: mainLoopHandler
    The function takes a character which refers menu option and then calls appropriate function.
-}
mainLoopHandler :: Char -> (Ninjas -> IO())
mainLoopHandler c
    | elem c ['a', 'A'] = viewCountrysNinjaInformation
    | elem c ['b', 'B'] = viewAllCountrysNinjaInformation
    | elem c ['c', 'C'] = makeRoundBetweenNinjas
    | elem c ['d', 'D'] = makeRoundBetweenCountries
    | elem c ['e', 'E'] = exit
    | otherwise         = errorMessage
    
{-
Function: mainLoop
    The function prints the menu and takes action from the user, then calls the function mainLoopHandler to perform the appropriate function.  
-}
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
 
