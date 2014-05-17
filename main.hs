-- 
-- MATHFUN - Discrete Mathematics and Functional Programming
-- Functional Programming Assignment 2013/14 
-- UP653418
--

import Text.Printf
import Data.Char

--
--
-- ------------------------------- 'Film' DEFINITION -------------------------------
--
--

	
data Film = Film String String Int [(String, Int)]
		deriving (Eq,Ord,Show,Read)


--
--
-- ------------------------------- TEST DATABASE -------------------------------
--
--


testDatabase :: [Film]
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 [("Amy",6), ("Bill",9), ("Ian",7), ("Kevin",9), ("Emma",4), ("Sam",5), ("Megan",4)], Film "The Fly" "David Cronenberg" 
				1986 [("Megan",4), ("Fred",7), ("Chris",5), ("Ian",0), ("Amy",5)], Film "Psycho" "Alfred Hitchcock" 1960 [("Bill",4), ("Jo",4), ("Garry",8), ("Kevin",7), 
				("Olga",8), ("Liz",10), ("Ian",9) ], Film "Body Of Lies" "Ridley Scott" 2008 [("Sam",3), ("Neal",7), ("Kevin",2), ("Chris",5), ("Olga",6)], 
				Film "Avatar" "James Cameron" 2009 [("Olga",2), ("Wally",8), ("Megan",9), ("Tim",5), ("Zoe",8), ("Emma",3)], Film "Titanic" "James Cameron" 1997 [("Zoe",7), 
				("Amy",2), ("Emma",5), ("Heidi",3), ("Jo",8), ("Megan",5), ("Olga",7), ("Tim",10)], Film "The Departed" "Martin Scorsese" 2006 [("Heidi",2), ("Jo",8), 
				("Megan",5), ("Tim",2), ("Fred",5)], Film "Aliens" "Ridley Scott" 1986 [("Fred",8), ("Dave",6), ("Amy",10), ("Bill",7), ("Wally",2), ("Zoe",5)], 
				Film "Prometheus"  "Ridley Scott" 2012 [("Garry",3), ("Chris",4), ("Emma",5), ("Bill",1), ("Dave",3)], Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 
				1982 [("Ian",7), ("Amy",2), ("Emma",7), ("Sam",8), ("Wally",5), ("Zoe",6)], Film "The Birds" "Alfred Hitchcock" 1963 [("Garry",7), ("Kevin",9), ("Olga",4), 
				("Tim",7), ("Wally",3)], Film "Goodfellas" "Martin Scorsese" 1990 [("Emma",7), ("Sam",9), ("Wally",5), ("Dave",3)], Film "The Shawshank Redemption" 
				"Frank Darabont" 1994 [("Jo",8), ("Sam",10), ("Zoe",3), ("Dave",7), ("Emma",3), ("Garry",10), ("Kevin",7)], Film "Gladiator" "Ridley Scott" 2000 [("Garry",7), 
				("Ian",4), ("Neal",6), ("Wally",3), ("Emma",4)], Film "The Green Mile" "Frank Darabont" 1999 [("Sam",3), ("Zoe",4), ("Dave",8), ("Wally",5), ("Jo",5)], 
				Film "True Lies" "James Cameron" 1994 [("Dave",3), ("Kevin",4), ("Jo",0)], Film "Minority Report" "Steven Spielberg" 2002 [("Dave",5), ("Garry",6), 
				("Megan",2), ("Sam",7), ("Wally",8)], Film "The Wolf of Wall Street" "Martin Scorsese" 2013 [("Dave",6), ("Garry",6), ("Megan",0), ("Sam",4)], 
				Film "War Horse" "Steven Spielberg" 2011 [("Dave",6), ("Garry",6), ("Megan",3), ("Sam",7), ("Wally",8), ("Zoe",8)], Film "Lincoln" "Steven Spielberg" 
				2012 [("Ian",3), ("Sam",7), ("Wally",3), ("Zoe",4), ("Liz",7), ("Megan",4)], Film "Vertigo" "Alfred Hitchcock" 1958 [("Bill",7), ("Emma",5), ("Zoe",9), 
				("Olga",6), ("Tim",10)], Film "The Terminal" "Steven Spielberg" 2004 [("Olga",3), ("Heidi",8), ("Bill",2), ("Sam",6), ("Garry",8)], Film "Jaws" 
				"Steven Spielberg" 1975 [("Fred",3), ("Garry",0), ("Jo",3), ("Neal",9), ("Emma",7)], Film "Hugo" "Martin Scorsese" 2011 [("Sam",4), ("Wally",3), 
				("Zoe",4), ("Liz",7)]]

--
--
-- ------------------------------- FUNCTIONAL CODE -------------------------------
--
--


-- Helper functions

-- Calculates the average of a set of ratings


addUp :: [(String, Int)] -> Float
addUp [] = 0
addUp ((_, rating):ratings) = a + addUp ratings
			where a = fromIntegral rating :: Float

average :: [(String, Int)] -> Float
average [] = 0
average ratings = addUp ratings / fromIntegral (length ratings)


--
-- Main functional programs
--


-- i) Add a new film to the database

addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm newTitle newDirector newYear database = database ++ [Film newTitle newDirector newYear [("", 0)]]



-- ii) Get a list of all the current films in the database

giveFilms :: [Film] -> String
giveFilms [] = [] -- Case for an empty string
giveFilms ((Film title director year ratings):restOfFilms) = "Title: " ++ title ++ " - Director: " ++ director ++ " - Year: " ++ show year ++ " - Rating: " ++ printf "%3.1f" (average ratings) ++ "\n" ++ giveFilms restOfFilms



-- iii) List all the films by a specified director in the database

giveFilmsByDirector :: String -> [Film] -> String
giveFilmsByDirector _ [] = []
giveFilmsByDirector requestedDirector ((Film title director year ratings):restOfFilms)
	| requestedDirector == director =  giveFilms [Film title director year ratings] ++ giveFilmsByDirector requestedDirector restOfFilms 
	| otherwise = giveFilmsByDirector requestedDirector restOfFilms 



-- iv) View all the sites with a website rating of 6 or higher

siteRating6OrHigher :: [Film] -> String
siteRating6OrHigher [] = []
siteRating6OrHigher ((Film title director year ratings):restOfFilms)
	| average ratings >= 6.0 = giveFilms [Film title director year ratings] ++ siteRating6OrHigher restOfFilms
	| otherwise = siteRating6OrHigher restOfFilms


-- v) Get scores for director

avgRatingsForDirector :: String -> [Film] -> String
avgRatingsForDirector director database = printf "%3.1f" (avg (avgRatingsForDirectorMain director database))

avg directorRating = add directorRating / fromIntegral (length directorRating)
	where add = foldr (+) 0 -- adds up a given list of Ints

avgRatingsForDirectorMain :: String -> [Film] -> [Float]
avgRatingsForDirectorMain _ [] = []
avgRatingsForDirectorMain requestedDirector ((Film _ director _ ratings):restOfFilms)
	| requestedDirector == director = [average ratings] ++ avgRatingsForDirectorMain requestedDirector restOfFilms
	| otherwise = avgRatingsForDirectorMain requestedDirector restOfFilms



-- vi) Find a users film ratings

filmsUserHasRated :: String -> [Film] -> String
filmsUserHasRated _ [] = []
filmsUserHasRated user ((Film title _ _ ((name, rating):ratings)):restOfFilms)
 	| filmsUserHasRatedHelper user ratings || user == name = "Title: " ++ title ++ " - Your rating: " ++ show rating ++ "\n" ++ filmsUserHasRated user restOfFilms
 	| otherwise = filmsUserHasRated user restOfFilms

--filmsUserHasRatedHelper :: String -> [(String, Int)] -> Bool
filmsUserHasRatedHelper _ [] = False
filmsUserHasRatedHelper user (x:xs)
	| filmsUserHasRatedHelper2 user x == True = True
	| otherwise = filmsUserHasRatedHelper user xs

--filmsUserHasRatedHelper2 :: String -> (String, Int) -> Bool
filmsUserHasRatedHelper2 user (userName, _)
	| user == userName = True
	| otherwise = False


-- vii) allow a given user to rate (or re-rate) a ﬁlm (only the latest rating from the user should remain recorded)

--rateAFilm :: String -> String -> Int -> [Film] -> String
--rateAFilm userName requestedTitle newRating ((Film title _ _ ratings):restOfFilms)
--	| requestedTitle == title = map (\rating -> if fst rating == userName then (userName, newRating) else rating) ratings
--	| otherwise = rateAFilm userName requestedTitle newRating restOfFilms


--rateAFilmHelper _ [] = False
--rateAFilmHelper userName ((name, _:ratings)
--	| userName == name = True
--	| otherwise = rateAFilmHelper userName ratings




-- viii) Give a list of films from a given year sorted by website rating

-- *not sorted by rating*

filmsReleasedInYear :: Int -> [Film] -> String
filmsReleasedInYear _ [] = []
filmsReleasedInYear requestedYear ((Film title director year ((_, rating):ratings)):restOfFilms)
	| requestedYear <= year = giveFilms [Film title director year ratings] ++ filmsReleasedInYear requestedYear restOfFilms
	| otherwise = filmsReleasedInYear requestedYear restOfFilms


--
--
-- ------------------------------- DEMO FUNCTIONS -------------------------------
--
--



-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
demo 1  = putStrLn (giveFilms (addFilm "Gravity" "Alfonso Cuaron" 2013 testDatabase)) -- all films after adding 2013 film "Gravity" by "Alfonso Cuaron" to testDatabase
demo 2  = putStrLn (giveFilms testDatabase)                                           -- show all films in the database
demo 3  = putStrLn (giveFilmsByDirector "James Cameron" testDatabase)                 -- all films by "James Cameron"
demo 4  = putStrLn (siteRating6OrHigher testDatabase)                                 -- all films with website rating >= 6
demo 5  = putStrLn (avgRatingsForDirector "James Cameron" testDatabase)               -- average website rating for "James Cameron"
demo 6  = putStrLn (filmsUserHasRated "Zoe" testDatabase)                             -- film titles and user ratings for "Zoe"
--demo 7  = putStrLn all films after Zoe rates "Jaws" 8
--demo 77 = putStrLn all films after Zoe rates "Vertigo" 3
demo 8  = putStrLn (filmsReleasedInYear 2009 testDatabase)                            -- films from or after 2009 sorted by rating
demo _ = putStrLn "demo doesn't exist"

--
--
-- ------------------------------- UI CODE -------------------------------
--
--

main :: IO ()
main = do
	database <- readFile "films.txt"
	putStrLn "Welcome to the film database, please enter your name: "
	userName <- getLine
	if userName == "" then do
		putStrLn "Please enter a name"
		main
	else do
		putStrLn ("\n Welcome " ++ userName)
		mainMenu userName (read database :: [Film])

mainMenu :: String -> [Film] -> IO ()
mainMenu userName database = do
	putStrLn ("\n\n  -- Main Menu -- \n\nChoose one of the options below: \n\n 1. Add a new film to the database\n 2. Show all the films in the database\n 3. Show all films by a given director\n 4. Show all the films with a website rating of 6 or higher\n 5. Show the average rating of a given director's films\n 6. Show ratings for films that you have rated\n 7. Rate or re-rate a film in the database\n 8. Show all films released in a year\n\n Type 'exit' to quit and save the database \n\n Enter a number to get started: ")
	func <- getLine
	if isEmpty func && isANumber func then
		if (read func :: Int) > 0 && (read func :: Int) < 9 then do
			startFunctional (read func :: Int) userName database
		else do
			putStrLn "\n\n** Incorrect selection **\n\n"
			mainMenu userName database
	else do
		if func == "exit" then
			exitIO database
		else do
			putStrLn "\n\n** Incorrect selection **\n\n"
			mainMenu userName database
		

exitIO :: [Film] -> IO ()
exitIO database = do
	writeFile "films.txt" (show database)
	putStrLn "Database saved press any key to exit"
	x <- getLine
	return ()
	return () 
	return () -- breaks out of the function

isEmpty :: [Char] -> Bool
isEmpty [] = False
isEmpty _ = True

isANumber :: [Char] -> Bool 
isANumber [] = True 
isANumber (x:xs) = isDigit x && isANumber xs 


-- Function to initiate all functional programs

startFunctional :: Int -> String -> [Film] -> IO ()

startFunctional 1 userName database = do
	putStrLn "Please enter the film's title"
	title <- getLine
	putStrLn "Please enter the film's director"
	director <- getLine
	putStrLn "Please enter the film's release year"
	year <- getLine
	if title == "" || director == "" || year == "" then
		startFunctional 1 userName database
	else do
		if isANumber year then do
			putStrLn ("\n\n" ++ (giveFilms (addFilm title director (read year :: Int) database)) ++ "\n\n\n Press enter to return to the main menu")
		else do
			putStrLn "Year you entered was not a number"
			startFunctional 1 userName database
	x <- getLine
	
	-- Return to main menu
	mainMenu userName (addFilm title director (read year :: Int) database)

startFunctional 2 userName database = do
	putStrLn ("\n\n" ++ (giveFilms database) ++ "\n\n\n Press enter to return to the main menu")
	x <- getLine
	
	-- Return to main menu
	mainMenu userName database

startFunctional 3 userName database = do
	putStrLn "Please enter a director's name"
	director <- getLine
	if director == "" || giveFilmsByDirector director database == "" then do
		putStrLn "Director does not exist"
		startFunctional 3 userName database
	else do
		putStrLn ("\n\n" ++ (giveFilmsByDirector director database) ++ "\n\n\n Press enter to return to the main menu")
	x <- getLine
	
	-- Return to main menu
	mainMenu userName database

startFunctional 4 userName database = do
	if siteRating6OrHigher database == "" then
		putStrLn "There are no films rated 6 or higher in the database"
	else do
		putStrLn ("\n\nAll films with a website rating of 6 or higher: \n\n" ++ (siteRating6OrHigher database) ++ "\n\n\n Press enter to return to the main menu")
	x <- getLine
	
	-- Return to main menu
	mainMenu userName database

startFunctional 5 userName database = do
	putStrLn "Please enter a director's name"
	director <- getLine
	if director == "" then do
		startFunctional 5 userName database
	else do
		if avgRatingsForDirector director database == "NaN" || avgRatingsForDirector director database == "" then
			putStrLn "Director does not exist \n\n\n Press enter to return to the main menu"
		else do
			putStrLn ("\n\nAverage website rating: " ++ (avgRatingsForDirector director database) ++ "\n\n\n Press enter to return to the main menu")
	x <- getLine
	
	-- Return to main menu
	mainMenu userName database

startFunctional 6 userName database = do
	if filmsUserHasRated userName database == "" then
		putStrLn "\n\nYou have not rated any films \n\n\n Press enter to return to the main menu"
	else do
		putStrLn ("\n\n" ++ (filmsUserHasRated userName database) ++ "\n\n\n Press enter to return to the main menu")
	x <- getLine

	-- Return to main menu
	mainMenu userName database

startFunctional 7 userName database = do
	putStrLn "\n\n Not completed \n\n Press enter to return to the main menu"
	x <- getLine
	
	-- Return to main menu
	mainMenu userName database	


startFunctional 8 userName database = do
	putStrLn "Please enter a year"
	year <- getLine
	if isANumber year && year /= "" then
		if filmsReleasedInYear (read year :: Int) database == "" then do
			putStrLn "No films released on or after that date"
		else do
			putStrLn ("\n\n ** NOT SORTED BY RATING ** \n\n" ++ (filmsReleasedInYear (read year :: Int) database) ++ "\n\n Press enter to return to the main menu")
	else do
		putStrLn "\n\nEnsure your entry is a number\n\n"
		startFunctional 8 userName database
	x <- getLine
	
	-- Return to main menu
	mainMenu userName database	
