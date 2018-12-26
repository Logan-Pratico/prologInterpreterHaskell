
import Data.List
import Data.Char
import Text.Parsec
import Text.Printf
import Text.Read
import Text.ParserCombinators.Parsec.Combinator
import qualified Text.ParserCombinators.Parsec.Token as Token



type Groups = [String]

type Permutations = [(String, [Int])]

type CorrectAnswers = [[Char]]

type Format = (Groups, Permutations, CorrectAnswers)

type GivenAnswers = (Groups, Groups, [Int])

type SParsec = Parsec String ()

main = do    
    putStrLn "Welcome to the Prolog interpreter!"
    putStrLn "This interpreter was created by Logan Pratico as the final assignment for CS2104 at the National University of Singapore."
    putStrLn "To get started, type a command below. For a list of commands that work with the provided file (familyTree.pl) type -h"
    putStrLn "Type 'q' to quit"
    putStrLn " "
    runi

runi = do
    --f <- getLine
    dd <- getLine
    --d <- getLine
    ff <- read_file "familyTree.pl"
    if dd == "q" 
        then putStrLn "Goodbye." 
        else if dd == ""
             then do
                   putStrLn "false."
                   runi
             else if dd == "-h"
                  then do
                        putStrLn "type male(X) to return a list of males in familyTree.pl\ntype male(person) to see if the the person listed is a male\nseparate commands with ',' to represent AND or ';' to represent OR\n "
                        runi
                  else if (returnLast dd) /= '.'
                       then do
                             putStrLn "Error: missing terminating character on command"
                             runi
                       else if (checkAdd (run anotherAnd dd) == "FALSE")
                            then do
                                  checkCmd (organize ( giveItAShot  (combine (run another dd) (run parseTestd dd)) [])) ff dd
                                  runi
                            else do
                                  putStrLn $ calcu (run parseTestn (head(run parseTestdAnd dd)))
                                  runi

calcu :: [String] -> String
calcu (x:y:z) = 
                 if isLetter (head x) && isUpper (head x)
                 then show $ (read (head z) :: Integer) - (read y :: Integer)
                 else if isLetter (head y) && isUpper (head y)
                      then show $ (read (head z) :: Integer) - (read x :: Integer)
                      else if isLetter (head (head z)) && isUpper (head (head z))
                           then show $ (read x :: Integer) + (read y :: Integer)
                           else if (read x :: Integer) + (read y :: Integer) == (read (head z) :: Integer)
                                then "true"
                                else "false"
checkAdd :: [String] -> String
checkAdd input = 
                 if(head input) == "add"
                 then "TRUE"
                 else "FALSE"

checkCmd ((x,ys):xs) ff dd = 
                        if checkUpper(ys) == "True"
                        then if tail(ys) == []
                             then printOnce $ passedVarz (head(organize (  giveItAShot  (combine (run another dd) (run parseTestd dd)) []))) (organize ( giveItAShot ff [])) ["false."]
                             else printOnce $ passedVarz (head(organize (  giveItAShot  (combine (run another dd) (run parseTestd dd)) []))) (organize ( giveItAShot ff [])) ["false."]
                        else putStrLn $ andOrOr (organize (giveItAShot ff [])) ff dd

passedVarz :: (String, [String]) -> [(String, [String])] -> [String] -> [String]
passedVarz (n,p) ((x,y):xs) ys = 
                              if n == x && xs /= []
                              then passedVarz (n,p) (xs) ((compareNames p y [] ) ++ ys)
                              else if x /= n && ((xs) == [])
                                   then ys
                                   else if x /= n && xs /= []
                                        then passedVarz (n,p) (xs) ys
                                        else (compareNames p y [] ) ++ ys

compareNames :: [String] ->  [String] ->[String] -> [String]
compareNames (x:xs) (y:ys) mayb =
                            if ys == [] && x /= y && isUpper(head x)
                            then mayb ++ [y]
                            else if ys == []
                                 then if x /= y && isLower (head x)
                                      then []
                                      else mayb
                                 else if x /= y && isUpper(head x)
                                      then compareNames xs ys (mayb ++ [y])
                                      else if x /= y && isLower(head x)
                                           then []
                                           else compareNames xs ys mayb
                        
                        
checkUpper :: [String] -> String
checkUpper (x:xs) = 
                     if xs == []
                     then if isUpper (head x)
                          then "True"
                          else "False"
                     else if isUpper(head x)
                          then "True"
                          else checkUpper xs

combine :: [String] -> [String] -> [String]
combine xs ys =
                   let aux (n:ns) (p:ps) ts =
                                               if ns == []
                                               then ts ++ [n ++ "(" ++ p ++ ")" ++ "."]
                                               else aux ns ps (ts ++ [n++ "(" ++ p ++ ")" ++ "."])
                   in aux xs ys []
                        
printOnce (x:xs) = 
             if xs == []
             then putStrLn x
             else do 
                   putStrLn x
                   getLine                   
                   printOnce xs
                   
andOrOr :: [(String, [String])]  -> [String] -> String -> String
andOrOr ff' ff dd =
                        if (runc parseTests dd) == "AND"
                        then checkIts ff' ( (organize ( giveItAShot  (combine (run anotherAnd dd) (run parseTestdAnd dd)) [])))
                        else checkItOr ff' ( (organize ( giveItAShot  (combine (run another dd) (run parseTestd dd)) [])))
                   

passedVar :: (String, [String]) -> [(String, [String])] -> [String] -> [String]
passedVar (n,p) ((x,y):xs) ys = 
                              if n == x && xs /= []
                              then passedVar (n,p) (xs) (y ++ ys)
                              else if x /= n && ((xs) == [])
                                   then ys
                                   else if x /= n && xs /= []
                                        then passedVar (n,p) (xs) ys
                                        else y ++ ys

checkIts :: [(String, [String])] -> [(String, [String])] -> String
checkIts xs (y:ys) = 
                      if ys == []
                      then checkIt xs y
                      else if (checkIt xs y) == "false."
                           then "false."
                           else checkIts xs ys
                           
checkItOr :: [(String, [String])] -> [(String, [String])] -> String
checkItOr xs (y:ys) = 
                      if ys == []
                      then checkIt xs y
                      else if (checkIt xs y) == "true."
                           then "true."
                           else checkIts xs ys

           
checkIt :: [(String, [String])] -> (String, [String]) -> String
checkIt (xs) dd = 
                  let aux ((n,ys):zs) (a, bs) = 
                                              if (zs == []) && ( (n /= a) || ((getListAns ys bs) == 0))
                                              then "false." 
                                              else if n == a && ((getListAns ys bs) == 1)
                                                   then "true."
                                                   else checkIt zs (a,bs)
                  in aux xs dd
                  
organize :: [[String]] -> [(String, [String])]
organize input = 
                 let aux (x:xs) ys = 
                                     if xs == []
                                     then ys ++ [(head x,(tail x))]
                                     else aux xs (ys ++ [(head x,(tail x))])
                 in aux input []

                  
getListAns :: [String] -> [String] -> Int
getListAns (x:xs) (y:ys) = 
                            if xs == [] && ys == [] && x == y
                            then 1
                            else if (x /= y) || (xs == []) || (ys == []) 
                                 then 0
                                 else getListAns xs ys



giveItAShot :: [String] -> [[String]] -> [[String]]
giveItAShot (x:xs) ys = 
                     if xs == []
                     then if runEq parseEq x == "EQUALSIGN"
                          then ys
                          else ys ++ [parseCommas(run parseQt x) []]
                     else if runEq parseEq x == "EQUALSIGN"
                          then giveItAShot xs (ys)
                          else giveItAShot xs (ys ++ [parseCommas(run parseQt x) []])

parseCommas :: [String] -> [String] -> [String]
parseCommas (x:xs) ys = 
                         if xs == []
                         then ys ++ run parseComm x
                         else parseCommas xs (ys ++ run parseComm x)
                         
                         
parseComm :: SParsec [String]
parseComm = commaSep $ parseCom

parseCom :: SParsec String
parseCom = many1(alphaNum <|> space)
    

parseEq :: SParsec String
parseEq = manyTill (alphaNum <|> char '(' <|> char ')' <|> char '[' <|> char ']' <|> char ',' <|> space <|> char ';') (char '.')             

runEq :: SParsec String -> String -> String
runEq p input
            = case (parse p "" input) of
                Left err -> do{ "EQUALSIGN"
                               }
                Right x ->     "NOEQUALSIGN"

                     
another :: SParsec [String]
another =  (try (semiSep sss)) <|> (try (commaSep sss)) 

anotherAnd :: SParsec [String]
anotherAnd =  (try (commaSep sss)) <|> (try (semiSep sss))

sss :: SParsec String
sss =  ((many(alphaNum <|> space))) <* ((parseParen $ many (alphaNum <|> char ',' <|> space)))

parseTestd :: SParsec [String]
parseTestd = (try(semiSep aprs)) <|> (try (commaSep aprs)) 

parseTestn :: SParsec [String]
parseTestn =  commaSep aprt

parseTestdAnd :: SParsec [String]
parseTestdAnd = (try (commaSep aprs)) <|> (try(semiSep aprs))

parseTests :: SParsec [String]
parseTests = commaSep testOrAnd

testOrAnd :: SParsec String
testOrAnd = manyTill (alphaNum <|> char '(' <|> char ')' <|> char '[' <|> char ']' <|> char ',' <|> space) (try(char '.'))

aprs :: SParsec String
aprs = ( (many(alphaNum <|> space))) >> ((parseParen $ many (alphaNum <|> char ',' <|> space))) 

aprt :: SParsec String
aprt = ( many (alphaNum <|> space))

read_file :: FilePath -> IO [String] 
read_file x =
  do
    content <- readFile x
    let llcontent = lines content
        lz = length llcontent
     in do
          return llcontent
          

runc :: SParsec [String] -> String -> String
runc p input
            = case (parse p "" input) of
                Left err -> do{ "OR"
                               }
                Right x ->     "AND"

run :: SParsec [String] -> String -> [String]
run p input
            = case (parse p "" input) of
                Left err -> do{ [show err]
                               }
                Right x -> x

runo :: SParsec String -> String -> String
runo p input
            = case (parse p "" input) of
                Left err -> do{ show err
                               }
                Right x -> x                  
  
        
whitespaces :: SParsec [Char]
whitespaces = many (tab <|> space)



comma = whitespaces >> char ',' >> whitespaces

colon = whitespaces >> char ':' >> whitespaces

semiColon = whitespaces >> char ';' >> whitespaces
paren = whitespaces >> char '(' >> whitespaces
par = whitespaces >> char ')' >> whitespaces
bracket = whitespaces >> ((char '[') <|> (char '(')) >> whitespaces

parseSqBr :: SParsec a -> SParsec a
parseSqBr x = between (char '[') (char ']') x

parseParen :: SParsec a -> SParsec a
parseParen x = between (char '(') (char ')') x

parseCrBr :: SParsec a -> SParsec a
parseCrBr x = between (char '{') (char '}') x

parseQuote :: SParsec a -> SParsec a
parseQuote x = between (char '"') (char '"') x

semiSep p  = p `sepBy` semiColon


commaSep p  = p `sepBy` comma
parSep p = p `sepBy` par


colonSep p = p `sepBy` colon

parenSep p  = p `sepBy` paren

bracketSep p = p `sepBy` bracket

oparen :: SParsec [Char]
oparen = many ((char '(') <|> (char ')'))

commas :: SParsec [Char]
commas = many (char ',')

parseAto :: SParsec String
parseAto = many (lower <|> digit)

parseVar :: SParsec String
parseVar = many (upper <|> lower) --start with upper only

parseQt :: SParsec [String]
parseQt =  bracketSep $ (try parseLa) <|> (try parseDa) <|> (try parseV)

parseLan :: SParsec [[String]]
parseLan = (commaSep $ parseQt)  -- differentiate between and & or


-- If is uppper or _ for first char only
parseV :: SParsec String
parseV = many1 (letter <|> space <|> char ',')


parseLa :: SParsec String
parseLa = (parseSqBr $ parseV) 

parseDa :: SParsec String
parseDa = (parseParen $ parseV) 
----------------------------------------------------------------------------------------

returnLast :: Eq a => [a] -> a
returnLast (x:xs) = 
                 if xs == [] 
                 then x
                 else returnLast xs
