
import Data.List
import Data.List.Split

-------------------------------------
-- remove substring
-- e.g. "bc" `removeFrom` "abcdeabcde" = "adeade"
removeFrom :: Eq a => [a] -> [a] -> [a]
removeFrom sublst lst = concat $ splitOn sublst lst

  
-------------------------------------
-- replace substring
-- e.g. replaceSublist "bc" "123" "abcdeabcde" = "a123deade"
replaceSublist :: Eq a => [a] -> [a] -> [a] ->[a]
replaceSublist oldlst newlst biglst = intercalate newlst $ splitOn oldlst biglst


-------------------------------------
-- is 'demolished' expression, a regular arithmetic expression 
-- odd length
-- +,-,*,/ at odd indexes
-- everything else at even indexes. Everything else - it can only be (0 or a,b,c .. z or some (sub)expression)
control :: [[Char]] -> Bool
control lst = cond1 && cond2 && cond3
  where cond1 = findIndices (`elem` ["+","-","*","/"]) lst == [1,3 .. length lst - 2] 
        cond2 = findIndices (`elem` (map (\x -> [x]) $ '0':['a' .. 'z'])) lst3 == [0,2 .. length lst - 1] 
        cond3 = odd $ length lst -- tricky! prevents even number of elements with some strange last element (like @#$&...)
        lst3 = map (\ x -> if length x > 1 then "a" else x) lst 


-------------------------------------
-- is 'demolished' expression, a regular multilication/division expression (without parentheses)
-- length 3 or bigger
-- *,/ at odd indexes
-- everything else at even indexes. Everything else - it can only be (0 or a,b,c .. z or some (sub)expression)
-- odd length
controlmd :: [[Char]] -> Bool
controlmd lst = cond1 && cond2 && cond3 && cond4
  where cond1 = length lst > 2
        cond2 = findIndices (`elem` ["*","/"]) lst == [1,3 .. length lst - 2] -- (-2) - tricky! it also prevents last string to be * or /
        cond3 = findIndices (`elem` [[t] | t <- ('0':['a' .. 'z'])]) lst3 == [0,2 .. length lst - 1]
        cond4 = length lst `mod` 2 /= 0
        lst3 = map (\ x -> if length x > 1 then "a" else x) lst


-------------------------------------
extractMultsDivs :: [[Char]] -> [[[Char]]]
extractMultsDivs lst = clearPrefix $ clearSuffix [t | t <- contsublists lst, controlmd t]
  where contsublists ls = [t | i <- inits ls, t <- tails i, not $ null t]


-------------------------------------
-- removes elements which are sufix of previous elements
clearSuffix :: Eq a => [[a]] -> [[a]]
clearSuffix lst = reverse . clear2 $ reverse lst
  where 
    clear2 [] = []
    clear2 [x] = [x]
    clear2 (x:y:xs) = if x `isSuffixOf` y then clear2 (y:xs) else x : clear2 (y:xs)
  

-------------------------------------
-- removes elements which are sufix of previous elements
clearPrefix :: Eq a => [[a]] ->[[a]]
clearPrefix [] = []
clearPrefix [x] = [x]
clearPrefix (x:y:xs) = if x `isPrefixOf` y then clearPrefix (y:xs) else x : clearPrefix (y:xs)


-------------------------------------
-- consecutive * and / parts of expression, will be encircled by parantheses
handlemd :: [[Char]] ->[[Char]]
handlemd lst = foldl (\ acc x -> replaceSublist x [concat $ "(" : x ++ [")"]] acc) lst (extractMultsDivs lst) 
  
  
--------------------------------------------------
-- rpn-style reordering
-- e.g. a*b/c*d/e*f -> ab*c/d*e/f*
reorder :: [[Char]] -> [[Char]]
reorder lst
  | even $ length lst = [errorString]
  | otherwise = head lst : (swap $ tail lst)
  where
    swap [] = []
    swap (x : y : xs) = y : x : swap xs


-------------------------------------
-- controlPrtss -- control parentheses
-- there is no "()"
-- number of '(' == number of ')'
-- all left substrings contain not less '('s than ')'s
controlPrtss :: [Char] -> Bool
controlPrtss str = cond1 && cond2 && cond3
  where cond1 = not $ "()" `isInfixOf` str
        cond2 = length (elemIndices '(' str) == length (elemIndices ')' str)
        cond3 = all (\ x -> length (elemIndices '(' x) >= length (elemIndices ')' x) ) $ inits str


-------------------------------------
-- returns first good parentheses substring
-- e.g. (a+b)+(c+d) -> (a+b)
takePss :: [Char] -> [Char]
takePss str = head goodstrings
  where goodstrings = filter controlPrtss $ map (\ x -> (1+x) `take` str) indexes
        indexes = elemIndices ')' str


-------------------------------------
-- returns 'important rpn-parts' from a string
demolish :: [Char] -> [[Char]]
demolish str
  | str == "" = []
  | otherwise = [] ++ [part] ++ demolish stail   
  where part = if x /= '(' then [x] else takePss str  
        stail = drop (length part) str
        x = head str
        

--------------------------------------------------
-- first delete whitespaces from expression (str->str2)
-- if expression is encircled by parentheses, end everything inside is OK - ignore the parentheses
-- if expression  begins with +, ignore that +
-- if expression  begins with -, prefix it with 0
errorString = "error - irregurar expression"

rpn :: [Char] -> [Char]
rpn [] = errorString
rpn (x:[])     = if x `elem` ['a' .. 'z'] then [x] else errorString
rpn str
  | all (`elem` "+-") $ take 2 str = errorString  -- tricky !
  | not $ controlPrtss str = errorString
  | head str2 == '(' && last str2 == ')' && controlPrtss middle = rpn middle
  | head str2 == '+' = rpn $ tail str2 
  | head str2 == '-' = rpn $ '0' : str2
  | otherwise = if "error" `isInfixOf` str8 then errorString else str8
  where str2 = " " `removeFrom` str
        middle = init $ tail str2
        lst4 = let lst3 = demolish str2 in if control lst3 then lst3 else [errorString]
        lst5 = reorder $ if controlmd lst4 then lst4 else handlemd lst4
        str7 = concat $ map (\x -> if length x == 1 then x else rpn x) lst5 
        str8 = if lst4 /= [errorString] && lst5 /= [errorString] then str7 else errorString


{-
-- usage examples:

rpn "x*(a*(b+c*(d-e)+f)-g)+y"   ( = "xabcde-*+f+*g-*y+" )
rpn "(-((a*(b+c-d*e))))+(f-g*h)"   ( = "0abc+de*-*-fgh*-+" )
rpn " - (a + b) / ( (-  c - d - e) ) * ( f-g*h / i+j) "   ( = "0ab+0c-d-e-/fgh*i/-j+*-" )

-}
