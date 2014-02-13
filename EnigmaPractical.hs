{-
Author: Adam Howell
Company: Student
Email: adam.dhowell.19@gmail.com
-}

{-# LANGUAGE GADTs, KindSignatures #-}
import Data.Char
import Data.List
import System.IO

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z|SP -- data type to make it easier to work with letters
				deriving(Eq,Ord,Show,Enum)

type SuperRotor = [(Letter,Letter)] -- rotor to combine a scrambled list of letters with a list of correctly listed letters

type Rotor = [Letter]

final :: Rotor -- rotor to determine the output letter
final = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,SP]

rotor1 :: SuperRotor -- SuperRotor with scrambled letter starting in the upper left corner of keyboard while going down and each column to the right after
rotor1 = [(Q,A),(A,B),(Z,C),(W,D),(S,E),(X,F),(E,G),(D,H),(C,I),(R,J),(F,K),
			(V,L),(T,M),(G,N),(B,O),(Y,P),(H,Q),(N,R),(U,S),(J,T),(M,U),(I,V),
			(K,W),(O,X),(L,Y),(P,Z),(SP,SP)]

rotor2 :: SuperRotor -- SuperRotor with scrambled letter starting in the upper right corner of keyboard while going across left and ever row after that
rotor2 = [(P,A),(O,B),(I,C),(U,D),(Y,E),(T,F),(R,G),(E,H),(W,I),(Q,J),(L,K),
			(K,L),(J,M),(H,N),(G,O),(F,P),(D,Q),(S,R),(A,S),(M,T),(N,U),(B,V),
			(V,W),(C,X),(X,Y),(Z,Z),(SP,SP)]

rotor3 :: SuperRotor -- SuperRotor with scrambled letter starting in the middle of keyboard while going down and each column to the right after
rotor3 = [(Y,A),(G,B),(V,C),(T,D),(F,E),(C,F),(R,G),(D,H),(X,I),(E,J),(S,K),
			(Z,L),(W,M),(A,N),(Q,O),(P,P),(L,Q),(O,R),(K,S),(M,T),(I,U),(J,V),
			(N,W),(U,X),(H,Y),(B,Z),(SP,SP)]

reflector :: SuperRotor -- SuperRotor with scrambled letter +13 in the alphabet of the position
reflector = [(O,A),(P,B),(Q,C),(R,D),(S,E),(T,F),(U,G),(V,H),(W,I),(X,J),(Y,K),
				(Z,L),(A,M),(B,N),(C,O),(D,P),(E,Q),(F,R),(G,S),(H,T),(I,U),(J,V),
				(K,W),(L,X),(M,Y),(N,Z),(SP,SP)]

runRotor :: Rotor->Letter->Letter -- function to take in rotor and letter, breaks input letter into a number and goes down the rotor that many spaces and outputs the letter there
runRotor  r l = r !! (fromEnum l)

(<>) :: Rotor->Rotor->Rotor -- function to allow combination of rotors into a bigger rotor
(<>) r1 r2 = 	[ runRotor r2 (runRotor r1 letter)
				| letter <- [A .. SP]
				]

rotateRotor :: Rotor->Rotor -- function to take the head of the rotor list and put it at the tail in order to "rotate" the rotor
rotateRotor (x:xs) = (xs ++ [x])

rotateSuperRotor :: SuperRotor->SuperRotor --function similar to rotateRotor but this is for SuperRotor
rotateSuperRotor (x:xs) = (xs ++ [x])

convertChartoLetter :: Char->Letter -- function to change a character into new data type letter
convertChartoLetter x = if isSpace x == True
			then do -- if character is space it is a different area of the Char numbering sstem
					let y = fromEnum x-6
					toEnum y::Letter
			else do -- Char numbering system for uppercase Chars is 'A' = 65 so thus must make it A = 0 and etc
					let y = fromEnum x-65
					toEnum y::Letter

convertLettertoChar :: Letter->Char -- function to change new data type letter into character 
convertLettertoChar x = if x == SP
			then do -- if Letter is SP need to change back into space or ' '
					let y = fromEnum x+6
					toEnum y::Char
			else do -- add 65 back to Letter to get Char
					let y = fromEnum x+65
					toEnum y::Char

finalLetter :: Rotor->Letter->Letter -- function to get position of output letter in the rotor and output the letter of the value of that position
finalLetter r1 l = case elemIndex l r1 of
					Just n-> (runRotor final (toEnum n::Letter))
					Nothing->SP

swap :: (Letter,Letter)->(Letter,Letter)
swap (a,b) = (b,a)

flipRotor :: SuperRotor->SuperRotor
flipRotor r = map swap r -- swap all entries in SuperRotor

searchRotorLoop :: Rotor->Rotor->Letter->Letter -- take in unchanged rotor and rotated rotor with a letter
searchRotorLoop r r' l = do 					-- get letter from head of rotor and check to see if that head and letter input match
							let rl = head r' 	-- if they do output the final letter
							case l == rl of		-- else rotate rotor and try again in next iteration
								True->finalLetter r l
								_->do
									let r'' = rotateRotor r'
									searchRotorLoop r r'' l

searchRotor :: SuperRotor->Letter->Letter 	-- take in a SuperRotor and letter
searchRotor sr l = do 						-- unzip SuperRotor into two seperate rotors
					let (r1,r2) = unzip sr 	-- run letter through the scrambled letters
					let nl = runRotor r1 l 	-- find the final output letter
					searchRotorLoop r2 r2 nl
{- 
	Needs fixing, when compiled with follor code get this error message:
		Couldn't match expected type `(Letter, Letter)'
                with actual type `Letter'
	    In the return type of a call of `searchRotor'
	    In the expression: searchRotor r2 (searchRotor r1 letter)
	    In the expression:
	      [searchRotor r2 (searchRotor r1 letter) | letter <- [A .. SP]]

	Don't know how to fix this.

(><) :: SuperRotor->SuperRotor->SuperRotor
(><) r1 r2 =	[searchRotor r2 (searchRotor r1 letter)
				| letter <- [A .. SP]
				]
-}

encodeEnigmaLoop :: SuperRotor->SuperRotor->SuperRotor->SuperRotor->[Letter]->[Letter]->Int->[Letter]
encodeEnigmaLoop r1 r2 r3 ref ls1 ls2 i = case ls1 of
								[] -> ls2
								_  -> do
										let l 		= head ls1
										let ls1' 	= drop 1 ls1
										let nl  	= searchRotor r1 l -- run letter through rotor and find it in the right side, would like to have this setup where I can combine all rotors into one function
										let nl'		= searchRotor r2 nl
										let nl''	= searchRotor r3 nl'
										let rfl		= searchRotor ref nl''
										let rfl'	= searchRotor r3 rfl
										let rfl''	= searchRotor r2 rfl'
										let fin 	= searchRotor r1 rfl''
										let ls2' 	= ls2 ++ [fin]
										if (i+1) > 54 -- if i is more than 54 then we need to rotate all rotors
										then do
												let [r1',r2',r3'] = map rotateSuperRotor [r1,r2,r3]
												encodeEnigmaLoop r1' r2' r3' ref ls1' ls2' (i+1)
										else if (i+1) > 27 -- if i is more than 27 we need to rotate the third and second rotors
											 then do
											 		let [r2',r3'] = map rotateSuperRotor [r2,r3]
													encodeEnigmaLoop r1 r2' r3' ref ls1' ls2' (i+1)
											 else do -- i<27 we still rotate 
													let r3' = rotateSuperRotor r3
													encodeEnigmaLoop r1 r2 r3' ref ls1' ls2' (i+1)

decodeEnigmaLoop :: SuperRotor->SuperRotor->SuperRotor->SuperRotor->[Letter]->[Letter]->Int->[Letter]
decodeEnigmaLoop r1 r2 r3 ref ls1 ls2 i = case ls1 of
								[] -> ls2
								_  -> do
										let l 					= head ls1
										let ls1' 				= drop 1 ls1
										let [r1',r2',r3',ref']	= map flipRotor [r1,r2,r3,ref] -- invert all rotors
										let nl  				= searchRotor r1' l -- run letter through rotor and find it in the right side
										let nl'					= searchRotor r2' nl
										let nl''				= searchRotor r3' nl'
										let rfl					= searchRotor ref' nl''
										let rfl'				= searchRotor r3' rfl
										let rfl''				= searchRotor r2' rfl'
										let fin 				= searchRotor r1' rfl''
										let ls2' 				= ls2 ++ [fin]
										if (i+1) > 54 -- if i is more than 54 then we need to rotate all rotors
										then do 
												let [r1',r2',r3'] = map rotateSuperRotor [r1,r2,r3]
												decodeEnigmaLoop r1' r2' r3' ref ls1' ls2' (i+1)
										else if (i+1) > 27 -- if i is more than 27 we need to rotate the third and second rotors
											 then do 
											 		let [r2',r3'] = map rotateSuperRotor [r2,r3]
													decodeEnigmaLoop r1 r2' r3' ref ls1' ls2' (i+1)
											 else do -- i<27 we still rotate
											 		let r3' = rotateSuperRotor r3
													decodeEnigmaLoop r1 r2 r3' ref ls1' ls2' (i+1)

getLetter :: (Letter,Letter)->Letter -- get the left letter of the tuple
getLetter (letter,_) = letter

initSetting :: Letter->SuperRotor->SuperRotor -- set rotors to the initial letter settings
initSetting l sr = do
						let sr1 = head sr
						let srl = getLetter sr1
						case l == srl of
							True-> sr 
							_	-> do
									let sr' = rotateSuperRotor sr
									initSetting l sr' 

encode :: String->String->String -- take in two strings and output a string
encode str1 str2 = do
					let a 		= map convertChartoLetter str1 -- convert the message into letter format
					let b 		= map convertChartoLetter str2 -- convert key letters into letter format
					let r1 		= initSetting (head b) rotor1 -- initial settings for rotors
					let r2 		= initSetting (head (drop 1 b)) rotor2
					let r3 		= initSetting (head (drop 2 b)) rotor3
					let r3' 	= rotateSuperRotor r3 -- rotate rotor closest to reflector before ever running encrypt
					let blah 	= encodeEnigmaLoop r1 r2 r3' reflector a [] 0 -- may need to change this function
					map convertLettertoChar blah

decode :: String->String->String -- take in two strings and output a string
decode str1 str2 = do
					let a 		= map convertChartoLetter str1 -- convert message into letter format
					let b 		= map convertChartoLetter str2 -- convert key letters into letter form
					let r1 		= initSetting (head b) rotor1 -- inital setting for rotors
					let r2 		= initSetting (head (drop 1 b)) rotor2
					let r3 		= initSetting (head (drop 2 b)) rotor3
					let r3' 	= rotateSuperRotor r3 -- rotate rotor closest to reflector before ever running decrypt
					let blah 	= decodeEnigmaLoop r1 r2 r3' reflector a [] 0 -- may need to change this function
				 	map convertLettertoChar blah

isAlphaLoop::String->String->String -- check to see if elements of a string are all char or spaces
isAlphaLoop str1 str2 = case str1 of
								[] -> str2
								_  -> do
										let l = head str1
										case isAlpha l of
											False 	-> case isSpace l of
															True 	-> do
																		let str1' = drop 1 str1
																		let str2' = str2 ++ [l]
																		isAlphaLoop str1' str2'
															_		-> do
																		let str1' = drop 1 str1
																		isAlphaLoop str1' str2
											_ 		-> do
														let str1' = drop 1 str1
														let str2' = str2 ++ [l]
														isAlphaLoop str1' str2'

encodeDecode :: String->String->String->IO() -- get strings ready for encoding or decoding
encodeDecode str1 str2 str3 = if or (map (head str1 ==) ['e','E','d','D'])
								then if head str1 == 'e'|| head str1 == 'E'
									 then do
											let str2'	= isAlphaLoop str2 []
											let str2''  = map toUpper str2
											let str3'	= map toUpper str3
											let str2''' = encode str2'' str3'
											putStrLn ("Encoded message: " ++ str2''')
									 else do
											let str2'	= isAlphaLoop str2 []
											let str2''  = map toUpper str2
											let str3' 	= map toUpper str3
											let str2''' = decode str2'' str3'
											putStrLn ("Decoded message: " ++ str2''')
								else do
										putStrLn "Please enter encode or decode"
										xs <- getLine
										encodeDecode xs str2 str3

check :: String->String->String->String->IO()
check str1 str2 str3 str4 = do
								if head str4 /= 'y' && head str4 /= 'n'
								then do 
										putStrLn "Please input yes or no."
										yn <- getLine 
										let yn' = map toLower yn
										check str1 str2 str3 yn
								else if head str4 == 'n'
									 then 
									 	loop str1 str2 str3 str4
									 else do
									 		putStrLn "Are you encoding or decoding?"
											ende <- getLine
											putStrLn "What is the message you are trying to encode or decode?"
											ys <- getLine
											putStrLn "What is the key letters?"
											zs <- getLine
											loop ende ys zs str4


loop :: String->String->String->String->IO() -- wrapper loop
loop str1 str2 str3 str4 = do
						if head str4 /= 'y' && head str4 /= 'n'
						then do 
								putStrLn "Please input yes or no."
								yn <- getLine 
								let yn' = map toLower yn
								loop str1 str2 str3 yn
						else if head str4 == 'n'
							 then do
							 		putStrLn "Thank you and goodbye"
									return ()
							 else if length str3 > 3
								  then do
								 		putStrLn "Please input a set of key letters or key word that is three long"
								 		key <- getLine
								 		loop str1 str2 key str4
								   else do 	  	
										encodeDecode str1 str2 str3
										putStrLn "Would you like to encode or decode again?"
										xs <- getLine
										let xs' = map toLower xs
										if head xs /= 'y' && head xs /= 'n'
										then
											check str1 str2 str3 xs'
										else do
												putStrLn "Are you encoding or decoding?"
												ende <- getLine
												putStrLn "What is the message you are trying to encode or decode?"
												ys <- getLine
												putStrLn "What is the key letters?"
												zs <- getLine
												loop ende ys zs xs'

main :: IO() -- main function
main = do
	putStrLn "German Enigma Machine"
	putStrLn "Are you encoding or decoding?"
	xs <- getLine
	putStrLn "What is the message you are trying to encode or decode?"
	ys <- getLine
	putStrLn "What is the key letters?"
	zs <- getLine
	loop xs ys zs "yes"
