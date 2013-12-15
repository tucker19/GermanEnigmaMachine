{-# LANGUAGE GADTs, KindSignatures #-}
import Data.Char
import Data.List
import System.IO
-- Encoding
-- start all rotors at different positions through alphabet -> 3 rotors
-- save or print initial rotor positions
-- take in key press
-- step "right" rotor one step
-- step "middle" rotor two steps?
-- step "left" rotor one or two steps?
-- run key press through rotors
-- reflect key back through rotors
-- print encoded key press to file

-- Decode
-- read initial settings
-- set rotors to initial settings
-- read in file
-- for each letter step rotors
-- reflect letter
-- print out decoded file to screen

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z|SP 
				deriving(Eq,Ord,Show,Enum)

type Rotor = [Letter]
				-- deriving(Show)

final :: Rotor
final = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z]

rotor1 :: Rotor
rotor1 = [Q,A,Z,W,S,X,E,D,C,R,F,V,T,G,B,Y,H,N,U,J,M,I,K,O,L,P]

rotor2 :: Rotor
rotor2 = [P,O,I,U,Y,T,R,E,W,Q,L,K,J,H,G,F,D,S,A,M,N,B,V,C,X,Z]

rotor3 :: Rotor
rotor3 = [Y,G,V,T,F,C,R,D,X,E,S,Z,W,A,Q,P,L,O,K,M,I,J,N,U,H,B]

reflector :: Rotor
reflector = [O,P,Q,R,S,T,U,V,W,X,Y,Z,A,B,C,D,E,F,G,H,I,J,K,L,M,N]

runRotor :: Rotor->Letter->Letter
runRotor  r l = if l == SP
				then SP
				else r !! (fromEnum l)

(<>) :: Rotor->Rotor->Rotor
(<>) r1 r2 = 	[ runRotor r2 (runRotor r1 letter)
				| letter <- [A .. SP]
				]

rotateRotor :: Rotor->Rotor
rotateRotor (x:xs) = (xs ++ [x])

convertChartoLetter :: Char->Letter
convertChartoLetter x = if isSpace x == True
			then do
					let y = fromEnum x-6
					toEnum y::Letter
			else do
					let y = fromEnum x-65
					toEnum y::Letter

convertLettertoChar :: Letter->Char
convertLettertoChar x = if x == SP
			then do
					let y = fromEnum x+6
					toEnum y::Char
			else do
					let y = fromEnum x+65
					toEnum y::Char

finalLetter :: Rotor->Letter->Letter
finalLetter r1 l = case elemIndex l r1 of
					Just n-> (runRotor final (toEnum n::Letter))
					Nothing->SP

indexTuple :: Rotor->Letter->(Int,Int)
indexTuple r l = case elemIndex l r of
					Just n -> (n,fromEnum l)
					Nothing -> (27,27) 

swap :: (Int,Int)->(Int,Int)
swap (a,b) = (b,a)

tupleToLetter :: (Int,Int)->Letter
tupleToLetter (a,b) = toEnum b::Letter

flipRotor :: Rotor->Rotor
flipRotor r = do
				let q   = map (indexTuple r) r
				let q'  = map swap q
				let q'' = sort q'
			  	map tupleToLetter q''

encodeEnigmaLoop :: Rotor->Rotor->Rotor->Rotor->[Letter]->[Letter]->[Letter]
encodeEnigmaLoop r1 r2 r3 ref ls1 ls2 = case ls1 of
								[] -> ls2
								_  -> do
										let l 		= head ls1
										let ls1' 	= drop 1 ls1
										-- let r1' 	= rotateRotor r1 -- should rotate here but will not work
										let nl  	= runRotor (r1<>r2<>r3<>ref<>r3<>r2<>r1) l
										let fin 	= finalLetter r1 nl
										let ls2' 	= ls2 ++ [fin]
										encodeEnigmaLoop r1 r2 r3 ref ls1' ls2'

decodeEnigmaLoop :: Rotor->Rotor->Rotor->Rotor->[Letter]->[Letter]->[Letter]
decodeEnigmaLoop r1 r2 r3 ref ls1 ls2 = case ls1 of
								[] -> ls2
								_  -> do
										let l 		= head ls1
										let ls1' 	= drop 1 ls1
										-- let r1' 	= rotateRotor r1 -- should rotate her but will not work
										let r1'		= flipRotor r2
										let r2'		= flipRotor r3
										let r3'		= flipRotor ref
										let r2''	= flipRotor r1
										let nl  	= runRotor (r1'<>r2'<>r3'<>r2'<>r1'<>r2''<>r1') l
										let fin 	= finalLetter r1' nl
										let ls2' 	= ls2 ++ [fin]
										decodeEnigmaLoop r1 r2 r3 ref ls1' ls2'

encode :: [Char]->String
encode str = do
				let a = map convertChartoLetter str
				let blah = encodeEnigmaLoop rotor1 rotor2 rotor3 reflector a [] -- may need to change this function
				map convertLettertoChar blah

decode :: [Char]->String
decode str = do
				let a = map convertChartoLetter str
				let blah = decodeEnigmaLoop rotor1 rotor2 rotor3 reflector a [] -- may need to change this function
			 	map convertLettertoChar blah

encodeDecode :: String->String->IO()
encodeDecode str1 str2 = if or (map (head str1 ==) ['e','E','d','D'])
							then if head str1 == 'e'|| head str1 == 'E'
								 then do
										let str2'  = map toUpper str2
										let str2'' = encode str2'
										putStrLn ("Encoded message: " ++ str2'')
								 else do
										let str2'  = map toUpper str2
										let str2'' = decode str2'
										putStrLn ("Decoded message: " ++ str2'')
							else do
									putStrLn "Please enter encode or decode"
									xs <- getLine
									encodeDecode xs str2

loop :: String->String->IO()
loop str1 str2 = do
					encodeDecode str1 str2
					putStrLn "Would you like to encode or decode again?"
					xs <- getLine
					let xs' = map toLower xs
					if xs == "yes"
					then do
						putStrLn "Are you encoding or decoding?"
						str3 <- getLine
						putStrLn "What is the message you are trying to encode or decode?"
						ys <- getLine
						loop str3 ys
					else
						return ()

main :: IO()
main = do
	putStrLn "German Enigma Machine"
	putStrLn "Are you encoding or decoding?"
	xs <- getLine
	putStrLn "What is the message you are trying to encode or decode?"
	ys <- getLine
	loop xs ys

{- 
*All test code/ code that I changed to be used above. Samples of a single rotor and reflector for rotation. 
*Rotation will not work based on this implementation. Must have have a tuple format that pulls in input for left side
	then matches onto the right side. Then it passes the index value to the next tuple rotor.

encodeRotatingEnigma :: Rotor->Rotor->[Letter]->[Letter]->[Letter]
encodeRotatingEnigma r1 ref ls1 ls2 = case ls1 of
								[]->ls2
								_ -> do	
										let l 		= head ls1
										let ls1' 	= drop 1 ls1
										let r1' 	= rotateRotor r1
										let nl  	= runRotor (r1'<>ref<>r1') l
										let fin 	= finalLetter r1' nl
										let ls2' 	= ls2 ++ [fin]
										encodeRotatingEnigma r1' ref ls1' ls2'

decodeRotatingEnigma :: Rotor->Rotor->[Letter]->[Letter]->[Letter]
decodeRotatingEnigma r1 ref ls1 ls2 = case ls1 of
								[]->ls2
								_ -> do
										let l 		= head ls1
										let ls1' 	= drop 1 ls1
										let r1' 	= rotateRotor r1
										let r1''	= flipRotor ref
										let ref'	= flipRotor r1'
										let nl  	= runRotor (r1''<>ref'<>r1'') l
										let fin 	= finalLetter r1' nl
										let ls2' 	= ls2 ++ [fin]
										decodeRotatingEnigma r1' ref ls1' ls2'

enigmaLoop :: Rotor->Rotor->Rotor->Rotor->Rotor->Int->Int->Int->[Letter]->[Letter]->[Letter]
enigmaLoop r1 r2 r3 r4 f i1 i2 i3 ls1 ls2 = case ls1 of
												[] -> ls2
												_ -> case i1 >= 25 of
-}