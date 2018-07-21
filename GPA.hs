--
-- GPA.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module GPA where

import System.Environment (getArgs)
import Safe (headMay, readMay)
import System.Exit (exitFailure)
import System.IO
import System.Directory (doesFileExist)

data GMod = Minus
          | Plus
          | Flat
data Grade = A
           | B
           | C
           | D
           | F

type Scoring = (Grade, GMod) -> Float

type ModScoring = GMod -> Float
type BaseScoring = Grade -> Float


assignedScoring :: (Float, Float, Float, Float, Float) -> BaseScoring
assignedScoring (a,b,c,d,f) A = a
assignedScoring (a,b,c,d,f) B = b
assignedScoring (a,b,c,d,f) C = c
assignedScoring (a,b,c,d,f) D = d
assignedScoring (a,b,c,d,f) F = f

basicMod :: Float -> ModScoring
basicMod f Minus = -f
basicMod f Plus = f
basicMod f Flat = 0

standardGPABase :: BaseScoring
standardGPABase = assignedScoring (4,3,2,1,0)

standardGPAMod :: ModScoring
standardGPAMod = basicMod 0.3

independentScoring :: BaseScoring -> ModScoring -> Scoring
independentScoring base mod (grade,gmod) = (base grade) + (mod gmod)

standardGPAScoring = independentScoring standardGPABase standardGPAMod

defaultScoring = standardGPAScoring

parseLetter :: Char -> Maybe Grade
parseLetter 'A' = Just A
parseLetter 'B' = Just B
parseLetter 'C' = Just C
parseLetter 'D' = Just D
parseLetter 'F' = Just F
parseLetter _ = Nothing

parseMod :: Char -> Maybe GMod
parseMod '+' = Just Plus
parseMod '-' = Just Minus
parseMod _ = Nothing

parseGrade :: String -> Maybe (Grade,GMod)
parseGrade [lt] = do
  g <- parseLetter lt
  return (g,Flat)
parseGrade [lt,md] = do
  g <- parseLetter lt
  gm <- parseMod md
  return (g,gm)
parseGrade _ = Nothing

parseRest :: [String] -> Maybe (Float, Bool)
parseRest [] = return (1.0, False)
parseRest ["!"] = return (1.0, True)
parseRest [ch] = do
  chf <- (readMay ch :: Maybe Float)
  return (chf, False)
parseRest [ch,"!"] = do
  chf <- (readMay ch :: Maybe Float)
  return (chf, True) 
parseRest _ = Nothing

parseGPAs :: String -> Maybe (Grade, GMod, Float, Bool)
parseGPAs str =
  let
    ws = words str
  in do
    grade <- headMay ws
    (g,gm) <- parseGrade grade
    (hs,maj) <- parseRest (tail ws)
    return (g,gm,hs,maj)

score :: Scoring -> (Grade, GMod, Float, a) -> (Float, Float, a)
score f (x,y,z,w) = (z*(f (x,y)), z, w)

summify :: [(Float, Float, Bool)] -> (Float, Float, Float, Float)
summify [] = (0,0,0,0)
summify ((x,d,True):xs) =
  let
    (f,g,n,m) = summify xs 
  in
    (f+x,g+x,n+d,m+d)
summify ((x,d,False):xs) =
  let
    (f,g,n,m) = summify xs 
  in
    (f+x,g,n+d,m)

average :: [(Float,Float,Bool)] -> (Float, Float)
average = (\(f,g,n,m) -> (f/n,g/m)) . summify

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x:xs) = case f x of
  Nothing ->
    filterMap f xs
  Just b ->
    b:(filterMap f xs)

computeGPAs :: String -> (Float, Float)
computeGPAs = average . map (score defaultScoring) . filterMap parseGPAs . lines

main :: IO ()
main = do
  args <- getArgs
  let filename = headMay args :: Maybe String
  contents <-
    case filename of
      Just fp ->
        do
          ex <- doesFileExist fp
          if ex
            then withFile fp ReadMode hGetContents
            else do
              hPutStrLn stderr "Error file does not exist"
              exitFailure

      Nothing ->
        getContents
  let (gpa,mgpa) = computeGPAs contents
  putStrLn $ "GPA: " ++ (show gpa)
  putStrLn $ "Major GPA: " ++ (show mgpa)
  

