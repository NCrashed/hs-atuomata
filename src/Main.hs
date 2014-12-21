module Main where

import Data.List (foldl', nub, unzip3, sort)
import Data.Functor ((<$>))
import Control.Monad (guard, when, unless)

type State = Char
type Universe = [Char]
type InChar = Char
type Tape = [Char]
type FinStates = [State]

transitions::[(InChar,State,State)]
transitions= nub [
            ('c','1','1'),
            ('a','1','2'),
            ('b','1','4'),
            ('a','2','2'),
            ('b','2','5'),
            ('c','2','3'),
            ('a','4','3'),
            ('b','4','5'),
            ('c','4','4'),
            ('a','3','3'),
            ('b','3','5'),
            ('c','3','3'),
            ('a','5','5'),
            ('b','5','5'),
            ('c','5','5')
            ]
            

step::State -> InChar->State
step s a = (\(_,_,x)->x) . head . filter (\(x,y,_) -> (x == a) && (y==s)) $ transitions

tape::Tape
tape = "ccaacacacb"

finstate::FinStates
finstate = ['3']

masCheck::(Eq a,Ord a)=>[a]->[a]->Bool
masCheck a b
  |length a /= length b = False
  |otherwise = and $ zipWith(==) (sort a) (sort b)

checker::[(InChar,State,State)]->Bool
checker trans = all stateValid states
  where
    states = let (_, a, b) = unzip3  trans
             in nub $ a ++ b
    alphabet = let (cs, _, _) = unzip3  trans
               in nub cs
    stateValid :: State -> Bool
    stateValid s = masCheck list alphabet
      where
        fst3 (x, _, _) = x
        snd3 (_, x, _) = x
        list = fst3 <$> filter ((==s).snd3) trans

main::IO()
main = do
  unless (checker transitions) $ return (error "Invalid table!")
  print $ (foldl' step '1' tape) `elem` finstate