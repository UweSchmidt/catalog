{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}

module Automaton.GenDot where

import Automaton.Types (DFA', NFA', I, Q, Automaton(A))

import Data.List       (intercalate)
import Data.Set.Simple
import Data.Map.Simple

import Text.Utils

-- ----------------------------------------

genDotDFA :: (GenDotAttr a) =>
             String ->
             DFA' Q a -> String
genDotDFA name 
  = unlines .
    genDotAutomaton genDotDeltaDFA name

genDotDeltaDFA :: Set Q -> Set I -> (Q -> I -> Maybe Q) -> Prog
genDotDeltaDFA qs is delta
  = genDotDD (foldMap (\ q -> [(q, delta1 q)]) qs)
  where
    delta1 q
      = toListMap $ foldr (\ (i, q') m -> insertMap q' [i] m) emptyMap ips
      where
        ips = foldMap (\ i -> case delta q i of
                        Nothing -> []
                        Just q' -> [(i, q')]
                      ) is

    genDotDD ::[(Q, [(Q, [I])])] -> Prog
    genDotDD qmap
      = concatMap genDot1 qmap
      where
        genDot1 (q, qis)
          = concatMap genDot11 qis
          where
            genDot11 (q1, is')
              = pr (show q ++ " -> " ++ show q1 ++ " [label=\"" ++ genDotInterval is' ++ "\"];")

genDotNFA :: (GenDotAttr a) =>
             String ->
             NFA' Q a -> String
genDotNFA name 
  = unlines .
    genDotAutomaton genDotDeltaNFA name

genDotDeltaNFA :: Set Q -> Set I -> (Q -> Maybe I -> Set Q) -> Prog
genDotDeltaNFA qs is delta
  = genDotDN (foldMap (\ q -> [(q, delta1 q)]) qs)
    ++ genDEps deltaEps
  where
    delta1 q
      = toListMap $ foldr (\ (i, q') m -> insertMap q' [i] m) emptyMap ips
      where
        ips = foldMap ( \ i -> case delta q (Just i) of
                                qs' | isEmpty qs' -> []
                                    | otherwise   -> [(i, qs')]
                      ) is

    
    deltaEps
      = foldMap (\ q -> eqs q (delta q Nothing)) qs 
      where
        eqs q' qs'
          | isEmpty qs' = []
          | otherwise   = [(q', qs')]
                          
    genDotDN ::[(Q, [(Set Q, [I])])] -> Prog
    genDotDN qmap
      = concatMap genDot1 qmap
      where
        genDot1 (q, qsis)
          = concatMap genDot11 qsis
          where
            genDot11 (qs1, is')
              = foldMap genDot111 qs1
              where
                genDot111 q1
                  = pr (show q ++ " -> " ++ show q1 ++ " [label=\"" ++ genDotInterval is' ++ "\"];")

    genDEps
      = concatMap genCase'
      where
        genCase' (q', qs')
          = foldMap genCase'' qs'
          where
            genCase'' q1'
              = pr (show q' ++ " -> " ++ show q1'
                    ++ " [label=\"e\", fontname=Symbol, fontcolor=red];")

genDotAutomaton :: (GenDotAttr a) =>
                   (Set Q -> Set I -> delta -> Prog) ->
                   String ->
                   Automaton delta Q a -> Prog
genDotAutomaton genEdges name (A qs is _q0 fs delta attr)
  = ( pr ("digraph " ++ name ++ " {")
      +> [ "rankdir=LR;"
         , "fondname=" ++ fontName ++ ";"
         , "fontsize=" ++ show fontSizeG ++ ";"
         ]
      ++ nl
      ++ ( "node " ++> [ "[fontname=" ++ fontName
                       , ",fontsize=" ++ show fontSizeG
                       , ",fixedsize=false"
                       , ",style=filled"
                       , ",color=" ++ color
                       , ",fillcolor=" ++ fillColor
                       , "];"
                       ]
         )
      ++ nl
      ++ ( "edge " ++> [ "[fontname=" ++ fontName
                       , ",fontsize=" ++ show fontSizeE
                       , ",color=" ++ color
                       , "];"
                       ]
         )
      ++ nl
      ++ genNodes qs
      ++ nl
      ++ genEdges qs is delta
      ++ nl
      ++ ( "Start " ++> [ "[width=.1"
                        , ",shape=plaintext"
                        , ",label=\"\""
                        , ",fillcolor=white"
                        , ",color=white"
                        , "];"
                        ]
         )
      ++ pr "Start -> 1;"
    )
    ++ pr "}"
  where
    genNodes qs'
      = foldMap genNode qs'
      where
        genNode q
          = pr (show q ++ " [shape=circle" ++ genLabel ++ genCircle ++ "];")
          where
            genCircle
              | q `member` fs
                  = ", peripheries=2"
              | otherwise
                  = ""
            genLabel
              = ", label=\"" ++ genDotAttr (attr q) ++ "\""
        
    fontName  = "Courier"
    fontSizeG = 18::Int
--  fontSizeN = 18::Int
    fontSizeE = 14::Int
    color     = "steelblue"
    fillColor = "lightgrey"

-- --------------------

class GenDotAttr a where
  genDotAttr :: a -> String

instance GenDotAttr () where
  genDotAttr = const ""

instance GenDotAttr Q where
  genDotAttr = show

instance GenDotAttr q => GenDotAttr (Set q) where
  genDotAttr = intercalate "," . foldMap (\ q -> [genDotAttr q])

instance (GenDotAttr a, GenDotAttr b) => GenDotAttr (a, b) where
  genDotAttr (x1, x2)
    | null s1   = s2
    | null s2   = s1
    | otherwise = s1 ++ "\\n" ++ s2
    where
      s1 = genDotAttr x1
      s2 = genDotAttr x2

instance GenDotAttr String where
  genDotAttr = id

-- --------------------

genDotInterval    :: [I] -> String
genDotInterval
    = quoteInterval . tail . init . show . formatInterval . interval
      where
      quoteInterval cs
          = concatMap quoteC cs
            where
            quoteC '\"' = "\\\""
            quoteC '\\' = "\\\\"
            quoteC c    = [c]
      formatInterval cs
          | cs == "."
              = "[.]"
          | length cs <= 1
              = cs
          | otherwise
              = "[" ++ cs ++ "]"
      interval (c1:c2:cs)
          | fromEnum c2 == fromEnum c1 + 1
              = c1 : '-' : interval' (c2:cs)
          | otherwise
              = c1 : interval (c2:cs)
      interval cs = cs

      interval' (c1:c2:cs)
          | fromEnum c2 == fromEnum c1 + 1
              = interval' (c2:cs)
      interval' (c1:cs)
          = c1 : interval cs
      interval' _cs
          = error "interval': illegal argument"
            
-- ----------------------------------------
