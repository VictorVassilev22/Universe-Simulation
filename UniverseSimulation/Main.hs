module Main where

import Particles

toIOParticle :: Particle -> IO [Particle]
toIOParticle p = return [p]

decideDecay :: IO Bool -> IO [Particle] -> IO [Particle] -> IO [Particle] --decides decay of particle
decideDecay isdecideed old new = do
  accept <- isdecideed
  if accept
    then new
    else old


particleDecay :: Particle -> IO [Particle]
particleDecay particle = decideDecay (decide (getChance particle)) (toIOParticle particle) (getDecay particle) --performes a particle dacay decision

getAt :: [Particle] -> Int -> IO Particle
getAt l idx = getElement l (return idx)

getAllParticleDecay :: [Particle] -> [IO [Particle]]
getAllParticleDecay = map particleDecay

foldParticleLists :: [IO [Particle]] -> IO [Particle]
foldParticleLists = foldr (+++) (return [])

(+++) :: IO [Particle] -> IO [Particle] -> IO [Particle]
ms1 +++ ms2 = do
    s1 <- ms1
    s2 <- ms2
    return $ s1 ++ s2

printSeq :: [Particle] -> Int -> IO String --pass list with only higgs to begin with
printSeq p index = do
  cp <- getAt p index
  sym <- getSymbol cp
  putStr " "
  putStr sym
  let len = length p
  if index >= len - 1
    then do
      line <- getLine
      if line == "stop"
        then return "finished!"
        else do
      x <- foldParticleLists (getAllParticleDecay p)
      printSeq x 0
    else printSeq p (index + 1)

start :: IO String
start = printSeq [higgsBozone] 0

startWith :: [Particle] -> IO String
startWith p = printSeq p 0

main :: IO String
main = do
  start