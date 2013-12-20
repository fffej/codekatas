module CSProj where

import Text.XML.Light


data CSProj = String

exampleFile :: String
exampleFile = "c:/c/CurrentProjects/SQLTest/Branches/Trunk/Source/Engine/Engine.csproj"


tryParse :: String -> Either String CSProj
tryParse = undefined
