import qualified Data.ByteString.Lazy as L
{- The qualified import allows us to provide our own name for a module.
L.take makes it clear that we want to use take from the 
Data.ByteString.Lazy module.

By using the qualified import, we can choose to load a different module
in order to do benchmark testing when trying to figure out which module
provides better specs for the problem at hand.
-}

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content  = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]
    
isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path -- Reads chunks of up to 64KB at a time
    return (hasElfMagic content)