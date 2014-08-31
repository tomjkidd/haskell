main = do
    contents <- getContents
    print (sumFile contents)
  where sumFile = sum . map read . words
  
{- Chapter 8 opens with this file. Here it mentions that String is being used as default type for reading the provided file.

Breaking it down, String is a list of Char values, [Char].
Each element of a list is allocated individually.
Each element has bookkeeping overhead.
These factors affect memory consumption and performance.

The bytestring library provides a fast, cheap alternative to the String type.
The library supplies two modules, Data.ByteString and Data.ByteString.Lazy
Data.ByteString defines a strict type ByteString, single array
Data.ByteString.Lazy defines a lazy type, also ByteString, represented as a list of chunks, arrays of up to 64 KB in size.

Use the lazy version for streaming large quantities of data. The strict version is for applications less concerned with memory footprint or that need random access.
-}