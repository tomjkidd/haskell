module Notes where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))
{- Chapter 6 introduced some stuff toward the end that didn't fully
register with me, so I thought it would be beneficial to analyze each 
of the pieces created and to bring them all into the same file to easily work with each piece in ghci.

The starting point was the SimpleJSON module.
Here, the algebraic data type JValue is created to express the valid 
JSON pieces as type constructors (JString, JNumber, JBool, JNull, 
JObject, and JArray). I liked that JSON was chosen as the focus of a 
project because I am familiar with JSON.

Next, get functions for each constructor were provided to allow 
access to the underlying values used with each type constructor. An 
isNull function was created as a convenience as well.

PutJSON was then created to provide a simple mechanism to output the
JSON using JValues. The intercalate function from Data.List was used 
to simply enclose objects (with {}) and arrays (with []), while 
rendering each of the primitive types was done with the show function 
for Strings and Numbers and through simple Strings for Bool and Null 
values.

With the potentially cluttered output that PutJSON provides 
with it's putJValue function, Prettify.hs was created as a generic 
interface to pretty print things, and PrettyJSON was created to use 
Prettify to create pretty representations of JValues from SimpleJSON.
-}

{- JValue was originally in the SimpleJSON file. It provides the structured basis for the JSON types we want to handle -}

data JValue = JString String
           | JNumber Double
           | JBool Bool
           | JNull
           | JObject [(String, JValue)]
           | JArray [JValue]
              deriving (Eq, Ord, Show)

{- Doc was originally in the Prettify file. It provides the constructors that describe what a Doc (Document) is.

A key idea is that each of the Doc type constructors is meant to be 
combined or composed with the others. What this means is that when we 
go to render a Document, we want to be able to render each of these 
Doc type constructors to serialize a representation of the 
information (which for our current goal is the JSON representation of 
a JValue, which is composed of any valid combination that is allowed 
in JSON).

Empty, Char, Text, and line as the first few type constructors are 
straight forward, but I didn't understand the Concat and Union type 
constructors just through the algebraic data type definition.

Concat is used to define a new operator, (<>), that concatenates two 
Doc type constructors together. Collecting two Doc values into 
another Doc value was found to be helpful in order to organize the 
tree used to represent the more complicated JArray and JObject 
structures as Doc structures.

Union is used only by the compact/pretty functions to help display 
the Doc structure. The fsep function is the place where Unions are 
introduced into the Doc structure. The fsep function uses the 
softline function. The softline function calls group with a line.
Here, the group function creates a Union which stores the flattened 
line as the first argument and the whole line as the second argument.

When a compact structure is desired to represent the Doc structure, 
the second argument of the Union is used as the representation, which 
just puts all the details together. When the pretty structure is 
desired, both the first and second arguments are analyzed to see if 
the can both be fit together on the same line. If so, then will, but 
if not, they will be split up (determined by the nicest function).
 -}
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)
           
empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other   = other

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) = 
            case d of
                Empty -> transform ds
                Char c -> c : transform ds
                Text s -> s ++ transform ds
                Line -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) = 
                case d of
                    Empty -> best col ds
                    Char c -> c : best (col + 1) ds
                    Text s -> s ++ best (col + length s) ds
                    Line -> '\n' : best 0 ds
                    a `Concat` b -> best col (a:b:ds)
                    a `Union` b -> nicest col (best col (a:ds))
                                              (best col (b:ds))
          best _ _ = ""
          nicest col a b | (width - least) `fits` a = a
                         | otherwise = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = (w-1) `fits` cs

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

{- The renderJValue function was defined in PrettyJSON.
The primitive Doc functions are used to take in JValues and return 
Doc values. The JValue structure will be translated into a 
corresponding Doc structure.

The primitive JValue to Doc functions are straight forward, where the 
complications arise from serializing JArray and JObject.

The series function is responsible for serializing these more 
complicated structures. It takes four arguments, open, close, item, 
and the JValue. The open argument is a Char, so in JSON '[' is used 
for an array and '{' is used for an object. Similarly, close is a 
Char, ']' for an array and '}' for an object. 

The item function is a function from (a -> Doc), where a is a JValue for our purposes. For the JArray, renderJValue is passed in as the (a -> Doc) function, which will determine the correct type constructor to use for each element in the array ary. For the JObject, the field function is used as the (a -> Doc) function, which needs a little explanation. Looking at the field function, it takes a (name, val) as it's input, and then creates the Doc representation for a key/value pair in the list of fields in the form of (String, JValue) in the JObject. For each Field, a key:value Doc is created. 

The last argument to the series function is either a JArray or JObject.

Looking at the body of the function, it is a composition that starts 
with map item and the JArray or JObject. This creates an [Doc] which 
is then passed to the punctuate function. The punctuate function with 
a comma as the argument will create a Concat with the comma and each 
element (except the last) in the original [Doc]. The fsep function is 
then used to insert a softline between each element to allow for 
possible breaks given small columns for pretty print. Finally, this 
result is passed with open and close to the enclose function, which 
takes the [Doc] and reduces it to a Doc, which is returned to 
represent the whole JArray or JObject.

The other functions (oneChar, simpleEscapes, smallHex, hexEscape, and 
astral) are important, but are for technical details of transforming 
otherwise invalid characters. The functions are simple and used 
intuitively.
-}
renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name, val) = string name
                            <> text ": "
                            <> renderJValue val

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
                Just r -> text r
                Nothing | mustEscape c -> hexEscape c
                        | otherwise -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])
{- simpleEscapes is called an association list, or alist -}

smallHex :: Int -> Doc
smallHex x = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
    where d = ord c
    
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item