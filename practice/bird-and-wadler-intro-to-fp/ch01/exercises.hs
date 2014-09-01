square x = x * x
-- 1.1.1
quad = square . square

-- 1.1.2
max' x y | x >= y = x
         | otherwise = y
         
-- 1.1.3
circleArea r = (22/7) * (square r)

{- referential transparency: when in mathematics a 'variable' is made to be a particular value, it remains that value in the context it is being evaluated in. This is in contrast with most programming languages that through assignment allow variables to be modified.

let (=>) mean 'reduces to'

consider one way:
square (3 + 4) => square 7 (+)
    => 7 x 7 (square)
    => 49 (x)
    
consider another way:
square (3 + 4) => (3 + 4) x (3 + 4) (square)
    => 7 x (3 + 4) (+)
    => 7 x 7 (+)
    => 49 (x)

canonical (or normal form): when expression cannot be further reduced
bottom: in order to create an expression to represent when a computation errors out or goes into an infinitely long sequence of calculations without producing a result, the symbol is bottom (an upside-down T). It has a special status, similar to infinity.
-}

-- 1.2.1
{- Count the number of different ways that the following can be reduced to normal form.
square (square (3 + 7))

    => square (square (10)) (+)
    => square (100) (square)
    => 10000 (square)
    
    => square ((3+7) x (3+7)) (square)
    => ((3+7) x (3+7)) x ((3+7) x (3+7)) (square)
    => (10 x 10) x (10 x 10) (+)
    => 10000 (x)
-}

-- 1.2.2
{- Consider the definition
three x = 3

In how many ways can three (3+4) be reduced to normal form?
three (3+4)
    => three 7 (+)
    => 3 (three)
    
    => 3 (three)
    
-}

-- 1.2.3
{-
Imagine a language of expressions for representing integers defined by the syntax rules:
(i) zero is an expression
(ii) if e is an expression, then so are (succ e) and (pred e)

An evaluator reduces expressions in this language by applying the following rules repeatedly until no longer possible:
    (succ (pred e)) => e (succ.1)
    (pred (succ e)) => e (pred.1)
    
Simplify the expression
    (succ(pred(succ(pred(pred zero)))))
    
    work from outside to inside
    (succ(pred(succ(pred(pred zero)))))
    => succ(pred(pred zero)) (succ.1)
    => pred zero (succ.1)
    
    work from inside to outside
    succ(pred(succ(pred(pred zero)))))
    => succ(pred(pred zero)) (succ.1)
    => pred zero (succ.1)
-}

{- 1.3 Types
There are 2 types of analysis that expressions go through before evaluation:
    sytax-analysis
    type-analysis
-}

{- 1.4 Functions and definitions
    f :: A -> B
    A is the source type
    B is the target type
    functions themselves are types, and are distinct from function application.
    
1.4.1 Type Information
square :: num -> num
id :: a -> a (here a is called a type variable)
When a type expression contains type variables, we say that it denotes a polymorphic type.
This is pretty well ingrained in me do to the work I've done in Haskell to date...

1.4.2 Forms of definition
Here, the motivation for guards and where are provided to describe functions.

1.4.3 Currying
function application associates to the left to aid currying
min x y means (min x) y and not min (x y)
-}
-- 1.4.1
{- Describe one appropriate type for the definite integral function of mathematical analysis, as used in the phrase 'the integral of f from a to b'
f :: num -> num -> num 
-}

{- 1.5 Specifications and implementations
Specifications are expressions of the programmer's intent (or client's expectations)
Implementations are expressions for execution by computer.
The link between the two is the requirement that the implementation satisfies the specification, and the serious programmer is obliged to provide a proof that this is indeed the case.
-}
