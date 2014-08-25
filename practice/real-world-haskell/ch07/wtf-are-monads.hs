{- IO is the first place where the rwl authors decided to talk about monads. IO is a Monad.

'I/O doen'st just change the state of a program. You can think of I/O as changing the state of the world.'

actions are the Haskell tool used to work with I/O.
actions resemble functions.
actions do nothing when defined, but perform some taske when invoked.

Monads are a powerful way of chaining functions together purely. They are not mentioned except for the fact that IO is one here in Chapter 7.
-}