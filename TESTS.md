# Introduction

This file is a test suite. Each section maps to an HSpec test, and
each line that is followed by a Haskell code fence is tested to make
sure re-formatting that code snippet produces the same result.

You can browse through this document to see what HIndent's style is
like, or contribute additional sections to it, or regression tests.

# Modules

Empty module

``` haskell
```

Double shebangs

``` haskell
#!/usr/bin/env stack
#!/usr/bin/env stack
main = pure ()
```

Extension pragmas

```haskell
{-# LANGUAGE TypeApplications #-}

fun @Int 12
```

Module header

``` haskell
module X where

x = 1
```

Exports

``` haskell
module X
  ( x
  , y
  , Z
  , P(x, z)
  ) where
```

Exports, indentation 4

``` haskell 4
module X
    ( x
    , y
    , Z
    , P(x, z)
    ) where
```

# Imports

Import lists

``` haskell
import Data.Text
import Data.Text
import qualified Data.Text as T
import qualified Data.Text (a, b, c)
import Data.Text (a, b, c)
import Data.Text hiding (a, b, c)
```

Sorted

```haskell given
import B
import A
```

```haskell expect
import A
import B
```

Explicit imports - capitals first (typeclasses/types), then operators, then identifiers

```haskell given
import qualified MegaModule as M ((>>>), MonadBaseControl, void, MaybeT(..), join, Maybe(Nothing, Just), liftIO, Either, (<<<), Monad(return, (>>=), (>>)))
```

```haskell expect
import qualified MegaModule as M
  ( Either
  , Maybe(Just, Nothing)
  , MaybeT(..)
  , Monad((>>), (>>=), return)
  , MonadBaseControl
  , (<<<)
  , (>>>)
  , join
  , liftIO
  , void
  )
```

Pretty import specification

```haskell
import A hiding
  ( foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  )

import Name hiding ()

import {-# SOURCE #-} safe qualified Module as M hiding (a, b, c, d, e, f)
```

# Declarations

Type declaration

``` haskell
type EventSource a = (AddHandler a, a -> IO ())
```

Type declaration with infix promoted type constructor

```haskell
fun1 :: Def ('[ Ref s (Stored Uint32), IBool] 'T.:-> IBool)
fun1 = undefined

fun2 :: Def ('[ Ref s (Stored Uint32), IBool] ':-> IBool)
fun2 = undefined
```

Instance declaration without decls

``` haskell
instance C a
```

Instance declaration with decls

``` haskell
instance C a where
  foobar = do
    x y
    k p
```

Symbol class constructor in instance declaration

```haskell
instance Bool :?: Bool

instance (:?:) Int Bool
```

GADT declarations

```haskell
data Ty :: (* -> *) where
  TCon
    :: { field1 :: Int
       , field2 :: Bool}
    -> Ty Bool
  TCon' :: (a :: *) -> a -> Ty a
```

# Expressions

Lazy patterns in a lambda

``` haskell
f = \ ~a -> undefined
-- \~a yields parse error on input ‘\~’
```

Bang patterns in a lambda

``` haskell
f = \ !a -> undefined
-- \!a yields parse error on input ‘\!’
```

List comprehensions, short

``` haskell
map f xs = [f x | x <- xs]
```

List comprehensions, long

``` haskell
defaultExtensions =
  [ e
  | EnableExtension {extensionField1 = extensionField1} <-
      knownExtensions knownExtensions
  , let a = b
    -- comment
  , let c = d
    -- comment
  ]
```

List comprehensions with operators

```haskell
defaultExtensions =
  [e | e@EnableExtension {} <- knownExtensions] \\
  map EnableExtension badExtensions
```

Parallel list comprehension, short

```haskell
zip xs ys = [(x, y) | x <- xs | y <- ys]
```

Parallel list comprehension, long

```haskell
fun xs ys =
  [ (alphaBetaGamma, deltaEpsilonZeta)
  | x <- xs
  , z <- zs
  | y <- ys
  , cond
  , let t = t
  ]
```

Record, short

``` haskell
getGitProvider :: EventProvider GitRecord ()
getGitProvider =
  EventProvider {getModuleName = "Git", getEvents = getRepoCommits}
```

Record, medium

``` haskell
commitToEvent :: FolderPath -> TimeZone -> Commit -> Event.Event
commitToEvent gitFolderPath timezone commit =
  Event.Event
    {pluginName = getModuleName getGitProvider, eventIcon = "glyphicon-cog"}
```

Record, long

``` haskell
commitToEvent :: FolderPath -> TimeZone -> Commit -> Event.Event
commitToEvent gitFolderPath timezone commit =
  Event.Event
    { pluginName = getModuleName getGitProvider
    , eventIcon = "glyphicon-cog"
    , eventDate = localTimeToUTC timezone (commitDate commit)
    }
```

Record with symbol constructor

```haskell
f = (:..?) {}
```

Record with symbol field

```haskell
f x = x {(..?) = wat}

g x = Rec {(..?)}
```

Cases

``` haskell
strToMonth :: String -> Int
strToMonth month =
  case month of
    "Jan" -> 1
    "Feb" -> 2
    _ -> error $ "Unknown month " ++ month
```

Operators, bad

``` haskell
x =
  Value <$> thing <*> secondThing <*> thirdThing <*> fourthThing <*>
  Just thisissolong <*>
  Just stilllonger <*>
  evenlonger
```

Operators, good

```haskell pending
x =
  Value <$> thing <*> secondThing <*> thirdThing <*> fourthThing <*>
  Just thisissolong <*> Just stilllonger <*> evenlonger
```

Operator with `do`

```haskell
for xs $ do
  left x
  right x
```

Operator with lambda

```haskell
for xs $ \x -> do
  left x
  right x
```

Operator with lambda-case

```haskell
for xs $ \case
  Left x -> x
```

Operator in parentheses

```haskell
cat = (++)
```

Symbol data constructor in parentheses

```haskell
cons = (:)

cons' = (:|)
```

n+k patterns

``` haskell
f (n+5) = 0
```

Binary symbol data constructor in pattern

```haskell
f (x :| _) = x

f' ((:|) x _) = x

f'' ((Data.List.NonEmpty.:|) x _) = x

g (x:xs) = x

g' ((:) x _) = x
```

Type application

```haskell
{-# LANGUAGE TypeApplications #-}

fun @Int 12
```

Transform list comprehensions

```haskell
list =
  [ (x, y, map the v)
  | x <- [1 .. 10]
  , y <- [1 .. 10]
  , let v = x + y
  , then group by v using groupWith
  , then take 10
  , then group using permutations
  , t <- concat v
  , then takeWhile by t < 3
  ]
```

Type families

```haskell
type family Id a
```

Type family annotations

``` haskell
type family Id a :: *
```

Type family instances

```haskell
type instance Id Int = Int
```

Type family dependencies

```haskell
type family Id a = r | r -> a
```

Binding implicit parameters

```haskell
f =
  let ?x = 42
   in f
```

Closed type families

```haskell
type family Closed (a :: k) :: Bool where
  Closed x = 'True
```

Unboxed sum types

```haskell
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

f :: (# (# Int, String #) | String #) -> (# Int | String #)
f (# (# n, _ #) | #) = (# n | #)
f (# | b #) = (# | b #)

f' ::
     (#  (# Int, String #)
       | Either Bool Int
       | Either Bool Int
       | Either Bool Int
       | Either Bool Int
       | Either Bool Int
       | String #)
  -> (# Int | String #)
```

# Template Haskell

Expression brackets

```haskell
add1 x = [|x + 1|]
```

Typed expression brackets

```haskell
add1 x = [||x + 1||]
```

Pattern brackets

```haskell
mkPat = [p|(x, y)|]
```

Type brackets

```haskell
foo :: $([t|Bool|]) -> a
```

Typed variable splice

```haskell
add2Typed = [||$$myFuncTyped . $$(myFuncTyped)||]
```

Quoted data constructors

```haskell
cons = '(:)
```

Pattern splices

```haskell
f $pat = ()

g =
  case x of
    $(mkPat y z) -> True
    _ -> False
```

# Type signatures

Long argument list should line break

```haskell
longLongFunction ::
     ReaderT r (WriterT w (StateT s m)) a
  -> StateT s (WriterT w (ReaderT r m)) a
```

Class constraints should leave `::` on same line

``` haskell
-- see https://github.com/chrisdone/hindent/pull/266#issuecomment-244182805
fun ::
     (Class a, Class b)
  => fooooooooooo bar mu zot
  -> fooooooooooo bar mu zot
  -> c
```

Class constraints

``` haskell
fun :: (Class a, Class b) => a -> b -> c
```

Symbol class constructor in class constraint

```haskell
f :: (a :?: b) => (a, b)
f' :: ((:?:) a b) => (a, b)
```

Tuples

``` haskell
fun :: (a, b, c) -> (a, b)
```

Quasiquotes in types

```haskell
fun :: [a|bc|]
```

Default signatures

```haskell
-- https://github.com/chrisdone/hindent/issues/283
class Foo a where
  bar :: a -> a -> a
  default bar :: Monoid a =>
    a -> a -> a
  bar = mappend
```

Implicit parameters

```haskell
f :: (?x :: Int) => Int
```

Symbol type constructor

```haskell
f :: a :?: b
f' :: (:?:) a b
```

Promoted list (issue #348)

```haskell
a :: A '[ 'True]
a = undefined

-- nested promoted list with multiple elements.
b :: A '[ '[ 'True, 'False], '[ 'False, 'True]]
b = undefined
```

Promoted list with a tuple (issue #348)

```haskell
a :: A '[ '( a, b, c, d)]
a = undefined

-- nested promoted tuples.
b :: A '[ '( 'True, 'False, '[], '( 'False, 'True))]
b = undefined
```

Prefix promoted symbol type constructor

```haskell
a :: '(T.:->) 'True 'False
b :: (T.:->) 'True 'False
c :: '(:->) 'True 'False
d :: (:->) 'True 'False
```

# Function declarations

Prefix notation for operators

``` haskell
(+) :: Num a => a -> a -> a
(+) a b = a
```

Where clause

``` haskell
sayHello = do
  name <- getLine
  putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"
```

Guards and pattern guards

``` haskell
f x
  | x <- Just x
  , x <- Just x =
    case x of
      Just x -> e
  | otherwise = do e
  where
    x = y
```

Multi-way if

``` haskell
x =
  if | x <- Just x,
       x <- Just x ->
       case x of
         Just x -> e
         Nothing -> p
     | otherwise -> e
```

Case inside a `where` and `do`

``` haskell
g x =
  case x of
    a -> x
  where
    foo =
      case x of
        _ -> do
          launchMissiles
      where
        y = 2
```

Let inside a `where`

``` haskell
g x =
  let x = 1
   in x
  where
    foo =
      let y = 2
          z = 3
       in y
```

Lists

``` haskell
exceptions = [InvalidStatusCode, MissingContentHeader, InternalServerError]

exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  , InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
```

Long line, function application

```haskell
test = do
  alphaBetaGamma deltaEpsilonZeta etaThetaIota kappaLambdaMu nuXiOmicron piRh79
  alphaBetaGamma deltaEpsilonZeta etaThetaIota kappaLambdaMu nuXiOmicron piRho80
  alphaBetaGamma
    deltaEpsilonZeta
    etaThetaIota
    kappaLambdaMu
    nuXiOmicron
    piRhoS81
```

Long line, tuple

```haskell
test
  (alphaBetaGamma, deltaEpsilonZeta, etaThetaIota, kappaLambdaMu, nuXiOmicro79)
  (alphaBetaGamma, deltaEpsilonZeta, etaThetaIota, kappaLambdaMu, nuXiOmicron80)
  ( alphaBetaGamma
  , deltaEpsilonZeta
  , etaThetaIota
  , kappaLambdaMu
  , nuXiOmicronP81)
```

Long line, tuple section

```haskell
test
  (, alphaBetaGamma, , deltaEpsilonZeta, , etaThetaIota, kappaLambdaMu, nu79, )
  (, alphaBetaGamma, , deltaEpsilonZeta, , etaThetaIota, kappaLambdaMu, , n80, )
  (
  , alphaBetaGamma
  ,
  , deltaEpsilonZeta
  ,
  , etaThetaIota
  , kappaLambdaMu
  ,
  , nu81
  ,)
```

# Record syntax

Pattern matching, short

```haskell
fun Rec {alpha = beta, gamma = delta, epsilon = zeta, eta = theta, iota = kappa} = do
  beta + delta + zeta + theta + kappa
```

Pattern matching, long

```haskell
fun Rec { alpha = beta
        , gamma = delta
        , epsilon = zeta
        , eta = theta
        , iota = kappa
        , lambda = mu
        } =
  beta + delta + zeta + theta + kappa + mu + beta + delta + zeta + theta + kappa
```

Symbol constructor, short

```haskell
fun ((:..?) {}) = undefined
```

Symbol constructor, long

```
fun (:..?) { alpha = beta
           , gamma = delta
           , epsilon = zeta
           , eta = theta
           , iota = kappa
           , lambda = mu
           } =
  beta + delta + zeta + theta + kappa + mu + beta + delta + zeta + theta + kappa
```

Symbol field

```haskell
f (X {(..?) = x}) = x
```

Punned symbol field

```haskell
f' (X {(..?)}) = (..?)
```

# Johan Tibell compatibility checks

Basic example from Tibbe's style

``` haskell
sayHello :: IO ()
sayHello = do
  name <- getLine
  putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs
```

Data declarations

``` haskell
data Tree a
  = Branch !a !(Tree a) !(Tree a)
  | Leaf

data Tree a
  = Branch
      !a
      !(Tree a)
      !(Tree a)
      !(Tree a)
      !(Tree a)
      !(Tree a)
      !(Tree a)
      !(Tree a)
  | Leaf

data HttpException
  = InvalidStatusCode Int
  | MissingContentHeader

data Person =
  Person
    { firstName :: !String -- ^ First name
    , lastName :: !String -- ^ Last name
    , age :: !Int -- ^ Age
    }

data Expression a
  = VariableExpression
      { id :: Id Expression
      , label :: a
      }
  | FunctionExpression
      { var :: Id Expression
      , body :: Expression a
      , label :: a
      }
  | ApplyExpression
      { func :: Expression a
      , arg :: Expression a
      , label :: a
      }
  | ConstructorExpression
      { id :: Id Constructor
      , label :: a
      }
```

Spaces between deriving classes

``` haskell
-- From https://github.com/chrisdone/hindent/issues/167
data Person =
  Person
    { firstName :: !String -- ^ First name
    , lastName :: !String -- ^ Last name
    , age :: !Int -- ^ Age
    }
  deriving (Eq, Show)
```

Hanging lambdas

``` haskell
bar :: IO ()
bar =
  forM_ [1, 2, 3] $ \n -> do
    putStrLn "Here comes a number!"
    print n

foo :: IO ()
foo =
  alloca 10 $ \a ->
    alloca 20 $ \b ->
      cFunction fooo barrr muuu (fooo barrr muuu) (fooo barrr muuu)
```

# Comments

Comments within a declaration

``` haskell
bob -- after bob
 =
  foo -- next to foo
  -- line after foo
    (bar
       foo -- next to bar foo
       bar -- next to bar
     ) -- next to the end paren of (bar)
    -- line after (bar)
    mu -- next to mu
    -- line after mu
    -- another line after mu
    zot -- next to zot
    -- line after zot
    (case casey -- after casey
           of
       Just -- after Just
        -> do
         justice -- after justice
          *
           foo
             (blah * blah + z + 2 / 4 + a - -- before a line break
              2 * -- inside this mess
              z /
              2 /
              2 /
              aooooo /
              aaaaa -- bob comment
              ) +
           (sdfsdfsd fsdfsdf) -- blah comment
         putStrLn "")
    [1, 2, 3]
    [ 1 -- foo
    , ( 2 -- bar
      , 2.5 -- mu
       )
    , 3
    ]
    -- in the end of the function
  where
    alpha = alpha
    -- between alpha and beta
    beta = beta
    -- after beta

foo = 1 -- after foo

gamma = do
  delta
  epsilon
  -- in the end of a do-block 1

gamma = do
  delta
  epsilon
  -- the very last block is detected differently
```

Doesn't work yet (wrong comment position detection)

```haskell pending
gamma = do
  -- in the beginning of a do-block
  delta
  where
    -- before alpha
    alpha = alpha
```

Haddock comments

``` haskell
-- | Module comment.
module X where

-- | Main doc.
main :: IO ()
main = return ()

data X
  = X -- ^ X is for xylophone.
  | Y -- ^ Y is for why did I eat that pizza.

data X =
  X
    { field1 :: Int -- ^ Field1 is the first field.
    , field11 :: Char
      -- ^ This field comment is on its own line.
    , field2 :: Int -- ^ Field2 is the second field.
    , field3 :: Char -- ^ This is a long comment which starts next to
      -- the field but continues onto the next line, it aligns exactly
      -- with the field name.
    , field4 :: Char
      -- ^ This is a long comment which starts on the following line
      -- from from the field, lines continue at the sme column.
    }
```

Comments around regular declarations

``` haskell
-- This is some random comment.
-- | Main entry point.
main = putStrLn "Hello, World!"
-- This is another random comment.
```

Multi-line comments

``` haskell
bob {- after bob -}
 =
  foo {- next to foo -}
  {- line after foo -}
    (bar
       foo {- next to bar foo -}
       bar {- next to bar -}
     ) {- next to the end paren of (bar) -}
    {- line after (bar) -}
    mu {- next to mu -}
    {- line after mu -}
    {- another line after mu -}
    zot {- next to zot -}
    {- line after zot -}
    (case casey {- after casey -}
           of
       Just {- after Just -}
        -> do
         justice {- after justice -}
          *
           foo
             (blah * blah + z + 2 / 4 + a - {- before a line break -}
              2 * {- inside this mess -}
              z /
              2 /
              2 /
              aooooo /
              aaaaa {- bob comment -}
              ) +
           (sdfsdfsd fsdfsdf) {- blah comment -}
         putStrLn "")
    [1, 2, 3]
    [ 1 {- foo -}
    , ( 2 {- bar -}
      , 2.5 {- mu -}
       )
    , 3
    ]

foo = 1 {- after foo -}
```

Multi-line comments with multi-line contents

``` haskell
{- | This is some random comment.
Here is more docs and such.
Etc.
-}
main = putStrLn "Hello, World!"
{- This is another random comment. -}
```

# MINIMAL pragma

Monad example

```haskell
class A where
  {-# MINIMAL return, ((>>=) | (join, fmap)) #-}
```

Very long names #310

```haskell
class A where
  {-# MINIMAL averylongnamewithnoparticularmeaning
            | ananotherverylongnamewithnomoremeaning #-}
```

# Behaviour checks

Unicode

``` haskell
α = γ * "ω"
-- υ
```

Empty module

``` haskell
```

Trailing newline is preserved

``` haskell
module X where

foo = 123
```

# Complex input

A complex, slow-to-print decl

``` haskell
quasiQuotes =
  [ ( ''[]
    , \(typeVariable:_) _automaticPrinter ->
        (let presentVar = varE (presentVarName typeVariable)
          in lamE
               [varP (presentVarName typeVariable)]
               [|(let typeString = "[" ++ fst $(presentVar) ++ "]"
                   in ( typeString
                      , \xs ->
                          case fst $(presentVar) of
                            "GHC.Types.Char" ->
                              ChoicePresentation
                                "String"
                                [ ( "String"
                                  , StringPresentation
                                      "String"
                                      (concatMap
                                         getCh
                                         (map (snd $(presentVar)) xs)))
                                , ( "List of characters"
                                  , ListPresentation
                                      typeString
                                      (map (snd $(presentVar)) xs))
                                ]
                              where getCh (CharPresentation "GHC.Types.Char" ch) =
                                      ch
                                    getCh (ChoicePresentation _ ((_, CharPresentation _ ch):_)) =
                                      ch
                                    getCh _ = ""
                            _ ->
                              ListPresentation
                                typeString
                                (map (snd $(presentVar)) xs)))|]))
  ]
```

Random snippet from hindent itself

``` haskell
exp' (App _ op a) = do
  (fits, st) <- fitsOnOneLine (spaced (map pretty (f : args)))
  if fits
    then put st
    else do
      pretty f
      newline
      spaces <- getIndentSpaces
      indented spaces (lined (map pretty args))
  where
    (f, args) = flatten op [a]
    flatten :: Exp NodeInfo -> [Exp NodeInfo] -> (Exp NodeInfo, [Exp NodeInfo])
    flatten (App _ f' a') b = flatten f' (a' : b)
    flatten f' as = (f', as)
```

Quasi quotes

```haskell
exp = [name|exp|]

f [qq|pattern|] = ()
```

# C preprocessor

Conditionals (`#if`)

```haskell
isDebug :: Bool
#if DEBUG
isDebug = True
#else
isDebug = False
#endif
```

Macro definitions (`#define`)

```haskell
#define STRINGIFY(x) #x
f = STRINGIFY (y)
```

Escaped newlines

```haskell
#define LONG_MACRO_DEFINITION \
  data Pair a b = Pair \
    { first :: a \
    , second :: b \
    }
#define SHORT_MACRO_DEFINITION \
  x
```

# Regression tests

jml Adds trailing whitespace when wrapping #221

``` haskell
x = do
  config <- execParser options
  comments <-
    case config of
      Diff False args -> commentsFromDiff args
      Diff True args -> commentsFromDiff ("--cached" : args)
      Files args -> commentsFromFiles args
  mapM_ (putStrLn . Fixme.formatTodo) (concatMap Fixme.getTodos comments)
```

meditans hindent freezes when trying to format this code #222

``` haskell
c :: forall new.
     ( Settable "pitch" Pitch (Map.AsMap (new Map.:\ "pitch")) new
     , Default (Book' (Map.AsMap (new Map.:\ "pitch")))
     )
  => Book' new
c = set #pitch C (def :: Book' (Map.AsMap (new Map.:\ "pitch")))

foo ::
     ( Foooooooooooooooooooooooooooooooooooooooooo
     , Foooooooooooooooooooooooooooooooooooooooooo
     )
  => A
```

bitemyapp wonky multiline comment handling #231

``` haskell
module Woo where

hi = "hello"
{-
test comment
-}
-- blah blah
-- blah blah
-- blah blah
```

cocreature removed from declaration issue #186

``` haskell
-- https://github.com/chrisdone/hindent/issues/186
trans One e n =
  M.singleton
    (Query Unmarked (Mark NonExistent)) -- The goal of this is to fail always
    (emptyImage {notPresent = S.singleton (TransitionResult Two (Just A) n)})
```

sheyll explicit forall in instances #218

``` haskell
-- https://github.com/chrisdone/hindent/issues/218
instance forall x. C

instance forall x. Show x => C x
```

tfausak support shebangs #208

``` haskell given
#!/usr/bin/env stack
-- stack runghc
main =
 pure ()
-- https://github.com/chrisdone/hindent/issues/208
```

``` haskell expect
#!/usr/bin/env stack
-- stack runghc
main = pure ()
-- https://github.com/chrisdone/hindent/issues/208
```

joe9 preserve newlines between import groups

``` haskell
-- https://github.com/chrisdone/hindent/issues/200
import Data.List
import Data.Maybe

import FooBar
import MyProject

import GHC.Monad

-- blah
import Hello

import CommentAfter -- Comment here shouldn't affect newlines
import HelloWorld

import CommentAfter -- Comment here shouldn't affect newlines

import HelloWorld

-- Comment here shouldn't affect newlines
import CommentAfter

import HelloWorld
```

Wrapped import list shouldn't add newline

```haskell given
import ATooLongList
       (alpha, beta, gamma, delta, epsilon, zeta, eta, theta)
import B
```

```haskell expect
import ATooLongList (alpha, beta, delta, epsilon, eta, gamma, theta, zeta)
import B
```

radupopescu `deriving` keyword not aligned with pipe symbol for type declarations

``` haskell
data Stuffs
  = Things
  | This
  | That
  deriving (Show)

data Simple =
  Simple
  deriving (Show)
```

sgraf812 top-level pragmas should not add an additional newline #255

``` haskell
-- https://github.com/chrisdone/hindent/issues/255
{-# INLINE f #-}
f :: Int -> Int
f n = n
```

ivan-timokhin breaks code with type operators #277

```haskell
-- https://github.com/chrisdone/hindent/issues/277
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

type m ~> n = ()

class (a :< b) c
```

ivan-timokhin variables swapped around in constraints #278

```haskell
-- https://github.com/chrisdone/hindent/issues/278
data Link c1 c2 a c =
  forall b. (c1 a b, c2 b c) =>
            Link (Proxy b)
```

ttuegel qualified infix sections get mangled #273

```haskell
-- https://github.com/chrisdone/hindent/issues/273
import qualified Data.Vector as V

main :: IO ()
main = do
  let _ = foldr1 (V.++) [V.empty, V.empty]
  pure ()

-- more corner cases.
xs = V.empty V.++ V.empty

ys = (++) [] []

cons :: V.Vector a -> V.Vector a -> V.Vector a
cons = (V.++)
```

ivan-timokhin breaks operators type signatures #301

```haskell
-- https://github.com/chrisdone/hindent/issues/301
(+) :: ()
```

cdepillabout Long deriving clauses are not reformatted #289

```haskell
newtype Foo =
  Foo Proxy
  deriving ( Functor
           , Applicative
           , Monad
           , Semigroup
           , Monoid
           , Alternative
           , MonadPlus
           , Foldable
           , Traversable
           )
```

ivan-timokhin Breaks instances with type operators #342

```haskell
-- https://github.com/chrisdone/hindent/issues/342
instance Foo (->)

instance Foo (^>)

instance Foo (T.<^)
```

Indents record constructions and updates #358
```haskell
foo =
  assert
    sanityCheck
    BomSnapshotAggr
      { snapshot = Just bs
      , previousId = M.bomSnapshotHistoryPreviousId . entityVal <$> bsp
      , nextId = M.bomSnapshotHistoryNextId . entityVal <$> bsn
      , bomEx = bx''
      , orderSubstitutes =
          S.fromList . map OrderSubstituteAggrByCreatedAtAsc $ subs
      , snapshotSubstitute = msub
      }
```

paraseba Deriving strategies with multiple deriving clauses
```haskell
-- https://github.com/commercialhaskell/hindent/issues/503
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foo where

import Data.Typeable
import GHC.Generics

newtype Number a =
  Number a
  deriving (Generic)
  deriving newtype (Show, Eq)
  deriving anyclass (Typeable)
```

Deriving via
```haskell
{-# LANGUAGE DerivingVia #-}

import Numeric

newtype Hex a =
  Hex a

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex a) = "0x" ++ showHex a ""

newtype Unicode =
  U Int
  deriving (Show) via (Hex Int)
  deriving ( Functor
           , Applicative
           , Monad
           , Semigroup
           , Monoid
           , Alternative
           , MonadPlus
           , Foldable
           , Traversable
           ) via (Hex Int)

-- >>> euroSign
-- 0x20ac
euroSign :: Unicode
euroSign = U 0x20ac
```

neongreen "{" is lost when formatting "Foo{}" #366

```haskell
-- https://github.com/chrisdone/hindent/issues/366
foo = Nothing {}
```

jparoz Trailing space in list comprehension #357

```haskell
-- https://github.com/chrisdone/hindent/issues/357
foo =
  [ (x, y)
  | x <- [1 .. 10]
  , y <- [11 .. 20]
  , even x
  , even x
  , even x
  , even x
  , even x
  , odd y
  ]
```

ttuegel Record formatting applied to expressions with RecordWildCards #274

```haskell
-- https://github.com/chrisdone/hindent/issues/274
foo (Bar {..}) = Bar {..}
```

RecursiveDo `rec` and `mdo` keyword #328

```haskell
rec = undefined

mdo = undefined
```

sophie-h Record syntax change in 5.2.2 #393

```haskell
-- https://github.com/commercialhaskell/hindent/issues/393
data X
  = X
      { x :: Int
      }
  | X'

data X =
  X
    { x :: Int
    , x' :: Int
    }

data X
  = X
      { x :: Int
      , x' :: Int
      }
  | X'
```

k-bx Infix data constructor gets reformatted into a parse error #328

```haskell
-- https://github.com/commercialhaskell/hindent/issues/328
data Expect =
  String :--> String
  deriving (Show)
```

tfausak Class constraints cause too many newlines #244

```haskell
-- https://github.com/commercialhaskell/hindent/issues/244
x :: Num a => a
x = undefined

-- instance
instance Num a => C a

-- long instance
instance Nuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuum a =>
         C a where
  f = undefined
```

expipiplus1 Always break before `::` on overlong signatures #390

```haskell
-- https://github.com/commercialhaskell/hindent/issues/390
fun :: Is => Short
fun = undefined

someFunctionSignature ::
     Wiiiiiiiiiiiiiiiiith
  -> Enough
  -> (Arguments -> To ())
  -> Overflow (The Line Limit)
```

duog Long Type Constraint Synonyms are not reformatted #290

```haskell
-- https://github.com/commercialhaskell/hindent/issues/290
type MyContext m
   = ( MonadState Int m
     , MonadReader Int m
     , MonadError Text m
     , MonadMask m
     , Monoid m
     , Functor m)
```

ocharles Type application differs from function application (leading to long lines) #359

```haskell
-- https://github.com/commercialhaskell/hindent/issues/359
thing ::
     ( ResB.BomEx
     , Maybe [( Entity BomSnapshot
              , ( [ResBS.OrderSubstituteAggr]
                , ( Maybe (Entity BomSnapshotHistory)
                  , Maybe (Entity BomSnapshotHistory))))])
  -> [(ResB.BomEx, Maybe ResBS.BomSnapshotAggr)]
```

NorfairKing Do as left-hand side of an infix operation #296

```haskell
-- https://github.com/commercialhaskell/hindent/issues/296
block =
  do ds <- inBraces $ inWhiteSpace declarations
     return $ Block ds
     <?> "block"
```

NorfairKing Hindent linebreaks after very short names if the total line length goes over 80 #405

```haskell
-- https://github.com/commercialhaskell/hindent/issues/405
t =
  f "this is a very loooooooooooooooooooooooooooong string that goes over the line length"
    argx
    argy
    argz

t =
  function
    "this is a very loooooooooooooooooooooooooooong string that goes over the line length"
    argx
    argy
    argz
```

ivan-timokhin No linebreaks for long functional dependency declarations #323

```haskell
-- https://github.com/commercialhaskell/hindent/issues/323
class Foo a b | a -> b where
  f :: a -> b

class Foo a b c d e f
  | a b c d e -> f
  , a b c d f -> e
  , a b c e f -> d
  , a b d e f -> c
  , a c d e f -> b
  , b c d e f -> a
  where
  foo :: a -> b -> c -> d -> e -> f
```

utdemir Hindent breaks TH name captures of operators #412

```haskell
-- https://github.com/commercialhaskell/hindent/issues/412
data T =
  (-)

q = '(-)

data (-)

q = ''(-)
```

utdemir Hindent can not parse empty case statements #414

```haskell
-- https://github.com/commercialhaskell/hindent/issues/414
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

f1 = case () of {}

f2 = \case {}
```

TimoFreiberg INLINE (and other) pragmas for operators are reformatted without parens #415

```haskell
-- https://github.com/commercialhaskell/hindent/issues/415
{-# NOINLINE (<>) #-}
```

NorfairKing Hindent breaks servant API's #417

```haskell
-- https://github.com/commercialhaskell/hindent/issues/417
type API = api1 :<|> api2
```

andersk Cannot parse @: operator #421

```haskell
-- https://github.com/commercialhaskell/hindent/issues/421
a @: b = a + b

main = print (2 @: 2)
```

andersk Corrupts parenthesized type operators #422

```haskell
-- https://github.com/commercialhaskell/hindent/issues/422
data T a =
  a :@ a

test = (:@)
```

NorfairKing Infix constructor pattern is broken #424

```haskell
-- https://github.com/commercialhaskell/hindent/issues/424
from $ \(author `InnerJoin` post) -> pure ()
```

NorfairKing Hindent can no longer parse type applications code #426

```haskell
-- https://github.com/commercialhaskell/hindent/issues/426
{-# LANGUAGE TypeApplications #-}

f :: Num a => a
f = id

x = f @Int 12
```

michalrus Multiline `GHC.TypeLits.Symbol`s are being broken #451

```haskell
-- https://github.com/commercialhaskell/hindent/issues/451
import GHC.TypeLits (Symbol)

data X (sym :: Symbol)
  deriving (Typeable)

type Y = X "abc\n\n\ndef"
```

DavidEichmann Existential Quantification reordered #443

```haskell
-- https://github.com/commercialhaskell/hindent/issues/443
{-# LANGUAGE ExistentialQuantification #-}

data D =
  forall a b c. D a b c
```

sophie-h Regression: Breaks basic type class code by inserting "|" #459

```haskell
-- https://github.com/commercialhaskell/hindent/issues/459
class Class1 a =>
      Class2 a
  where
  f :: a -> Int

class (Eq a, Show a) =>
      Num a
  where
  (+), (-), (*) :: a -> a -> a
  negate :: a -> a
  abs, signum :: a -> a
  fromInteger :: Integer -> a
```

michalrus `let … in …` inside of `do` breaks compilation #467

```haskell
-- https://github.com/commercialhaskell/hindent/issues/467
main :: IO ()
main = do
  let x = 5
   in when (x > 0) (return ())
```

sophie-h Breaking valid top-level template haskell #473

```haskell
-- https://github.com/commercialhaskell/hindent/issues/473
template $
  haskell
    [ ''SomeVeryLongName
    , ''AnotherLongNameEvenLongToBreakTheLine
    , ''LastLongNameInList
    ]
```

schroffl Hindent produces invalid Syntax from FFI exports #479

```haskell
-- https://github.com/commercialhaskell/hindent/issues/479
foreign export ccall "test" test :: IO ()

foreign import ccall "test" test :: IO ()

foreign import ccall safe "test" test :: IO ()

foreign import ccall unsafe "test" test :: IO ()
```

ptek Reformatting of the {-# OVERLAPPING #-} pragma #386

```haskell
-- https://github.com/commercialhaskell/hindent/issues/386
instance {-# OVERLAPPING #-} Arbitrary (Set Int) where
  arbitrary = undefined
```

cdsmith Quotes are dropped from package imports #480

```haskell
-- https://github.com/commercialhaskell/hindent/issues/480
{-# LANGUAGE PackageImports #-}

import qualified "base" Prelude as P
```

alexwl Hindent breaks associated type families annotated with injectivity information #528

```haskell
-- https://github.com/commercialhaskell/hindent/issues/528
class C a where
  type F a = b | b -> a
```

sophie-h Fails to create required indentation for infix #238

```haskell
-- https://github.com/commercialhaskell/hindent/issues/238
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception

x :: IO Int
x =
  do putStrLn "ok"
     error "ok"
     `catch` (\(_ :: IOException) -> pure 1) `catch`
  (\(_ :: ErrorCall) -> pure 2)

```

lippirk Comments on functions in where clause not quite right #540

```haskell
-- https://github.com/chrisdone/hindent/issues/540
topLevelFunc1 = f
  where
    -- comment on func in where clause
    -- stays in the where clause
    f = undefined

topLevelFunc2 = f . g
  where
    {- multi
       line
       comment -}
    f = undefined
    -- single line comment
    g = undefined
```
