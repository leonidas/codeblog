
# Concatenative, Row-Polymorphic Programming in Haskell

I've just read the article "[Why Concatenative Programming Matters](http://evincarofautumn.blogspot.com/2012/02/why-concatenative-programming-matters.html)", in which the author states the following:

> In particular, there is no uniform way to compose functions of different numbers of arguments or results. To even get close to that in Haskell, you have to use the curry and uncurry functions to explicitly wrap things up into tuples. No matter what, you need different combinators to compose functions of different types, because Haskell doesn't’t have row polymorphism for function arguments, and it inherently can’t.

This got me thinking about whether this sort of polymorphism is really impossible in Haskell or if there is some way to emulate it. In this blog post, I'll show one way to implement a concatenative DSL inside Haskell, which employs the same sort of genericity in function composition as the linked article.


## Heterogeneous Polymorphic Stacks

The article highlights the problem that in Haskell point-free programming quickly becomes complicated when you start to compose functions of different arities or if you want to, for example, re-use a value multiple times in the expression. To side-step these problems, we are going to define our DSL so that all our functions operate on a stack (which is the traditional model for concatenative languages), so basically every function will have the signature

```haskell
f :: s -> s'
```

Where the function `f` takes in a stack `s` and returns a modified stack `s'`. The input and output stacks need to have a different type, because we want to have strict, compile-time type-checking, so whenever a function pops a value out of a stack or pushes in a new value, the type of the stack changes.

The stack needs to be heterogeneous, so the easiest way to model it in Haskell is to use a tuple. However, there are no generic functions to operate on n-arity tuples, so we will store the stack as nested pairs and use unit `()` to signify an empty stack. So, for example, if we have a stack which has an `Int` value on top, followed by a `String` value, the stack will be represented in Haskell like this:

```haskell
exampleStack :: (((), String), Int)
exampleStack = (((), "foobar"), 5)
```

Now we can implement fully polymorphic stack functions such as `push`, `dup` and `swap` very naturally.

```haskell
push :: a -> s -> (s, a)
push value stack = (stack, value)

dup :: (stack, value) -> ((stack, value), value)
dup (stack, value) = (((stack), value), value)

swap :: ((s, a), b) -> ((s, b), a)
swap ((s, a), b) = ((s, b), a)
```

See how the type signatures beautifully describe the stack operations (in the case of `swap` and `dup`, the type signature and implementation are basically the same).

The generic parameter `s` here is the "rest of the stack" which can have any type and any number of elements. This is equal to the *∀A* parameter in the linked article. This achieves the same effect as "row polymorphism", because e.g. the `swap` function can take in any kind of stack `s` as long as it has at least two elements, `a` and `b`.

This is how we implement a function that adds two numbers.

```haskell
add :: Num n => ((s, n), n) -> (s, n)
add ((s, a), b) = (s, a + b)
```

Again, the type system will statically guarantee that we cannot call `add` on an empty stack or a stack which only contains one element, or a stack which contains something other than two numbers at the top of the stack. But at the same time, we preserve full polymorphism since the parameter `s` is fully generic and its exact value is not examined inside the function.

We can compose all these function using the normal function composition operator

```haskell
test = add . push 2 . push 1
```
or we can use the `>>>` operator from `Control.Arrow` in order to have the operations in the order which they are executed

```haskell
import Control.Arrow

test = push 1 >>> push 2 >>> add
```

We can also create partially applied functions.

```haskell
addTwo = push 2 >>> add
```

And we can check in GHCi that the inferred type makes sense

```
*Main> :t addTwo
addTwo :: (s, Integer) -> (s, Integer)
```

As we can see, `addTwo` expects the stack to have one integer on the top of the stack and the same holds for the result (the original number is popped out of the stack, and the addition result is pushed back in). The function type signature also guarantees that the structure of the rest of the stack stays unchanged.


## Nicer Stack Notation

Writing deeply nested tuples is impractical, so we can use a GHC extension called "type operators" to make the syntax a bit nicer.

```haskell
{-# LANGUAGE TypeOperators #-}
type s:.a = (s, a)

infixl 1 :.
```

Now we can re-write the above function signatures like this:

```haskell
push :: a -> s -> s:.a

dup :: s:.a -> s:.a:.a

swap :: s:.a:.b -> s:.b:.a

exampleStack :: ():.String:.Int

add :: Num n => s:.n:.n -> s:.n
```

Now it is much easier to see the structure of the stack from the type signatures, but we still need to use the clumsy nested tuples when building stacks, for example:

```haskell
dup :: s:.a -> s:.a:.a
dup (s,a) = ((s, a), a)
```

We can combine `TypeOperators` with infix data constructors to define a new type which allows us to use `:.` everywhere in types, patterns and expressions.

```haskell
data s :. a = !s :. !a
```

This is semantically identical to a two-element, strict tuple, but it allows us to implement the above functions like this

```haskell
push :: a -> s -> s:.a
push a s = s:.a

dup :: s:.a -> s:.a:.a
dup (s:.a) = (s:.a:.a)

swap :: s:.a:.b -> s:.b:.a
swap (s:.a:.b) = s:.b:.a

add :: P.Num n => s:.n:.n -> s:.n
add (s:.a:.b) = s:.a + b
```


## Function Lifting

We cannot use "regular" Haskell functions directly with our stack, but we can define a few utility functions to make it easy to convert curried functions into functions that operate on the stack.


```haskell
liftS :: (a -> b) -> (s:.a -> s:.b)
liftS f (s:.a) = s:.f a

liftS2 :: (a -> b -> c) -> (s:.a:.b -> s:.c)
liftS2 f (s:.a:.b) = s:.f a b
```

We can also hide some of the default functions and operators from Prelude and define our own versions that are suitable for stack computations.

```haskell
import Prelude hiding (Num(..), Eq(..), Ord(..), (/), null, (++))
import qualified Prelude as P

(+) :: P.Num n => s:.n:.n -> s:.n
(+) = liftS2 (P.+)

(-) :: P.Num n => s:.n:.n -> s:.n
(-) = liftS2 (P.-)

(*) :: P.Num n => s:.n:.n -> s:.n
(*) = liftS2 (P.*)

(/) :: P.Fractional n => s:.n:.n -> s:.n
(/) = liftS2 (P./)

(<) :: P.Ord n => s:.n:.n -> s:.Bool
(<) = liftS2 (P.<)

(>) :: P.Ord n => s:.n:.n -> s:.Bool
(>) = liftS2 (P.>)

(>=) :: P.Ord n => s:.n:.n -> s:.Bool
(>=) = liftS2 (P.>=)

(<=) :: P.Ord n => s:.n:.n -> s:.Bool
(<=) = liftS2 (P.<=)

(==) :: P.Eq n => s:.n:.n -> s:.Bool
(==) = liftS2 (P.==)


-- List functions

(++) :: s:.[a]:.[a] -> s:.[a]
(++) = liftS2 (P.++)

null :: s:.[a] -> s:.Bool
null = liftS P.null

decons :: s:.[a] -> s:.[a]:.a
decons (s:.(x:xs)) = s:.xs:.x

cons :: s:.[a]:.a -> s:.[a]
cons (s:.xs:.x) = s:. x:xs
```

We can also define basic "shuffle" functions similar to what you can find in e.g. [Factor](http://factorcode.org/) to do primitive stack manipulation.

```haskell
drop :: s:.a -> s
drop (s:.a) = s

nip :: s:.a:.b -> s:.b
nip (s:.a:.b) = s:.b

over :: s:.a:.b -> s:.a:.b:.a
over (s:.a:.b) = (s:.a:.b:.a)

pick :: s:.a:.b:.c -> s:.a:.b:.c:.a
pick (s:.a:.b:.c) = (s:.a:.b:.c:.a)

rotl :: s:.a:.b:.c -> s:.b:.c:.a
rotl (s:.a:.b:.c) = s:.b:.c:.a

rotr :: s:.a:.b:.c -> s:.c:.a:.b
rotr (s:.a:.b:.c) = s:.c:.a:.b
```

It's nice to note here how the type signatures are practically equal to the implementations.


## Higher Order Functions

It turns out that it's also easy to model higher order functions for this sort of structure, and again the signatures end up telling a lot about how the functions operate on the stack. The simplest possible higher-order function is `apply`, which executes a function stored in the stack.

```haskell
apply :: s:.(s -> s') -> s'
apply (s:.f) = f s
```

In order to make nicer looking quotations, we'll add `q` as a synonym for `push`. Now we can write functions using `apply` like this.

```haskell
applyTest = push 2 >>> q( push 3 >>> (+) ) >>> apply
```

Another very commonly used combinator in stack based programming is `dip`, which executes a stack function under the top element of the stack and `dip2` which skips the top two elements. I.e.

```haskell
dip :: s:.a:.(s -> s') -> s':.a
dip (s:.a:.f) = f s :. a

dip2 :: s:.a:.b:.(s -> s') -> s':.a:.b
dip2 (s:.a:.b:.f) = f s :. a :. b
```

`keep` is a combinator which executes a stack function, but preserves the top element of the stack.

```haskell
keep :: s:.a:.(s:.a -> s') -> s':.a
keep (s:.a:.f) = f (s:.a) :. a
```

Let's then take a look at a basic if-then-else construct.

```haskell
if_ :: s:.Bool:.(s -> s'):.(s -> s') -> s'
```

An if statement expects a boolean value, followed by two stack manipulation functions (these are equivalent to so called "quotations"). Here, once again, the strict typing conveys a wealth of information. The "then" and "else" blocks need to be able to operate on `s`, which is the state of the stack before the boolean value, and type system also ensures that both branches have to leave the stack to the same shape (`s'`), i.e. values can differ but the size of the stack and types of values must match.

The implementation itself is straightforward.

```haskell
if_ (s:.cond:.then_:.else_)
    | cond      = then_ s
    | otherwise = else_ s
```

Next, let's see what a `map` function would look like

```haskell
map :: s:.[a]:.(s:.a -> s:.b) -> s:.[b]
```

As we can see, `map` takes in a stack that contains a list `[a]` and a function that maps an `a` on top of the stack to `b`, and results in a stack with a new list `[b]` on top. The implementation is a bit trickier, since we need to carry the state of the stack `s` throughout the map.

```haskell
map (s:.lst:.f) = (uncurry (:.)) (mapAccumL step s lst) where
    step s x = let s:.y = f (s:.x) in (s,y)
```

We also have enough building blocks already that we can implement recursive higher-order functions with just stack operations, without dropping back to "native" Haskell code. For example, `foldl`, `foldr` and `filter`.

```haskell
foldr :: s:.[x]:.acc:.(s:.acc:.x -> s:.acc) -> s:.acc
foldr = pick
    >>> null
    >>> q( drop >>> nip )
    >>> q( q decons
        >>> dip2
        >>> rotl
        >>> q( q foldr >>> keep ) >>> dip
        >>> swap
        >>> apply )
    >>> if_


foldl :: s:.[x]:.acc:.(s:.x:.acc -> s:.acc) -> s:.acc
foldl = pick
    >>> null
    >>> q( drop >>> nip )
    >>> q( q( decons >>> swap )
        >>> dip2
        >>> rotl
        >>> q( q apply >>> keep )
        >>> dip
        >>> rotr
        >>> foldl )
    >>> if_


filter :: s:.[x]:.(s:.x -> s:.Bool) -> s:.[x]
filter = swap >>> push [] >>> q step >>> foldr >>> nip where
    step = rotr
        >>> pick
        >>> q( q( apply
                >>> q( q cons )
                >>> q( q drop )
                >>> if_ ) >>> keep
            ) >>> dip2
        >>> q rotl >>> dip
        >>> swap
        >>> apply
```

There are probably simpler ways to implement the above functions using concatenative stack languages, but I don't have any real experience in programming with them, so this was the best I could come up with.

It's worth mentioning that Haskell's type system was immensely helpful when piecing the more complex functions together. Although the error messages were pretty complex and difficult to read, the great thing is that any type error would contain both the actual and expected type of the stack (and the type of the stack of course contains the structure and type of all items in the stack), which was a great debugging tool.


## Side-Effectful Stack Programming

In order to make any real programs, we need to implement a way to have side-effects like input and output in our stack programs. This requires a surprisingly small change. Instead of `(s -> s')`, the side-effectful stack functions have the type `Monad m => (s -> m s')`. We can keep all our existing functions as they are and gain a neat separation between pure and monadic stack operations. Monadic stack operations are chained using "Kleisli composition" (`>=>`) and pure stack operations need to end in `return` in order to be composable with monadic ones.

Here are some simple stack IO functions

```haskell
putStr :: s:.String -> IO s
putStr (s:.x) = P.putStr x >> return s

putStrLn :: s:.String -> IO s
putStrLn (s:.x) = P.putStrLn x >> return s

getLine :: s -> IO (s:.String)
getLine s = P.getLine >>= \ln -> return (s:.ln)
```

Which we can use like this

```haskell
hello = push "What's your name? " >>> return
    >=> putStr
    >=> push "Hello, " >>> return
    >=> getLine
    >=> (++) >>> push "!" >>> (++) >>> return
    >=> putStrLn
```

## Conclusion

While there aren't many practical uses for this kind of programming in Haskell, I still found it interesting how the concepts mapped onto the Haskell type system. But is this "real" row polymorphism or are there cases where this model is less generic than the one described in "Why Concatenative Programming Matters"? Let me know if I've missed something!


All the code in this post can be found in [this gist](https://gist.github.com/1851086).

<hr/>
Sami Hangaslammi <[sami.hangaslammi@leonidasoy.fi](mailto://sami.hangaslammi@leonidasoy.fi)>

Leonidas Oy <[http://leonidasoy.fi](http://leonidasoy.fi)>
