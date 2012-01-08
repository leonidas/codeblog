# Generalizing Streams into Coroutines

In programming, the term "[stream][1]" is usually used to mean a sequence of values that is generated on-demand. In type theory and functional programming, it commonly refers specifically to infinite sequences.

In a lazy language like Haskell, defining an infinite stream of values is straightforward.

```haskell
data Stream a = Stream a (Stream a)
```

i.e. a stream of values (of type a) consists of the first value and the rest of the stream, just like a non-empty list consists of its head and tail.

We can then define, for example, a stream of increasing integer values starting from some *n*.

```haskell
intsFrom n :: Stream Integer
intsFrom n = Stream n $ intsFrom $ n + 1
```

If you tried to naively implement the same thing in a non-lazy language like Python

```python
def intsFrom(n):
    return (n, intsFrom(n+1))
```

you would get an infinite recursion and overflow the call-stack.

One way to simulate laziness in a non-lazy language is to use explicit function calls to defer evaluation. So in Python, we could define that a stream is a function that returns a value and a new function for getting the rest of the stream.

```python
def intsFrom(n):
    return lambda: (n, intsFrom(n+1))
```

Now we can evaluate the stream one item at a time.

```
>>> intsFrom(1)()
(1, <function <lambda> at 0x02B90D70>)
>>> _[1]()
(2, <function <lambda> at 0x02B90D30>)
>>> _[1]()
(3, <function <lambda> at 0x02B90D70>)
```

If Haskell was a strict language, we could use a similar approach to implement the infinite stream type.

```haskell
data Stream a = Stream (() -> (a, Stream a))

intsFrom n = Stream $ \() -> (n, intsFrom (n+1))
```

This is essentially identical to the Python implementation.

Now let's return to the regular, lazy Haskell. Even with laziness, the above pattern might prove useful if we generalize it a bit. Since we already have a function that continues the stream, why not use that function call to pass some relevant information back to the stream generator that can alter the course of the stream, i.e.

```haskell
data Stream b a = Stream (b -> (a, Stream b a))
```

Now we are able to construct streams where the consumer of the stream can feed information back to the generator of the stream at every step. Our stream type essentially becomes a kind of [coroutine][2]. However, unlike normal coroutines, this kind of coroutine never terminates, so it can always be resumed.

Since we only have a single constructor with a single parameter, it's more idiomatic (and efficient) in Haskell to represent the type as a `newtype`.

```haskell
newtype Coroutine i o = Coroutine { runC :: i -> (o, Coroutine i o) }
```

Here *i* is the input type of the coroutine and *o* is the type of the output. We can use `runC` to call the coroutine.

So, just to recap. Whereas a normal function from input to output would have the type `i -> o`, the coroutine returns, for every invocation, an output value as well as a new coroutine. From the caller's point of view, using coroutine usually means calling the coroutine (using `runC`), then discarding the original coroutine and calling the new coroutine with the next input value. I.e.

```haskell
let (output1, newCo)   = runC co input1
    (output2, newerCo) = runC newCo input2
```

Of course, you would normally use e.g. recursion instead of manually assigning the different versions of `co`.

The property which makes coroutines interesting as a control structure is that at each step, both the coroutine and its caller can make branching decisions based on input and output respectively.


## Coroutines as Functors

Whenever you implement a new parametric data type in Haskell, it's beneficial to consider whether it fits any of the commonly used abstractions. Providing instances for the many standard type-classes will immediately give you a lot of functionality for practically free, and what's best, it will be easy to understand for users that are already well acquainted with these abstractions.

A functor is, simply put, any parametric type `T a` which you can map into `T b` using a function `a -> b`. This usually means a collection type like list, where you can turn `[a]` into `[b]` by applying a function `a -> b` for each element (i.e. [`map`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html#v:map)), but functors are not limited to collections.

For coroutines, a functor instance needs to implement `fmap :: (a -> b) - > Coroutine i a -> Coroutine i b`.

```haskell
instance Functor (Coroutine i) where
    fmap f co = Coroutine $ \i ->
        let (o, co') = runC co i
        in (f o, fmap f co')
```

The implementation itself is very simple. Whenever the fmapped coroutine gets an input value, it calls the original coroutine and then applies *f* to the output value and `fmap f` recursively to the continuation.

Note that we are specifically declaring `Coroutine i` to be an instance of functor so that `fmap (a -> b)` maps from `Coroutine i a` to `Coroutine i b`, i.e. the input type of the coroutine stays the same.


## Coroutines as Applicative Functors

Using `fmap`, you can apply a function of type `a -> b` to a `Coroutine i a`, but what if you had a function of type `a -> b -> c`? If you use `fmap` like before, you end up with with a result of type `Coroutine i (b -> c)`. In order to feed in the second parameter `b`, we need a more powerful abstraction: applicative functor.

Applicative functors are functors that have two additional properties. A parametric type `T a` is an applicative functor if:

* You can put any "pure" value inside T, i.e. you have a function `a -> T a`
* You can apply a function that is inside T to a value that is inside T, i.e. you have a function `T (a -> b) -> T a -> T b`

In Haskell, the above two functions are called `pure` and `<*>`.


```haskell
instance Applicative (Coroutine i) where
    pure x = Coroutine $ const (x, pure x)

    cof <*> cox = Coroutine $ \i ->
        let (f, cof') = runC cof i
            (x, cox') = runC cox i
        in (f x, cof' <*> cox')
```

For coroutines, the implementation of `pure` turns a constant value into a coroutine that returns that value for every invocation of the coroutine. `<*>` composes two coroutines `cof :: Coroutine i (x -> y)` and `cox :: Coroutine i x` into a new coroutine of type `Coroutine i y`. So the first coroutine produces functions and the second produces values that are applied to the functions.

The two coroutines both get the same input values and advance in lock-step fashion, so if we were to feed in the inputs *i1*, *i2* and *i3*, *cof* and *cox* would produce the functions *f1*, *f2* and *f3* and the values *x1*, *x2* and *x3* respectively. The final outputs of the combined coroutine would be the result values from the function applications *f1(x1)*, *f2(x2)* and *f3(x3)*.

Next we'll define a convenience function that lets us test our coroutines by feeding them a list of input values and returning the outputs.

```haskell
evalList :: Coroutine i o -> [i] -> [o]
evalList _  []     = []
evalList co (x:xs) = o:evalList co' xs
    where (o, co') = runC co x
```

As the simplest example, let's re-implement the `intsFrom` stream as a coroutine that ignores its input and test our functor and applicative implementations.

```haskell
intsFrom :: Integer -> Coroutine () Integer
intsFrom n = Coroutine $ \_ -> (n, intsFrom (n+1))
```

```
*Main> let i = intsFrom 5
*Main> evalList i [(),(),()]
[5,6,7]
*Main> let i2 = fmap (*2) i
*Main> evalList i2 [(),(),()]
[10,12,14]
*Main> let z = (,) <$> i <*> i2
*Main> evalList z [(),(),()]
[(5,10),(6,12),(7,14)]
```

(the operator `<$>` is an alias for `fmap`)


## Coroutines as Arrows

The [`Category`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Category.html#t:Category) type-class can be thought of as a generalization of the function arrow `->`.

The `Category` instance for coroutines defines an identity coroutine `id :: Coroutine a a`, which just returns every input value unchanged. The other function in the type-class is the composition operator `.`, which lets us compose two coroutines into one, just like we do with regular function composition.

```haskell
import Prelude hiding (id, (.))

import Control.Arrow
import Control.Category

instance Category Coroutine where
    id = Coroutine $ \i -> (i, id)

    cof . cog = Coroutine $ \i ->
        let (x, cog') = runC cog i
            (y, cof') = runC cof x
        in (y, cof' . cog')
```

(We need to hide the default implementations of `id` and `.` from Prelude, since [`Control.Category`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Category.html) contains more generic implementations for them)

As an example, let's define a coroutine which keeps an accumulating sum of the values it is fed.

```haskell
accumSum :: Coroutine Integer Integer
accumSum = Coroutine $ step 0 where
    step s i = (s+i, Coroutine $ step (s+i))
```

Now we can compose `intsFrom` and `accumSum` using `.`

```haskell
*Main> let sumFrom = accumSum . intsFrom 0
*Main> evalList sumFrom [(),(),(),()]
[0,1,3,6]
```

We can also generalize the idea of accumulation into a function called `scan` (analogous to [`Data.List.scanl`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html#v:scanl)).

```haskell
scan :: (a -> b -> a) -> a -> Coroutine b a
scan f i = Coroutine $ step i where
    step a b = let a' = f a b in (a', scan f a')
```

Now `accumSum` can be defined as

```haskell
accumSum = scan (+) 0
```

The [Arrow](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Arrow.html#t:Arrow) type-class extends Category in two ways. First, we get `arr` which is used to convert plain old functions into Arrows (i.e. Coroutines in our case). Second, we gain a set of new ways to compose coroutines that operate on pairs of values.

The minimal definition for an Arrow instance requires the implementations for [`arr`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Arrow.html#v:arr) and [`first`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Arrow.html#v:first).

```haskell
instance Arrow Coroutine where
    arr f = Coroutine $ \i -> (f i, arr f)

    first co = Coroutine $ \(a,b) ->
        let (c, co') = runC co a
        in ((c,b), first co')
```

The signature of `first` is

```haskell
first :: Coroutine a b -> Coroutine (a, c) (b, c)
```

So it transforms a coroutine so that it can be applied to the first value of a tuple, while the second value stays unchanged. This might not seem too useful at first glance, but tuples in arrow computations can be thought of as multiple lines of computation running side-by-side, and `first` (and its pair, [`second`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Arrow.html#v:second)) enables us to apply different operations on each line.


## Practical Applications

So what are these kind of coroutines good for? One interesting observation is that the arrow instance of coroutines can be thought of as a kind of stateful stream processor, where `Coroutine a b` takes in a stream of *a*'s and returns a stream of *b*'s, while potentially maintaining some internal state. These kind of stream processors are applicable for many things, one of which is [functional reactive programming][3]. So, in the next blog post, we'll implement a simple FRP-library using coroutines and arrows.


<hr/>
Sami Hangaslammi <[sami.hangaslammi@leonidasoy.fi](mailto://sami.hangaslammi@leonidasoy.fi)>

Leonidas Oy <[http://leonidasoy.fi](http://leonidasoy.fi)>


[1]: http://en.wikipedia.org/wiki/Stream_(computing)
[2]: http://en.wikipedia.org/wiki/Coroutine
[3]: http://en.wikipedia.org/wiki/Functional_reactive_programming
