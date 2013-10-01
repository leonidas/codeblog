# Purely Functional, Declarative Game Logic Using Reactive Programming

In the [previous article](https://github.com/leonidas/codeblog/blob/master/2012/2012-01-08-streams-coroutines.md) I introduced the `Coroutine` data type. In this second part I will show how coroutines can be used to implement a fixed time-step reactive programming library and use that library for modeling a simple game. The code examples will require a [basic proficiency in reading Haskell code](http://learnyouahaskell.com/).

## Classical FRP

The classic model of functional reactive programming has two main concepts:

* time varying values, which can be though of as functions `Time -> a`
* events, which can be thought of as time-ordered (possible infinite) streams of `(Time, e)` pairs

These can be used to model dynamic systems with continous time, but there are also many domains such as games and physics simulations which tend to use fixed time-steps instead, and these kind of systems can be modeled very conveniently using coroutines so that each call to the coroutine represents a single time-step.

## Fixed Time-Step FRP

In fixed time-step FRP, coroutines are analogous to time varying values (or behaviors, as they are sometimes called). A coroutine of type `Coroutine a b` can be thought of as a time varying value of type `b` that is dependent on another time varying value of type `a`.

So, for example, the position of a player character in a game can be thought of as `Position` that is dependent on `KeyboardInput`, so it would have the type

```haskell
playerPosition :: Coroutine KeyboardInput Position
```

Events are simply time varying lists, where the list contains the events that occur during the current time-step.

```haskell
type Event a = [a]
```

Since they are time varying values, events too depend on other time varying values. So for example

```haskell
playerCollisions :: Coroutine Position (Event Collision)
```

The above type signature indicates that `Collision` events are emitted depending on the current player position.

If a value is dependent on multiple other values, we use tuples.

```haskell
pacmanCollisions :: Coroutine (PacPosition, [GhostPosition]) (Event Collision)
```

I.e. the collision events for [pac-man](http://en.wikipedia.org/wiki/Pac-Man) would depend on the position of pac-man and the positions of all ghosts.

## Connecting Dependent Values

The `Arrow` instance we defined for coroutines in the previous article comes in handy for connecting time varying values with their dependencies. Instead of being functions of `Time -> a`, the time varying values in our fixed time-step system are analoguous to value streams, with one concrete value for every time step. Coroutines can then be seen as stream processors that take in a stream of dependencies and produce a stream of derived values.

Using the above examples, you could for example pipe `playerPosition` and `playerCollision` together (since the collisions were dependent on the position) like this

```
playerPosition >>> playerCollisions
```

The above expression forms a new coroutine with the type `Coroutine KeyboardInput (Event Collision)`, where the keyboard state is first used to calculate the current player position, which is then used to generate collision events.

I mentioned in the previous blog post that tuples in arrow operations can be thought as separate lines of computation. Thus, if we have a time varying tuple `(a, b)` and we want to pipe `a` and `b` into two different coroutines, we can do that using the operator [`***`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Arrow.html#v:-42--42--42-). If we want to pipe a single value `a` into two different coroutines, we can split it using the operator [`&&&`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Arrow.html#v:-38--38--38-). [This wiki page](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows) is an excellent resource if you want a more detailed explanation (with illustrations) on how the different arrow operators work conceptually.

## Basic Combinators

Let's extend the coroutine library with some simple event utilities that are useful for FRP.

```haskell
-- | Map events into different kinds of events
mapE :: (e -> e') -> Coroutine (Event e) (Event e')
mapE = arr . map

-- | Filter events based on a predicate function
filterE :: (e -> Bool) -> Coroutine (Event e) (Event e)
filterE = arr . filter

-- | Replace every event occurence with a fixed event
constE :: e -> Coroutine (Event e') (Event e)
constE = mapE . const

-- | Merge two time varying values using a combining function
zipWithC :: (a -> b -> c) -> Coroutine (a, b) c
zipWithC = arr . uncurry

-- | Merge two event streams together
zipE :: Coroutine (Event e, Event e) (Event e)
zipE = zipWithC (++)
```

Another function that turns out to be very useful in practice is `scanE`, which is like the `scan` coroutine we introduced earlier, but for events.

```haskell
scanE :: (a -> e -> a) -> a -> Coroutine (Event e) a
scanE f i = Coroutine $ step i where
    step a e = let a' = foldl' f a e in (a', scanE f a')
```

`scanE` takes an initial value and a function that is used to combine incoming events with the value. The result is a value which can be changed by events, but otherwise stays constant.

Other useful utilities for manipulating and combining time varying values are:

```haskell
-- | Split a value into (current value, previous value) using the given
--   initial value as the previous value during first call.
withPrevious :: a -> Coroutine a (a, a)
withPrevious first = Coroutine $ \i -> ((i, first), step i) where
    step old = Coroutine $ \i -> ((i, old), step i)

-- | Delay the value by a single time-step, using the given initial value for
--   the first call.
delay :: a -> Coroutine a a
delay a = withPrevious a >>> arr snd

-- | Integrate a numerical value over time
integrate :: Num a => a -> Coroutine a a
integrate = scan (+)

-- | Derivate a numerical value over time (i.e. return the delta between current
--   and previous time-step.
derivate :: Num a => Coroutine a a
derivate = withPrevious 0 >>> zipWithC (-)

-- | Trigger an event whenever the value satisfies the given predicate function
watch :: (a -> Bool) -> Coroutine a (Event a)
watch f = Coroutine $ \i ->
    if f i
        then ([i], watch f)
        else ([], watch f)
```

## FRP Pong

Using the utility functions defined above, let's try to define the rules of a simple Pong clone as a collection of coroutines. We'll begin by defining the necessary coroutines for the player to move his bat.

```haskell
playerPos :: Coroutine Keyboard PlayerPos
playerPos = playerSpeed >>> integrate startPos >>> arr (\y -> (10, y))

playerSpeed :: Coroutine Keyboard Int
playerSpeed = arr keyboardDir where
    keyboardDir kb
        | isKeyDown kb up   = -batSpeed
        | isKeyDown kb down = batSpeed
        | otherwise         = 0
```

The types and constants used in the above coroutines are:

```haskell
type Pos       = (Int, Int)
type PlayerPos = Pos

batSpeed = 5
batSize  = (10,40)
startPos = 200
```

The implementation of `playerPos` should be quite straightforward to follow. The player's y-position is calculated by integrating the player's speed and adding `startPos`. Notice how we don't store or manipulate state anywhere. We merely declare _how_ `playerPos` depends on the keyboard state.

Modeling the ball is a more interesting problem. We'll start by declaring a few more helpful types and utility functions.

```haskell
type BallPos  = Pos
type Velocity = (Int, Int)

ballInitPos = (400,200)
ballSize    = (8,8)
ballInitVel = (-6, -6)

topWall    = 10
bottomWall = 590

-- Ball bounce events for horizontal and vertical bounce
data BallBounce = HBounce | VBounce

-- Multiply a vector by a scalar
vecMul :: Int -> (Int, Int) -> (Int, Int)
vecMul c (x,y) = (x*c,y*c)

-- Add two vectors
vecAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecAdd (a,b) (c,d) = (a+c,b+d)

-- Adjust velocity based on a bounce event
bounce :: Velocity -> BallBounce -> Velocity
bounce (dx,dy) b = case b of
    HBounce -> (-dx,dy)
    VBounce -> (dx,-dy)
```

Now we can define the ball position in terms of bounce events:

```haskell
ballPos :: Coroutine (Event BallBounce) BallPos
ballPos = scanE bounce ballInitVel >>> scan vecAdd ballInitPos
```

We process the bounce events using the `bounce` function we defined above to calculate the current ball velocity, which is then used to calculate the ball position using a cumulative vector sum.


Let's see how we can generate the bounce events.

```haskell
wallBounce :: Coroutine BallPos (Event BallBounce)
wallBounce = watch (\(_,y) -> y < topWall || y > bottomWall) >>> constE VBounce

batBounce :: Coroutine (PlayerPos, BallPos) (Event BallBounce)
batBounce = watch collision >>> constE HBounce

collision :: (PlayerPos, BallPos) -> Bool
collision ((px,py),(bx,by)) = abs (px-bx) < w' && abs (py-by) < h' where
    w' = (bw + pw) `div` 2
    h' = (bh + ph) `div` 2
    (bw,bh) = ballSize
    (pw,ph) = batSize

```

Here we are starting to see a problem. The ball position depends on bounce events which are generated from collisions with the top and bottom walls as wells as the player bat. However, in order to generate those collisions we need to know the ball position. Chicken and egg.

So, what do we do? Let's assume for a moment, that we could somehow know the ball position before we generate it and write ballPos like this:

```haskell
ballPos :: Coroutine (PlayerPos, BallPos) BallPos
ballPos = arr (\(ppos, bpos) -> ((ppos, bpos), bpos))
    >>> batBounce *** wallBounce
    >>> zipE
    >>> scanE bounce ballInitVel
    >>> scan vecAdd ballInitPos
```

Here we are perfectly able to formulate the dependencies of the ball position.  As long as we already know the position, that is.

Enter recursive arrows (aka black magic).

## Recursive Arrows

In order to support arrow recursion, we need to define a new instance for our Coroutine.

```haskell
instance ArrowLoop Coroutine where
    loop co = Coroutine $ \b ->
        let ((c,d),co') = runC co (b,d)
        in (c, loop co')
```

The signature of loop is

```haskell
loop :: Coroutine (b,d) (c,d) -> Coroutine b c
```

What this means is that given a coroutine that takes in `(b,d)` tuples and outputs `(c,d)` tuples, we build a coroutine from *b* to *c*. So what happends to *d*? Let's illustrate with ascii art.

```
         +-----------+
-- b --> | Coroutine | -- c -->
     +-> | (b,d)(c,d)| -+
     |   +-----------+  |
     |                  |
     +-------- d -------+
```

So what `loop` does is it takes the output *d* and wraps it around back as an input. So the coroutine ends up receiving as input the value that it will itself produce *in the future*.

This works only because of laziness, and you also have to be very careful about how you pipe your data so you don't create a paradox and destroy the universe. In our game, we avoid the paradox by piping out the previous value of the ball position, which can then be used to calculate the current position.

Then we tie the ends together with `loop` and call it a day.

```haskell
ballPos :: Coroutine PlayerPos BallPos
ballPos = loop $ arr (\(ppos, bpos) -> ((ppos, bpos), bpos))
    >>> batBounce *** wallBounce
    >>> zipE
    >>> scanE bounce ballInitVel
    >>> scan vecAdd ballInitPos
    >>> withPrevious ballInitPos
```

The magic happens at the last line, where we split the ball position into `(current pos, previous pos)` using `withPrevious`. During the first iteration, when no previous value exists, we use the `ballInitPos` as a placeholder. The previous position is then fed back into the couroutine by `loop`.

## Arrow Notation

When you have several event streams all interacting with each other, it will become more difficult and cumbersome to express the game logic in terms of splitting and merging these streams using the arrow operators. Another option is to use the special [arrow notation](http://www.haskell.org/ghc/docs/7.2.1/html/users_guide/arrow-notation.html) of GHC. The arrow notation must be enabled either with the compiler command line flag `-XArrows` or by adding the following line at the beginning of the source file.

```haskell
{-# LANGUAGE Arrows #-}
```

For example, we could rewrite the original `playerPos` coroutine

```haskell
playerPos :: Coroutine Keyboard PlayerPos
playerPos = playerSpeed >>> integrate startPos >>> arr (\y -> (10, y))
```

in arrow notation like this:

```haskell
playerPos :: Coroutine Keyboard PlayerPos
playerPos = proc kb -> do
    spd <- playerSpeed -< kb
    y   <- integrate startPos -< spd
    returnA -< (10, y)
```

This notation is a bit more verbose, but it is a lot easier to see what is going on. We assign to "local variables" with the left arrow `<-` just like in monadic do-blocks, but arrows also require that you pipe in input from the other end. The mnemonic for the operators is that they form one long arrow going from right to left `<- -<` with the actual arrow operation in the middle.

If we want to define recursive arrows using the arrow notation, we need to add the keyword `rec` to the recursive part. The `ballPos` coroutine could be written using the arrow notation like this:


```haskell
ballPos :: Coroutine PlayerPos BallPos
ballPos = proc plPos -> do
    rec batB  <- batBounce  -< (plPos, pos)
        wallB <- wallBounce -< pos
        vel   <- scanE bounce ballInitVel <<< zipE -< (batB, wallB)
        pos   <- delay ballInitPos <<< scan vecAdd ballInitPos -< vel

    returnA -< pos
```

Inside the rec-block, we can use a variable before we assign it (like we do with `pos` in the above example). And like before, we delay `pos` to its previous value so that it can actually be evaluated.

### Additional Arrow Syntax

Since combining events from two sources (like `batB` and `wallB` above) is so common, we can define a new helper function that makes it a bit more convenient. It's also a good excuse to introduce another piece of arrow notation.

```haskell
mergeE :: Coroutine i (Event e) -> Coroutine i (Event e) -> Coroutine i (Event e)
mergeE = liftA2 (++)
```

Since `Coroutine` is an instance of `Applicative`, we can use [`liftA2`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#v:liftA2) to "lift" the standard list concatenation operator to work on two coroutines.

Now it would be nice if we could just write something like `mergeE batBounce wallBounce`, but our `mergeE` function only works on coroutines that have the same input type. Luckily, when using arrow notation there is special syntax for these kind of functions and thus we can rewrite `ballPos` as:

```haskell
ballPos :: Coroutine PlayerPos BallPos
ballPos = proc plPos -> do
    rec bounces <- (| mergeE (batBounce -< (plPos, pos)) (wallBounce -< pos) |)
        vel     <- scanE bounce ballInitVel -< bounces
        pos     <- delay ballInitPos <<< scan vecAdd ballInitPos -< vel

    returnA -< pos
```

Inside the `(|  |)`-brackets (sometimes called the "banana brackets"), we can call a function so that we pipe in different inputs for each parameter. For infix operators we don't even need the special brackets, so we could define an operator like

```haskell
(<++>) :: Coroutine i (Event e) -> Coroutine i (Event e) -> Coroutine i (Event e)
(<++>) = liftA2 (++)
```

and use it like this

```haskell
ballPos :: Coroutine PlayerPos BallPos
ballPos = proc plPos -> do
    rec bounces <- (batBounce -< (plPos, pos)) <++> (wallBounce -< pos)
        vel     <- scanE bounce ballInitVel -< bounces
        pos     <- delay ballInitPos <<< scan vecAdd ballInitPos -< vel

    returnA -< pos
```


### Resetting the ball position

Next we want to change the ball behavior so that when it goes out of the screen
it is reset back to its initial position. We can generalize this kind of behaviour into a helper function that transforms a coroutine so that it will restart from the beginning when it receives an event.

```haskell
restartWhen :: Coroutine a b -> Coroutine (a, Event e) b
restartWhen co = Coroutine $ step co where
    step c (i, ev) = (o, Coroutine cont) where
        (o, c') = runC c i
        cont
            | null ev   = step c'
            | otherwise = step co
```

We can then define the new resetting behaviour as another recursive arrow that uses the old `ballPos` behaviour.

```haskell
resettingBallPos :: Coroutine PlayerPos BallPos
resettingBallPos = proc plPos -> do
    rec pos   <- restartWhen ballPos -< (plPos, reset)
        reset <- watch outOfBounds -< pos
    returnA -< pos
    where outOfBounds (x,_) = x < 0 || x > 800
```

or, alternatively, without using the arrow notation:

```haskell
resettingBallPos :: Coroutine PlayerPos BallPos
resettingBallPos = loop $ restartWhen ballPos >>> id &&& watch outOfBounds
    where outOfBounds (x,_) = x < 0 || x > 800
```

Now the ball resets back to its initial position whenever it flies out of the screen.

## Putting it All Together

The main coroutine has the type `Coroutine Keyboard Rects`, i.e. every time it is called, it gets the current state of the keyboard as a parameter, and it returns a collection of rectangles that should be rendered on the screen. This allows our game logic to be a pure function which doesn't have to know anything about the library that is used for reading the keyboard and to do the actual graphics rendering.


```haskell
game :: Coroutine Keyboard [Rect]
game = proc kb -> do
    plPos <- playerPos -< kb
    blPos <- resettingBallPos -< plPos
    returnA -< [mkRect plPos batSize, mkRect blPos ballSize]

mkRect :: Pos -> Size -> Rect
mkRect (x,y) (w,h) = ((x-w',y-h'),(w,h)) where
    w' = w `div` 2
    h' = h `div` 2
```

We have now covered all the building blocks that are required for a simple game like pong (the complete code can be viewed [here](https://github.com/shangaslammi/frp-pong/blob/master/Pong/Game.hs)). Usually, most required behaviours can be defined using high level arrows and combinators, but sometimes you might want to drop down to a lower level, and code a game-specific coroutine, such as a state switcher.

One important thing that we didn't cover yet is how to handle dynamic collections of game objects, where new objects with dynamic behaviour can be created mid-game and existing ones can disappear. These will be covered in the next article, along with a more complete example of game logic from a more complex game.

<hr/>

Sami Hangaslammi <[sami.hangaslammi@leonidasoy.fi](mailto://sami.hangaslammi@leonidasoy.fi)>

Leonidas Oy <[http://leonidasoy.fi](http://leonidasoy.fi)>


