# Statically Typed Vector Algebra Using Type Families

I've never used [type families](http://www.haskell.org/haskellwiki/GHC/Type_families) in Haskell before, but I've wanted to learn about them for a long time now, so I decided to get my feet wet and implement some sort of toy library that takes advantage of the new features the syntax allows.

So let that be a disclaimer: I'm a complete newbie with type families and there is a high likelihood that any code below is unidiomatic and/or just plain bad. :)

## A More Flexible Num Class

One particular thing that always bothered me in Haskell when I started learning it was the [`Num`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Num) typeclass, which only allows arithmetic operations on two `Num` instances if they have the same type. This is fine for plain numbers, but it would be nice to be able to overload the standard numeric operators to work between, for example, numbers and vectors or vectors and matrices.

Defining such heterogeneous operators has been possible for a long time using the language extensions [`MultiParamTypeClasses`](http://www.haskell.org/haskellwiki/Multi-parameter_type_class) and [`FunctionalDependencies`](http://www.haskell.org/haskellwiki/Functional_dependencies), but type families make it even nicer, in my opinion.

So, let's get started.

```haskell
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Data.Algebra where

import Prelude hiding (Num(..))
import qualified Prelude as P
```

We'll hide the built-in `Num` typeclass and all its functions, and re-import `Prelude` so that we can access the original functions with the prefix `P.` when needed. We'll then break down the functionality of the `Num` typeclass to smaller parts, because not all type combinations support all operations. For example, matrices and vectors can be multiplied together, but not added. The `LANGUAGE` pragma enables the type families and multi-param typeclasses language extensions.

## Defining Addition Using an Associated Type Synonym

Let's first declare the typeclass for types that support addition.

```haskell
class Add a b where
    type AddResult a b

    (+) :: a -> b -> AddResult a b
```

One thing that the type families extension lets us do is to declare that some type varies in the different instances of the type class. So the type declaration `type AddResult a b` inside the typeclass means that AddResult is a type alias for some actual type, which depends on the generic types `a` and `b` and which needs to be specified in the instances of this typeclass.

Integers can be added together, so let's define an instance for `Int`.

```haskell
instance Add Int Int where
    type AddResult Int Int = Int

    (+) = (P.+)
```

Here we are saying that the result of the addition between two ints is an int. For the implementation of the addition operator, we just use the (+) from `Prelude`.

You can think of `AddResult` as a sort of function that operates on types. It takes two types as parameters and returns a new type.

Now, we could define an instance of `Add` for all number types, such as floats, doubles etc. by hand, but that would be too tedious. We can simply declare that any type that is a `Num` automatically supports addition. So let's replace the above type instance with a more generic one.

```haskell
instance P.Num n => Add n n where
    type AddResult n n = n

    (+) = (P.+)
```

In order to compile this, you need to enable the `FlexibleInstances` language extension.

This added genericity comes with a downside, however. Number literals in Haskell are a bit special in that if no explicit type is given, the compiler picks a suitable type for the literal. This allows us to do e.g.

```
Prelude> 1 + 2.5
3.5
```

Because the compiler will decide that the literal `1` is actually a double in this context.

However, with the more generic `Add` typeclass, the compiler can no longer decide what the actual types of literals should be, because `Int + Float` might have a completely different implementation than `Float + Float`. So unfortunately, we will have to annotate all literals with explicit types when using these new typeclasses.

```haskell
*Data.Algebra> (1::Float) + (2.5::Float)
3.5
```

Multiplication for number types is straightforward after implementing `Add`.

```haskell
class Mul a b where
    type MulResult a b

    (*) :: a -> b -> MulResult a b

instance P.Num n => Mul n n where
    type MulResult n n = n

    (*) = (P.*)
```

For division, we can make a special case for int division, so that it actually returns rational numbers.

```haskell
import Data.Ratio (Ratio, (%))

class Div a b where
    type DivResult a b

    (/) :: a -> b -> DivResult a b

instance Div Float Float where
    type DivResult Float Float = Float

    (/) = (P./)

instance Div Int Int where
    type DivResult Int Int = Ratio Int

    (/) = (%)
```

It is unfortunate that we cannot simply create generic instances for all types that implement `Fractional` or `Integral`, because that would lead to ambiguities since you could, in theory, declare a type that implements both.

We can now test the instances and see that the result of division correctly depends on the operand types.

```
*Data.Algebra> (1::Float) / (2::Float)
0.5
*Data.Algebra> (1::Int) / (2::Int)
1 % 2
```

## Vectors

Let's implement a vector type using arrays.

```haskell
import Data.Array.Unboxed

newtype Vector = Vector { vecArray :: UArray Int Float }
```

Vectors can be multiplied by scalars.

```haskell
instance Mul Float Vector where
    type MulResult Float Vector = Vector

    x * (Vector v) = Vector . amap (P.* x) $ v
```

In order to test the multiplication, we'll define a constructor and a `Show` instance for vectors.

```haskell
vector2d :: Float -> Float -> Vector
vector2d x y = Vector $ listArray (1,2) [x,y]

instance Show Vector where
    show = show . elems . vecArray
```

```
*Data.Algebra> (1.5::Float) * vector2d 3.0 4.0
[4.5,6.0]
```

Addition between two vectors adds them element-wise, but addition can only be performed between vectors that have the same number of components.

It would be really nice if the type system enforced this somehow for vectors with known dimensionality, and it turns out we can achieve this quite easily using [phantom types](http://www.haskell.org/haskellwiki/Phantom_type).

A phantom type is essentially a type parameter of a parametric data type that is not used in the actual type definition. E.g.

```haskell
newtype Vector d = Vector { vecArray :: UArray Int Float }
```

Here the `Vector` type has a parameter `d` for dimensionality, but its internal structure doesn't use this type information in any way. We can use empty types for annotating dimensionality using the language extension [`EmptyDataDecls`](http://www.haskell.org/haskellwiki/Empty_type).

```haskell
data D1
data D2
data D3
```

We can now add more precise type signatures.

```haskell
vector2d :: Float -> Float -> Vector D2
vector2d x y = Vector $ listArray (1,2) [x,y]

vector3d :: Float -> Float -> Float -> Vector D3
vector3d x y z = Vector $ listArray (1,3) [x,y,z]

instance Mul Float (Vector d) where
    type MulResult Float (Vector d) = (Vector d)

    x * (Vector v) = Vector . amap (P.* x) $ v

instance Show (Vector d) where
    show = show . elems . vecArray
```

The constructors now return vectors of specific dimensionality, and the multiplication explicitly declares that the operation doesn't change the number of dimensions. Now we can implement vector addition so that we have a compile-time type check that ensures we can only add vectors that have the same length.

```haskell
instance Add (Vector d) (Vector d) where
    type AddResult (Vector d) (Vector d) = Vector d

    (Vector a) + (Vector b) = Vector $ listArray bds els where
        bds = bounds a
        els = zipWith (P.+) (elems a) (elems b)
```

However, this instance declaration overlaps with the existing instance `Num n => Add n n` (I assume this is because somebody might create a `Num` instance for vectors), so we need to add yet another language extension: [`OverlappingInstances`](http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap)

Let's see how it works:

```
*Data.Algebra> vector2d 2.0 3.0 + vector2d 3.0 4.0
[5.0,7.0]
*Data.Algebra> vector2d 2.0 3.0 + vector3d 1.0 2.0 3.0

<interactive>:1:18:
    No instance for (Add (Vector D2) (Vector D3))
      arising from a use of `+'
    Possible fix:
      add an instance declaration for (Add (Vector D2) (Vector D3))
    In the expression: vector2d 2.0 3.0 + vector3d 1.0 2.0 3.0
    In an equation for `it':
        it = vector2d 2.0 3.0 + vector3d 1.0 2.0 3.0
```

That's exactly what we wanted! A compiler error when we try to add 2D and 3D vectors together.


## Matrices

Matrices can be implemented like vectors, but with two dimension parameters.

```haskell
newtype Matrix m n = Matrix { matArray :: UArray (Int,Int) Float }
```

The matrix multiplication *A x B* is only valid if the dimensions of matrices A and B are *m x p* and *p x n*, which results in a *m x n* matrix. This can be defined very naturally with our associated type.

```haskell
instance Mul (Matrix m p) (Matrix p n) where
    type MulResult (Matrix m p) (Matrix p n) = Matrix m n

    (Matrix a) * (Matrix b) = Matrix $ listArray bds els where
        bds = ((1,1),(m,n))
        els = el <$> [1..m] <*> [1..n]

        el i j = sum [a!(i,k) * b!(k,j) | k <- [1..p]]

        (_,(m,p)) = bounds a
        (_,(_,n)) = bounds b
```

Multiplying matrices and vectors together is, again, a bit ambiguous because we might want to use the vector either as a row-vector or a column-vector. So let's make our intention explicit.

```haskell
row :: Vector d -> Matrix D1 d
row (Vector v) = Matrix $ ixmap ((1,1),(1,n)) (\(1,j) -> j) v where
    (_,n) = bounds v

col :: Vector d -> Matrix d D1
col (Vector v) = Matrix $ ixmap ((1,1),(m,1)) (\(i,1) -> 1) v where
    (m,_) = bounds v
```

Here, again, the mere type-signatures of the functions describe their behavior wonderfully.


## Type Algebra

Constructing matrices poses the problem that if we need to write an explicit constructor for every size combination, that'll be a lot of tedious work. One option is to construct vectors and matrices from small pieces, one dimension at a time.

However, to do that, we need to do more than just tag dimensionality with empty types. We need to somehow express the relation between types such as `D2` and `D3`.

The so called [Peano axioms](http://en.wikipedia.org/wiki/Peano_axioms) define natural numbers in terms of *zero* and *S(n)* where *S(n)* is the successor for any natural number *n*. So, for example, the natural number one is defined as *S(zero)* and the number two is *S(S(zero))* and so on. We can use a similar system to establish relations between the different dimensionalities.

```haskell
data D1
data Succ d
type D2 = Succ D1
type D3 = Succ D2
```

We disallow vectors with zero dimensions, so we start with `D1` and define each following dimensionality in terms of `D1` and `Succ`.

Now we can define a builder operation that prepends new dimensions to vectors and matrices.

```haskell
class Cons e c where
    type ConsResult e c

    (+:) :: e -> c -> ConsResult e c

instance Cons Float (Vector d) where
    type ConsResult Float (Vector d) = Vector (Succ d)

    x +: (Vector v) = Vector . listArray bds . (x:) . elems $ v where
        bds = second succ $ bounds v
```

Again, thanks to type families, we are able to encode at type level, that prepending a new element to a vector increases its dimensionality by one.

For matrices, we'll cons row-vectors.

```haskell
instance Cons (Vector n) (Matrix m n) where
    type ConsResult (Vector n) (Matrix m n) = Matrix (Succ m) n

    (Vector a) +: (Matrix b) = Matrix $ listArray bds els where
        bds = second (second succ) $ bounds b
        els = elems a ++ elems b
```

Now we can build matrices of any size from vectors and still have the dimensions statically checked.

```haskell
*Data.Algebra> let a = vector3d 1 2 3
*Data.Algebra> let b = vector3d 4 5 6
*Data.Algebra> let c = vector3d 7 8 9

*Data.Algebra> let m = a +: b +: row c
*Data.Algebra> m
[1.0,2.0,3.0]
[4.0,5.0,6.0]
[7.0,8.0,9.0]

*Data.Algebra> :t m
m :: Matrix (Succ (Succ D1)) (Succ D2)
```

While ghci doesn't always show us the nice type aliases, we can easily see that the type of the resulting matrix is indeed equal to `Matrix D3 D3`.


## Statically Typed Concatenation

What if we want to concatenate two vectors together? How do we maintain the type information then?

```haskell
class Concat a b where
    type ConcatResult a b

    (++:) :: a -> b -> ConcatResult a b
```

Now the dimensionality of the resulting vector needs to be the sum of the dimensionalities of the two operand vectors, and as crazy as it sounds, we can actually perform arithmetic operations with the types themselves.

```haskell
class AddT a b where
    type AddTResult a b

instance AddT D1 a where
    type AddTResult D1 a = Succ a

instance AddT (Succ a) b where
    type AddTResult (Succ a) b = AddTResult a (Succ b)
```

Unfortunately, this kind of type manipulation opens up another can of worms and, again, we need a new extension: [`UndecidableInstances`](http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#undecidable-instances).

However, now we can implement concatenation with proper types.

```haskell
instance Concat (Vector a) (Vector b) where
    type ConcatResult (Vector a) (Vector b) = Vector (AddTResult a b)

    (Vector a) ++: (Vector b) = Vector $ listArray bds els where
        bds = (\(_,i)(_,j) -> (1,i+j)) (bounds a) (bounds b)
        els = elems a ++ elems b
```

Let's test that the type-checker allows operations between vectors that have been built in different ways:

```
*Data.Algebra> let v = (vector2d 1 2 ++: vector2d 3 4) + ((1::Float) +: vector3d 2 3 4)
*Data.Algebra> v
[2.0,4.0,6.0,8.0]
*Data.Algebra> :t v
v :: Vector (Succ (Succ (Succ D1)))
```

## Statically Checked Indexing

One more thing we can do is to ensure that when we access vector components, the index is verified to be valid during compile time. This will unfortunately require some plumbing (let me know if you can come up with a simpler way!).

```haskell
data Get d = Safe

getPrev :: Get (Succ d) -> Get d
getPrev _ = Safe

class GetIndex g where
    getIndex :: g -> Int

instance GetIndex (Get D1) where
    getIndex = const 1

instance GetIndex (Get d) => GetIndex (Get (Succ d)) where
    getIndex = (P.+ 1) . getIndex . getPrev
```

Now we have a type that can map the dimensionality types to array indices.

We'll start with the simple case, i.e. it is always safe to index a vector with a value that is equal to the vector's dimensionality.

```haskell
class SafeGet g c where
    type GetElem c

    (!!!) :: c -> g -> GetElem c

instance GetIndex (Get d) => SafeGet (Get d) (Vector d) where
    type GetElem (Vector d) = Float

    Vector v !!! g = v ! getIndex g
```

Now we can index, for example, a two element vector with the type `D2`.

```
*Data.Algebra> let v = vector2d 1 2
*Data.Algebra> v !!! (Safe::Get D2)
2.0
```

but indexing with `D1` doesn't match our instance declaration.

```
*Data.Algebra> v !!! (Safe::Get D1)

<interactive>:1:3:
    No instance for (SafeGet (Get D1) (Vector D2))
      arising from a use of `!!!'
    Possible fix:
      add an instance declaration for (SafeGet (Get D1) (Vector D2))
    In the expression: v !!! (Safe :: Get D1)
    In an equation for `it': it = v !!! (Safe :: Get D1)
```

What we really want to express is that indexing is safe whenever the indexing dimension is the same *or lower* than the vector's dimensionality. I.e. we need to be able to compare types and have type level conditionals.

```haskell
data TTrue -- type level truth value

-- type level less-or-equal
class TLessEq x y where
    type IsLessEq x y

-- D1 is equal to D1
instance TLessEq D1 D1 where
    type IsLessEq D1 D1 = TTrue

-- D1 is less than any successor type
instance TLessEq D1 (Succ d) where
    type IsLessEq D1 (Succ d) = TTrue

-- The ordering of x and y is the same as (Succ x) and (Succ y)
instance TLessEq x y => TLessEq (Succ x) (Succ y) where
    type IsLessEq (Succ x) (Succ y) = IsLessEq x y
```

Now we can rewrite the `SafeGet` instance for vectors using the type operator `~` which declares that its two type operands must be equal.

```haskell
-- Instance for all types (Get i) and (Vector d) for which i <= d
instance (GetIndex (Get i), IsLessEq i d ~ TTrue) => SafeGet (Get i) (Vector d) where
    type GetElem (Vector d) = Float

    Vector v !!! g = v ! getIndex g
```

And we can then index two-dimensional vectors with D1 and D2, but get a compile-time error when indexing with D3.

```
*Data.Algebra> let v = vector2d 1 2
*Data.Algebra> v !!! (Safe::Get D1)
1.0
*Data.Algebra> v !!! (Safe::Get D2)
2.0
*Data.Algebra> v !!! (Safe::Get D3)

<interactive>:1:3:
    Couldn't match type `IsLessEq (Succ D1) D1' with `TTrue'
    arising from a use of `!!!'
    In the expression: v !!! (Safe :: Get D3)
    In an equation for `it': it = v !!! (Safe :: Get D3)
```

The instance for safe matrix indexing is similar:

```haskell
instance (GetIndex (Get i), GetIndex (Get j), IsLessEq i m ~ TTrue, IsLessEq j n ~ TTrue)
    => SafeGet (Get i, Get j) (Matrix m n) where

    type GetElem (Matrix m n) = Float

    Matrix m !!! (i,j) = m ! (getIndex i, getIndex j)
```

## Vectors and Matrices of Arbitrary Size

In order to be generic, we might also want to support unsafe vectors and matrices whose size is not known during compile-time. For that, we can define a type that represents unknown, arbitrary dimensionality and functions for type-casting safe values into unsafe values.

```haskell
data Arb

class ToArb a where
    type ArbResult a

    toArb :: a -> ArbResult a

instance ToArb (Vector d) where
    type ArbResult (Vector d) = Vector Arb

    toArb (Vector v) = Vector v

instance ToArb (Matrix m n) where
    type ArbResult (Matrix m n) = Matrix Arb Arb

    toArb (Matrix m) = Matrix m
```

Because how our instances are defined, expressions such as `(m :: Matrix Arb Arb) * (v :: Vector Arb)` type-check, but can trigger run-time errors. Now we can use the exact same library functions for handling vectors and matrices that are of unknown size.


## Conclusion

I learned a lot of new things about Haskell's type system while writing this entry, and hopefully there were bits that were interesting for you too.

While statically checked vectors and matrices might not be all that useful for general purpose computation, I think they could be very handy in application domains such as computer graphics, where you mostly work with 2D and 3D vectors and their transformation matrices.

You could have, for example, a projection function with explicit types

```haskell
project :: Matrix D3 D3 -> Vector D3 -> Vector D2
```

so that you know for certain that you are not mixing up screen coordinates with world coordinates somewhere.

All the code written for this post can be viewed and downloaded [here](https://gist.github.com/1506540).

## Updates

### 2011-12-21 21:13 UTC

The user ryani over at the [reddit thread](http://www.reddit.com/r/haskell/comments/nlctb/statically_typed_vector_algebra_with_type_families/) made a great point about `AddT`. Since the typeclass doesn't contain any methods, it doesn't make much sense to use a typeclass with associated types. You can simply use a top level type family, which also gets rid of the requirement for UndecidableInstances. This is a big deal, as the aforementioned extension has a lot of drawbacks.

```haskell
type family AddT a b
type instance AddT D1 b = Succ b
type instance AddT (Succ a) b = Succ (AddT a b)
```

--
Sami Hangaslammi <[sami.hangaslammi@leonidasoy.fi](mailto://sami.hangaslammi@leonidasoy.fi)>

Leonidas Oy <[http://leonidasoy.fi](http://leonidasoy.fi)>
