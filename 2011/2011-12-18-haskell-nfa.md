# NFA in a Single Line of Haskell (aka. The List Monad is Awesome)

[Nondeterministic finite automata][1] are most often used in the context of regular languages, and while the efficient approach in most practical applications is to translate the NFA to an equivalent [deterministic finite automaton][3], simulating the non-determinism directly makes for an interesting coding problem. In Haskell, it turns out we can leverage the power of the list monad to implement an NFA almost trivially.

A nondeterministic finite automaton is defined in terms of

 * the initial state
 * a set of accepting states
 * a transition function, which takes a state and an input symbol and returns all possible next states

Given the above, we can define an NFA type in Haskell as

```haskell
data NFA q s = NFA
    { intialState :: q
    , isAccepting :: q -> Bool
    , transition  :: q -> s -> [q]
    }
```

Note that the types for state and input grammar are generic, so we don't have to care about their internal representations.

To test whether a given NFA accepts the given input, we want to write a function which takes an NFA and a list of input symbols and returns whether the NFA accepts the input or not.

```haskell
testNFA :: NFA q s -> [s] -> Bool
```

Because of nondeterminism (i.e. each transition can lead to several possible states), the implementation has to try different branches and backtrack if the current branch didn't hit an accept state at the end of input. In practice, we can achieve the same end result if we maintain a list of all possible states where we can be at any given point in the input.

In the end, the actual implementation boils down to

```haskell
testNFA (NFA i a t) = any a . foldM t i
```

At first glance, it might be hard to believe that this is actually a fully functioning NFA implementation. We can define a simple test grammar to try it out:

```haskell
data State  = Q1 | Q2 | Q3 | Q4 | Q5 deriving (Eq, Show)
data Symbol = A | B | C | D deriving (Eq, Show)

-- initial state
i = Q1

-- accept criteria
a = (`elem` [Q4,Q5])

-- state transitions
t Q1 A = [Q2]
t Q2 A = [Q3,Q4]
t Q2 B = [Q1,Q2]
t Q2 C = [Q3,Q4]
t Q3 D = [Q5]
t Q4 A = [Q2,Q4]
t _  _ = []

nfa = NFA i a t
```

Now we can test different input sequences to see whether they are accepted by our grammar.

```
*Main> testNFA nfa [A,B,C,D]
True
*Main> testNFA nfa [A,A,B,B]
False
```

So what actually happens? The real workhorse here is obviously `foldM` (from module `Control.Monad`). It does a [left fold][2] over a list using a monadic function, which in our case is the transition function *f*. We take advantage of the fact that lists are monads, and indeed, if we had a deterministic finite automaton, where the transition function had the type `q -> s -> q` (i.e. each transition only has one possible next state), we could write the function using a regular left fold.

```haskell
testDFA (DFA i a t) = a . foldl t i
```

For NFAs, the accumulator for the the fold function is still the current state, but the transition function returns a list of states which is clearly incompatible with the regular `foldl`, but as we can see from the signatures, `foldM` matches our use-case perfectly.

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
```

The standard implementation of `foldM` calls the given monadic function with the initial value of the accumulator and the first item in the list, and then uses the monadic bind operator `>>=` to call itself recursively.

```haskell
foldM _ a []     = return a
foldM f a (x:xs) = f a x >>= \fax -> foldM f fax xs
```

For the list monad, the bind operator works so that `xs >>= f` feeds every value of list `xs` to the function `f` and then concatenates the results. So, for example, given the example transition function we defined earlier, calling

```haskell
[Q1] >>= \q -> f q A
```

is the same thing as just calling `f Q1 A`

but calling

```haskell
[Q1,Q2] >>= \q -> f q A
```

concatenates all the possible next states from both `f Q1 A` and `f Q2 A`.

Now as we look back to the implementation of `testNFA`, we can see that for each symbol in the input list, foldM feeds the list of possible states we could be in now to the transition function using the bind operator of the list monad, resulting in the list of all possible states where the current input symbol could take us.

The final result form the `foldM` is the list of all possible states that we could be in when the input is fully consumed. This list is then compared against our accept criterion with `any a` to determine whether the input was valid.

One more thing worth mentioning is that the state transition function can use some other monadic context besides the list monad. For example, in a recent Lambda-Saturday meet up, we implemented a [probability distribution monad][4] as an exercise, and used that to implement a [probabilistic automaton][5].

[1]: http://en.wikipedia.org/wiki/Nondeterministic_finite-state_machine
[2]: http://en.wikipedia.org/wiki/Fold_(higher-order_function)#Folds_on_lists
[3]: http://en.wikipedia.org/wiki/Deterministic_finite_automaton
[4]: https://github.com/leonidas/lambda-5/blob/master/Data/Prob.hs
[5]: https://github.com/leonidas/lambda-5/blob/master/pa.hs
