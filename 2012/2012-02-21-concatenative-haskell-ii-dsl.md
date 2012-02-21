# Concatenative Haskell Part II: Custom DSL Syntax Using QuasiQuotes

In the [previous post](https://github.com/leonidas/codeblog/blob/master/2012/2012-02-17-concatenative-haskell.md) I introduced a way to emulate concatenative, stack based programming in Haskell code. The solution works, but it is a bit impractical to write

```haskell
foo = push 1 >>> push 2 >>> (+)
```

instead of being able to simply write something like

```haskell
foo = cc 1 2 (+)
```


I've been trying for the past day to implement something like `cc` using various typeclass and type level programming tricks, but while it's probably possible, I found my knowledge in this area lacking. Even though some solutions came close to kind-of-working like the above example, the implementations were very brittle and required lots of explicit type annotations to compile.


So, I decided to tackle the problem in a completely different way using [Template Haskell](http://www.haskell.org/ghc/docs/7.0-latest/html/users_guide/template-haskell.html), so the above example becomes

```haskell
foo = [cc| 1 2 (+) |]
```

Not quite as nice, but tolerable, and it'll be a good Template Haskell exercise. If you are completely new to Template Haskell, you can read my [`Basic Tutorial of Template Haskell`](https://github.com/leonidas/codeblog/blob/master/2011/2011-12-27-template-haskell.md) for a short introduction.


## Quasi-Quotes

The syntax `[foo| ... |]` is called a [quasi-quote](http://www.haskell.org/ghc/docs/7.0-latest/html/users_guide/template-haskell.html#th-quasiquotation) where `foo` is a specific, named "quoter". The section between the pipe characters can contain any syntax that the quoter recognizes.

From the implementor's point of view, to define a new quasi-quoter you basically just need to define a function that takes a string, parses it and produces Haskell [AST](http://en.wikipedia.org/wiki/Abstract_syntax_tree) using Template Haskell.

The nice thing about quasi-quoting based internal DSLs (compared to external DSLs) is that since the DSL is translated into Haskell AST, we gain all the benefits of Haskell's static type system. This means that any syntax or type error we make using the DSL is caught at compile time.


## Stack Language Parsing

We could implement our own custom parser, which would give us complete freedom to define any kind of syntax, but because 1) I'm pathologically lazy and 2) I've been looking for an excuse to do something with the [`haskell-src-meta`](http://hackage.haskell.org/package/haskell-src-meta) package, I'm going to define the custom postfix language syntax in terms of Haskell's syntax and use the Haskell parser from [`haskell-src-exts`](http://hackage.haskell.org/package/haskell-src-exts) package.

The real work-horse in this entry will be the [`parseExp`](http://hackage.haskell.org/packages/archive/haskell-src-exts/1.11.1/doc/html/Language-Haskell-Exts-Parser.html) function, which parses a string containg a Haskell expression and returns the syntax tree. The `haskell-src-meta` package is used to convert the `haskell-src` package's syntax tree to a format usable with Template Haskell.


## Test-Driven Development

This time, I will proceed in test-driven style and specify the syntax of the DSL incrementally via test cases using [`HUnit`](http://hackage.haskell.org/package/HUnit) and [`hspec`](http://hackage.haskell.org/package/hspec).

Here's the basic structure for the test module.

```haskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeOperators #-}

import Test.HUnit
import Test.Hspec
import Test.Hspec.HUnit

import Prelude hiding (drop, foldl, null)

import Language.Concat

specs = describe "Concatenative DSL" []

main = hspec specs
```

The `Language.Concat` module is a cleaned up version of the code from the previous entry, which exports the stack data type `s :. a` and the basic stack functions. You can browse the module contents [here](https://github.com/shangaslammi/concat-dsl-qq/blob/master/Language/Concat/Stack.hs).

As the first, simplest possible test case, let's add a test for an empty concatenative program, which should result in a Haskell function that takes a stack and returns it unchanged.

```haskell
specs = describe "Concatenative DSL" $
    [ it "should treat an empty program as the stack identity function" $ do
        let prog :: s -> s
            prog = [cc| |]
        prog () @?= ()
        prog (():.1) @?= () :. 1
    ]
```

The [`@?=`](http://hackage.haskell.org/packages/archive/HUnit/latest/doc/html/Test-HUnit-Base.html#v:-64--63--61-) operator is the equality assertion operator from HUnit, whereas  `:.` is the stack building operator from the [previous post](https://github.com/leonidas/codeblog/blob/master/2012/2012-02-17-concatenative-haskell.md).

The `cc` quasi-quotation evaluates to a Haskell function that takes in a stack and returns a (potentially) modified stack, and we test the function with an empty and non-empty stacks.


## Quasi-Quoter Implementation

The scaffolding for our quoter looks like this

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Language.Concat.TH (cc) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

cc = QuasiQuoter
    { quoteExp  = parseCC
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

parseCC :: String -> Q Exp
parseCC = undefined
```

The `cc` quoter can only be spliced into an expression context, so we set the `quoteExp` field and leave the rest undefined.

In order to implement the functionality described in the first test-case, we'll simply test whether the input string is empty after stripping out all whitespace and if so, return a [`VarE`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Syntax.html#v:VarE) (variable expression) that refers to the identity function.

```haskell
{-# LANGUAGE ViewPatterns #-}

import Data.Char (isSpace)

parseCC :: String -> Q Exp
parseCC (stripSpace -> "") = return $ VarE 'id  -- ' (workaround for github's  syntax highlight issue)

stripSpace :: String -> String
stripSpace = filter (not.isSpace)
```

The arrow syntax in the pattern of parseCC is part of the [View Patterns](http://hackage.haskell.org/trac/ghc/wiki/ViewPatterns) extension. The single-quote prefix in `'id` (which github's syntax highlight unfortunately trips on) is used for translating identifiers in the current scope into fully qualified name nodes in Template Haskell AST.


## Literal Values

The next thing we'll implement is the handling of literal values, starting again from the simplest case.

```haskell
    it "should implicitly push a lone literal value into the stack" $ do
        let prog = [cc| 1 |]
        prog () @?= ():.1
        prog (():."foo") @?= () :. "foo" :. 1
```

Let's test in GHCi what the `parseExp` function from `haskell-src-exts` returns for the string in question.

```haskell
> parseExp " 1 "
ParseOk (Lit (Int 1))
```

Now we know what to pattern-match against:

```haskell
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as S
import qualified Language.Haskell.Meta as M

parseCC :: String -> Q Exp
parseCC (stripSpace -> "") = return $ VarE 'id  -- '
parseCC (parseExp -> ParseOk (S.Lit lit)) =
    return $ AppE (VarE 'Stack.push) $ LitE $ M.toLit lit
```

[`AppE`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Syntax.html#v:AppE) (apply expression) applies a function to a value, in this case the `Stack.push` function to the literal value.

[`LitE`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Syntax.html#v:LitE) is a literal expression and [`toLit`](http://hackage.haskell.org/packages/archive/haskell-src-meta/0.5.1/doc/html/Language-Haskell-Meta-Syntax-Translate.html#v:toLit) from the `haskell-src-meta` package converts the literal node from the Haskell parser to a literal node in the Template Haskell AST.

### Multiple Literals

What about more than one literal value?

```haskell
    it "should push consecutive literals into the stack" $ do
        let prog = [cc| 1 "foo" 3 |]
        prog () @?= ():.1:."foo":.3
```

Perhaps surprisingly, the string `1 "foo" 3` is syntactically valid Haskell. Even though it wouldn't type-check in a real program, the parser will happily produce a syntax tree for us.

```haskell
> parseExp "1 \"foo\" 3"
ParseOk (App (App (Lit (Int 1)) (Lit (String "foo"))) (Lit (Int 3)))
```

What it means is that first we apply the function `1` to the value `"foo"` and then apply the resulting function to `3`. Of course integers aren't functions so the expression wouldn't compile, but at the syntax level, everything is fine.

As the syntax trees get more complex, it's time to rethink the structure our implementation a bit. Since chains of operations on the stack are expressed as nested function applications, we should have a recursive function that processes the syntax tree and produces a list of expressions where each element represents a "word" (in [Factor](http://factorcode.org/) terminology). A second function will take a list of words and fold them together using the arrow operator [`>>>`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Category.html#v:-62--62--62-).

```haskell
import Control.Arrow ((>>>))
import Data.List (foldl1')

parseCC :: String -> Q Exp
parseCC (stripSpace -> "") = return $ VarE 'id  -- '
parseCC (parseExp -> ParseOk exp) = return . combineExps . extractWords $ exp

extractWords :: S.Exp -> [Exp]
extractWords exp = case exp of
    S.Lit lit -> [push $ M.toExp $ M.toLit lit]
    S.App f v -> extractWords f ++ extractWords v

combineExps :: [Exp] -> Exp
combineExps = foldl1' step where
    step r l = InfixE (Just r) arr (Just l)
    arr = VarE '(>>>)  -- '

push :: Exp -> Exp
push = AppE $ VarE 'Stack.push
```

As planned, `extractWords` takes a syntax tree and produces a list of (Template Haskell) expressions. It currently handles literals and function application, but we'll expand the case statement as we add new features.

`combineExps` takes the list of expressions and folds them together by creating a tree of [`InfixE`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Syntax.html#v:InfixE) (infix expression) nodes, so that `[expA, expB, expC]` becomes `expA >>> expB >>> expC`.


## Function Words

After literals, it's time to tackle stack functions such as `dup`, so let's add another test case.

```haskell
    it "should apply named stack functions to the current stack" $ do
        let prog = [cc| 1 dup |]
        prog () @?= () :. 1 :. 1
```

It turns out this is a trivial addition to our current `extractWords` function.

```haskell
extractWords :: S.Exp -> [Exp]
extractWords exp = case exp of
    S.Lit lit -> [push $ M.toExp $ M.toLit lit]
    S.App a p -> extractWords a ++ extractWords p
    var@(S.Var _) -> [M.toExp var]
```

We'll assume that in the context of the DSL, all variables refer to stack functions, so we'll just return the variable as-is. So for example, in our test-case, `[cc| 1 dup |]` is transformed into the equivivalent of `push 1 >>> dup`.


## Infix Operators

Since all infix operators take two parameters, we can make it so that the quoter takes care of "lifting" for us. This means that instead of creating our own lifted operators like in the previous blog post (`(+) = liftS2 (Prelude.+)`), we can just use any regular infix operator. The test case looks like this

```haskell
    it "applies infix operators to the top two elements of the stack" $ do
        let prog = [cc| 1 2 (+) |]
        prog () @?= () :. 3
```

Again, we can use GHCi to examine the parse tree for our test case

```haskell
> parseExp "1 2 (+)"
ParseOk (App (App (Lit (Int 1)) (Lit (Int 2))) (Var (UnQual (Symbol "+"))))
```

It would be nice if we could drop the brackets, but `"1 2 +"` would be a syntax error in Haskell. I first thought about wrapping the whole expression in brackets, because "(1 2 +)" _would_ be syntactically valid, but the same trick wouldn't work for programs such as "(1 2 3 + +)", so let's just live with the bracketed operators for now.

```haskell
extractWords :: S.Exp -> [Exp]
extractWords exp = case exp of
    S.Lit lit -> [push $ M.toExp $ M.toLit lit]
    S.App a p -> extractWords a ++ extractWords p
    var@(S.Var (S.UnQual (S.Symbol _))) -> [liftS2 (M.toExp var)]
    var@(S.Var _) -> [M.toExp var]

liftS2 :: Exp -> Exp
liftS2 = AppE $ VarE 'Stack.liftS2  -- '
```

The implementation is almost the same as for function words in the previous step. We just apply `liftS2` to the given symbol to turn a regular infix operator into a stack operator.


## Quotations

Quotations (again, in Factor terminology, not be confused with quasi-quotations) are code blocks that are "quoted" (i.e. not executed immediately) and stored in the stack. In Factor's syntax quotations are enclosed in square brackets and we'll borrow that syntax for our DSL as well.

```haskell
    , it "should push operations in square brackets to the stack without executing them" $ do
        let prog = [cc| 1 [2 (+)] call |]
        prog () @?= () :. 3
```

Surprisingly, list values are not categorized as literals in the syntax tree, but have their own node type.

```haskell
> parseExp "[2 (+)]"
ParseOk (List [App (Lit (Int 2)) (Var (UnQual (Symbol "+")))])
```

Again, the implementation is just a matter of adding a new pattern to our case expression in `extractWords`.

```haskell
extractWords :: S.Exp -> [Exp]
extractWords exp = case exp of
    S.Lit lit -> [push $ M.toExp $ M.toLit lit]
    S.App a p -> extractWords a ++ extractWords p
    var@(S.Var (S.UnQual (S.Symbol _))) -> [liftS2 (M.toExp var)]
    var@(S.Var _) -> [M.toExp var]
    S.List q -> return $ push $ extractQuot q

extractQuot :: [S.Exp] -> Exp
extractQuot []  = VarE 'id  -- '
extractQuot [e] = combineExps $ extractWords e
```

An empty list means a quotation that does nothing (i.e. the stack identity function) and a single element list is recursively processed with `extractWords` and combined into a single expression that can be pushed to the stack.

Now we have enough functionality that we can rewrite the `foldl` example from the previous blog post using the new syntax.

```haskell
foldl = [cc|
    pick null
    [drop nip]
    [ [decons swap] dip2 rotl
      [[call] keep] dip rotr foldl
    ] if_
|]
```


## List Building

There's one problem, though. Since we used the square brackets syntax for quotations, we are left with no way to actually build lists in our DSL. We'll solve this by adding a new stack function that lets us turn a quotation into a list.

```haskell
    it "should turn quotations into lists with the 'list' word" $ do
        let prog = [cc| [1 2 dup] list |]
        prog () @?= () :. [1, 2, 2]
```

This functionality isn't really related to our DSL and requires no changes to the parser. We just need to implement a new stack function, `list`, which executes a quotation in an empty stack and then folds each item in the resulting stack into a list.

```haskell
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

list :: BuildList s' a => s :. (() -> s') -> s :. [a]
list (s:.q) = s :. (P.reverse $ buildList (q ()))

class BuildList s a | s -> a where
    buildList :: s -> [a]

instance BuildList () a where
    buildList () = []

instance BuildList s a => BuildList (s:.a) a where
    buildList (s:.a) = a : buildList s
```

The implementation of `list` goes slightly deeper into typeclass territory. With the `BuildList s a` class we define that for some type `s` we can build a list of type `[a]`, where the element type `a` depends on `s`. Then we just define typeclass instances so that the empty stack `()` always returns an empty list, and any list-buildable stack with a single element on top builds into a list with that element as head.

Now we can create lists to test the `foldl` function we defined earlier.

```haskell
> [cc| [1 2 3 4 5] list 0 [(+)] foldl |] ()
() :. 15
```


## Conclusion

Hopefully this post has given you some ideas on how to utilize `haskell-src-exts` and `haskell-src-meta` to create DSLs that extend or mutate the base Haskell syntax. If you have any comments or questions, please feel free to message me via github's messaging system or [email](mailto://sami.hangaslammi@leonidasoy.fi).


You can browse the code in [this github repo](https://github.com/shangaslammi/concat-dsl-qq).


You can also discuss this article on [Reddit]().


<hr/>
Sami Hangaslammi <[sami.hangaslammi@leonidasoy.fi](mailto://sami.hangaslammi@leonidasoy.fi)>

Leonidas Oy <[http://leonidasoy.fi](http://leonidasoy.fi)>
