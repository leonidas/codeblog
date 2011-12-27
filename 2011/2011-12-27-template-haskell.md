# Basic Tutorial of Template Haskell

I've been trying to learn [Template Haskell](http://www.haskell.org/haskellwiki/Template_Haskell) lately, but finding easy to understand, up-to-date tutorial material seems to be somewhat of a challenge, so I decided to write down my experiences while trying to decipher the basics of Haskell meta-programming.

The code in this post has been tested with GHC 7.0.3 and template-haskell 2.5.0.0.

## Quotations

The essence of Template Haskell is to write Haskell code that generates new Haskell code. To ensure that the generated code is well structured, we don't generate Haskell source code, but instead generate the [abstract syntax trees](http://en.wikipedia.org/wiki/Abstract_syntax_tree) directly. So, for example, the function call `f x` is described by the syntax tree `AppE (VarE f) (VarE x)`. However, building the syntax trees manually for anything but the simplest expressions is a lot work, so Template Haskell makes the task easier with quotations.

Quotations can be thought of as syntactic sugar for generating ASTs. There are several different kinds of quotations, depending on the context we are working in.

* Expression quotations are used for generating regular Haskell expressions, and the have the syntax `[|expression|]`. So for example `[|1+2|]` is syntactic sugar for the infix expression `InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))`.

* Declaration quotations are used for generating top-level declarations for constants, types, functions, instances etc. and use the syntax `[d|declaration|]`. Example: `[d|x = 5|]` results in `[ValD (VarP f) (NormalB (LitE (IntegerL 5))) []]`. Note that the quotation can contain multiple declarations, so it evaluates to a list of declaration values.

* Type quotations are used for generating type values, such as `[t|Int|]`

* Pattern quotations are used for generating patterns which are used, for example, in function declarations and case-expressions. `[p|(x,y)|]` generates the pattern `TupP [VarP x,VarP y]`.

* The last type is the so called "quasi-quotation", which lets us build our own, custom quotations, but these are a more advanced topic that won't be covered in this post.

For simple tasks, we don't really need type and pattern quotations, because they can be generated as part of a larger expression or declaration quotation. For example: `[d|head' (x:_) = x|]` results in `[FunD head' [Clause [InfixP (VarP x_1) GHC.Types.: WildP] (NormalB (VarE x_1)) []]]`.

The ASTs values generated using quotations are contained in the monad [`Q`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Syntax.html#t:Q). The `Q` monad handles such things as generating unique names and introspection of data types, but you don't really need to know anything about the inner workings of `Q`. The important thing is that declarations and expressions inside a `Q` monad can be used for "splicing" the generated ASTs into regular Haskell code.


## Example: Generating a Show Instance

As a simple example, let's see how we could automatically generate `Show` instances for data types using Template Haskell.

As a first step, we'll create a `Show` instance that always returns an empty string.

```haskell
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module CustomShow where

import Language.Haskell.TH

emptyShow :: Name -> Q [Dec]
emptyShow name = [d|instance Show $(conT name) where show _ = ""|]
```

So given a name, we declare an instance for the type by that name where `show` always returns `""`. The `[d|` prefix denotes this as a declaration quotation, and inside the quotation, we use `$()` to splice in another quotation, `conT name`. The [`conT`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Lib.html#v:conT) function constructs a `Q Type` value from a name.

(The language extension `FlexibleInstances` really shouldn't be needed here, but for some reason, splicing the declaration like we do here doesn't compile without it. I suspect it is because the `$()` splice in the instance declaration could potentially return a more complex type.)

We'll then define a test data type in `test_show.hs`:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import CustomShow

data MyData = MyData
    { foo :: String
    , bar :: Int
    }

emptyShow ''MyData

main = print $ MyData { foo = "bar", bar = 5 }
```

Since the return type of `emptyShow` is `Q [Dec]` (i.e. a list of declarations in the `Q` monad), we can call it directly at the top-level of the module to insert the generated declarations there. The two single-quotes in `''MyData` are used for escaping a name. This is the same as calling `mkName "MyData"`.

The `emptyShow` function has to be in a different file, because it needs to be already fully compiled at the point where we use it.

### Introspection Using reify

In order to show the fields of a record type, we need to introspect the type declaration. This is done in Template Haskell using a function called [`reify`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Syntax.html#v:reify). It returns an [`Info`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Syntax.html#t:Info) value in the `Q` monad.

```haskell
listFields :: Name -> Q [Dec]
listFields name = do
    TyConI (DataD _ _ _ [RecC _ fields] _) <- reify name
```

The structure returned by `reify` is rather complex, but in this toy-example, we just pattern-match with the hard-coded assumption that `reify` returns a type constructor, which is a data declaration, which contains exactly one record type constructor. From that, we can get a list of record fields. If our assumptions do not hold for some type, we will get a compile-time error.

The `fields` list contains `(name, strict, type)` tuples, but we are just interested in the field name for now, so let's separate that.

```haskell
    let names = map (\(name,_,_) -> name) fields
```

Next, we are going to build a quotation for a function that takes a record and shows the name and value of a specific field.

```haskell
    let showField :: Name -> Q Exp
        showField name = [|\x -> s ++ " = " ++ show ($(global name) x)|] where
            s = nameBase name
```

We use the expression quotation to generate a lambda function that returns the string "*name* = *value*". The content of the quotation is just regular Haskell code, except for the splice `$(global name)`, which is used to access the getter function for the specific named field. It is also noteworthy that we are able use the local variable `s` inside the quotation as is. In the generated AST, it will come out as a plain string literal.

Now that we can generate code for showing a single field, we simply need to iterate all the different field names.

```haskell
    let showFields :: Q Exp
        showFields = listE $ map showField names
```

The [`listE`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Lib.html#v:listE) utility function takes a list of expression quotations `[Q Exp]` and turns that into a single expression that returns a list. So, given a list of quotations that all generate a function of type `a -> String` we get a quotation for a list literal of type `[a -> String]`.

Finally, we declare the `Show` instance itself with a declaration quotation.

```haskell
    [d|instance Show $(conT name) where
        show x = intercalate ", " (map ($ x) $showFields)|]
```

As you can see, the Template Haskell language extension makes the `$` operator context sensitive. In order to use it as a function application operator (like in `map ($ x)`, it needs to be surrounded by space, otherwise it is interpreted as a splicing operator by Template Haskell (as in `$showFields`).

Other than having to prefix the identifier with `$`, you can treat `showFields` just like any other list literal. Here we map over the list, and pass `x` (which is the record object we are trying to show) to each function in the list. This results in a list of strings, which we intercalate with the separator `", "`.

Now we have a custom Show-macro that can be used to print out the fields of any record type.

```haskell
> print $ MyData { foo = "bar", bar = 5 }
foo = "bar", bar = 5
```

## Conclusion

There's much more to Template Haskell, but hopefully this will get you started. I've focused on the things that I feel are the most important hurdles in the beginning for most Template Haskell users, namely, expression quotations, declaration quotations, splicing and introspecting record data types. Quasi-quotes will be covered in more detail in a future blog post.

All the code in this entry can be viewed and downloaded [here](https://gist.github.com/1524967).

You can discuss this entry on [Reddit](http://www.reddit.com/r/haskell/comments/nsmq0/basic_tutorial_of_template_haskell/).

--
Sami Hangaslammi <[sami.hangaslammi@leonidasoy.fi](mailto://sami.hangaslammi@leonidasoy.fi)>

Leonidas Oy <[http://leonidasoy.fi](http://leonidasoy.fi)>

