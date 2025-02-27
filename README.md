# rearrangements

This repo takes the rearrangements part of [rearrange](https://github.com/finnbar/rearrange) for easy inclusion into other projects. New TemplateHaskell versions of rearrangements have also been added.

## rearrangements via typeclasses

To perform rearrangements like in the original rearrange work, it suffices to import the following two modules:

* `Data.HList` provides a simple heterogeneous list implementation.
* `Rearrange.Typeclass` provides a typeclass-based implementation of rearrangements.

Then we write rearrangements using `rearr` and `rearrDel`:

```haskell
import Data.HList
import Rearrange.Typeclass

list :: HList '[Int, Bool, Int]
list = 3 :+: True :+: 4 :+: HNil

list' :: HList '[Int, Int]
list' = let (res, spare) = rearrDel list in res
-- res = 3 :+: 4 :+: HNil
-- spare = True :+: HNil

list'' :: HList '[Bool, Int]
list'' = rearr list
-- will return True :+: 3 :+: HNil
```

We can also apply rearrangements to alternative structures by specifying instances of `Rearrangeable` and `RearrangeableStar` for them. The latter allows for rearrangements of nested structures, like rearranging `HList '[HList '[Bool, Int], ()]` to `HList '[Bool, HList '[(), Int]]`, but as a result requires those structures to be of kind `Type`. This is useful for structures which are isomorphic to `HList`, so that we do not need to convert to and from `HList`s to use rearrange. More details can be found in `Rearrange.Rearrangeable`.

## rearrangements via TemplateHaskell

The `Rearrange.TH` module provides a typed TemplateHaskell version of these rearrangements. These are used near identically to rearrangements with type classes, but with slightly different names:

```haskell
import Data.HList
import Rearrange.TH

list :: HList '[Int, Bool, Int]
list = 3 :+: True :+: 4 :+: HNil

list' :: HList '[Int, Int]
list' =
    let (res, spare) = $$(rearrangeDelTH @'[Int, Bool, Int] @'[Int, Int]) list
    in res
-- res = 3 :+: 4 :+: HNil
-- spare = True :+: HNil

list'' :: HList '[Bool, Int]
list'' = $$(rearrangeTH @'[Int, Bool, Int] @'[Bool, Int]) list
-- will return True :+: 3 :+: HNil
```

Note that the TemplateHaskell versions require type annotations: this is due to [a limitation in Typed TemplateHaskell](https://gitlab.haskell.org/ghc/ghc/-/issues/10271).