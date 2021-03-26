# Reasoning about Haskell memory usage: What is the overhead of a `Storable` Vector?

_by nh2, 2021-03-20_

I have a program that uses `hmatrix` to represent 3D points (3-vectors of `Float`/`Double`), and stores many of them in larger vectors for various purposes (e.g. to represent point clouds).
3 `Doubles`s should occupy `3 * 8 B = 24 B (Bytes)`, but my program needed a lot more.
`hmatrix` uses `Data.Vector.Storable` for its vectors.
So I set out to determine what the memory overhead of a `Storable` vector is.

This is an exercise very similar to Johan Tibell's blog post [_Computing the size of a HashMap_](http://blog.johantibell.com/2011/06/computing-size-of-hashmap.html); conveniently he even inspected some of the same data types. I intend to be slightly more verbose and explicit, as reasoning about Haskell memory usage is considered a challenging topic, and hopefully more practical examples are useful to other users.

Let us unfold the representation of a `hmatrix` `Vector` from high to low level.

[This import in `hmatrix`](https://github.com/haskell-numerics/hmatrix/blob/2694f776c7b5034d239acb5d984c489417739225/packages/base/src/Internal/Vector.hs#L41) re-exports the `Data.Vector.Storable.Vector` as `hmatrix`'s `Vector`:

```haskell
module Internal.Vector(
  Vector,
  ...
) where

import Data.Vector.Storable(Vector, ...)
```

The `Storable` vector is [defined here](https://hackage.haskell.org/package/vector-0.12.2.0/docs/src/Data.Vector.Storable.html#Vector) and contains a length and a `ForeignPtr`:

```haskell
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(ForeignPtr a)
```

The `{-# UNPACK #-}` pragma is a memory saving technique:
Usually GHC would store store (into the memory behind the `Vector` value constructor) pointers to the `Int` and `ForeignPtr a` constructors.
The pragma makes GHC store their _actual_ constructors directly, in place of those pointers.
You can read more about this pragma [in the docs](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/pragmas.html#unpack-pragma).
It only works on _strict_ constructor fields (indicated by a _bang_ `!`).

On my 64-bit computer, an unboxed `Int`, and a pointer, each occupy 64 Bits = 8 Bytes = 1 "Word". I will assume 64-bit computers for the rest of this post, and will count in Words for simplicity.

There are a few more useful rules to know:

1. Any field in a `data` constructor (like the `Int` field above) is always at least pointer-sized, thus at least 1 Word.
2. For a `data` type that has multiple constructors, such as `data Either a b = Left a | Right b`, a 1-Word _tag_ needs to be stored to determine at run-time which constructor it is.
3. If a `data` having only a single constructor is `UNPACK`ed into a field, then no such tag is needed or stored.

But back to the type-unfolding. What's a `ForeignPtr`? Let's look at [the code](https://www.stackage.org/haddock/lts-17.7/base-4.14.1.0/src/GHC-ForeignPtr.html#ForeignPtr):

```haskell
data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents
```

Here, `Addr#` is a memory address; it is an [_Unboxed type_](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/primitives.html#unboxed-types) (indicated by the trailing `#`) that is built into the compiler, and such values are always strict. Because they are compiler primitives, there's no concept of `UNPACK`ing for them; an `Int#` is a raw 64-bit integer right in that location, and `Addr#` and `MutableByteArray#` are raw pointers.

Being a raw pointer, the `Addr#` is 1 Word.

What are `ForeignPtrContents`? Let's look at [the code](https://www.stackage.org/haddock/lts-17.7/base-4.14.1.0/src/GHC-ForeignPtr.html#ForeignPtrContents) just below:

```haskell
data ForeignPtrContents
  = PlainForeignPtr !(IORef Finalizers)
  | MallocPtr       (MutableByteArray# RealWorld) !(IORef Finalizers)
  | PlainPtr        (MutableByteArray# RealWorld)
```

So `ForeignPtrContents` can be one of three cases; in our case of a `Storable` Vector, it is usually a `PlainPtr`. As Johan Tibell's blog post shows, a `MutableByteArray#` takes 2 Words.

Now that we have fully unfolded our data type, we can start summing up the memory usage.


## How to NOT count

You may be tempted to write down the memory usage required by each type independently, and accounting for the size of lower-level types in higher-level types.

This will not work, because of rule (3) above:

> If a `data` having only a single constructor is `UNPACK`ed into a field, then no such tag is needed or stored.

According to this rule, we cannot determine whether a single-constructor data type will be unboxed (and thus save a Word) by looking just at the data type's definition; we have to look at where it's _used_ in other data types.

This is why in his blog post, Johan Tibell first performs all unboxing substitutions on his data types.

So the following is WRONG:

```haskell
data ForeignPtrContents             --     1 Word tag
  = ...
  | PlainPtr
      (MutableByteArray# RealWorld) --     1 Word pointer to `MutableByteArray#`
                                    --     2 Word size of `MutableByteArray#`

                                    == total: 4 Words

data ForeignPtr a =                 --     1 Word tag
  ForeignPtr
    Addr#                           --     1 Word
    ForeignPtrContents              --     1 Word pointer (since no UNPACK)
                                    --     4 Words size of `ForeignPtrContents`

                                    == total: 7 Words

data Vector a =                     --     1 Word tag
  Vector
    {-# UNPACK #-} !Int             --     1 Word
    {-# UNPACK #-} !(ForeignPtr a)  --     7 Words size of `ForeignPtr a`

                                    == total: 9 Words (this is wrong, note above)
```

Above we arrive at 9 Words overhead for the Vector, which is WRONG.


## How to count

```haskell
data ForeignPtrContents             --     1 Word tag
  = ...                             --       (certainly, as it's a
                                    --       multi-constructor sum type)
  | PlainPtr
      (MutableByteArray# RealWorld) --     1 Word pointer to `MutableByteArray#`
                                    --     2 Word size of `MutableByteArray#`

                                    == total: 4 Words

data ForeignPtr a =                 --       optional Word tag (depends on use site)
  ForeignPtr
    Addr#                           --     1 Word
    ForeignPtrContents              --     1 Word pointer (since no UNPACK)
                                    --     4 Words size of `ForeignPtrContents`

                                    == total: 6 Words

data Vector a =                     --       optional Word tag (depends on use site)
  Vector
    {-# UNPACK #-} !Int             --     1 Word
    {-# UNPACK #-} !(ForeignPtr a)  --       no pointer (due to UNPACK)
                                    --       no constructor (due to UNPACK)
                                    --     6 Words size of `ForeignPtr a`

                                    == total: 7 Words
```

Note how here, for single-constructor types, we did not count the _tag_ to the data type's size, but instead pushed it down into the use site, writing `no pointer (due to UNPACK)` and `no constructor (due to UNPACK)`.
If `Vector` was defined as `= Vector ... (ForeignPtr a)`, without `!` and `UNPACK`, then we'd account instead: `1 Word pointer (since no UNPACK)` together with `1 Word constructor (since no UNPACK)`.

Since `data Vector` also has a single constructor, we again cannot determine just from its definition whether or not its tag will need to be be stored somewhere; we'd have to look at its use sites.
Since `hmatrix` just re-exports `Vector`, there are no use sites we can inspect.
So let us make up a hypothetical use case:

Assume we have N many such Storable Vectors lying around on the Haskell heap.
This would be the case when a non-`UNPACK`ing data structure refers to them, such as a `[]` list or a normal (non-Storable) `Data.Vector`.
In that case, the N Storable Vectors would be heap objects, and must carry around their data constructor tag.

Thus their total memory usage would be `7 + 1 = 8 Words`, so 64 Bytes.

This result is different from from the incorrect 9 Words we obtained before.


## Alternative how to count

We could also perform data constructor substitution like Johan does in his post:

```haskell
data ForeignPtrContents             --     same as in earlier code listings

data Vector a =
  Vector
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !(ForeignPtr a)

-- Substitute `Int` by its unpacked type `Int#` and `(ForeignPtr a)` by
-- that type's single-constructor fields (without constructor tag):

data Vector a =
  Vector
    Int#                            --     1 Word
    -- ForeignPtr fields:
    Addr#                           --     1 Word
    ForeignPtrContents              --     1 Word pointer to `ForeignPtrContents`
                                    --     4 Words size of `ForeignPtrContents`

                                    == total: 7 Words
```

I do not like this method as much, because the substitution tends to be less compositional, creating large data structures that are difficult to keep track of independently if there are multiple levels of `UNPACK`ing.


## Measuring it with `weigh`

Reasoning is good, measuring is better.

Let's write a quick benchmark using Chris Done's [`weigh`](https://github.com/fpco/weigh) package:

```haskell
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Weigh
-- [..]

main = Weigh.mainWith $ do

  let powersOf10 = [0..7]

  let makeNManyStorableVectors n =
        V.generate n $ \i -> -- outer vector we use to allocate `n` many inner vectors
          VS.singleton (i :: Int) -- inner vector, what we want to measure

  for_ powersOf10 $ \(e :: Int) -> do
    let n = 10 ^ e
    Weigh.func ("Storable Vectors " <> " " <> show n) makeNManyStorableVectors n
```

[Full code here.](/code/haskell-memory-usage-storable-vector/) It also has some comments on how to measure correctly with `weigh`.

Above we create an outer normal Vector of Storable vectors, with memory layout:

```
outerVector = [
  [ (index 0) pointer to -> StorableVector=[0 :: Int]
  , (index 1) pointer to -> StorableVector=[1 :: Int]
  , (index 2) pointer to -> StorableVector=[2 :: Int]
  ...
  ]
```

We only want to compute the memory occupied by the `StorableVector`s, so we'll subtract the other parts below.

Output of the benchmark:

```
Case                                      Allocated  GCs         Live  Check          Max          MaxOS
singleton Storable Vectors  1                   144    0          528  OK             528              0
singleton Storable Vectors  10                  864    0        1,032  OK           1,032              0
singleton Storable Vectors  100              12,152    0       10,160  OK          10,160              0
singleton Storable Vectors  1000            104,560    0       80,968  OK          80,968              0
singleton Storable Vectors  10000         1,040,872    0      801,280  OK         801,280              0
singleton Storable Vectors  100000       10,399,880    9    8,000,288  OK       8,000,288     12,582,912
singleton Storable Vectors  1000000     104,006,432   92   80,006,840  OK      80,006,840    135,266,304
singleton Storable Vectors  10000000  1,040,080,112  921  800,080,520  OK     800,080,520  1,428,160,512
```

We can see that `1000000` entries need `80,006,840` Bytes, so that's around 80 Bytes per entry.

We can conclude the memory overhead of a Storable Vector (not including its contents) by looking at the `Max` memory usage for this test, and (assuming 64-bit) subtracting:

* 1 Word each of the outer Vector's entry pointers
* 1 Word for the contents being 1 `Int` in an inner Storable Vector

Ths `80 - 2*8 = 64 Bytes` overhead per Storable Vector.

This agrees with the result from our reasoning above.


## Summary

By accounting our data structures the simple method shown above, and then measuring it with `weigh`, we found that a `Data.Vector.Storable` as a heap object carries 8 Words = 64 Bytes of overhead.

This is quite a bit, when storing:

* 3-vectors of `Double`, it is 64/(64+24) = 73% overhead (`3.6x` more than needed).
* 3-vectors of `Float`, it is 64/(64+12) = 84% overhead (`6.3x` more than needed).

So my next task is clear: By changing my program from using many individual small Storable vectors, to fewer individual Storable vectors which each have more than 3 entries, I could reduce my memory usage by the indicated factors.


## Appendix: Eschewing single-constructor allocation with `UNPACK`

The fact that `UNPACK`ing single constructors avoids the allocationg of not only the pointer, but also the single constructor, does not seem to be documented in user-facing GHC docs yet.

Johan Tibell uses this knowledge in his blog post, but does not state the rule explicitly, which puzzled me when reading it.

Thus, find below my conversation on the `#ghc` IRC channel on the topic with Ben Gamari, who's the current GHC release manager and intimately familiar with how GHC's runtime data structures work.

nh2:

> I have a question about memory layout: In http://blog.johantibell.com/2011/06/computing-size-of-hashmap.html, tibbe actues that `data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents` gets unpacked into the ByteString constructor, resulting in `PS Addr# ForeignPtrContents ...`. However, given that it's `data ForeignPtr`, where is its constructor tag? Is there something special about `data`s with a single contructor? I've crawled the wiki, including the pages about pointer tagging, but that seems to be only relevant if there's an info table pointer in which we can tag some bits, but there's no pointer that could be used in his illustration.

bgamari:

> Yes, there is no need for a tag
> This is why we can't unpack fields of strict sum types currently.
> Let me rephrase:
> This is why we can't unpack strict fields whose value is a sum type currently.
> There's no place to put the tag.
> E.g. `data Ty = Ty !(Maybe Int)` won't be unpacked.
> This is orthogonal to pointer tagging though. Pointer tagging is merely an optimisation. The tag can also be recovered from the info table.

nh2:

> Does this entail that if I have a data D = Con {-# UNPACK #-} !Something, this is essentially equivalent in memory representation and behaviour to newtype D = D Something, as long as the D is itself unpacked wherever it's used? Or are there remaining differences?

bgamari:

> In in-memory representation, yes.
> But operationally, no.
> Since pattern matching on the `data D` will allocate a `Something`, whereas matching on the `newtype D` will just cast.

nh2:

> Thank you, that's exactly what I needed to know. Final question: Do you know wheter this already documented somewhere? I had a hard time determining it exactly from the user guide, and it seems like a key thing to know to reason about Haskell memory usage. Tibbe obviously knew it for the post, but he didn't state the rule there either.

bgamari:

> I suspect the answer is "no".
> It's possible you'd be able to synthesize this conclusion from a good read of the users guide, although I can't say with confidence that this is true.
> It would be good to state this explicitly.

nh2:

> Is the tag also eschewed when the single-constructor data is not unboxed, but a normal heap object?

bgamari:

> No, the info table pointer is always present.

TODO: For myself: Add a section `Reasoning about memory usage` to the GHC User Guide.
      The closest thing so far is https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects but that's really targeting GHC developers and very low-level.
