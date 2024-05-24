# Let-generalization: Let’s not?

# TL;DR

The proposal is to limit let-generalization to

- values that are syntactically functions
- values that are syntactically number literals

All other values bound to an identifier that have free type variables will not be generalized, leaving their free variables to be resolved to exactly one concrete type based on future usage. For the purposes of this document, we call such type variables “weak” while they have not been yet resolved to a concrete type

This proposal also prevents modules from exposing values with still-weak type variables. Now, a module may only expose values that are explicitly a concrete type, or explicitly generalized. In practice, this means that you can expose

- number literals, with or without type annotations
- syntactic functions, with or without type annotations
- non-literal number values only with type annotations, or as a thunk
- tag union values only with type annotations, or as a thunk
- non-syntactic functions only with type annotations

All of these restrictions with come with new, friendly error messages.

# Summary

Let-generalization is a great feature of Roc that allows you to use values polymorphically, so that you can have a function that operates over many datatypes, or define numeric constants that can be used as many different concrete number types without needing conversion.

However, today,

- Roc’s let-generalization is too powerful, and can lead to serious problems in the runtime semantics of a program
- Roc’s compilation scheme for arbitrary let-generalized values is also somewhat computationally-expensive, and limiting its scope would improve compile-times in general
- The current form of let-generalization means that there is a certain class of programs that we successfully type-check, but whose compilation are open technical questions. Limiting let-generalization is one solution to those questions.

The proposal comes in two parts. The suggestion is to implement the extended proposal first; if that causes issues in practice, it can be loosened to the baseline proposal.

The baseline proposal is to limit let-generalization to values that are either

- typed as functions, or
- are number literals

All other values with free type variables prior to let-generalization will have those variables demoted to weak type variables; that is, they can be used as at most one concrete type.

After the baseline proposal, an extension proposal is introduced that further limits let-generalization to

- values that are syntactically functions, or
- are number literals

and introduces a restriction on the exposing of eta-reduced function values from a module.

Okay, let me disambiguate exactly what’s being discussed here, and describe the problem.

## Background

### What is let-generalization?

Roc supports let-generalization of bound identifiers. When an identifier is assigned a value, any free type variables (a free type variable is one that is not yet known to have a concrete type, like `Nat` or `Str`) may be promoted to generalized type variables, which allows using that identifier as multiple different concrete types in the scope the identifier is present.

As an example, consider

```python
id = \x -> x

{ a: id "", b: id 1u8 }
```

In this program, `id` is implicitly generalized to a type `a -> a`, and each use of `id` in the remaining body permits the use of `id` as a specialized type; in particular, `Str -> Str` and `U8 -> U8`.

Put another way, the definition of `id` on the first line does not have a concrete type, and is only concretized at each location it is used. Each concretization instructs the compiler to produce a version of `id` specialized to that concrete type. That means this program is compiled equivalently to

```python
idStr = \x -> x
idU8 = \x -> x

{ a: idStr "", b: idU8 1u8 }
```

Roc also allows you to explicitly opt in or out of let-generalization via type annotations. Typing `id` as `id : a -> a` is a way to tell the Roc compiler that `id` should be let-generalized; typing `id` as, for example, `id : Str -> Str` instructs the compiler that `id` should be specialized only to the concrete type `Str -> Str`.

### Where does let-generalization happen today?

Any value that has a type with an unbound (free) type variable can be let-generalized today. Today, there are three such cases:

1. Functions, for example `id = \x -> x` which has an unbound type variable in the input and output type, and can be let-generalized to `a -> a`.
2. Tag unions, for example `A (B) (C)`, which can be inferred, or explicitly typed, to have the (nested) open tag union extension `[A [B]* [C]*]*` (equivalently, `[A [B]f [C]g]h`).
    1. Note: as of recently, these unbound type variables are hidden from the user when a value tag union’s type is inferred. However, the semantics in terms of generalization remain the same.
3. Un-suffixed numbers, for example `5`, which can be inferred, or explicitly typed, as `Num *`, `Int *`, or `Frac *`.

It is important to note that today, generalization is based on the type of a value, not its syntactic shape. That means that

- In the program
    
    ```python
    f = \{} -> \x -> x
    g = f {}
    ```
    
    `g` may be typed as the generalized function `a -> a`, despite its definition not explicitly looking like a function.
    
- In the program
    
    ```python
    j = List.walk ["", ""] 0 \i, _ -> Continue i + 1
    ```
    
    `j` may be typed as the generalized number `Num *`, despite its definition not being a number literal. A similar pattern holds for tag unions.
    

# Baseline Proposal

To explain why we would want to limit generalization, let me show you some cases where let-generalization doesn’t do us good.

## Problems

### Pessimistic runtime performance

Consider the program

```python
chompWhile : (U8 -> Bool), (List U8) -> (List U8)
chompWhile = \while, input ->
	index = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
	
	if index == 0 then
	    input
	else
	    List.drop input index
```

here, `index` can be typed as, or otherwise will be inferred as, `Num *`.

There are two usages of `index`:

- `index == 0` does not specialize `index` further, since `0` is also typed as `Num *`. In this case, Roc uses the default number type, currently `I64`. So this usage demands a concretization of `index` as `I64`.
- `List.drop input index` demands a concretization of `index` as `Nat`.

Since each concretization instructs the compiler to produce a version of `index` specialized to that concrete type, this program gets compiled as the equivalent of

```python
chompWhile : (U8 -> Bool), (List U8) -> (List U8)
chompWhile = \while, input ->
	indexI64 : I64
	indexI64 = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i

	indexNat : Nat
	indexNat = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
	
	if indexI64 == 0i64 then
	    input
	else
	    List.drop input indexNat
```

This is hardly expected - certainly the intention of `chompWhile` is not to use the polymorphism of `index` in order to specialize it twice. The program’s runtime performance is punished because the author relied on Roc’s convenient type inference to do the right thing. In this case, the type inference is overly conservative about what semantics the author would like.

It is reasonable to suggest that a Roc compiler could, in fact, see that `index == 0` imposes no further constraints on `index`, and that in fact both `index` and `0` could be specialized to `Nat`, eliminating the unnecessary work here. Consider however the diff

```diff
-	if index == 0 then
+	if index == 0u32 then
	    input
	else
	    List.drop input index
```

Now, `index == 0u32` does impose a concretization of `index`, and in particular this program will again end up with two concretizations of `index` to `U32` and `Nat`.

It remains that the author of `chompWhile` likely did not intend for `index` to actually be specialized to two different types here, and that they would prefer a notification that there are two different demands for `index` in this function.

Any scenario where an expensive computation yields a value that can be let-generalized is a potential source of this issue. One other notable example is a decoder that yields a number or tag union, for example of the form

```python
{ result, rest } = Decode.fromBytesPartial (Str.toUtf8 "-1234") Json.fromUtf8
```

It is unlikely that a developer would want the value in `rest` to be generalized to any number of signed integers. Indeed, if `rest` needs to be used as multiple signed integers, it is much more runtime-performant to decode it as one signed integer and performant integer casts to others, than to decode a string multiple times.

### Unexpected dbg/expect effects

Consider a modification of the above-presented program to

```python
chompWhile : (U8 -> Bool), (List U8) -> (List U8)
chompWhile = \while, input ->
	index =
    idx = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
    dbg idx
    idx
	
	if index == 0 then
	    input
	else
	    List.drop input index
```

Following how this program would be compiled today, it means that each invocation of `chompWhile` would have `dbg idx` executed twice (in each of the I64 and Nat concretizations).

While this could be seen as a signal that the compiler is in fact producing multiple concretizations of a value, and that maybe this should be refactored, I believe it is more likely that duplicate dbg/expect statements in cases like this will lead a developer to believe that they have a semantics in their usage of the `chompWhile` function. For example, perhaps they may suspect that `chompWhile` is being called twice where they expected it to be called once.

This suspicion is reasonable, but would be incorrect, and today the compiler provides no additional tooling to invalidate the suspicion. Instead, the developer must either understand this behavior of Roc’s compile-time semantics and observe them statically, or interpret duplicate dbg/expects as a possible indication of this semantics.

My intuition is that explicit tooling from the compiler to notify of the developer when cases like this appear is the better option, and leaves dbg/expect as tools for understanding the runtime behavior of a program, rather than also as tools for understanding the compile-time semantics of Roc.

**Effect hoisting**

One idea that has been brought up is to hoist dbg/expect effects into an outer scope, leaving the pure-functional value behind, and free for generalization. For example, one could imagine the compiler transforming

```python
x =
  dbg "A"
  expect 1 == 1
  5
```

to

```python
xFx =
  dbg "A"
  expect 1 == 1

x = 5
```

which does not experience the described problems due to unexpected dbg/expect. However,

- this technique does not scale. It can only be enforced for top-level values. In general, the transformation cannot be done for values in a function definition. For example, no such transformation can be done for a program like
    
    ```python
    main =
    	input <- Stdin.getLine |> Task.await
    	inputNum =
    	  {result} = Decode.fromUtf8 input Json.fmt
    	  expect result == Ok 5
    	  result
    ```
    
    without also limiting the let-generalization of `inputNum`.
    
- this does not address the described runtime-performance concerns

## Proposal solution

The baseline proposal is to limit let-generalization to values that are either

- typed as functions, or
- are number literals

all other values that have unbound type variables, namely

- values typed as number but are not number literals
- values typed as tag unions

will not have those unbound type variables promoted to generalized variables, leaving them as weak type variables that can be bound to at most one concrete type when used.

Moreover, values that are not let-generalized and have weak type variables cannot be exposed - they must be explicitly made general instead. This distinction and its motivation is presented last.

### Weak type variables

Going forward, I will use

- single-lowercase-letters or wildcards, like `a`, `b`, or `*`, for generalized type variables
- weak-indexed names, like `weak1`, `weak2`, for weak type variables

This is not an endorsement of the syntax, and I do not suggest here that weak type variables be allowed to be typed in Roc type annotations - this is purely illustrative.

Unlike generalized type variables, weak type variables can have at most one concretization, based on how they are used. After concretization, a weak type variable “becomes” that concrete type, and can only be used as that type from thereafter.

As an illustration of the difference (weak annotations are not proposed syntax)

```python
id : a -> a # generalized id
id = \x -> x

{ 
  a : id ""  # creates a concrete `id : Str -> Str`; the name `id` is still `a -> a`
  b : id 1u8 # creates a concrete `id : U8 -> U8`;   the name `id` is still `a -> a`
}

wid : weak1 -> weak1 # weak `wid: weak1 -> weak1`
wid = \x -> x

{
  a : wid ""  # concretizes `weak1 = Str`; the name `wid` is now `wid : Str -> Str`
  b : wid 1u8 # Type Error! `wid` wants a `Str` in its first parameter, but a `U8` is given
}
```

“Weak type variables” come from and are used in other languages, like OCaml, ML, and Cyclone, for mutable values, which Roc does not support.

## Programs that are still allowed

The following patterns are still allowed:

- Using syntactic functions polymorphically
    
    ```python
    id = \x -> x
    { a : id "", b : id 1u8 }
    ```
    
- Using values typed as functions polymorphically
    
    ```python
    getId = \{} -> \x -> x
    id = getId {}
    { a : id "", b : id 1u8 }
    ```
    
    (NOTE: this type checks, but does not compile today; the extension proposal explains why and will suggest disallowing polymorphism of `id` here too. But the baseline proposal suggests allowing it.)
    
- Using number literals polymorphically
    
    ```python
    pi = 3.14159267
    
    decPiR2 : Dec -> Dec
    decPiR2 = \r -> pi * r * r
    
    f64PiR2 : F64 -> F64
    f64PiR2 = \r -> pi * r * r
    ```
    
    This program will type `pi` as `Frac *` and create two concretizations of `pi`, to `Dec` and `F64`.
    
    The motivation for allowing number literals to be used polymorphically is that such constants are likely to commonly defined to be used in many contexts, and the compile-time and run-time overhead for multiple concretizations of number literals is minimal.
    

## Programs that are no longer allowed

The following patterns are no longer allowed:

- Using numbers that are non-literals polymorphically
    
    ```python
    chompWhile : (U8 -> Bool), (List U8) -> (List U8)
    chompWhile = \while, input ->
    	index = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
      # here, index is inferred as weak1
    	
    	if index == 0 then # 0 is inferred as weak1
    	    input
    	else
    	    List.drop input index # inference makes weak1 = Nat
    ```
    
    This is one of the motivating examples from the section on today’s problems. With the new weak-variable model, `index` is concretized exactly once, to `Nat` in this case. If `index` were to be `dbg` or `expect`ed, that would only print once, because only one concretization of `index` exists at runtime.
    
    The restriction means that the following program is no longer admitted:
    
    ```python
    chompWhile : (U8 -> Bool), (List U8) -> (List U8)
    chompWhile = \while, input ->
    	index = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
    	# here, index is inferred as weak1
    
    	if index == 0u16 then # inference makes weak1 = U16
    	    input
    	else
    	    List.drop input index # Type Error! `index` is a U16, but is being used as a Nat
    ```
    
    Later sections will discuss how we can emit nice error messages here, and why we’d prefer to make this a type error rather than, for example, a warning or lint error.
    
- Using number that could be constant-evaluated to literals, but are not literals, polymorphically
    
    ```python
    piApprox = 22 / 7 # piApprox : weak1
    
    decPiR2 : Dec -> Dec
    decPiR2 = \r -> pi * r * r # weak1 = Dec
    
    f64PiR2 : F64 -> F64
    f64PiR2 = \r -> pi * r * r # Type Error! `piApprox` is a Dec, but is being used as an F64
    ```
    
    It is clear that a Roc compiler could evaluate `piApprox` to a number literal at compile time, and even if it couldn’t, the runtime overhead of computing `22 / 7` is probably small enough that it is okay to concretize multiple times for different types.
    
    The motivation for not supporting this today is:
    
    - If generalization of this pattern were supported, some line of what constitutes a “small enough” number expression to be permitted to be let-generalized would have to be determined. We would not want the motivating example of
        
        ```python
        index = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
        ```
        
        to be generalized, so we could, for example, say that an expression that does not call out to a non-`Num` function, but is typed as a number, will be let-generalized.
        
        However, even checking something of that form can be very expensive at compile-time. The rule that “only number literals can be let-generalized” is simple and is effectively a single conditional at compile-time.
        
    - Currently, the Roc compiler does not have support for constant-folding, so certain computations (even if they do not call out to non-`Num` functions) could still be “expensive enough” that you may not want them to be concretized and evaluated multiple times at runtime
    - A Roc developer can always opt-in to explicitly generalizing such number values by wrapping them in a thunk function, like
        
        ```python
        piApprox = \{} -> 22 / 7 # piApprox : {} -> Frac *.
        ```
        
    
    It is suggested that this limitation be revisited after Roc has support for guaranteed constant-folding, at least for top-level values.
    
- Using tag union values, literal or not, polymorphically
    
    ```python
    pastel : [Purple, Mauve] -> Str
    vibrant : [Purple, Orange] -> Str
    
    color = Purple # here, color is inferred as [Purple]weak1 (extension variable hidden)
    
    pastel color # inference makes color : [Purple, Mauve]
    |> Str.concat (vibrant color) # Type Error! Need [Purple, Orange] but color is [Purple, Mauve]
    ```
    
    The polymorphism of tag union values has long been a source of confusion for Roc users of all experience. This proposal extends the current set of techniques that have been put in to place to try to limit the confusion by saying that, okay, `color` really is only one type here - it ends up being the type `[Purple, Mauve]`.
    
    The motivation here is two-fold:
    
    - The proposal assumes that tag union constants like there are relatively unlikely to be used polymorphically, especially relative to the frequency that number constants are used polymorphically.
    - A Roc developer can always recover polymorphism of a tag union type by making it the output of a function, like most error tags are produced. An example in this case would be
        
        ```python
        color = \{} -> Purple # color : {} -> [Purple]a (`a` is hidden in printed types)
        ```
        
- Typing values not eligible for generalization as generalized. None of the following are allowed:
    
    ```python
    piApprox : Frac *
    piApprox = 22 / 7
    
    piApprox : Frac a
    piApprox = 22 / 7
    
    color : [Purple]* # (this gives a warning today that * is not needed)
    color = Purple
    
    color : [Purple]a
    color = Purple
    ```
    
    It is suggested that an error message similar to the following be produced in cases like these (wording is not definitive, just a rough sketch)
    
    ```python
    -- NUMBER IS NOT POLYMORPHIC --
    
    The type annotation on `piApprox` suggests that it can be used polymorphically:
    
    1 | piApprox : Frac *
    2 | piApprox = 22 / 7
    
    Unfortunately, I can't use `piApprox` as any fractional type! I can only use it
    as exactly one of `Dec`, `F32`, or `F64`.
    
    If you want me to infer the fractional type that should be used, you can use an
    inference annotation instead of `*`:
    
      piApprox : Frac _
    
    If you explicitly want `piApprox` to be able to be used polymorphically, consider
    making it a thunk:
    
      piApprox : {} -> Frac *
      piApprox = \{} -> 22 / 7
    ```
    
    The proposal is to explicitly not expose any syntax for annotating weak type variables. The compiler will automatically change inference annotations `_` to weak variables or generalized variables as appropriate.
    
    The motivation for this is that it does not expand the kinds of types that a Roc developer has to understand in order to use the Roc language. They can continue to use type variables like `a`, `*`, or even `weak1` as generalized type variables. Weak type variables are only a strange middle that exist only during the time their concrete type is not yet known. If a weak type variable is ever used, it will end up being concrete, so why introduce another syntax for it?
    
    In cases where weak type variables have to be printed out, the inference annotation `_` can be printed. This preserves that a developer can paste that annotation and the program will still be checked correctly. For example (illustrative, the exact wording of this error is different):
    
    ```python
    piApprox = 22 / 7
    r = { pi: piApprox, g: "" }
    
    f : { pi : Frac *, h : Str } -> {}
    
    f r
    ```
    
    ```python
    -- TYPE MISMATCH --
    
    The first argument to `f` is an unexpected type:
    
    6 | f r
    
    `r` is a record of type
    
      { pi: Frac _, g: Str }
    
    but `f` takes as its first parameter a value of type
    
      { pi : Frac *, h : Str }
    
    did you mean to define `r` with a field "h" instead of "g"?
    ```
    

## Error messages for attempted polymorphic usages

Suppose we have the following type-erroring program

```python
chompWhile : (U8 -> Bool), (List U8) -> (List U8)
chompWhile = \while, input ->
	index = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
	# here, index is inferred as weak1

	if index == 0u16 then # inference makes weak1 = U16
	    input
	else
	    List.drop input index # Type Error! `index` is a U16, but is being used as a Nat
```

The proposal is to yield an error message that designates a type error by pointing

- to where the name’s weak type became a concrete type
- where the type mismatch happened
- hinting that the name can be defined as a thunk is polymorphism is desirable

For example, the error message could look like (illustrative, wording is not suggested as definitive):

```python
-- TYPE MISMATCH --

This 1st argument to `List.drop` has an unexpected type:

9 |       List.drop input index
                          ^^^^^

`List.drop` needs its second argument to be

  Nat

but `index` is a number of type

  U16

`index` was constrained to that type here:

6 |  if index == 0u16 then
        ^^^^^

Note: if you want to use `index` polymorphically, you can define it as a thunk, like

  index = \{} -> <current implementation of index>

and then call it `index {}` where you're using `index` right now.
```

As an improvement on top of this error message, the Roc compiler could also

- collect information about what the `index` implementation looks like, and what its type was before any weak variables were resolved,
- check if making the implementation a thunk really would resolve the type error,
- suggest the thunk-transformation hint, with a full implementation, only if it actually would resolve the type error
- this suggestion could become an auto-fix

This improvement is noted but not suggested for this initial proposal.

### Definition re-ordering

A wrinkle in the quality of these error messages has to do with definition re-ordering. For example, consider the program

```python
piApprox = 22 / 7

z = piApprox + y + 1dec

y = piApprox + 1f64
```

Due to definition re-ordering, Roc currently would emit the proposed error message as

```python
-- TYPE MISMATCH --

This 1st argument to `add` has an unexpected type:

3 | z = piApprox + y + 1dec
        ^^^^^^^^

This addition needs its 2nd argument to be a

  Dec

but `piApprox` is a number of type

  F64

`piApprox` was constrained to that type here:

5 |  y = piApprox + 1f64
         ^^^^^^^^
```

This may be a bit surprising, because in the source program, `y` appears before `z`, but the error message is saying the constraint to `z` happened first! And indeed it did.

It is possible to change how Roc performs type inference/checking to alleviate this unexpected ordering. However, definitions that are out-of-order are already due to become warnings in Roc. By fixing those warnings, all instances of this kind of unexpected error message would be resolved as well.

Moreover, changing the inference algorithm to work on out-of-order defs would have material consequences on the current design and performance of the compiler (for defs that actually are generalized, this would require either treating out-of-order defs as mutually recursive, or needing two inference passes - both options are expensive).

## Exposing weak variables

A subtle, important consideration is what should happen if a module exposes a value that has a weak type variable that is not ever used in the module (that is, it is not resolved to a concrete type, and remains weak). For example:

```python
interface Tart exposes [piApprox] imports []

piApprox = 22 / 7 # exposing `piApprox : Frac weak1` !
```

There are at least three options here:

1. Resolve the weak variable globally, by examining all usages of `piApprox` across all dependent modules in a compilation. The first dependent module to use the weak variable concretely decides the concrete type. All other modules’ uses of `piApprox` must then conform to that concretization.
    
    This proposal does not recommend this option. The reasons include
    
    - This would induce user-visible dependencies between modules that are otherwise unrelated. For example, suppose both `Pie` and `Pastry` import `Tart`, but do not import each other (that is, the graph `Pie-->Tart<--Pastry`). If `Pie` uses `piApprox` as `F64` and `Pastry` uses `piApprox` as `Dec`, then one module must change its usage of `piApprox` to satisfy the other, despite there otherwise being no relationship between the two.
    - The Roc compiler processes modules that do not depend on each other concurrently. That means that today, on in a compilation where `Pie` and `Pastry` are type-checked concurrently, the weak variable resolution can race and the results of a compilation can be non-deterministic. The only resolution to that is to make compilation deterministic, which requires at least sequential resolution of weak variables. Neither non-deterministic compilation or (partially-)sequential compilation are desirable.
    - Most importantly, **this is unlikely what the author of `Tart` desired**. I cannot think of a reasonable use case where you would want to expose a value of an unknown type, that can take one exactly one concrete type. It is far more likely that the author desired `piApprox` to be exposed as a generalized type, or as a type that has one concrete type.
2. Resolve the weak variable per module. That is, each module that uses `piApprox` gets a unique weak-copy of `piApprox`, and can concretize it exactly once in that module.
    
    This provides the following advantages:
    
    - Each module can continue to be processed independently
    - The exported type can be used pseudo-generally, in that each dependent module can use it in (exactly one) concrete specialization
    
    This proposal does not recommend this option. The reasons include
    
    - As described above, **this is unlikely what the author of `Tart` desired.** It appears preferable to expose only either a generic value, or exactly one concrete one.
    - It can cause confusion to a developer who, upon seeing different usages of `piApprox` in different modules, suspects that `piApprox` is in fact polymorphic.
        
        For example, suppose a developer new to a codebase sees a pattern in `Pie` that uses `piApprox` as an F64, and a pattern in `Pastry` that uses it as Dec. Turns out that a similar pattern needs to be installed in `Pie` - great! they say, and massage the usage Dec usage in `Pastry` to one in `Pie`.
        
        Now the developer would get a type error, notifying them that the previous F64 usage in `Pie` means they can no longer use it as a Dec. This seems very confusing unless you “know the rule” that the weak variable can be used exactly once per module.
        
        This requires some amount of knowledge of what weak variables are and their limitations. As discussed in the error messages section, this proposal suggests limiting the extent to which such things have to be known by developers. Extending how weak variables behave across modules does not aid limitation.
        
3. Explicitly disallow exposing values that contain weak variables.
    
    Let’s recall our running example
    
    ```python
    interface Tart exposes [piApprox] imports []
    
    piApprox = 22 / 7 # exposing `piApprox : Frac weak1` !
    ```
    
    The suggested error message (illustrative, wording not suggested as definitive) would be
    
    ```python
    -- UNKNOWN EXPOSED TYPE --
    
    `piApprox` is exposed, but I don't know what concrete type to give it:
    
    2 | piApprox = 22 / 7
        ^^^^^^^^
    
    I see that piApprox is a fractional type, but I don't know if it's an F64,
    F32, or Dec. Anyone who imports `piApprox` won't know either!
    
    If you used `piApprox` in this module, I could infer its type.
    Or, you could provide a type annotation to `piApprox` to tell me what type
    you mean to expose it as.
    
    If you want to expose `piApprox` as any fractional type, `Frac *`, wrap it
    in a thunk!
    
      piApprox = \{} -> 22 / 7
    ```
    
    This provides the following advantages:
    
    - The cognitive scope of “this value’s type is unknown, but it can be exactly one” is strongly limited to exactly the module it’s defined in, and moreover, only in the assumed-unlikely case that it is exposed without a concrete value
    - As above, dependents of the module get either a concrete or generalized type, not something in-between
    - The error message provides both ways to go to either a concrete type or a generalized type. In the future those could be automated fixes.
    
    This has the following disadvantages:
    
    - In the case where you want to expose `piApprox` as a concrete type, you have to add a type annotation. However, this is no different from today, and as such does not impede complete type inference.
    - This may be confusing for developers who run into this error message. “Why can’t anyone use this as a `Frac *` in the way I wrote it?", they may ask. It is a reasonable question, and the compiler’s error message is mostly hiding the real reason away from them. However, it is suspected that the cases people really run into this will not be very common. Moreover, the compiler error gives concrete steps to progress and unblock the developer, even if the long-winded answer is not provided.

## Alternatives; or, why a type error?

Supposing that it is agreed that the present behavior of let-generalization can lead to unexpected runtime behavior, we should ask

- should the proposed restricted patterns be caught as new type-checking rules that yield errors?
- can they be implemented as warnings, lints, or something else, rather than type errors?

### Why not warnings?

Currently, warnings are provided where a Roc program does something unnecessary that can be safely removed without affecting the semantics of the program. Examples include unused variables or definitions that don’t introduce any new values.

On the other hand, the patterns that this proposal attempts to prevent both

- have a serious runtime cost if overlooked
- can require diligent effort to remove, for example changing everything to a concrete type or making more values into thunks

IMO these factors makes prevention-via-warnings insufficient. These problems are worth catching as soon as possible, via compile errors.

### Why not lints?

Lints from a linter, which is not yet provided via the Roc toolchain but will be in the future, should also have the property that they do not affect the semantics of a program, but simply point out certain issues a developer may want to note.

In this way, lints are very similar to warnings. Moreover, they are likely to be run much less frequently than the compiler (think about linters in other toolchains - eslint, clippy, go-vet, etc - how often do you run it?). Since the runtime issues these patterns present are serious enough that they can require diligent refactors, it is suggested that they be surfaced ASAP.

## Implementation Strategy

### Typing Rules

The secret Roc compiler developers don’t know you to know is that we already have weak type variables. They are implemented via [levels/ranks](https://okmij.org/ftp/ML/generalization.html), and are already used to prevent [things](https://github.com/roc-lang/roc/issues/2489) like [polymorphic recursion](https://github.com/roc-lang/roc/pull/4560).

As such I believe the new algorithmic typing rule to be enough as follows:

- Introduce a flag to let-generalization constraining rules that marks whether the let-generalized value is a number literal
- During constraining, set the flag appropriately. Note that the flag will only be set when the LHS is a name and the RHS is a number literal (in all other cases, that is already a type mismatch)
- After solving a let-defined body but prior to generalization, check if one of the following applies
    - The number literal flag is set, and the concrete type (modulo aliases and opaques) is a number
    - The concrete type (modulo aliases and opaques) is a function
- If the check is positive, adjust-rank for all variables introduced in the generalization with the maximum rank being that of the generalization rank. If the check is negative, adjust-rank with the maximum rank being the current-scoped, non-generalizable rank.

### Error messages

**Finding constraint sites**

- Whenever a name is introduced with a non-generalized rank, record all introduced identifiers and weak variables into a table `weak_set`
- When unification concretizes a `weak_set` variable, mark the region of the relevant constraint that concretized the weak variable.
    - The abilities ambient lambda set specialization algorithm does something very similar to this for unspecialized lambda sets; see the ULS table for a description.
- If a later type mismatch involves an identifier or variable in `weak_set`, pull out the concretizing region and include it in the error message.

**Printing weak variables**

If an unbound type variable is printed but it is not at the generalization rank, print it as `_`.

### Polymorphic expression compilation

To support compiling polymorphic numbers we need to keep around some form the current polymorphic compilation system. However it can be made much simpler:

- Polymorphic expression compilation currently marks all let-generalized values for polymorphic compilation. Now this only matters for values that are number literals (functions are already handled independently)
- There is no longer a need to save variables for polymorphic number literals
- Polymorphic expression compilation requires copying AST state. This can all be ripped out, since only number literals are relevant.
- In the future, we could replace polymorphic number literal references with the number literal itself at the usage site

# Extension Proposal

The extension is everything that the baseline admits, with the additional restriction

- Not all values that are typed as functions can be let-generalized; only let-bindings that are syntactically functions can be let-generalized.

A let-binding is syntactically a function if it looks like a function definition on the surface; e.g. `x = \p1, ..., pn -> <body>`.

(Note: the exact restriction I will suggest is a bit different; if you are a Roc compiler developer, see the section on implementation below for the exact definition.)

The following let-binding are not syntactically functions:

- Eta-reduced function values, like `g` in the following program
    
    ```python
    f = \{} -> \x -> x
    g = f {} # g : a -> a
    ```
    
- Function-syntax values with setup in their let-bindings
    
    ```python
    f =
      x = 1
      expect x == 1
      \{} -> x
    ```
    

The error messages of the baseline proposal, and the restriction on exposing let-bindings that are not syntactically functions, applies in this extended proposal.

The following let-bindings will be permitted to be generalized:

- Syntactic functions, like `id = \x -> x`
- Syntactic functions immediately under a syntactic opaque type wrapping, like `id = @MyFunc \x -> x`
- Tag functions, like `t = T` if this is a function that produces tags `T` via, for example, `t : a -> [T a]`
- Opaque type functions, like `custom = @Decoder` , for example, `custom : (List U8, fmt -> val) -> Decoder val fmt | val has Decoding, fmt has DecoderFormatting`

 It is expected that this will

- Avoid unexpected duplicate `expect`/`dbg`s seen due to polymorphic function values
- Resolve a couple technical challenges the Roc compiler currently faces for compiling eta-reduced functions, and certain usage patterns of abilities.

So, I think the most important user-facing benefit of this extension is removing unexpected `expect`/`dbg`s. I’ll describe that first, and then mostly shrink away and spend the rest of this document talking about compiler-internal details and how this resolves technical implementation challenges. The former may be interesting to everybody, but I suspect the latter will only be important for the compiler’s developers.

## Problems (Runtime semantics)

As in the baseline proposal, both issues of duplicate work and duplicate expect/dbg effects can arise when non-syntactic function let-bindings are used polymorphically. These usage patterns are far more contrived than that of numbers/tag literals, but they do exist, and can be surprising!

As an example of duplicate work, suppose we had the program

```python
chompWhile : (U8 -> Bool), (List U8) -> (List U8)
chompWhile = \while, input ->
	getIndex =
    idx = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
    \{} -> idx
	
	if getIndex {} == 0 then
	    input
	else
	    List.drop input (getIndex {})
```

I don’t know why you would write a program like this, but you can - and today, as the baseline proposal noted, this would result in the compiler generating a program equivalent to

```python
chompWhile : (U8 -> Bool), (List U8) -> (List U8)
chompWhile = \while, input ->
	getIndexI64 =
    idx : I64
    idx = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
    \{} -> idx

	getIndexNat =
    idx : Nat
    idx = List.walkUntil input 0 \i, _ -> if while i then Continue (i + 1) else Break i
    \{} -> idx
	
	if getIndexI64 {} == 0i64 then
	    input
	else
	    List.drop input (getIndexNat {})
```

This program again experiences a pessimistic runtime characteristic due to let-generalization being too powerful.

A similar construction applies for expect/dbg effects, as in the baseline proposal - if let-bound function value has `expect`s or `dbg`s in its body prior to the function definition, those `expect`s and `dbg`s will be run for each concrete function value produced based on polymorphic usages. The only way to avoid this is to prevent polymorphic usage.

## Usage patterns this restriction limits

The most common usage pattern this proposal extension will limit is the polymorphic use of eta-reduced functions. I don’t think these are terribly common, but here are two examples:

- The [generator pattern found here](https://github.com/roc-lang/roc/issues/4734), by [Shritesh Bhattarai](https://github.com/shritesh)
    
    ```python
    Generator a b c : RNG, a, (RNG, b -> c) -> c
    
    # Simple linear congruential generator
    RNG := U32
    
    default : RNG
    default = @RNG 0
    
    andThen : Generator a b c, a, (d, b -> e) -> Generator d e c
    andThen = \generator, init, fn ->
        \rng, newInit, newFn ->
            newRng, value <- generator rng init
            newFn newRng (fn newInit value)
    
    u32 : Generator {} U32 *
    u32 = \@RNG seed, {}, fn ->
        value =
            seed
            |> Num.mulWrap 1664525
            |> Num.addWrap 1013904223
    
        fn (@RNG value) value
    
    real : Generator {} F64 *
    real =
        {}, u32Value <- andThen u32 {}
        max = Num.toF64 Num.maxU32 |> Num.add 1
    
        Num.toF64 u32Value / max
    
    between : Generator { min : F64, max : F64 } F64 *
    between =
        { min, max }, realValue <- andThen real {}
    
        min + (max - min) * realValue
    ```
    
    Today, `between` and `real` would be generalized in the type variable `c`. In the new model, this would not be permitted.
    
- The way generic decoder are built up, for example `decodeListElems` in the following code
    
    ```python
    Decoder val fmt := List U8, fmt -> DecodeResult val | fmt has DecoderFormatting
    
    DecoderFormatting has
      list : Decoder elem fmt -> Decoder (List elem) fmt | fmt has DecoderFormatting
    
    Decoding has
      decoder : Decoder val fmt | val has Decoding, fmt has DecoderFormatting
    
    custom : (List U8, fmt -> DecodeResult val) -> Decoder val fmt | fmt has DecoderFormatting
    custom = \x -> @Decoder x
    
    decodeListElems = custom \bytes, fmt -> decodeWith bytes (list decoder) fmt
    # decodeListElems : List U8, fmt -> Decoder (List elem) fmt
    ```
    
    This is actually the way that the compiler derives decoding for lists today. The proposal suggests special-casing derived definitions to work around this restriction, because it is very important that the compiler is able to derive polymorphic implementations rather than those with weak type variables. While compiler-derived implementations of `decodeListElems` will be permitted to be used polymorphically, a user-written code like this will result in `decodeListElems` being weak in `elem` and `fmt`.
    

## A technical description

Okay, we now begin the technical description of how this system will work. If you are a Roc compiler developer, I would appreciate diligent critique of this description. If you are a Roc user, you might not find this interesting or useful; feel free to stop reading at any time, and feel free to also provide comments!

### Behavior of weak variables after function reference

Obviously functions are the most interesting part of let-generalization, so the description of weak variables in function applications is also the most interesting.

The key insight is that **once the head of a function is applied or referenced, all bound variables in its signature become weak**.

For example, let’s consider

```python
f = \z -> \y -> \x -> x
# f : z -> y -> x -> x (generalized)
```

we now have that (I’ll abbreviate `w_i` for `weak_i`)

```python
g = f
# g : w_z -> w_y -> w_x -> w_x

h = f ""
# h : w_y1 -> w_x1 -> w_x1

i = (f "") 1u8
# i : w_x2 -> w_x2

j = ((f "") 1u8) A
# j : [A]w_ext
```

The way to recover generalization after a function is referenced is eta-abstraction. Eta-abstracting each weak variable recovers that variable’s generalization in a new binding. So we can recover generalization of `g` as follows

```python
g_gen = \z -> \y -> \x -> ((f z) y) x
```

The reason this will work is that `z`, `y`, and `x` are introduced in the scope of `g` and hence eligible for generalization. The remain unbound, and are never demoted to weak variables before the generalization of `g_gen`.

A detail that is in some sense obvious, but that we should take care to remember here, is that generalized variables are only introduced in the input or output positions of a function signature.

- In particular, for functions that do not use abilities, generalized variables can only be introduced from the input positions of a function signature. It is impossible to define a function of form `t -> a` or even just `a` , where `a` is generalized and `t` is a type that does not reference `a`, without using abilities.
- Abilities may not obey this semantic, since they can introduce unbound variables “out of thin air”. I’ll show that this plays an important factor later on, and is why we’ll need to special-case compiler-derived signatures to be generalized when they don’t obey the syntactic rule, or else introduce a higher-region restriction.

### Problems this resolves for non-ability compilation

**Function references**

You may notice that a program like

```python
f = \{} -[f]-> \x -[anon1]-> x
g = f {} # g : a -[anon1]-> a

{ r1: g "", r2 : g 1u8 }
```

does not compile today. The reason is that `g` is seen as a reference to a specialized function, and is compiled to hold only the closure data of `anon1`. In particular, we demarcate `g` with the layout `g : {} -> LambdaSet { anon1 }`.

This layout does not capture information about the specialization of `anon1` that `g` points to. Hence, the call `g ""` produces the specialization of `g : {} -> LambdaSet { anon1 }`, which implicitly produces a specialization of `anon1 : Str -> Str` as `g` is being specialized. However, the call `g 1u8` expects only a specialization of `g : {} -> LambdaSet { anon1 }`, which is already produced, so `g` is not visited again and the expected specialization of `anon1 : U8 -> U8` is not produced.

In the new model, this program infers instead as

```python
f = \{} -[f]-> \x -[anon1]-> x
g = f {} # g : w_a -[anon1]-> w_a

{
  r1: g "",   # w_a ~ Str
  r2 : g 1u8  # w_a=Str !~ U8 , Type Error!
}
```

Implicitly resolving the compilation problem.

There are other techniques we could use to solve this problem, for example recoding the specialization of `anon1` as a niche in the signature of `g`. We have already seen that is not quite enough, for example in the program

```python
andThen = \{} ->
    x = 10
    \newFn -[5]-> Num.add (newFn {}) x

between = andThen {}

it = between \{} -[7]-> between \{} -[8]-> 10
```

Which demands the specializations

```python
between : ({} -[[7]]-> U8) -[[5 U8]]-> U8
between : ({} -[[8]]-> U8) -[[5 U8]]-> U8
```

Written in terms of layouts with recorded niches, we have

```python
between : {} -> LambdaSet {5 [U8]}   (niche: {} -LambdaSet {7}-> U8) -LambdaSet {5 [U8]}-> U8
between : {} -> LambdaSet {5 [U8]}   (niche: {} -LambdaSet {8}-> U8) -LambdaSet {5 [U8]}-> U8
```

The reason this is not enough is that `andThen`, despite being a syntactic function with specialization semantics, **also** has the layout signature

```python
andThen : {} -> LambdaSet {5 [U8]}
```

in both specializations, since it just returns the closure data of `[5]`! The point here is that `[5]` is specialized based on `newFn`, which is missing from both specialization signatures of `between` and `andThen`. Because it is missing from the explicit signatures of `andThen` and `between` (since those are not eta-abstracted), we are left to implicitly determine it from elsewhere in the specialization signature, in a way that is not reflected today.

Weakening `between` helps us a little bit here. With the weakening, we have that the lambda sets `between` references are weak:

```python
andThen = \{} ->
    x = 10u8
    \newFn -[5]-> Num.add (newFn {}) x
# andThen : {} -[andThen]-> ({} -[]-> U8) -[5]-> U8

between = andThen {}
# between : ({} -w_[]-> U8) -w_[5]-> U8

it = between \{} -[7]-> between \{} -[8]-> 10u8
                        ^^^^^^^^^^^^^^^^^^^^^^^    ({} -w_[]-> U8) -w_[5]-> U8
                                                ~  ({} -[8]-> U8)
                                                => ({} -w_[8]-> U8) -w_[5]-> U8

             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    ({} -w_[8]-> U8) -w_[5]-> U8
                                                ~  ({} -[7]-> U8)
                                                => ({} -w_[7, 8]-> U8) -w_[5]-> U8
```

Now, when `between` specializes `andThen`, it does so with `newFn` having the lambda set `[7, 8]`, and only one specialization of both `[5]` and `andThen` is wanted.

This does mean that `[7]` and `[8]` are now compiled larger, being part of the same lambda set at runtime and `[5]` needing a conditional to dispatch to them, rather than each having their own specialization of `5`, but it seems like a reasonable price to pay.

**Well, kind of**

Okay, I am deceiving you a bit. Of course, this is not quite enough, because the fact that `newFn` is implicit and not reflected in signatures still remains. weakening of references **to** `andThen` (rather than just `andThen` itself) does not generally solve the problem here. For example we could imagine

```python
andThen = \{} ->
    x = 10u8
    \newFn -[5]-> Num.add (newFn {}) x
# andThen : {} -[andThen]-> ({} -[]-> U8) -[5]-> U8

between1 = andThen {}
# between1 : ({} -w_[]-> U8) -w_[5]-> U8

between2 = andThen {}
# between2 : ({} -w_[]-> U8) -w_[5]-> U8

it = between1 \{} -[7]-> between2 \{} -[8]-> 10u8
                         ^^^^^^^^^^^^^^^^^^^^^^^^    ({} -w_[]-> U8) -w_[5]-> U8
                                                  ~  ({} -[8]-> U8)
                                                  => ({} -w_[8]-> U8) -w_[5]-> U8

              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    ({} -w_[]-> U8) -w_[5]-> U8
                                                  ~  ({} -[7]-> U8)
                                                  => ({} -w_[7]-> U8) -w_[5]-> U8
```

And now we have the same problem as before - we need two specializations of between, even with niches

```python
between : {} -> LambdaSet {5 [U8]}   (niche: {} -LambdaSet {7}-> U8) -LambdaSet {5 [U8]}-> U8
between : {} -> LambdaSet {5 [U8]}   (niche: {} -LambdaSet {8}-> U8) -LambdaSet {5 [U8]}-> U8
```

but both ask `andThen` only for a specialization of `{} -> LambdaSet {5 [U8]}`.

I believe this means that we will have to implement argument/return types being recorded in the lambda set layouts. By seeing that `andThen` needs the specializations

```python
andThen : {} -> (Function (Function {} -[[7]]-> U8) -[[5 U8]]-> U8)
andThen : {} -> (Function (Function {} -[[8]]-> U8) -[[5 U8]]-> U8)
```

the general problem is resolved.

Nevertheless, weakening has value here. It is trading off polymorphic instantiation for conditional dispatch at runtime. Since weakening will be limited to modules, and the whole point of lambda sets is to make the overhead of conditional dispatch no more than an integer conditional, I think it is reasonable to limit polymorphism in this way for faster compile times and a simpler model.

### Problems this resolves for abilities compilation

**Specialization where higher regions are generalized today**

Recall that when an ability member is introduced, its lambda sets are numbered increasingly from 1, in pre-order:

```python
Producer has
    id : (prod -> elem) -> (prod -> elem) | prod has Producer
# id : (prod -[[] + prod:id:2]-> elem) -[[] + prod:id:1]-> (prod -[[] + prod:id:3]-> elem) | prod has Producer
```

When `prod` is specialized to a concrete type, the [current algorithm](https://www.notion.so/Ambient-Lambda-Set-Specialization-50e0208a39844ad096626f4143a6394e?pvs=21) unifies the ambient functions of each unspecialized lambda set region with the specialized ambient function in descending order. That is, it will unify at region 3, then 2, then 1.

The algorithm relies on a particular property to be complete for the purposes of code generation. The property is that specialization for a given type variable and ability member (e.g. `prod:id` above) will **always** reach the base region 1 in its specialization. As long as this property is observed, we can be sure that all ad-hoc-generalized type variables in the specialization signature will be resolved. That’s because an ability member can only vary over one type variable; in the example above, namely `prod`, which every specialization must provide concretely somewhere in its signature. The base region 1 is the entire function signature, so when it is reached, we know that we have covered the entire specialization signature.

In the today’s model of let-generalization this property does not hold. Here’s an example where this can go wrong:

```python
Producer has
    str : prod -> Str | prod has Producer
    id : (prod -> elem) -> (prod -> elem) | prod has Producer

MyProducer := {} has [Producer {str: produceStr, id: produceId}]

produceStr = \@MyProducer {} -> "ab"   # specialization for `str`
# produceStr : MyProducer -[[produceStr]]-> Str

produceId = \produce ->   # specialization for `id`
    thunk = \@MyProducer {} -> produce (@MyProducer {})
    thunk
# produceId : <t1>=(MyProducer -[[]]-> elem) -[[produceId]]-> (MyProducer -[[<t1>=(MyProducer -[[]]-> elem)]]-> elem)

exampleProducer = id str
                     ^^^ prod' -[[] + prod':str:1 + prod':id:2]-> Str | prod' has Producer
                  ^^    (prod' -[[] + prod':str:1 + prod':id:2]-> Str) -[[] + prod':id:1]-> (prod' -[[] + prod':id:3]-> Str) | prod' has Producer
# exampleProducer : (prod' -[[] + prod':id:3]-> Str) | prod' has Producer
```

Generalization of `exampleProducer` means that the generalized variable `prod'` is only bound to the higher region `3` of `id`, and specialization of `prod'` for `id` will never reach region 1! This causes serious problems, because as we can see in the specialization `produceId`, the type `<t1>` we want for the lambda set `exampleProducer` should resolve to is only given by the ambient functions at region 2 and 1. If we had access to region 2 and 1, we would see that the lambda set resolves to `prod':str:1` (which in turn is the singleton lambda set `produceStr`), but alas, `prod'` is only recorded to need to specialize for region 3.

This has been a tricky problem to resolve. I see two ways:

- In the cases where a higher region escapes into generalization without the region 1 also present, supplement the missing “shadow” type; for example, every time `exampleProducer` is used, we not only create a specialized version of `(prod1 -[[] + prod1:id:3]-> Str) | prod1 has Producer`, but we also somehow copy in `(prod1 -[[] + prod1:str:1 + prod1:id:2]-> Str) -[[] + prod1:id:1]-> (prod1 -[[] + prod1:id:3]-> Str) | prod1 has Producer` so that lower regions are ready to be specialized.
    - This approach might work, but I believe it to be very expensive in general.
    - For one, I do not see how this can be done without keeping backlinks in the types (we need to see that `(prod1 -[[] + prod1:id:3]-> Str)` has a certain type as its parent). I don’t think this will work very well with unification at all, and it’s quite expensive to keep such backlinks (we have them for lambda sets’ ambient functions, but the scope of that is much smaller).
    - For another reason, we can no longer treat types locally - it’s not enough to just understand the return type of `exampleProducer` now, we have to treat it as if it were a whole eta-abstracted function at the type-level, despite it not being so in either the compile-time or runtime semantics.
- Disallow generalization of values that contain unspecialized lambda sets (like `prod':id`) of higher regions that are missing the base region 1 (resp. `prod':id:1`). This is the “higher region restriction”.
    - This approach is efficient, but it has a serious problem. If implemented on its own, when the higher region restriction blocks generalization, it is impossible for a Roc developer to understand why, without also understanding the semantics of both lambda sets and abilities. Even this is hard for me to understand, I would hate for a non-compiler developer to have to intimately understand it! So it’s not a great world to be in.

The weakening restriction is not a superset of the higher region restriction. As an example, consider

```python
exampleProducer = \{} -> id str
# exampleProducer : {} -> (prod' -[[] + prod':id:3]-> Str) | prod' has Producer
```

This function would be generalized via the proposed weakening restriction, but we need the higher region restriction to also come into play, and prevent generalization of this function so that the link of `prod'` to the base region `1` is not severed by instantiations of the `exampleProducer` type at usage sites.

The suggestion is to explicitly implement **both the syntactic function generalization restriction, and the higher region restriction**.

“Wait!” you say, “but this doesn’t solve the error-message problem at all, because it’s impossible to understand why this doesn’t work without understanding abilities and lambda sets!”

That’s true. The hypothesis is as follows:

- With the new let-generalization restriction, people will already be familiar that some values that are function types may not be polymorphic
- If people run into this, we can just let them know this is an additional special case they cannot use the value polymorphically, and that they should either eta-abstract `exampleProducer` again, or use one type. We can leave the exact reason unmentioned, as has been suggested to leave weak variables unmentioned in other error messages. For example, you could imagine a hint like
    
    ```python
    Hint: if you want to use `exampleProducer` to take another type of "prod", consider adding another function parameter for "prod":
    
      exampleProducer = \{}, producer -> (id str) producer
    ```
    
- I have no idea how often people will run into this, but the combination of defining a higher-order ability member, and then using it in a higher-order function, which itself is used polymorphically, seems somewhat rare. That said, abilities have not been used a lot yet, so maybe this is in fact a reasonable pattern people use often.
- **Trace of weak variables resolving the higher region generalization problem for the example above**
    
    ```python
    Producer has
        str : prod -> Str | prod has Producer
        id : (prod -> elem) -> (prod -> elem) | prod has Producer
    
    MyProducer := {} has [Producer {str: produceStr, id: produceId}]
    
    produceStr = \@MyProducer {} -> "ab"   # specialization for `str`
    # produceStr : MyProducer -[[produceStr]]-> Str
    
    produceId = \produce ->   # specialization for `id`
        thunk = \@MyProducer {} -> produce (@MyProducer {})
        thunk
    # produceId : <t1>=(MyProducer -[[]]-> elem) -[[produceId]]-> (MyProducer -[[thunk <t1>=(MyProducer -[[]]-> elem)]]-> elem)
    
    exampleProducer = id str
                         ^^^ w_prod -[[] + w_prod:str:1 + w_prod:id:2]-> Str | w_prod has Producer
                      ^^    (w_prod -[[] + w_prod:str:1 + w_prod:id:2]-> Str) -[[] + w_prod:id:1]-> (w_prod -[[] + w_prod:id:3]-> Str) | w_prod has Producer
    # exampleProducer : (w_prod -[[] + w_prod:id:3]-> Str) | w_prod has Producer
    
    main = exampleProducer (@MyProducer {})
    
    # w_prod = MyProducer
    # exampleProducer : (MyProducer -[[] + MyProducer:id:3]-> Str)
    # exampleProducer->str : MyProducer -[[] + MyProducer:str:1 + MyProducer:id:2]-> Str
    # exampleProducer->id : (MyProducer -[[] + MyProducer:str:1 + MyProducer:id:2]-> Str) -[[] + MyProducer:id:1]-> (MyProducer -[[] + MyProducer:id:3]-> Str)
    #                                                                                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ step1
    #                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ step 2 (for id)
    #                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ step 3
    #                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ step 4 (for str)
    
    # LAMBDA SET SPECIALIZATION BEGIN
    
    # Step 1: MyProducer -[[] + MyProducer:id:3]-> Str
    #
    #    MyProducer -[[]]-> Str
    # ~  MyProducer -[[<t1'>=(MyProducer -[[]]-> elem)]]-> elem
    # => MyProducer -[[<t1'>=(MyProducer -[[]]-> Str)]]-> Str
    
    # Step 2: MyProducer -[[] + MyProducer:str:1 + MyProducer:id:2]-> Str
    # FOR id
    #
    #    MyProducer -[[] + MyProducer:str:1]-> Str
    # ~  <t2'>=(MyProducer -[[]]-> Str)
    # => <t2'>=(MyProducer -[[] + MyProducer:str:1]-> Str)
    
    # Step 3: (<t2'>=(MyProducer -[[] + MyProducer:str:1]-> Str)) -[[] + MyProducer:id:1]-> (MyProducer -[[<t1'>=(MyProducer -[[]]-> Str)]]-> Str)
    #
    #    (<t2'>=(MyProducer -[[] + MyProducer:str:1]-> Str)) -[[]]->          (MyProducer -[[<t1'>=(MyProducer -[[]]-> Str)]]-> Str)
    # ~   <t3'>=(MyProducer -[[]]-> elem)                    -[[produceId]]-> (MyProducer -[[<t3'>=(MyProducer -[[]]-> elem)]]-> elem)
    # =>  <t3'>=(MyProducer -[[] + MyProducer:str:1]-> Str)  -[[produceId]]-> (MyProducer -[[<t3'>=(MyProducer -[[]]-> Str)]]-> Str)
    
    # Step 4: <t3'>=(MyProducer -[[] + MyProducer:str:1]-> Str)
    # FOR str
    #     <t3'>=(MyProducer -[[]]-> Str)
    # ~          MyProducer -[[produceStr]]-> Str
    # =>  <t3'>=(MyProducer -[[produceStr]]-> Str)
    
    # LAMBDA SET SPECIALIZATION END
    
    # exampleProducer->id: <t3'>=(MyProducer -[[produceStr]]-> Str) -[[produceId]]-> (MyProducer -[[<t3'>=(MyProducer -[[produceStr]]-> Str)]]-> Str)
    ```
    

### Loosening the weakening restriction for compiler-derived implementations

The Roc compiler will derive implementations of certain builtin-abilities for types as requested. For example, the program

```python
l : DecodeResult (List U8)
l = Decode.fromBytes [50, 51] Json.fromUtf8
```

implicitly wants a decoder for a `List U8`. The Roc compiler knows how to define a decoders for generic lists, and decoders for `U8`, and can derive both such decoders.

The current mechanism for derived implementations, including decoders, is compositional. That means that when Roc sees a need for a decoder of form `List t`, where `t` is any type variable that satisfies the `Decoding` ability, including `List U8` and the generalized `List a | a has Decoding`, it will generate a (generalized) derived implementation of a decoder for List. In particular, the current derived implementation is equivalent to

```python
decodeListElems : Decoder (List elem) fmt | elem has Decoding, fmt has DecoderFormatting
decodeListElems = custom \bytes, fmt -> decodeWith bytes (list decoder) fmt
```

The derived implementation must be resolved at the exact time a derived implementation is seen to be necessary, because the lambda sets from the derived implementation must be filled into the rest of the program.

I think there are ways to do this lazily only after a concrete type is resolved, but it would require us to

- Install a synthetic marker for a lambda set, and its captures, that may be resolved in the future. It would have to be unique from unspecialized lambda sets, because this marks lambda sets that can only be resolved for a derived implementation when a type is fully resolved to a ground type (with no free type variables)
- It’s not clear when exactly we check we can lazily resolve the new lambda set marker to a concrete lambda set. One is idea is during the specialization of a function in monomorphization. In any case, it would need to be eager, and not the lazy implementation that we currently have (and hence more expensive)
- We would need to de-duplicate the new lambda set markers, since two different markers may result in the same lambda set after the fact. This would require synchronization, even between independent modules, which always slows compilation down. The current implementation also needs synchronization but it is rather inexpensive - generating and deduplicating of derived lambda sets happens in parallel, and synchronization is only generating a derived implementation requires unique access. Once a derived implementation for a generic type (like decode for `List a`) is created, it is re-used for all other list decoders

Taken at face value, the derived implementation `decodeListElems` would not be let-generalized using the new syntactic generalization rule. This is not ideal; as described above, it would be nice to keep the derived implementation polymorphic.

There are a couple ways to resolve this:

- Implement only the higher-region restriction, and not the syntactic-function restriction. As described before, implementing just the higher region restriction seems unpreferrable, because it can lead to a seemingly-confusing restriction in the language
- Make an exception and permit this derived implementation to be generalized, despite the surface-language restriction.
    
    Since we have control of derived implementations, we can ensure that the derived implementation does not lead to the problems described above. In fact, `decodeListElems` is a non-syntactic function value that has neither the problems previously described. The reason is that `custom` is a wrapped of a function in an opaque type, so `decodeListElems` is the same as defining the lambda `\bytes, fmt -> decodeWith bytes (list decoder) fmt`, modulo wrapping it in the module-private `Decoder` opaque type. It’s just that the syntactic function restriction does not see this.
    
    The upside is that we retain generalization where we need it. The downsides include
    
    - The compiler’s implementation is “special” in that a Roc user cannot transparently define `decodeListElems` the same way and use it polymorphically. They will need to define their own decoder for each type they want to use it as, or wrap the decoder in a thunk:
        
        ```python
        decodeListElems : {} -> Decoder (List elem) fmt | elem has Decoding, fmt has DecoderFormatting
        decodeListElems = \{} -> custom \bytes, fmt -> decodeWith bytes (list decoder) fmt
        ```
        
        where now they will need to pass `decodeListElems {}` wherever they need a list decoder.
        
        This might suggest that there are other patterns like this, e.g. for parsers, where defining functions that
        
        - need to be wrapped in an opaque type, but
        - are defined outside the module the opaque type is defined in
        
        will need to be thunk’d, or else lose polymorphism.
        
        **Note**
        
        If a function like `decodeListElems` were defined in the `Decode` module, it could work around the syntactic function restriction by wrapping the lambda in the `Decoder` opaque type, since immediate wrapping by an opaque type does not violate the restriction. That is, this would be polymorphic as expected:
        
        ```python
        decodeListElems : Decoder (List elem) fmt | elem has Decoding, fmt has DecoderFormatting
        decodeListElems = @Decode \bytes, fmt -> decodeWith bytes (list decoder) fmt
        ```
        
    - This makes the mental model for compiler developers more complicated

The suggestion is to implement the second, and allow ad-hoc exemption from the described restriction for compiler-derived implementations.

## Complete Implementation Specification

As described, the implementation suggestion by the extension proposal is to

- restrict let-generalizing of functions to syntactic functions, and those that do not violate the higher region restriction
- exempt the above restriction for compiler-derived implementations
- implement argument/return type layouts in lambda sets

The expectation is this will

- avoid redundant, unexpected work at runtime
- avoid unexpected dbg/expect values
- resolve some class of problems we currently experience due to function references (by extending layouts of lambda sets)
- resolve a class of problems when compiling higher-order ability specializations
