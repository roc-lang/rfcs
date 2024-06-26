# Union Refinement

Union refinement permits narrowing branches of tag union pattern matches to the subset of variants not covered by previous branches. For example,

```jsx
t : [User, Admin, SuperAdmin]
when t is
  User -> ... 
  other -> ... # here, other has type [Admin, SuperAdmin]
```

This kind of behavior is also known as “type narrowing” and “flow-sensitive typing” in other languages, like TypeScript. The behavior would provide for a subtyping look-and-feel to Roc, where it’s not otherwise. Many Roc users have run into a want for this kind of behavior.

This document describes one approach for type refinement in Roc specific to only refining tag unions in `when` expressions. Control-flow-sensitive typing in general is not considered. The approach is intended to be “zero-cost” in that it is sugar for what would otherwise be an identical transformation made by a user.

## Summary

- Tag unions may be refined if they bound to named variables in `when` branch patterns, like in the introductory example.
- A **named variable pattern** like
    
    ```python
    when t is
      User -> ...
      other -> ...
    ```
    
    types `other` as the type of `t` without all of the exhaustively-matches types proceeding the type of `other` - in this case, `other` is typed as `typeof t - typeof User`. The behavior of `-` operator is described explicitly below, but can be thought of removing the appropriate set of tags from a tag union type.
    
- An **as-pattern** pattern like
    
    ```python
    t : [A U8, B Str, C]
    when t is
      A _ | B _ as x -> ...
      C -> ...
    ```
    
    types `x` as explicitly the type of `A _ | B _`, which is the union type `[A U8, B Str]`.
    
- No other patterns or variables can undergo union refinement. Condition variables, variables in `if` expressions, etc. are not refined.
- Union refinements effect only non-recursive tag unions, and the first level of recursive tag unions.
- Union refinements never descend under recursive types’ recursion points. ([See this example](https://www.notion.so/Union-Refinement-5bef5072afb84675948f89525a25dbb7?pvs=21))
- Union refinements compile down to the equivalent manual transformation that would have to be done by hand, except that the compiler ensures type-safety and can elide `crash` branches where they otherwise would be necessary.
- If union refinements do not change the type, the compiler elides the transformation.
- If union refinements change the type but not the runtime value of a tag union, the compiler may in the future elide the transformation. This is an optimization that could be applied to many other types (”transmutation”) as well.

## Inference Scheme

As a quick primer, the inference scheme for pattern matches

```jsx
when condition is
  pattern_1 -> ...
  pattern_2 -> ...
```

consists of determining `typeof(patterns)` and `typeof(condition)` independently, and then checking that those types are equivalent (making them equal under unification). For example,

```jsx
t : [User, Admin, SuperAdmin]
when t is
  User -> ... 
  other -> ...

# typeof(condition) = typeof t = [User, Admin, SuperAdmin]
# typeof(patterns)  = typeof User ~ typeof other = [User] ~ * = [User]
# => typeof(t) = [User, Admin, SuperAdmin] ~ [User] = [User, Admin, SuperAdmin]
```

To describe the behavior of union refinement, we will now also keep track of each individual branch pattern type (`t_bp1` denotes the type of the first branch pattern).

We will also introduce a union subtraction operator. `T - U` is defined as follows:

- If `T = [T1 p11 ... p1n, ..., Tn pn1 ... pnm]` and `U = [U1 q11 ... q1n, ..., Un qn1 .. qnm]` are tag unions with tags `T1, ..., Tn` (resp. for `U`) and `T1` has payloads `p11 ... p1n`
    
    Then let
    
    - `T_alone = [Tag p1 ... pn | (Tag p1 ... pn in T) and (Tag p1 ... pn not in U)]`
    - `Shared = [ Tag (p1 - q1) ... (pn - qn) | (Tag p1 ... pn in T) and (Tag q1 ... qn in U) and (exists i = [1, n] s.t. (pi - qi) != []) ]`
    
    And $\tt{T - U = T_{alone} \bigcup Shared}$
    
- If `T` is any other type and `U = *`, `T - U = []`. That is, subtracting “any type” from a type produces the empty type.
- If `T` and `U` are any other type, `T - U = T ~ U` or an error if the types do not unify. That is, type subtraction has no effect on all other concrete types.

### Concrete description

Okay, the description of the inference scheme is as follows. There are two cases - one for unqualified named variables in branches, and one for `as` patterns in branches. Let’s do the `as` pattern first, since it’s a bit easier.

**as-pattern refinement**

For a pattern `p1 | ... | pn as x`, we now have `typeof(x) = typeof(p1) ~ .. ~ typeof(pn)` (subject to further refinement in how `x` is used in the body of the pattern). For example,

```jsx
f = \t -> when t is
  A | B as x -> if Bool.true then x else C

# Step 1: typeof(x) = typeof(A) ~ typeof(B) = [A, B]
# Step 2: typeof(x) = typeof(x) ~ typeof(C) = [A, B, C]
```

As-pattern refinements can be nested. For example, the above refinement applies to the pattern

```jsx
C | D _ (A | B as x) | E
```

Note that the outer tag constructors `C`, `D`, `E` do not affect the refinement at all.

**Unqualified named variable patterns**

Consider a named variable `x` in a pattern that is the `n`th branch, and denote `>n>x` to be the path to that pattern. For example,

```jsx
f : [User, Admin, SuperAdmin] -> _
f = \t -> when t is
  User -> ...
  other -> ...
```

`other` is in the 2nd branch, and its type is thus denoted by `bp_2>2>other`.

The path `>2>other` can be applied to any other branch; in this case, the path `bp_1>2>other` refers to the pattern `User`. In our example above, the path is not really useful since the patterns have no depth, but see the **Examples** below for cases where the path is important.

The type of `bp_n>n>x` is

- Determined after the type of the condition variable `typeof(cond)` is determined. During the determination of the condition type, `bp_n>n>x` contributes nothing (except possibly openness constraints, disregard this if unfamiliar)
- Then, `typeof(bp_n>n>x) = typeof(cond) - typeof(bp_1>n>x) - ... - typeof(bp_{n-1}>n>x)`

Working off our example above,

```jsx
f : [User, Admin, SuperAdmin] -> _
f = \t -> when t is
  User -> ...
  other -> ...

# Step 1: type of the condition t  
#   typeof(condition) = typeof t = [User, Admin, SuperAdmin]
#   typeof(patterns)  = typeof User ~ typeof other = [User] ~ * = [User]
#   => typeof(t) = [User, Admin, SuperAdmin] ~ [User] = [User, Admin, SuperAdmin]
# Step 2: type of bp_2>2>other
#   typeof(bp_2>2>other) = [User, Admin, SuperAdmin] - typeof(User)
#                        = [Admin, SuperAdmin]
```

This inference scheme guarantees correctness via the following key property - the refined union type for a path `bp_n>n>x` will only ever be a subset of the condition type, and moreover all values reached in the path `bp_n>n>x` are members of that subset.

To see that this is correct, note that the `-` operator only removes parts of the type that are uninhabited (that is, no runtime value could ever be of that type), and the type of `bp_n>n>x` contains the type of the condition as an upper bound.

### Type Expansion

An important note is that the inference of refined unions does not necessarily preclude the refined union from growing in size based on later usages. For example, working off the example in the previous subsection, a possible derivative is

```jsx
f : [User, Admin, SuperAdmin] -> _
f = \t -> when t is
  User -> ...
  other -> if Bool.true then Unprivileged else other
```

In this case, `other` will ultimately be inferred to have type `[Unprivileged, Admin, SuperAdmin]` - the same as the condition type.

Notice that this ultimate inferred type is still correct from a semantics perspective. The definition site of `other` would refine to the exact type `[Admin, SuperAdmin]`, and it’s only the usage site that would allow `other` to be used in a wider context that includes `[Unprivileged]`.

## Compilation Scheme

There is only one direct way to implement this kind of refinement in userspace - by unpacking and repacking variants of the refined type as appropriate, and `crash`ing on the branches that are not reachable in the refinement.

The proposed compilation scheme treats union refinement as syntax sugar for exactly this implementation strategy. Because the compiler is advantaged by knowing the unpacking/repacking is type-safe, it can freely eliminate `crash` statements where they would otherwise be needed today.

Our last example above

```jsx
f : [User, Admin, SuperAdmin] -> _
f = \t -> when t is
  User -> ...
  other -> if Bool.true then Unprivileged else other
```

would be desugared as

```python
f : [User, Admin, SuperAdmin] -> _
f = \t -> when t is
  User -> ...
  _ ->
		other : [Unprivileged, Admin, SuperAdmin]
    other = when t is
      Admin -> Admin
      SuperAdmin -> SuperAdmin
      # The compiler would have the freedom to elide this branch, as it is never reachable.
      User -> crash "unreachable"

    if Bool.true then Unprivileged else other
```

If the refined union type is ultimately equivalent to the condition type, the compiler would have the freedom to avoid the conversion altogether.

If the refined union type ultimately has the same runtime representation as the condition type, the compiler could perform an optimization that elides the conversion. Detecting and applying such an optimization should be straightforward.

In any case, this transformation is expected to be negligible in cost. It is only applied to **as-patterns** and **unqualified named variable patterns**.

## Examples

Let’s close off with some examples of how the inference and compilation scheme would work for different kinds of patterns.

### Catch-all named variable

```python
\x -> when x is
  A -> B
  C -> D
  a -> a

# branch inference
#   typeof cond = [A, C]*
#   typeof a = typeof bp_3>3>x = [A, C]* - [A] - [C] = []*
# body inference
#   typeof a = []* ~ [B, D] = [B, D]*
```

Compilation for `typeof x = [A, C, E]`:

```python
\x -> when x is
  A -> B
  C -> D
  _ ->
    a : [B, D, E]
    a = when x is
      E -> E
      _ -> crash "unreachable"
```

### Catch-all unnamed variable

```python
\x -> when x is
  A -> B
  C -> D
  _ -> x
```

This example has no refinement, as `x` is assigned the condition type, and the last branch introduces no named variable eligible for union refinement.

### Catch-all named variable followed by redundant pattern

The following program has a type error - the `D` branch is redundant because it is covered by `a` - but the inference and compilation scheme is still correct.

```python
\x -> when x is
  A -> E
  C -> F
  a -> a
  D -> G

# branch inference
#   typeof cond = [A, C, D]*
#   typeof a = typeof bp_3>3>x = [A, C, D]* - [A] - [C] = [D]*
# body inference
#   typeof a = [D]* ~ [E, F, G] = [D, E, F, G]*
```

Compilation for `typeof x = [A, C, D, X]`:

```python
\x -> when x is
  A -> E
  C -> F
  _ ->
    a : [D, E, F, G, X]
    a = when x is
      D -> D
      X -> X
      _ -> crash "unreachable"
```

### Catch-all named variable subtracts covered types

```python
x : [Add U8 U8, Sub U8 U8]
when x is
  Add m n -> ...
  other -> ...

# typeof cond = [Add U8 U8, Sub U8 U8]

# typeof bp_1>1>m = U8
# typeof bp_1>1>n = U8
# typeof bp_2>2>other = [Add U8 U8, Sub U8 U8] - bp_1>2>other
#                     = [Add U8 U8, Sub U8 U8] - [Add * *]
#                     = [Sub U8 U8]
```

### Catch-all named variable does not subtract uncovered types

```python
x : [Add U8 U8, Sub U8 U8]
when x is
  Add 0 n -> ...
  Add m 0 -> ...
  other -> ...

# typeof cond = [Add U8 U8, Sub U8 U8]

# typeof bp_1>1>n = U8
# typeof bp_2>2>m = U8
# typeof bp_3>3>other = [Add U8 U8, Sub U8 U8] - bp_1>3>other - bp_2>3>other
#                     = [Add U8 U8, Sub U8 U8] - [Add U8 *] - [Add * U8]
#                     = [Add U8 U8, Sub U8 U8] - [Add * U8]
#                     = [Add U8 U8, Sub U8 U8]
```

### Catch-all named variable under covered recursive type

```python
LinkedList a : [Nil, Cons a (LinkedList a)]
x : LinkedList U8
when x is
  Nil -> ...
  Cons _ (Nil) -> ...
  other -> ...

# typeof cond = [Nil, Cons U8 <rec>] as <rec>
# typeof bp_3>3>other = [Nil, Cons U8 <rec>] as <rec> - bp_1>3>other - bp_2>3>other
#                     = [Nil, Cons U8 <rec>] as <rec> - [Nil] - [Cons * Nil]
#                     = [Cons U8 <rec>] as <rec> - [Cons * Nil]
#                     = [Cons U8 <rec>] as <rec>
```

Notice that no subtraction happens under recursive type points, since those aren’t tag unions (in the strict sense). This is intentional!

An alternative, less restrictive version would permit refining the recursive union type

```python
#                     = [Cons U8 <rec>] as <rec> - [Cons * Nil]
#                     = [Cons U8 [Nil, Cons U8 <rec>] as <rec>] - [Cons * Nil]
#                     = [Cons U8 [Cons U8 <rec>] as <rec>]
```

However, this can produce very complicated types, for nebulous value.

### Catch-all named variable has same type as condition

```python
a : [A I32, B, C]
when a is
    A val -> A (val + 1)
    x -> x

# branch inference:
#   typeof cond = [A I32, B, C]
#   typeof bp_2>2>x = [A I32, B, C] - [A *] = [B, C]
# body inference:
#   typeof x = typeof bp_2>2>x ~ [A I32] = [A I32, B, C]
```

Compilation:

```python
a : [A I32, B, C]
when a is
  A val -> A (val + 1)
  _ ->
    x : [A I32, B, C]
    x = a # compiler sees equal types, skips conversion
    x
```

### Catch-all named variable has larger type than condition

With this model it is possible to arbitrarily expand tag union types by naming them.

```python
expandError : [Io Str] -> [Io Str, Net Str]
expandError = \e -> when e is
  error -> error
```

Which would be compiled to

```python
expandError : [Io Str] -> [Io Str, Net Str]
expandError = \e -> when e is
  _ ->
    error : [Io Str, Net Str]
    error = when e is
      Io s -> Io s
    error
```

Since tag unions are open by default in output position, the value of this is nebulous - you could instead get the conversion automatically just because of how tag unions work. In fact, the compiler would see that! Nevertheless, this is noted for completeness.

## Prior references

- [https://roc.zulipchat.com/#narrow/stream/304641-ideas/topic/Narrowing.20types.20in.20when.20expressions](https://roc.zulipchat.com/#narrow/stream/304641-ideas/topic/Narrowing.20types.20in.20when.20expressions)
- [https://ayazhafiz.com/articles/22/simple-flow-refinement-of-anonymous-sum-types](https://ayazhafiz.com/articles/22/simple-flow-refinement-of-anonymous-sum-types)
