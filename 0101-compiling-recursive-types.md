# Compiling Recursive Types

Status: Review

## Problem

Roc supports structural recursive types and compilation to unboxed values. This
combination has proven difficult in the implementation of Roc's compiler.

Two structurally recursive types are defined to be compatible if they are
equivalent; that is, if unrolling their recursive types indefinitely would
produce equal types. Two unboxed values are defined to be compatible if their
memory layouts are equal.

Unfortunately, equivalent structurally recursive types do not necessarily
produce equal memory layouts in the current implementation. As a result,
recursive values with equivalent-but-not-equal types often miscompile.

## Background

Roc includes the following features:

1. A structural type system with complete inference.
   A value `A {f: ""}` can be assigned type `[A {f : Str}]`, without that type
   having to be explicitly named. Languages like TypeScript and OCaml also
   support structural type systems. Other languages, for example Go or C++, do
   not allow typing `{f: ""}` without wrapping it in a named type constructor;
   for example `type mytypename struct { f string }` in Go.
2. 
   <summary>
   Compilation to unboxed value representations.

   An unboxed value is compiled to the representation you would likely choose
   by hand, if you were only optimizing for the runtime cost of a single value.
   For example, a Roc value `A {f: 1u8}` of type
   `[A { f : U8 }, B { g : U16 }]` is represented at runtime in a form
   equivalent to the C struct

   ```c
   #include <stdint.h>

   struct type1 {
     uint8_t tag;     // either A or B
     uint16_t value;  // either f or g
   }
   ```

   which at runtime, takes the "optimal" size of 4 bytes.

   <details>
   In general, compilers make a tradeoff in how values are stored at runtime.

   Compilation to unboxed values allows software developers to nearly maximize
   performance ceilings in a language high-level than machine code or assembly.
   Indeed, this is the primary benefit. There are some auxiliary benefits; for
   example, the technology that must be developed to support such compilation
   can also be useful in developer tools.

   The main downside of compilation to unboxed values is implementation
   complexity and compile-time cost, since all generic functions must be
   specialized to unboxed values.

   The alternative is to represent all runtime values the same way in memory,
   typically by shoving the actual value onto the heap, and keeping around a
   fixed-size record on the stack that contains a pointer to the actual value,
   and some metadata about it.
   This is called a "boxed" representation, I guess because all boxes look the
   same.

   Boxed value have a number of benefits. The main one is simplicity. Compiler
   implementation is simplified, and support for things like typeclasses (Roc
   abilities), runtime reflection, reference counting, etc. is relatively easy.
   Since every value is represented the same way, implementations of runtime
   features generally have to consider only one representation. Another
   significant benefit is there is no need to specialize generic functions,
   which reduces code size and compile time.

   The main downside of boxed values is performance. In general memory is
   wasted, and the lack of specialization means that in general CPU time is
   wasted. The opposite-end alternative to boxed values is unboxed values,
   wherein all values are compiled to their "optimal" runtime form.
   </details>
   </summary>

These two features have emergent properties that are non-obvious. The main one
for this document to discuss is recursive types. Consider the following
recursive types:

```roc
Rec1 : [ From2 Rec2, End ]
Rec2 : { from1 : Rec1 }
```

spelling out the locations where `Rec1` and `Rec2` recurse as `<Rec1>` and
`<Rec2>` respectively, we have

```
Rec1 : [ From2 { from1 : <Rec1> }, End ]
Rec2 : { from1 : [ From2 <Rec2>, End ] }
```

Now suppose that we have the program

```
val1 : { from1 : Rec1 }
val1 = ...

val2 : Rec2
val2 = ...

expect val1 == val2
```

Unfortunately these two values have different runtime layouts. In particular,

```
val1 runtime layout:
{ from1: void* }
where void* points to a layout
{ tag: u8, payload: { from1: void* } }

val2 runtime layout:
{ from1: { tag: u8, payload: void* } }
where void* points to the above layout
```

This produces a compilation failure, because Roc's backends expect equivalent
types to have equal memory layouts. To see why this is a useful, and in fact
necessary property of the backends, imagine trying to write an implementation of
`eq` for these values given two different layouts.

These different layouts also do not play well with Roc's compilation
dependencies, including Morphic and LLVM. Ways to deal with this will be
discussed later.

## Discussion

The key insight here is that types can be equivalent, but layouts must be equal.
The equivalence, I mean that types must be equal if you unroll their place of
recursion indefinitely.

There are only three ways to reconcile this:

1. Types must be equal, not just equivalent.
2. Equivalent types must produce equal layouts.
3. Layouts must only be equivalent, not just equal.

Unless I am missing something, the solution space is bound to one of these three
approaches.

I think the solution space can further be reduced to only considering either
(1) or (3). After all, if we could do (2), then that must mean there is a normal
form for types such that equivalent types have equal normal forms. This is a bit
of a leap but hopefully uncontroversional - I'll make the claim that if there is
such a normal form, the normal form can also be a raised to the form of a type.
Hence, if we can do (2) for equivalent types, we should also be about to make
equivalent types equal as in (1).

## Alternatives

### 1. Recursive types must have a explicit point of recursion 

This approach attempts to force equivalent types to be equal. As far as I am
aware, only recursive types can be equivalent-but-not-equal today, and the
position where two equivalent types recurse determines whether they are equal.

One way to force equality is to force equivalent recursive types to choose adopt
one form as normal when they are unified. This is implemented today as an
operation called "fixpoint fixing" (adjusting the fixpoint of two recursive
types to be equivalent). However, this operation is insufficient when types are
exported across modules. When types are exported across modules, their
relationship to types in the source module is lost. This means that a type
exported from module A to module B may be fixpoint-fixed in module B, and end up
in a form that is equivalent, but not equal, to its form in module A. When
module A and B are compiled, that type will lower to equivalent but not equal
layouts.

At the time fixpoint fixing was implemented, this limitation was entirely
overlooked. I hope it is clear that the only way fixpoint fixing can resolve
this issue in the presence of modules is to break module isolation, and have
typechecking be performed globally, with sharing across all modules. This has a
number of problems, including making incremental compilation nearly impossible.
I am happy to elaborate more as desired.

The alternative is to consider a language-feature restriction that forces
equivalent recursive types to be equal. The simplest one I can think of is what
most other languages do - require recursive types to be nominal. In Roc, this
would mean that recursive types must be named as opaque types.

If a structural type is determined to be recursive, but has no recursion point
that is an opaque type, that will now be an error. Otherwise, two recursive
types can only be equivalent if they are the same opaque type - which
necessarily means they are equal, since opaque types can only have one
definition.

#### User implications

The main implication of this for Roc developers is that all recursive types will
now have to named as opaque types explicitly.

For example, the following function which calculates the length of a linked
list, would no longer typecheck:

```roc
length = \list ->
   when list is
      Nil -> 0
      Cons _ rest -> 1 + length rest
```

An appropriate error message for this program would be something like

```
--- ANONYMOUS RECURSIVE TYPE ---

This function contains a recursive type that has no name:

length = \list ->
^^^^^^
   when list is
      Nil -> 0
      Cons _ rest -> 1 + length rest

`length` has a type that looks like
  
  [Nil, Cons a <RecursiveType>] as <RecursiveType> -> Num *

Because `<RecursiveType>` is recursive, it must be defined as an opaque type.
Consider defining

  RecursiveType a := [Nil, Cons a (RecursiveType a)]

and changing the implementation of `length` to have type

  RecursiveType a -> Num *
```

Please defer discussion of the exact error message's wording to the implementation.
See below on how to implement this error message.

A suitable re-implementation of this function would be

```roc
LinkedList a := [Nil, Cons a (LinkedList a)]

length = \@LinkedList list ->
   when list is
      Nil -> 0
      Cons _ rest -> 1 + length rest
```

The most significant downside of this change is that recursive types that are
currently structural, and thus can be read or constructed by any module, would
now become opaque and the API surface for that type would be limited to the
defining module. It's unclear to me whether this would be an issue for programs
today, though I suspect likely not. Typically recursive types only appear in
data structures with well-defined APIs.

A lesser downside of this change is that it may make prototyping more cumbersome
since all type definitions must be spelled out. I suspect that in general this
will not matter much, since again recursive types are typically complex enough
that spelling them out is useful anyway.

#### Implementation implications

Explicitly disallow recursive type aliases. Recursive aliases, including the
`Type : ...` and `... as RecursiveType` forms, must now be defined as opaque
types instead.

When a recursive opaque type is defined, reduce it to a expanded, normal form.
Use this form in all places where the opaque type is referenced, in order to
ensure there is only one form of the opaque type.

Adjust the typing rules for recursive types to unify two types if their
recursive name (opaque type) is equal, and all type arguments unify. Remove all
logic for checking type equivalence up to position of recursion.

Note that recursive lambda sets are also nominal, because they must be named by
the lambda they appear under. Adjust the definition of lambda set types, if
necessary, to better match the adjusted definition of recursive types.

During type inference, existing logic for detecting structurally-recursive types
will have to be kept in place in order to produce useful error messages. If a
recursive type is detected when inferring the type of a value, perform an occurs
check on the recursive type. If no type in the occurs chain is a
`RecursivePointer` type constructor, choose an arbitrary recursion point, and
generate an error type as in the error message above.

No changes should be required in compiler passes past type inference/type
checking.

#### Language implications

This change would push Roc towards requiring named types in more cases. I do not
think this is in opposition with the goals of the language.
Structural types are useful for ad-hoc values, but recursive types are rarely
that; they are typically involved in well-defined data structures.

It may be worth considering whether there is a need for non-opaque nominal
types. Such a feature would have use cases besides supporting recursive types
with an API surface outside their defining module. I suggest considering such a
feature, but not blocking any desired adoption of this alternative on it.
Starting with the restriction of recursive types to opaque definitions, and
lessening the restriction if necessary, seems prudent.

### 2. Compilation with support for equivalent layouts

Instead of expecting layouts to be equal, flow information about layouts through
the backends, and handle transformations between equivalent-but-not-equal
layouts throughout each backend.

This is certainly doable but I'm going to consider it a non-starter for the
following reasons:

- At some point, equivalent layouts must be turned into equal ones. Otherwise,
    there is no way to hand off a Roc program to Morphic. There is also no way
    to hand off such a Roc program to LLVM without passing around opaque
    pointers and implementing runtime reflection to perform the transformations
    at runtime.

- The cost of transforming between equivalent layouts is non-zero even when it
    is compiler-generated. These transformations may be seen as "pointless" and
    seen a target of optimization for developers that want to optimize Roc
    performance. Such optimization will be futile, and potentially detrimental
    to the project.

- This would be a large change that each backend would have to account for.

However, there may be a way to do this in a straightforward manner that I fail
to see at the moment. I'm raising this idea for consideration from others, who
may see an elegant solution via this approach.

### 3. Constructing a normal form for structural recursive types

I don't have a concrete proposal for this, but I'm raising the idea for
considerations from others that are more creative than I am.

In this alternative, we find some way to determine the normal form a
structurally recursive type. That is, to find a common normal form for the types

```
Rec1 : [ From2 { from1 : <Rec1> }, End ]
Rec2 : { from1 : [ From2 <Rec2>, End ] }
```

via some kind of algorithm.

A very simple algorithm might be to pick the point of recursion by ordering
types, where tag unions < records. Tag unions are ordered by the order of
their tag names and recursive ordering of payloads. Records are ordered by
their field names recursive ordering of their payloads. The lowest-ordered type
in the recursive chain is assigned the position of recursion.

Such an algorithm would pick the normal form

```
Rec : [ From2 { from1 : <Rec> }, End ]
Rec1 : Rec
Rec2 : { from1 : Rec }
```

However, I'm not sure how well this algorithm works in practice, whether it is
complete, and whether there is any complete algorithm. 

## Recommendation

Alternative 1.
