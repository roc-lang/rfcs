# Type Erasure

## Introduction

As we look towards implementing effect interpreters and incremental builds, type erasure presents itself as a promising strategy for designing the implementation of these features.

This document discusses how we might implement type erasure in a language like Roc, with regard to various language features.

## Why consider type erasure?

### Polymorphism in effect interpreters

Certain effects, like `Map2` and `Storage`, must store polymorphic values whose types are not reflected in the type of the effect. Doing so requires rank-2 types, first order emulation of rank-2 types, or type erasure.

Rank-2 types are not intended to be supported. Even supporting rank-2 types in limited contexts requires first-order emulation to achieve optimal performance, or else compilation via type erasure, so their value for Roc is nebulous in the context of the other options.

A first-order emulation requires whole-program analysis for evaluation, similar to the first-order emulation provided by lambda sets. This slows compilation and adds another facet that incremental compilation must work around (see below).

Type erasure is more costly than a first-order emulation, and requires boxing (on the heap in general, though in some cases boxes need not live on the heap).

Type erasure provides the benefit of a uniform interface that is not polymorphic to a host’s glue code, and eases incremental compilation significantly (see below).

Effects using `Map2` are almost certainly not dominated by the overhead of boxing. `Stored` is intended mainly for tests and its boxes can be allocated in static memory. As such, the overhead of erasure for effects is considered to be negligible relative to the benefits it provides.

### Development builds

Today, the compilation of Roc programs requires several whole-program analyses. Besides monomorphization, which requires analyses linear in the call stack of a function, abilities induce dependency analyses that are not reflected in the call stack of a function, and lambda sets can also require arbitrarily-deep analyses. These analyses are powerful because they allow Roc to whittle down closures and interfaces to forms that have no runtime cost by trading off compile-time.

However, during development feedback loops, this is often an undesirable tradeoff - indeed, many developers may prefer lower compile times by trading off runtime cost if it improves their feedback loops. As it stands, Roc’s compile times must grow super-linearly to the size of a Roc project. Ideally, Roc compile times would be amortized at near-constant for incremental changes (see incremental compilation below), or be strictly linear to the size of a Roc project.

By type-erasing abilities (via dictionary passing) and lambda sets (via boxed closures), we get closer to cold development builds of Roc projects being linear in the size of the project. It doesn’t get us quite there because of monomorphization, but separate ideas with regard to that can also be discussed.

### Incremental builds

Ideally, hot builds of Roc projects would be linear in the incremental change to a Roc project, and require linear space in the size of the incremental change (amortized constant space and time relative to the project size). Today, with the best approaches we are able to imagine so far, incremental changes must require super-linear time and space relative to the size of the change, and the time and space complexity is bounded by the size of the entire Roc project and all its dependencies (!).

In development mode, as it is for cold development builds, incremental compilation should be as fast as possible. Type erasure of lambda sets and abilities gives us a mechanism to localize at least those features to individual modules or top-level functions in the incremental compilation scheme. This brings us closer to incremental compilations that are linear in both time and space relative to the size of the incremental change. Like cold development builds, incremental builds may still be super-linear to the incremental change in both time and space due to monomorphization, but reducing three “hard” facets to one is a win.

## Order of operations

Supposing we agree that type erasure is worthwhile and the approach described below sounds reasonable, the proposed relative order of operations would be

1. Adding support for type-erased values in static memory, as needed for Stored task values
2. Adding support for type-erased values and closures on the heap, as needed for Map2
3. Adding support for type-erased abilities. The previous two steps should not need support for abilities, since the erased value will never leak into userspace.

## General representation

I hope you agree that type erasure is worth exploring. Let’s take a look at what we need to get there.

In the most general case an erased type `Erased` must be a fat pointer consisting of

- a pointer to the boxed value. The location of the box is unimportant, the key is that we have a pointer to it (so that the erased type is uniform in size)
- a pointer to the value’s virtual call table (vtable)

where the vtable consists of

- The refcounting function for the value under the box, if any
- (For values used as abilities only) a dictionary of the ability methods the value implements
- (For lambda sets only) the function to dispatch to

Note that the vtable for a value can be determined fully ahead-of-time, and thus can be emplaced in static memory, from where it is referenced in the fat pointer representation.

The same applies for dictionaries of ability methods, but the dictionary size may not be consistent, so different vtables may have different sizes. We’ll discuss ability method dictionaries later on.

The C representation of `Erased` would be as

```c
struct erased {
  void* value;
  void (*refcounter)(void*);  // NULL if value needs no refcounting
  void* callee;  // NULL if the erased value is not a closure
  struct ability_dictionary* abilities;  // Discussed below 
};
```

## Erasing refcounts

Reference-counting an `Erased` value involves inductively reference-counting its boxed value. Note that `refcounter`, `callee`, and `ability_dictionary` should never need to be refcounted, since they are pointers into static memory. Since `Erased` is a value type, it never needs to reference counted either.

As such, the mechanism for reference counting `Erased` is

```c
void refcount_erased(struct erased er) {
  if (er.refcounter) {
    er.refcounter(er.value);
  }
}
```

Notice that since the value is erased, the implementation of `erased` must dispatch to a `refcounter` that too expects an opaque pointer. Only inside the `refcounter` implementation may the concrete type be dereferenced.

```c
struct erased erased_str = .{ refcounter=refcount_erased_str };

void refcount_erased_str(void* er_str) {
  RocStr str = *((RocStr*) er_str);
  refcount_str(str);
}
```

This concludes the requirements for reference counting erased values.

## Erasing lambda sets to Erased closures

A value that is typically a Roc lambda set can be erased to a boxed (typically heap-allocated) closure via `Erased`. In particular,

- The erased value is the closure set (`NULL` if the closure is empty)
- The reference counter is the reference counter for the closure set
- `callee` is the concrete lambda to dispatch to
    - The caller of a erased closure casts  to `return_type (arg_1, ..., arg_n, void*)` where the last argument is the erased closure set the concrete `callee` will unpack.
- `ability_dictionary` is empty, as closures never implement abilities directly (though opaque wrappers can, see below)

Let’s walk through a concrete example of what constructing and dispatching to an erased closure looks like.

Then, we’ll see that erased closures can, in some cases, seamlessly interop with lambda sets by leveraging the unification-based type system.

### Compilation scheme

Consider the program

```python
g = 5
f = if Bool.true
    then
      addG = \x, y -> x + y + g
			addG
    else
      sub = \x, y -> x - y
		  sub

n = f 1 2
n
```

With the compilation scheme described above, this program lowers to

```c
struct addG_closure { int g };
int addG(int x, int y, void* opaque_closure) {
  struct addG_closure* r_closure = (struct addG_closure*) opaque_closure;
  int g = r_closure->g;
  return x + y + g;
}

int sub(int x, int y) {
  return x - y;
}

void main() {
	int g = 5;
	struct erased f;
	if (1) {
	  struct addG_closure* clos = (struct addG_closure*) roc_alloc(sizeof(addG_closure));
    clos->g = g;
    f.value = (void*) clos;
    f.callee = (void*) addG;
    f.refcounter = NULL;  // could be implicit
  } else {
    f.value = NULL;  // because no captures; could be implicit
    f.callee = (void*) sub;
    f.refcounter = NULL;  // could be implicit
  }

  // Call f 1 2
  int n;
  if (f.value) {
    // Closure is material
    int (*clos)(int, int, void*) = f.callee;
    n = clos(1, 2, f.value);
  } else {
    // Closure is immaterial
    int (*f)(int, int) = f.callee;
    n = clos(1, 2);
  }
}
```

An interesting question is whether the switch on a material `f.value` is necessary, or whether all functions can be compiled to take a (possibly null) closure argument.

I believe that the material switch is necessary, as it support uniform compilation of top-level functions for use both in erased closures and not in erased closures. If all callees in an erased closure must take a closure argument, then top-level functions must be compiled twice - once without a closure argument, and once with.

### Interaction with Morphic

Since erased closures preclude lowering Roc programs to a first-order IR, we can no longer apply morphic for ownership/borrow specialization when erased closures are present.

NB Folkert: is there anything else significant here? Is it enough to turn morphic off if a program has erased closures? Can we have “partial morphic”? Something else?

### Type-level scheme

Lambda sets are fully determined during type inference, and erased closures effectively involve “forgetting” a lambda set type and propagating that requirement forward - so erased closures can be fully determined during type inference as well.

**Coexistence of lambda sets and erased closures**

For the purposes of symmetry, erased closures can be typed as singleton lambda sets with the unique lambda `%Erased`.

Lambda sets and erased closures can exist in harmony, if only we can describe what happens if they intersect. For example, consider a program like

```c
g = 5
f = if Bool.true
    then
      addG = \x, y -> x + y + g
			erase addG
    else
      sub = \x, y -> x - y
		  sub
```

where `erase` is some mechanism to erase a value, including a function value (not suggesting it’s in the Roc syntax). What is the type of `f`?

We must notice that a lambda set can be erased, but the inverse is not true. So, a unification of a lambda set and an erased closure is always dominated by the erased closure! We’ll say that in this sense, erased closures are **viral**.

```c
   I64, I64 -[[addG { I64 }]]-> I64
~  I64, I64 -[[%Erased]]-> I64
=> I64, I64 -[[%Erased]]-> I64
```

Note that this is only possible due to the unification-based scheme of the type system.

Notice that a generalized function may emit a lambda set, but during specialization, induce a specialization that requires an erased lambda. For example:

```python
getSub = \{} ->
  sub = (\x, y -> x - y)
  sub
# => getSub : {} -> (I64, I64 -[[sub]]-> I64)

f = if Bool.true
    then
      addG = \x, y -> x + y + g
			erase addG
    else
      getSub {}
# => f : I64, I64 -[[%Erased]]-> I64
```

Would undergo the following specialization

```python
# >> specialize f : I64, I64 -[[%Erased]]-> I64
f = if Bool.true
    then
      addG = \x, y -> x + y + g
			erase addG
    else
      getSub {}
			# >> specialize getSub : {} -> (I64, I64 -[[sub]]-> I64)
			# ~  use site   getSub : {} -> (I64, I64 -[[%Erased]]-> I64)
			# => generate   getSub : {} -> (I64, I64 -[[%Erased]]-> I64)
			# getSub = \{} ->
			  # sub = (\x, y -> x - y)
				# >> specialize sub : I64, I64 -[[sub]]-> I64
				# ~  use site   sub : I64, I64 -[[%Erased]]-> I64
				# => generate   sub : I64, I64 -[[%Erased]]-> I64
			  # sub
```

**Impact on abilities**

I believe the type-level co-presence scheme described above has no impact on the ambient-function specialization algorithm for abilities, except that in the presence of erased closures there is much less work to do.

**Utility of co-present lambda sets and erased closures**

I am not presently aware of immediate benefits that the co-presence of lambda sets and erased closures provides to us.

Perhaps this means that the Roc compiler can compile a program with dependencies A and B, where an incremental compilation of A with lambda sets is available, and an incremental compilation of B with erased closures is available. However, if these modules interact, then the virality of erased closures means that A will need to be updated to use erased closures as far as B reaches into it. So, it’s not clear to me if lambda sets and erased closures can meaningfully co-exist.

## Erasing abilities

I am not sure about the utility of erasing abilities in the presence of monomorphization. As long as we monomorphize, we must also resolve and specialize abilities, since in general abilities can be polymorphic over any number of parameters. If at some point we avoid monomorphization by instead compiling to uniform representations, then erasure of abilities becomes useful as a mechanism to avoid specialization of only abilities. The resolution step, however, is always necessary.

For alternative techniques on compiling abilities, see [Compiling abilities without monomorphization](https://www.notion.so/Compiling-abilities-without-monomorphization-3ba53c2ebfd74aada6bf09e659c68b34?pvs=21).

## Interaction with hosts

How hosts interact with Roc code does not have to change.

### Erased values

Glue can generate code to operate over `Erased` values, since those structures are uniform (and there is only a few things you can do with them, namely either call a function or call the reference count on an erased value).

### Erased closures

The interface for host-exposed functions need not change either. Currently, lambda sets are read into the host and then called by dispatching to Roc-generated functions. These pairs of functions look something like

```c
int get_lambda_set_size();
void write_lambda_set(void* lambda_set);
void* lambda_set_caller(void* lambda_set);
```

These functions work just as well for lambda sets as they do for erased closures. For erased closures, `get_lambda_set_size` is static, `write_lambda_set` writes the stack value of the erased closure, and `lambda_set_caller` performs the dispatching mechanism previously described.
