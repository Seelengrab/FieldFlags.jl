# Examples

This page contains some examples on how to use `@bitflags` and `@bitfields`, though if you are familiar with regular structs,
the usage should be familiar, as the interfaces are modeled after them.

## Basic bitflags

Starting with [`@bitflags`](@ref), which is used for tightly-packed boolean storage, which can be used like so:

```@repl basicBitflags
using FieldFlags
@bitflags struct MyFlags
    flagA
    flagB
    _ # padding
    flagC
end
```

The above defines a struct `MyFlags` with three fields, `flagA`, `flagB` and `flagC`. As the comment indicates, `_` is for specifying padding.
All fields specified in the `struct` take up a single bit - even the padding. The minimum size for the above is thus 4 bits. The fields are stored
from least significant bit to most significant bit, starting with `fieldA`.

While the minimum bitsize for the above struct is 4 bits, due to an implementation detail/compiler requirement, all structsizes are rounded
up to the next multiple of 8 bits. `MyFlags` is thus 8 bits, or 1 byte, large:

```@repl basicBitflags
sizeof(MyFlags)
```

That is, an instance of `MyFlags` has these bits:

|MSB   |      |      |      |LSB   |
|:----:|:----:|:----:|:----:|:----:|
|5-8   |flagC |_     |flagB |flagA |

With the 4 bits higher than `flagC` being implicit padding as well.

`@bitflags` gives us two default constructors; a zero-arg constructor as well as an `n`-arg constructor.

The zero-arg constructor allows us to construct an instance of `MyFlags` with all fields set to `false`:

```@repl basicBitflags
mf = MyFlags()
mf.flagA == mf.flagB == mf.flagC == false
```

As can be seen above, individual fields can be accessed with regular dot-syntax.

!!! note "Fields vs. Properties"
    Technically speaking, neither `@bitflags` nor `@bitfield` gives a struct with actual _fields_ - dot-syntax access is only simulating fields, by overloading `getproperty`.
    That is, a call like `getfield(mf, :flagA)` cannot succeed - use `getproperty(mf, :flagA)` instead, which handles the field unpacking for you. This is a technicality though,
    and as such `property` and `field` are used interchangeably in this documentation.

In contrast, the `n`-arg constructor takes one argument for each field:

```@repl basicBitflags
mf = MyFlags(true, false, true)
mf.flagA == mf.flagC == true
mf.flagB == false
```

## Mutability

While immutability can be useful, sometimes it is more convenient to mutate a flag in-place. This can be achieved by marking the struct given to `@bitflags`
as mutable:

```@repl mutableFlags
using FieldFlags

@bitflags mutable struct MutableFlags
    a
    _
    b
    _
    c
end
```

The above struct requires at least 5 bits, which means the bitlayout is like so:

|MSB   |      |      |      |      |LSB   |
|:----:|:----:|:----:|:----:|:----:|:----:|
|6-8   |c     |_     |b     |_     |a     |

The remaining upper 2 bits are once again implicit padding, while the overall size of the objects stay the same:

```@repl mutableFlags
sizeof(MutableFlags)
```

The available constructors are also once again the same:

```@repl mutableFlags
methods(MutableFlags)
```

The only difference is that we are now able to set individual fields in an object:

```@repl mutableFlags
mutf = MutableFlags(false, false, false)
mutf.a == false
muf.a = true
mutf.a == true
```

which we weren't able to do earlier:

```@repl basicBitflags
mf.flagA = true
```

!!! warning "Allocations"
    One limitation of allowing fields to be set is that the object is declared as `mutable`, which has the same effect
    as with regular structs that are marked as mutable. For example, `mutable` structs aren't guaranteed to be stored inline
    in other objects like wrapper structs or arrays, which may require additional allocations. Setting/reading flags of
    mutable objects does not lead to allocations - these stay allocation-free.

## Subtyping

On top of mutability, we can also specify an abstract supertype as usual:

```@repl supertypes
abstract type MyAbstract end
@bitflags struct MyConcrete <: MyAbstract
    foo
    _
    bar
    baz
end
supertype(MyConcrete) == MyAbstract
```

This allows for defining common fallback methods for `@bitfield` or `@bitflags` structs that may share some common fields or other invariants:

```@repl supertypes
@bitflags struct OtherConcrete <: MyAbstract
    foo
    _
    bak
end
fallback(ma::MyAbstract) = ma.foo

fallback(MyConcrete(true, false, false)) == true
fallback(OtherConcrete(false, true)) == false
```

## [`@bitfield`](@ref) structs

Structs defined with `@bitfield` are, in regards to mutability, bitsize and subtyping behavior, identical to those defined by `@bitflags`. The major difference is
that while `@bitflags` structs only hold one bit per field, `@bitfield` can hold multiple bits per field:

```@repl bitfield
using FieldFlags

@bitfield mutable struct MyField
    a:1
    _:2
    b:3
    _
    c:2
end
```

The above defines a struct `MyField`, with three fields `a`, `b` and `c`, with sizes (in bits) `1`,  `3` and `2` respectively. There are also two definitions of explicit padding
between fields, the first being `2` bits in size and the second one being `1` bit in size; taken implicitly from `_` not having a size annotated. The layout of the above struct is
like so:

|MSB   |   |   |   |   |   |   |   |   |LSB   |
|:----:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:----:|
|10-16 |c  |c  |_  |b  |b  |b  |_  |_  |a     |

With the additional padding bits, we come to a total of 9 bits. This is again rounded up to the next multiple of 8, which is 16 bits or 2 bytes:

```@repl bitfield
sizeof(MyField)
```