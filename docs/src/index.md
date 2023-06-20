# FieldFlags.jl Documentation

FieldFlags.jl is a small package without dependencies, giving users the ability
to create structs containing packed integers of various bitsizes.

The two main exports of this package are the two macros [`@bitflags`](@ref) and 
[`@bitfield`](@ref), for creating bit packed boolean flag structs and bit packed
integer field structs respectively.

This package is heavily inspired by C-style bitfields, though there are some limitations.

```@contents
Pages=["index.md", "examples.md", "api.md"]
Depth=3
```

## Goals

 * Low/Negligible overhead
    * I can get a [`bextract`](https://www.felixcloutier.com/x86/bextr) on my machine from extremely high level julia code
 * High performance
 * Good optimization by the compiler (constant folding, elimination of error paths)
    * The package should "feel" as if there were no special implementation
 * Good debuggability with JET.jl

## Limitations

 * Thread safety
   *  Accessing the objects produced by this package is not thread safe and atomic access is not planned to be supported. Users are advised to use proper locking to ensure safety.
 * Size of the objects
   * Due to a compiler limitation, the size of all objects created by this package is a multiple of 8 bits. This restriction may be removed in the future.
 * Type parameters cannot be supported - the size of a field needs to be known at definition time, so that the various bitshifts and masking operations done internally can be compiled away.
 * The widest a field can currently be is `8*sizeof(UInt)` bits, as `UInt` is currently the default return type for fields (other than those of width `1`, which return a `Bool`).

## Planned Features

 * Custom field type annotations
    * This would look like a regular field type annotation for exact sizes (i.e. `a::UInt16`), or like e.g. `a:3::UInt16` for objects that want to store the lower 3 bits of an `UInt16` and want to get that type back out when accessing the field.
    * Due to the nature of how these objects are stored internally, the types will need to be at least `isbitstype`, possibly even `isprimitivetype`, as it's unclear whether the padding potentially contained in an `isbitstype` is legal to observe (I suspect it isn't).
    * `<: Signed` types will need to be at least 2 bits in size, to store the sign bit. 
    * See [#9](https://github.com/Seelengrab/FieldFlags.jl/issues/9) for the issue tracking this.
 * Narrower field types
    * Currently, all field accesses (unless accessing a single bit field) return an `UInt`. This is not guaranteed, and may be narrowed in the future, such that a field annotated with width `2` returns an `UInt8` by default, width `9` an `UInt16` etc.
    * See [#7](https://github.com/Seelengrab/FieldFlags.jl/issues/7) for the issue tracking this.
