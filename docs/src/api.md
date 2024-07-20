# API Reference

## Public API

The following symbols are considered API for the purposes of semver.

### Macros

```@docs
@bitflags
@bitfield
```

### Functions

These functions are explicitly not exported, to prevent confusion with `Base.fieldoffset` and similar field
and property related functions.

```@docs
FieldFlags.propertyoffset
FieldFlags.fieldsize
FieldFlags.bitfieldnames
```

### Additional Supported API

These functions are listed because they are supported, but their docstrings can't be displayed without
having an instance of a type created via [`@bitfield`](@ref) or [`@bitflags`](@ref).

 * `Base.propertynames`
    * Gives a tuple of the properties given in the original expression given to [`@bitfield`](@ref) or [`@bitflags`](@ref).
 * `convert(::T, x::Union{Bool, Base.BitInteger})`
    * Converts `x` to a `T`, originally created via the macros of this package. If the sizes don't match, `x` is either truncated or its bitrepresentation is zero-extended to fit the size of `T`.

## Internal API

The following symbols are NOT considered API for the purposes of semver. They are documented here
as a useful reference, not as a statement of semver guarantees.

```@docs
FieldFlags.bitflags
FieldFlags.bitfield
FieldFlags.cast_extend_truncate
FieldFlags.cast_or_extend
```
