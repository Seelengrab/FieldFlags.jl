# Examples

## [`@bitflags`](@ref)

### Immutable

```@repl
using FieldFlags

@bitflags struct MyFlags
    a
    b
    _
    c
end

FieldFlags.propertyoffset.(MyFlags, (:a, :b, :c))

FieldFlags.fieldsize.(MyFlags, (:a, :b, :c))

(;a,b,c) = flags = MyFlags(true, false, true)

sizeof(flags)

a, b, c

flags.a = false

flags.a
```

### Mutable

```@repl
using FieldFlags

@bitflags mutable struct MyFlags
    a
    b
    _
    c
end

(;a,b,c) = flags = MyFlags(true, false, true)

sizeof(flags)

a, b, c

flags.a = false

flags.a

flags.b = 0x3

flags.b # `setproperty!` truncates
```

## [`@bitfield`](@ref)

```@repl
using FieldFlags

@bitfield mutable struct MyField
    a:1
    b:3
    _
    c:9
end

FieldFlags.propertyoffset.(MyField, (:a, :b, :c))

FieldFlags.fieldsize.(MyField, (:a, :b, :c))

# 1+3+1+9 = 13, so we need two bytes
sizeof(MyField)

(;a,b,c) = field = MyField(0, 4, 19)

# field access either returns a `Bool` or an `UInt`
a,b,c

field.b = 0xff

field.b

# only the lower three bits are stored
bitstring.((0xff, 0x07))

f(x) = x.c

using InteractiveUtils

# everything compiles away by the time it reaches LLVM
redirect_stdout(devnull) do # hide
@code_llvm f(field)
end # hide
io = IOContext(IOBuffer(), :color=>true) # hide
mark(io) # hide
code_llvm(io, f, (MyField,)) # hide
reset(io) # hide
read(io, String) |> print # hide

# the native code is efficient
redirect_stdout(devnull) do # hide
@code_native f(field)
end # hide
mark(io) # hide
code_native(io, f, (MyField,)) # hide
reset(io) # hide
read(io, String) |> print # hide
```