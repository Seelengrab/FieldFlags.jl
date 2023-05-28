# FlagFields.jl

![CI Stable](https://github.com/Seelengrab/FieldFlags.jl/actions/workflows/ci.yml/badge.svg)
![CI Nightly](https://github.com/Seelengrab/FieldFlags.jl/actions/workflows/nightly.yml/badge.svg)
[![docs-stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://seelengrab.github.io/FieldFlags.jl/stable)
[![docs-dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://seelengrab.github.io/FieldFlags.jl/dev)
[![codecov](https://codecov.io/github/Seelengrab/FieldFlags.jl/branch/main/graph/badge.svg?token=PBH8NJCHKS)](https://codecov.io/github/Seelengrab/FieldFlags.jl)

This package provides two tiny macros to create bitfield-like objects. For example:

```julia
@bitflags struct MyFlags
    flag_a
    flag_b
    flag_c
end
```

creates a new type `MyFlags`, which stores the boolean flags `flag_a`, `flag_b` and `flag_c` as its fields
in a compressed format. Due to compiler limitations, `MyFlags` is 1 byte large instead of 3 bits. All
structs created by `@bitflags` are a multiple of 8 bits large.

This may change in the future, but apart from that, the sky is the (literal) limit!

The above object can be accessed like any other struct:

```julia
julia> using FieldFlags

julia> @bitflags struct Foo
           a
           b
           c
       end

julia> methods(Foo)
# 1 method for type constructor:
 [1] Foo(a::Bool, b::Bool, c::Bool)
     @ none:0

julia> f = Foo(true, false, true)
Foo(true, false, true)

julia> f.a
true

julia> f.b
false

julia> f.c
true
```

and gives errors when a field that doesn't exist is accessed:

```julia
julia> f.z
ERROR: ArgumentError: Objects of type `Foo` have no field `z`
Stacktrace:
 [1] getproperty(x::Foo, s::Symbol)
   @ Main ~/Documents/projects/FieldFlags/src/FieldFlags.jl:57
 [2] top-level scope
   @ REPL[8]:1
```

as well as when the given struct either doesn't have any fields, or has duplicates:

```julia
julia> @bitflags struct Foo
       end
ERROR: LoadError: ArgumentError: `@bitflags` needs at least one field.
Stacktrace:
 [1] bitflags(expr::Expr)
   @ FieldFlags ~/Documents/projects/FieldFlags.jl/src/FieldFlags.jl:34
 [2] var"@bitflags"(__source__::LineNumberNode, __module__::Module, expr::Any)
   @ FieldFlags ~/Documents/projects/FieldFlags.jl/src/FieldFlags.jl:114
in expression starting at REPL[2]:1

julia> @bitflags struct Foo2
           a
           a
       end
ERROR: LoadError: ArgumentError: Fields need to be uniquely identifiable!
Stacktrace:
 [1] bitflags(expr::Expr)
   @ FieldFlags ~/Documents/projects/FieldFlags.jl/src/FieldFlags.jl:36
 [2] var"@bitflags"(__source__::LineNumberNode, __module__::Module, expr::Any)
   @ FieldFlags ~/Documents/projects/FieldFlags.jl/src/FieldFlags.jl:114
in expression starting at REPL[4]:1
```

Nevertheless, `getproperty` is implemented with efficiency in mind:

```julia
julia> foo(f) = f.a
foo (generic function with 1 method)

julia> @code_llvm foo(f)
;  @ REPL[9]:1 within `foo`
define i8 @julia_foo_199(i8 zeroext %0) #0 {
top:
	# [...]
; ┌ @ /home/sukera/Documents/projects/FieldFlags/src/FieldFlags.jl:93 within `getproperty`
   %4 = and i8 %0, 1
; └
  ret i8 %4
}
```

so no error paths survive during optimization (if the compiler can constant propagate the property access)
and keeps being efficient for even large structs:

```julia
julia> @bitflags struct Foo3
           a
           b
           c
           d
           e
           f
           g
           h
           i
           j
           k
           l
           m
           n
           o
           p
           q
           r
           s
           t
           u
           v
           w
           x
       end

julia> f = Foo3(rand(Bool, 24)...)
Foo3(false, true, true, false, false, true, true, true, true, true, true, false, true, false, false, true, false, true, false, false, false, false, true, true)

julia> foo(f) = f.w
foo (generic function with 1 method)

julia> @code_llvm foo(f)
;  @ REPL[12]:1 within `foo`
define i8 @julia_foo_269(i24 zeroext %0) #0 {
top:
	# [...] function setup from julia
  %4 = lshr i24 %0, 22
  %5 = trunc i24 %4 to i8
  %6 = and i8 %5, 1
  ret i8 %6
}

julia> @code_native foo(f)
	.text
	.file	"foo"
	.globl	julia_foo_290                   # -- Begin function julia_foo_290
	.p2align	4, 0x90
	.type	julia_foo_290,@function
julia_foo_290:                          # @julia_foo_290
; ┌ @ REPL[12]:1 within `foo`
	# [...] function setup from julia
	shr	eax, 22
	and	al, 1
	.cfi_def_cfa rsp, 8
	ret
.Lfunc_end0:
	.size	julia_foo_290, .Lfunc_end0-julia_foo_290
	.cfi_endproc
; └
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
```

## Bitfields

Apart from bitflags, which guarantee a `Bool` on property access, there's also `@bitfield`, for densely packing
integers of various sizes into an object, which also supports setting fields:

```julia
julia> @bitfield struct Foo
           a:1
           b:2
           c:1
       end

julia> f = Foo(0,3,1)
Foo(false, 0x0000000000000003, true)

julia> f.a
false

julia> f.b
0x0000000000000003

julia> f.c
true

julia> isone(f.c)
true

julia> iszero(f.a)
true

julia> f.a = 1
1

julia> f.a
true
```

One limitation of allowing fields to be set is that the object is declared as `mutable`, which may cause allocations
and results in the object no longer being `isbits`. This may change in the future.

## Unnamed fields

Both `@bitflags` and `@bitfield` also support unnamed, explicit padding bits:

```julia
julia> @bitfield struct Baz
           a:1
           _:7
           b:2
           _:16
           c:1
       end

julia> sizeof(Baz)
4

julia> names = propertynames(Baz(1,2,3))
(:a, :b, :c)

julia> FieldFlags.propertyoffset.(Baz, names)
(0, 8, 26)
```