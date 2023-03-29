# FlagFields.jl

This package provides a tiny macro to create bitfield-like objects. For example:

```julia
@flaggify struct MyFlags
    flag_a
    flag_b
    flag_c
end
```

creates a new type `MyFlags`, which stores the boolean flags `flag_a`, `flag_b` and `flag_c` as its fields
in a compressed format. Due to compiler limitations, `MyFlags` is 1 byte large instead of 3 bits. All
structs created by `@flaggify` are a multiple of 8 bits large.

This may change in the future, but apart from that, the sky is the (literal) limit!

The above object can be accessed like any other struct:

```julia
julia> using FieldFlags

julia> @flaggify struct Foo
           a
           b
           c
       end

julia> methods(Foo)
# 1 method for type constructor:
 [1] Foo(a::Bool, b::Bool, c::Bool)
     @ none:0

julia> f = Foo(true, false, true)
Foo(0x05)

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
julia> @flaggify struct Foo
       end
ERROR: LoadError: ArgumentError: `@flaggify` needs at least one field.
Stacktrace:
 [1] flaggify(expr::Expr)
   @ FieldFlags ~/Documents/projects/FieldFlags.jl/src/FieldFlags.jl:34
 [2] var"@flaggify"(__source__::LineNumberNode, __module__::Module, expr::Any)
   @ FieldFlags ~/Documents/projects/FieldFlags.jl/src/FieldFlags.jl:114
in expression starting at REPL[2]:1

julia> @flaggify struct Foo2
           a
           a
       end
ERROR: LoadError: ArgumentError: Fields need to be uniquely identifiable!
Stacktrace:
 [1] flaggify(expr::Expr)
   @ FieldFlags ~/Documents/projects/FieldFlags.jl/src/FieldFlags.jl:36
 [2] var"@flaggify"(__source__::LineNumberNode, __module__::Module, expr::Any)
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
julia> @flaggify struct Foo3
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
Foo3(0x1a1f65)

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