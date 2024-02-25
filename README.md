# FieldFlags.jl

[![CI Stable](https://github.com/Seelengrab/FieldFlags.jl/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/Seelengrab/FieldFlags.jl/actions/workflows/ci.yml)
[![CI Nightly](https://github.com/Seelengrab/FieldFlags.jl/actions/workflows/nightly.yml/badge.svg?branch=main)](https://github.com/Seelengrab/FieldFlags.jl/actions/workflows/nightly.yml)
[![docs-stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://seelengrab.github.io/FieldFlags.jl/stable)
[![docs-dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://seelengrab.github.io/FieldFlags.jl/dev)
[![codecov](https://codecov.io/github/Seelengrab/FieldFlags.jl/branch/main/graph/badge.svg?token=PBH8NJCHKS)](https://codecov.io/github/Seelengrab/FieldFlags.jl)

FieldFlags.jl is a small package for declaring [bitfield](https://en.wikipedia.org/wiki/Bit_field)-like structs, without
having to manually mask out bits. For more information, check out the [documentation](https://seelengrab.github.io/FieldFlags.jl/)!

```julia-repl
julia> using FieldFlags

julia> @bitfield mutable struct Foo
           a:1
           b:2
           _:7
           c:3
       end

# The type only takes up 2 bytes
julia> sizeof(Foo)
2

julia> f = Foo(1,2,3)
Foo(a: true, b: 0x2, c: 0x3)

julia> f.a = 2
2

# Assignments truncate leading bits!
julia> f.a
false
```
