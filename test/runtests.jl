using Test
using FieldFlags
using JET
using Random

const pos_fields = (Symbol.('a':('a'+9))...,)

function test_trunc_show(io, val)
    mark(io)
    FieldFlags.truncshow(io, val)
    reset(io)
    read(io, String)
end

@testset "All Tests" begin
@testset "truncshow" begin
    io = IOBuffer()
    @test test_trunc_show(io, 0x0)    == "0x0"
    @test test_trunc_show(io, 0x1)    == "0x1"
    @test test_trunc_show(io, 0x01)   == "0x1"
    @test test_trunc_show(io, 0x0101) == "0x101"
end

@testset "show" for nfields in (7,8,9)
    fields = pos_fields[1:nfields]
    name = Symbol("struct_" * randstring(5) * string(nfields))
    :(
        @bitflags struct $name
            $(fields...)
        end
    ) |> eval
    args = rand(Bool, nfields)
    obj = eval(:($name($(args...))))
    @testset "2-arg show" begin
        @test eval(Meta.parse(repr(obj))) == obj
    end
    mime_repr = repr(MIME"text/plain"(), obj)
    @testset "text/plain" for f in eachindex(fields)
        teststr = string(isone(f) ? "" : ", ", fields[f], ':')
        @test occursin(teststr, mime_repr)
    end
end

@testset "Failing convert" begin
    struct IntWrap
        x::Int
    end
    @bitfield struct ConvertTest
        a:3
    end
    err = ArgumentError("Cannot convert objects of type IntWrap to objects of type ConvertTest.")
    @test_throws err convert(ConvertTest, IntWrap(1))
end

@testset "Identity convert in a wrapper struct #10" begin
    @bitfield struct Convert_10
        a:2
    end
    # the type parameter is unused, but necessary to induce the identity `convert` call
    struct Wrapper_10{T}
        c::Convert_10
    end
    obj = Convert_10()
    @test Wrapper_10{Int}(obj) isa Wrapper_10{Int}
end

@testset "Subtyping" begin
    abstract type AbstractSupertype end
    @bitfield struct Concrete <: AbstractSupertype
        a:2
    end
    f(as::AbstractSupertype) = iseven(as.a)
    @test f(Concrete(2))
end

abstract type TestAbstract end

@testset "@bitflags" begin
@testset for nfields in (7,8,9)
@testset for sup in (true, false)
@testset "mutable: $mut" for mut in (true, false)
    fields = pos_fields[1:nfields]
    name = Symbol("struct_" * randstring(5) * string(nfields) * "_$(mut)_$sup")
    if sup
        supexpr = :($name <: TestAbstract)
        structexpr = Expr(:struct, mut, supexpr, Expr(:block, fields...))
    else
        structexpr = Expr(:struct, mut, name, Expr(:block, fields...))
    end
    eval(:(@bitflags $structexpr))
    T = eval(name)
    @test if sup
        supertype(T) === TestAbstract
    else
        supertype(T) === Any
    end

    args = rand(Bool, nfields)
    obj = T(args...)
    @test sizeof(obj) == ceil(Int, nfields/8)
    # these two should always pass/fail together
    @test !hasproperty(obj, :dummy)
    @test_throws ErrorException("type $name has no field dummy") getproperty(obj, :dummy)

    @test propertynames(obj) == fields
    @testset for f in 1:nfields
        # these two should always pass/fail together
        @test hasproperty(obj, fields[f])
        @test getproperty(obj, fields[f]) == args[f]
        if mut
            val = rand(Bool)
            setproperty!(obj, fields[f], val)
            @test getproperty(obj, fields[f]) == val
        end
    end
end
end
end # nfields
@testset "Empty bits" begin
    :(
        @bitflags struct EmptyFields
            a
            b
            _
            c
            _
            d
            _
            _
            e
        end
    ) |> eval
    @test sizeof(EmptyFields) == 2
    args = (rand(Bool, 5)...,)
    obj = EmptyFields(args...)
    fields = (:a,:b,:c,:d,:e)
    @test propertynames(obj) == fields
    @test !hasproperty(obj, :_)
    @testset for f in 1:5
        @test hasproperty(obj, fields[f])
        @test getproperty(obj, fields[f]) == args[f]
    end
end
end # end @bitflags

@testset "@bitfields" begin
@testset "non-pow-2 size" begin
    @bitfield struct NonPow2
        f:24
    end
    # This REALLY ought not to be how this works...
    if Sys.WORD_SIZE > 24
        @test 8*sizeof(NonPow2) == nextpow(2, 24)
    else
        @test 8*sizeof(NonPow2) == nextpow(Sys.WORD_SIZE, 24)
    end
end

@testset "mutable: $mut" for mut in (true, false)
@testset for sup in (true, false)
@testset for nfields in (7,8,9)
    fields = [ :($p:$n) for (n,p) in zip(shuffle(rand(2:4, nfields)), pos_fields[1:nfields]) ]
    name = Symbol("struct_" * randstring(5) * string(nfields) * '_' * string(mut)* '_' * string(sup))
    if sup
        supexpr = :($name <: TestAbstract)
        structexpr = Expr(:struct, mut, supexpr, Expr(:block, fields...))
    else
        structexpr = Expr(:struct, mut, name, Expr(:block, fields...))
    end
    eval(:(@bitfield $structexpr))
    args = rand(Bool, nfields)
    T = eval(:($name))
    @test if sup
        supertype(T) === TestAbstract
    else
        supertype(T) === Any
    end
    obj = T(args...)
    sumfields = sum(x -> x.args[3], fields)
    @test sizeof(getfield(obj, :fields)) == div(sumfields, 8, RoundUp)
    # these two should always pass/fail together
    @test !hasproperty(obj, :dummy)
    @test_throws ErrorException("type $name has no field dummy") getproperty(obj, :dummy)

    @test propertynames(obj) == ntuple(f -> fields[f].args[2], nfields)
    zeroobj = convert(T, 0)
    oneobj = convert(T, -1)
    @testset for f in 1:nfields
        # the empty convert preserves emptiness
        @test iszero(getproperty(zeroobj, fields[f].args[2]))
        # the full convert preserves fullness
        @test ndigits(getproperty(oneobj, fields[f].args[2]); base=2) === fields[f].args[3]

        # these two should always pass/fail together
        @test hasproperty(obj, fields[f].args[2])
        @test getproperty(obj, fields[f].args[2]) == args[f]
        rand_set = rand(Bool)
        if mut
            @test setproperty!(obj, fields[f].args[2], rand_set) == rand_set
            @test getproperty(obj, fields[f].args[2]) == rand_set
        else
            @test_throws ErrorException("setfield!: immutable struct of type $name cannot be changed") setproperty!(obj, fields[f].args[2], rand_set)
        end
    end
end # dense bitfields
end # supertype

@testset "Empty fields" begin
    name = Symbol("EmptyBitFields_" * string(mut))
    str = if mut
        :(
        @bitfield mutable struct $name
            a:1
            b:2
            _:1
            c:2
            _:4
            d:1
            _:3
            _:2
            e:1
        end)
    else
        :(@bitfield struct $name
            a:1
            b:2
            _:1
            c:2
            _:4
            d:1
            _:3
            _:2
            e:1
        end)
    end
    eval(str)
    args = (rand(Bool, 5)...,)
    obj = eval(:($name($(args...))))
    @test sizeof(typeof(obj)) == 4
    fields = (:a,:b,:c,:d,:e)
    @test propertynames(obj) == fields
    @test !hasproperty(obj, :_)
    offsets = (0,1,4,10,16)
    @testset for f in 1:5
        @test hasproperty(obj, fields[f])
        if isone(FieldFlags.fieldsize(typeof(obj), fields[f]))
            @test getproperty(obj, fields[f]) isa Bool
        end
        @test FieldFlags.propertyoffset(typeof(obj), fields[f]) == offsets[f]
        @test getproperty(obj, fields[f]) == args[f]
    end
end # empty fields
end # mutability

@testset "Implicit single padding bit" begin
    @bitfield struct ImplicitPaddingBitsize
        a:2
        _
        b:4
    end
    @test FieldFlags.propertyoffset(ImplicitPaddingBitsize, :a) == 0
    @test FieldFlags.propertyoffset(ImplicitPaddingBitsize, :b) == 2+1 # size of a + 1 from padding
end
end # end @bitfields

@testset "Propertyaccess to internal field" begin
    @testset "`fields` field has been specified by the user" begin
        @bitflags struct FieldsField
            _
            fields
        end
        args = rand(Bool)
        obj = FieldsField(args)
        @test hasfield(FieldsField, :fields)
        @test hasproperty(obj, :fields)
        @test obj.fields == args
        @test obj.fields isa Bool
        @test !(getfield(obj, :fields) isa Bool)
        # one gives the field, the other gives the internal object
        @test obj.fields != getfield(obj, :fields)
    end
    @testset "`fields` has NOT been specified by the user" begin
        @bitflags struct FooField
            _
            foo
        end
        args = rand(Bool)
        obj = FooField(args)
        @test hasfield(FooField, :fields)
        @test !(getfield(obj, :fields) isa Bool)
        @test !hasproperty(obj, :fields)
        @test_throws ErrorException("type FooField has no field fields") obj.fields
    end
end

@testset "Effects" begin
    @bitflags struct JETStruct
        a
        _
        b
    end
    @testset "foldableAccess" begin
        foldableAccess(j::JETStruct) = j.a
        @static if VERSION >= v"1.9"
            @test_call foldableAccess(JETStruct(false, true))
        else
            res = JET.report_call(foldableAccess, (JETStruct,))
            @test isempty(JET.get_reports(res))
        end
        effects = Base.infer_effects(foldableAccess, (JETStruct,))
        @test Core.Compiler.is_foldable(effects)
        @inferred Bool foldableAccess(JETStruct(true, false))
    end
    @testset "erroringAccess" begin
        erroringAccess(j::JETStruct) = j.z
        reps = JET.get_reports(report_call(erroringAccess, (JETStruct,)))
        @test !isempty(reps)
        @test reps[1].msg == "type JETStruct has no field z"
        effects = Base.infer_effects(erroringAccess, (JETStruct,))
        @test !Core.Compiler.is_nothrow(effects)
        rettypes = Base.return_types(erroringAccess, (JETStruct,))
        @test only(rettypes) == Union{}
    end
    @testset "effects of getproperty of :fields" begin
        erroringFields(j::FooField) = j.fields
        reps = JET.get_reports(report_call(erroringFields, (FooField,)))
        @test !isempty(reps)
        # there's gotta be a better way of testing that,
        # since this test will break if the internals change
        # @test reps[1].vst[2].sig._sig[3].val == "type FooField has no field fields"
        effects = Base.infer_effects(erroringFields, (FooField,))
        @test !Core.Compiler.is_nothrow(effects)
        rettypes = Base.return_types(erroringFields, (FooField,))
        @test only(rettypes) == Union{}

        # now what if we DO have a `fields` field?
        foldableFields(j::FieldsField) = j.fields
        @static if VERSION >=  v"1.9"
            @test_call foldableFields(FieldsField(true))
        else
            res = JET.report_call(foldableFields, (FieldsField,))
            @test isempty(JET.get_reports(res))
        end
        effects = Base.infer_effects(foldableFields, (FieldsField,))
        @test Core.Compiler.is_foldable(effects)
        @inferred Bool foldableFields(FieldsField(true))
    end
end

@testset "DSL constraints" begin
    @testset "Field names" begin
        expr = :(struct Foo
                (a,b):1
            end)
        @test_throws ArgumentError("Name of field is not a symbol: `(a, b)`") FieldFlags.bitfield(expr)
    end
    @testset "Non-literal field sizes" begin
        expr = :(struct Foo
                a:b
            end)
        @test_throws ArgumentError("Declared size of field `a` is not an integer literal!") FieldFlags.bitfield(expr)
        @testset "Allowed field size literals" for T in Base.BitInteger_types
            fieldsize = one(T)
            expr = :(struct Foo
                    a:$fieldsize
                end)
            @test FieldFlags.bitfield(expr) isa Expr
        end
    end
end
end # end All Tests