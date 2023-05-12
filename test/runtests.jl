using Test
using FieldFlags
using Random

#=
Every struct created by FieldFlags is a multiple of 8 bit, so (for now) we're only going
to test 7, 8 and 9 fields, to catch obvious off-by-one errors. That should probably
cover multiples thereof as well, which (hopefully) is sufficient for now.
TODO: Use PropCheck.jl to generate structs for testing instead, once PropCheck.jl has a reliable interface
=#

const pos_fields = (Symbol.('a':('a'+9))...,)

@testset "All Tests" begin
@testset "@bitflags" begin
@testset for nfields in (7,8,9)
@testset "mutable: $mut" for mut in (true, false)
    fields = pos_fields[1:nfields]
    name = Symbol("struct_" * randstring(5) * string(nfields) * "_$mut")
    if mut
        :(
            @bitflags mutable struct $name
                $(fields...)
            end
        ) |> eval
    else
        :(
            @bitflags struct $name
                $(fields...)
            end
        ) |> eval
    end

    args = rand(Bool, nfields)
    obj = eval(:($name($(args...))))
    @test sizeof(obj) == ceil(Int, nfields/8)
    # these two should always pass/fail together
    @test !hasproperty(obj, :dummy)
    @test_throws ArgumentError("Objects of type `$name` have no field `dummy`") getproperty(obj, :dummy)

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
@testset for nfields in (7,8,9)
    fields = [ :($p:$n) for (n,p) in zip(shuffle(rand(2:4, nfields)), pos_fields[1:nfields]) ]
    name = Symbol("struct_" * randstring(5) * string(nfields) * '_' * string(mut))
    str = if mut
        :(@bitfield mutable struct $name
              $(fields...)
          end)
    else
        :(@bitfield struct $name
              $(fields...)
          end)
    end
    eval(str)
    args = rand(Bool, nfields)
    T = eval(:($name))
    obj = T(args...)
    sumfields = sum(x -> x.args[3], fields)
    @test sizeof(getfield(obj, :fields)) == div(sumfields, 8, RoundUp)
    # these two should always pass/fail together
    @test !hasproperty(obj, :dummy)
    @test_throws ArgumentError("Objects of type `$name` have no field `dummy`") getproperty(obj, :dummy)

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
end # end @bitfields
end # end All Tests