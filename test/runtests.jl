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
    fields = pos_fields[1:nfields]
    name = Symbol("struct_" * randstring(5) * string(nfields))
    :(
        @bitflags struct $name
            $(fields...)
        end
    ) |> eval

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
@testset for nfields in (7,8,9)
    fields = pos_fields[1:nfields]
    name = Symbol("struct_" * randstring(5) * string(nfields))
    :(
        @bitflags struct $name
            $(fields...)
        end
    ) |> eval

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
    end
end
@testset "Empty fields" begin
    :(
        @bitfield struct EmptyBitFields
            a:1
            b:2
            _:1
            c:2
            _:4
            d:1
            _:3
            _:2
            e:1
        end
    ) |> eval
    @test sizeof(EmptyBitFields) == 4
    args = (rand(Bool, 5)...,)
    obj = EmptyBitFields(args...)
    fields = (:a,:b,:c,:d,:e)
    @test propertynames(obj) == fields
    @test !hasproperty(obj, :_)
    offsets = (0,1,4,10,16)
    @testset for f in 1:5
        @test hasproperty(obj, fields[f])
        if isone(FieldFlags.fieldsize(fieldtype(EmptyBitFields, :fields), fields[f])) 
            @test getproperty(obj, fields[f]) isa Bool
        end
        @test FieldFlags.propertyoffset(fieldtype(EmptyBitFields, :fields), fields[f]) == offsets[f]
        @test getproperty(obj, fields[f]) == args[f]
    end
end
end # end @bitefields
end # end All Tests