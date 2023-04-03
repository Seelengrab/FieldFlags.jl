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
@testset for nfields in (7,8,9)
    fields = pos_fields[1:nfields]
    name = Symbol("struct_" * randstring(5) * string(nfields))
    :(
        @flaggify struct $name
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
end