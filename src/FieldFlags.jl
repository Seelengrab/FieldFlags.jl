module FieldFlags

export @flaggify

function cast_or_extend(T::DataType, x::Union{UInt8, Bool})
    if sizeof(T) === 1
        Core.Intrinsics.bitcast(T, x)
    else # can only be larger - if sizeof(T) is zero, we threw
        Core.Intrinsics.zext_int(T, x)
    end
end

function cast_extend_truncate(T::DataType, x)
    if sizeof(x) < sizeof(T)
        # zero extension is always fine
        Core.Intrinsics.zext_int(T, x)
    elseif sizeof(x) > sizeof(T)
        # safe, due to x being limited to nfields
        # since one bit in T is equal to a field, extending x to that
        # length means it will always take more bits to have a value of that size
        # than to describe that value as a shift
        Core.Intrinsics.trunc_int(T, x) 
    else # ==
        # no extension/truncation needed, just bitcast
        Core.Intrinsics.bitcast(T, x)
    end
end

function flaggify(expr::Expr)
    (expr isa Expr && expr.head == :struct) || throw(ArgumentError("`@flaggify` needs a struct definition!"))
    typename = expr.args[2]
    T = esc(typename)
    fields = identity.(filter(s -> s isa Symbol, expr.args[3].args))
    isempty(fields) && throw(ArgumentError("`@flaggify` needs at least one field."))
    # TODO: Give a better error here, showing which fields are duplicated
    allunique(fields) || throw(ArgumentError("Fields need to be uniquely identifiable!"))
    nfields = length(fields)
    # `primitive type` currently requires a multiple of 8
    # also makes accessing the bits later easier
    d,r = divrem(nfields, 8)
    # don't want to oversize when we have exactly 8,16,.. fields
    nbytes = (d + (r != 0))
    typesize  = nbytes * 8
    typedef = :(
        primitive type $T $typesize end
    )

    # make the properties accessible
    fieldtuple = ntuple(x -> fields[x], length(fields))
    typefuncs = :(
        Base.propertynames(x::$T) = $fieldtuple
    )

    # prepare our `getproperty` overload
    getprop = :(
        function Base.getproperty(x::$T, s::Symbol)
            s ∈ propertynames(x) || throw(ArgumentError("Objects of type `$($T)` have no field `$s`"))
            maskbase = cast_or_extend($T, 0x1)
            zero = cast_or_extend($T, 0x0)
        end
    )

    # build constructor together with `getproperty`
    callargs = Any[T]
    bodyargs = Any[]
    # initialize return value of constructor
    push!(bodyargs, :(ret = cast_or_extend($T, 0x0)))
    # for every symbol of the original struct, generate an `if` 
    # checking for it. `push!` the expression to the `getproperty` expression
    # and generate the conversion in the constructor as well
    funcblock = getprop.args[2].args
    for (i,f) in enumerate(fields)
        shiftbase = i-1

        # name argument `f::Bool`
        push!(callargs, Expr(:(::), f, :Bool))
        cast_f = Symbol(f, :_cast)
        shift_f = Symbol(f, :_shift)
        body = :(
            # shift argument into the correct field position
            $cast_f = cast_or_extend($T, $f);
            $shift_f = cast_extend_truncate($T, $shiftbase);
            $cast_f = Core.Intrinsics.shl_int($cast_f, $shift_f);
            # `or` it into the result
            ret = Core.Intrinsics.or_int(ret, $cast_f)
        )
        push!(bodyargs, body)

        ifexpr = :(
            if s === $(QuoteNode(f)) # interpolate the literal symbol
                shift = cast_extend_truncate($T, $shiftbase)
                mask = Core.Intrinsics.shl_int(maskbase, shift)
                val = Core.Intrinsics.and_int(x, mask)
                return Core.Intrinsics.ne_int(val, zero)
            end
        )
        push!(funcblock, ifexpr)
    end
    push!(bodyargs, :(return ret))
    call = Expr(:call, callargs...)
    block = Expr(:block, bodyargs...)
    constr = Expr(:function, call, block)

    # FieldFlags, assemble!
    return :(
        $typedef;
        $constr;
        $typefuncs;
        $getprop
    )
end

macro flaggify(expr)
    flaggify(expr)
end

end # module FieldFlags
