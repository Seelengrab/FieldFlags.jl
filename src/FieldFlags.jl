module FieldFlags

export @flaggify, @bitfieldify, propertyoffset, propertysize

"""
  propertyoffset(x, s::Symbol)

Gives the offset (in bits) of the property `s` in the object `x`. Similar to `fieldoffset`.
"""
function propertyoffset end

"""
   propertysyize(x, s::Symbol)

Gives the size (in bits) the property `s` takes up in the object `x`. Similar to `fieldsize`.
"""
function propertysize end

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

# TODO: There is a lot of overlap between `flaggify` and `bitfieldify`, so consider merging these later

function bitfieldify(expr::Expr)
    expr.head == :struct || throw(ArgumentError("`@bitfieldify` needs a struct definition!"))
    typename = expr.args[2]
    typename_internal = Symbol(typename, :_fields)
    T = esc(typename)
    Ti = esc(typename_internal)
    fields = Pair{Symbol, Int}[]
    numbits = 0 # aggregate number of bits of all fields
    for ex in expr.args[3].args
        !(ex isa Expr) && continue
        (ex.head == :call
            && length(ex.args) == 3
            && first(ex.args) === :(:)) || continue # only Intx bitfields supported right now
        fieldname = ex.args[2]
        fieldsize = ex.args[3]
        numbits += fieldsize
        push!(fields, fieldname => fieldsize)
    end

    fieldtuple = ntuple(x -> first(fields[x]), length(fields))
    isempty(fieldtuple) && throw(ArgumentError("`@bitfieldify` needs at least one field."))
    # TODO: Give a better error here, showing which fields are duplicated
    allunique(fieldtuple) || throw(ArgumentError("Fields need to be uniquely identifiable!"))

    nfields = length(fields)
    # `primitive type` currently requires a multiple of 8
    # also makes accessing the bits later easier
    # don't want to oversize when we have exactly 8,16,.. fields
    typesize = 8*div(nfields, 8, RoundUp)

    # This primitive type is intentionally not an Integer
    # It's a composite type, there is no arithmetic here
    # Also helps the compiler/LLVM later on to not mix up types
    # The primitive type is explicitly wrapped in a `mutable struct`
    # which ends up providing the `setindex!` interface
    newob = Expr(:new, T, Expr(:call, :cast_or_extend, Ti, 0x0))
    typedefs = :(
        primitive type $typename_internal $typesize end;
        mutable struct $T
            fields::$Ti
            $T() = $newob
        end
    )

    # make the properties accessible
    typefuncs = :(
        Base.propertynames(_::$T) = $fieldtuple
    )

    # prepare our `getproperty` overload
    propsize = :(
        function FieldFlags.propertysize(x::$T, s::Symbol)
            s ∈ propertynames(x) || throw(ArgumentError("Objects of type `$($T)` have no field `$s`"))
            data = getfield(x, :fields)
        end
    )
    propoffset = :(
        function FieldFlags.propertyoffset(x::$T, s::Symbol)
            s ∈ propertynames(x) || throw(ArgumentError("Objects of type `$($T)` have no field `$s`"))
            data = getfield(x, :fields)
        end
    )
    getprop = :(
        function Base.getproperty(x::$T, s::Symbol)
            s ∈ propertynames(x) || throw(ArgumentError("Objects of type `$($T)` have no field `$s`"))
            data = getfield(x, :fields)
            maskbase = Core.Intrinsics.not_int(cast_or_extend($Ti, 0x0))
        end
    )
    setprop = :(
        function Base.setproperty!(x::$T, s::Symbol, v::W) where W
            s ∈ propertynames(x) || throw(ArgumentError("Objects of type `$($T)` have no field `$s`"))
            maskbase = Core.Intrinsics.not_int(cast_or_extend($Ti, 0x0))
            maskeddata = v & ~(~zero(W) << propertysize(x, s))
            val = cast_extend_truncate($Ti, maskeddata)
        end
    )

    # build constructor together with `getproperty`
    callargs = Any[T]
    bodyargs = Any[]
    # initialize return value of constructor
    push!(bodyargs, :(ret = cast_or_extend($Ti, 0x0)))
    running_offset = 0
    for (fieldname,fieldsize) in fields
        sizeexpr = :(
            if s === $(QuoteNode(fieldname))
                return $fieldsize
            end
        )
        offsetexpr = :(
            if s === $(QuoteNode(fieldname))
                return $running_offset
            end
        )
        getpropexpr = :(
            if s === $(QuoteNode(fieldname))
                offsetshift = cast_extend_truncate($Ti, propertyoffset(x, s))
                shifted = Core.Intrinsics.lshr_int(data, offsetshift)
                maskshift = cast_extend_truncate($Ti, propertysize(x, s))
                mask = Core.Intrinsics.not_int(Core.Intrinsics.shl_int(maskbase, maskshift))
                masked = Core.Intrinsics.and_int(shifted, mask)
                return cast_extend_truncate(UInt, masked)
            end
        )
        setpropexpr = :(
            if s === $(QuoteNode(fieldname))
                offsetshift = cast_extend_truncate($Ti, propertyoffset(x, s))
                shifted = Core.Intrinsics.shl_int(val, offsetshift)
                mask = Core.Intrinsics.not_int(Core.Intrinsics.shl_int(maskbase, propertysize(x, s)))
                mask = Core.Intrinsics.not_int(Core.Intrinsics.shl_int(mask, propertyoffset(x, s)))
                cleareddata = Core.Intrinsics.and_int(getfield(x, :fields), mask)
                newdata = Core.Intrinsics.or_int(cleareddata, shifted)
                setfield!(x, :fields, newdata)
                return maskeddata
            end
        )
        
        # constructor args
        push!(callargs, Expr(:(::), fieldname, Base.BitInteger))
        cast_f = Symbol(fieldname, :_cast)
        shift_f = Symbol(fieldname, :_shift)
        body = :(
            # shift argument into the correct field position
            # TODO: build second constructor
            $fieldname = 
            $cast_f = cast_or_extend($T, $fieldname);
            $shift_f = cast_extend_truncate($T, propertyoffset(x, s));
            $cast_f = Core.Intrinsics.shl_int($cast_f, $shift_f);
            # `or` it into the result
            ret = Core.Intrinsics.or_int(ret, $cast_f)
        )
        push!(bodyargs, body)

        running_offset += fieldsize
        push!(propsize.args[2].args, sizeexpr)
        push!(propoffset.args[2].args, offsetexpr)
        push!(getprop.args[2].args, getpropexpr)
        push!(setprop.args[2].args, setpropexpr)
    end
    mutstruct = last(typedefs.args)
    push!(bodyargs, :(return new(ret)))
    push!(getprop.args[2].args, :(return zero(UInt))) # never hit, only for type stability
    push!(setprop.args[2].args, :(return zero(W))) # never hit, only for type stability
    call = Expr(:call, callargs...)
    block = Expr(:block, bodyargs...)
    constr = Expr(:function, call, block)
    push!(mutstruct.args, constr)

    ###

    return :(
        $typedefs;
        $typefuncs;
        $propsize;
        $propoffset;
        $getprop;
        $setprop;
    )
end

macro bitfieldify(expr::Expr)
    bitfieldify(expr)
end

#####
# Flagstructs
#####

function flaggify(expr::Expr)
    expr.head == :struct || throw(ArgumentError("`@flaggify` needs a struct definition!"))
    typename = expr.args[2]
    T = esc(typename)
    fields = identity.(filter(s -> s isa Symbol, expr.args[3].args))
    isempty(fields) && throw(ArgumentError("`@flaggify` needs at least one field."))
    # TODO: Give a better error here, showing which fields are duplicated
    allunique(fields) || throw(ArgumentError("Fields need to be uniquely identifiable!"))
    nfields = length(fields)
    # `primitive type` currently requires a multiple of 8
    # also makes accessing the bits later easier
    # don't want to oversize when we have exactly 8,16,.. fields
    typesize  = div(nfields, 8, RoundUp) * 8
    typedef = :(
        primitive type $typename $typesize end
    )

    # make the properties accessible
    fieldtuple = ntuple(x -> fields[x], length(fields))
    typefuncs = :(
        Base.propertynames(_::$T) = $fieldtuple
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
        push!(callargs, Expr(:(::), f, Bool))
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
    push!(funcblock, :(return false)) # this is never hit, but makes the above typestable
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
