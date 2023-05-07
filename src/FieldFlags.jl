module FieldFlags

export @bitflags, @bitfield

"""
   propertyoffset(::Type{T}, s::Symbol)

Gives the offset (in bits) the field `s` is placed at in objects of type `T`.
"""
function propertyoffset end

"""
   fieldsize(::Type{T}, s::Symbol)

Gives the size (in bits) the field `s` takes up in objects of type `T`.
"""
function fieldsize end

function cast_or_extend(T::DataType, x)
    if sizeof(T) === sizeof(x)
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
        # we can't do anything other than truncating here
        # we need at least sizeof(T) bits in x to represent
        # all shifts/offsets we can think of - larger stuff
        # is swallowed
        Core.Intrinsics.trunc_int(T, x) 
    else # ==
        # no extension/truncation needed, just bitcast
        Core.Intrinsics.bitcast(T, x)
    end
end

# TODO: There is a lot of overlap between `bitflags` and `bitfields`, so consider merging these later

function bitfield(expr::Expr)
    expr.head == :struct || throw(ArgumentError("`@bitfields` needs a struct definition!"))
    mutable = expr.args[1]
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
    isempty(fieldtuple) && throw(ArgumentError("`@bitfields` needs at least one field."))
    # TODO: Give a better error here, showing which fields are duplicated
    allunique(filter(!=(:_), fieldtuple)) || throw(ArgumentError("Fields need to be uniquely identifiable!"))

    # `primitive type` currently requires a multiple of 8
    # also makes accessing the bits later easier
    # don't want to oversize when we have exactly 8,16,.. fields
    typesize = 8*div(numbits, 8, RoundUp)

    # This primitive type is intentionally not an Integer
    # It's a composite type, there is no arithmetic here
    # Also helps the compiler/LLVM later on to not mix up types
    # The primitive type is explicitly wrapped in a `mutable struct`
    # which ends up providing the `setindex!` interface, if the
    # requested struct is declared `mutable` as well
    newob = Expr(:new, T, Expr(:call, :cast_or_extend, Ti, 0x0))
    mutstruct = if mutable
        :(mutable struct $T
            fields::$Ti
            $T(t::$Ti) = new(t)
            $T() = $newob
        end)
    else
        :(struct $T
            fields::$Ti
            $T(t::$Ti) = new(t)
            $T() = $newob
        end)
    end
    typedefs = :(
        primitive type $typename_internal $typesize end;
        $mutstruct
    )

    # make the properties accessible
    filterednames = filter(!=(:_), fieldtuple)
    typefuncs = :(
        Base.propertynames(::$T) = $filterednames
    )

    # prepare our `getproperty` overload
    propsize = :(
        function FieldFlags.fieldsize(_::Type{$T}, s::Symbol)
            s ∈ $filterednames || throw(ArgumentError("Objects of type `$($T)` have no field `$s`"))
        end
    )
    propoffset = :(
        function FieldFlags.propertyoffset(_::Type{$T}, s::Symbol)
            s ∈ $filterednames || throw(ArgumentError("Objects of type `$($T)` have no field `$s`"))
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
            maskeddata = v & ~(~zero(W) << fieldsize($T, s))
            val = cast_extend_truncate($Ti, maskeddata)
        end
    )
    conv = :(
        function Base.convert(::Type{$T}, x::X) where X
            isprimitivetype(X) || throw(ArgumentError("Cannot convert objects of type $X to objects of type $($T)."))
            $T(cast_extend_truncate($Ti, x))
        end
    )

    # build constructor together with `getproperty`
    callargs = Any[T]
    bodyargs = Any[]
    # initialize return value of constructor
    push!(bodyargs, :(ret = cast_or_extend($Ti, 0x0)))
    running_offset = 0
    for (fieldname,fieldsize) in fields
        # TODO: Invent some way to get an integer type of the correct bitsize without `@eval`, like Zigs' iX
        casttype = isone(fieldsize) ? Bool : UInt
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
        running_offset += fieldsize
        fieldname === :_ && continue

        getpropexpr = :(
            if s === $(QuoteNode(fieldname))
                offsetshift = cast_extend_truncate($Ti, propertyoffset($T, s))
                shifted = Core.Intrinsics.lshr_int(data, offsetshift)
                maskshift = cast_extend_truncate($Ti, fieldsize($T, s))
                mask = Core.Intrinsics.not_int(Core.Intrinsics.shl_int(maskbase, maskshift))
                masked = Core.Intrinsics.and_int(shifted, mask)
                return cast_extend_truncate($casttype, masked)
            end
        )
        setpropexpr = :(
            if s === $(QuoteNode(fieldname))
                offsetshift = cast_extend_truncate($Ti, propertyoffset($T, s))
                shifted = Core.Intrinsics.shl_int(val, offsetshift)
                mask = Core.Intrinsics.not_int(Core.Intrinsics.shl_int(maskbase, fieldsize($T, s)))
                mask = Core.Intrinsics.not_int(Core.Intrinsics.shl_int(mask, propertyoffset($T, s)))
                cleareddata = Core.Intrinsics.and_int(getfield(x, :fields), mask)
                newdata = Core.Intrinsics.or_int(cleareddata, shifted)
                setfield!(x, :fields, newdata)
                return maskeddata
            end
        )
        
        # constructor args
        push!(callargs, Expr(:(::), fieldname, Union{Bool, Base.BitInteger}))
        cast_f = Symbol(fieldname, :_cast)
        shift_f = Symbol(fieldname, :_shift)
        mask_f = Symbol(fieldname, :_mask)
        body = :(
            # shift argument into the correct field position
            $mask_f = $fieldname & ~((~zero($fieldname)) << fieldsize($T, $(QuoteNode(fieldname))));
            $cast_f = cast_extend_truncate($Ti, $mask_f);
            $shift_f = cast_extend_truncate($Ti, propertyoffset($T, $(QuoteNode(fieldname))));
            $cast_f = Core.Intrinsics.shl_int($cast_f, $shift_f);
            # `or` it into the result
            ret = Core.Intrinsics.or_int(ret, $cast_f)
        )
        push!(bodyargs, body)

        push!(propsize.args[2].args, sizeexpr)
        push!(propoffset.args[2].args, offsetexpr)
        push!(getprop.args[2].args, getpropexpr)
        # only build the setproperty! expression when we actually need it
        mutable && push!(setprop.args[2].args, setpropexpr)
    end
    push!(bodyargs, Expr(:return, Expr(:call, :new, :ret)))
    push!(getprop.args[2].args, :(return zero(UInt))) # never hit, only for type stability
    if mutable
        push!(setprop.args[2].args, :(return zero(W))) # never hit, only for type stability
    else
        # the struct we're building is immutable anyway
        push!(setprop.args[2].args, :(error("setfield!: immutable struct of type $($T) cannot be changed")))
    end
    call = Expr(:call, callargs...)
    block = Expr(:block, bodyargs...)
    constr = Expr(:function, call, block)
    push!(mutstruct.args[3].args, constr)

    ###

    return :(
        $typedefs;
        $typefuncs;
        $conv;
        $propsize;
        $propoffset;
        $getprop;
        $setprop;
    )
end

macro bitfield(expr::Expr)
    bitfield(expr)
end

#####
# Flagstructs
#####

function bitflags(expr::Expr)
    expr.head == :struct || throw(ArgumentError("`@bitflags` needs a struct definition!"))
    typename = expr.args[2]
    T = esc(typename)
    fields = identity.(filter(s -> s isa Symbol, expr.args[3].args))
    isempty(fields) && throw(ArgumentError("`@bitflags` needs at least one field."))
    # TODO: Give a better error here, showing which fields are duplicated
    allunique(filter(!=(:_), fields)) || throw(ArgumentError("Fields need to be uniquely identifiable!"))
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
    filterednames = filter(!=(:_), fieldtuple)
    typefuncs = :(
        Base.propertynames(_::$T) = $filterednames
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
        f === :_ && continue
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

macro bitflags(expr)
    bitflags(expr)
end

end # module FieldFlags
