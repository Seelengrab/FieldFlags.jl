module FieldFlags

export @bitflags, @bitfield

"""
    propertyoffset(::Type{T}, s::Symbol) -> Int

Gives the offset (in bits) the field `s` is placed at in objects of type `T`.

See also [`FieldFlags.fieldsize`](@ref).

```jldoctest
julia> @bitflags mutable struct MyFlags
           flagA
           _ # padding
           flagB
       end

julia> FieldFlags.propertyoffset(MyFlags, :flagA)
0

julia> FieldFlags.propertyoffset(MyFlags, :flagB)
2
```
"""
function propertyoffset end

"""
    fieldsize(::Type{T}, s::Symbol) -> Int

Gives the size (in bits) the field `s` takes up in objects of type `T`.

See also [`FieldFlags.propertyoffset`](@ref).

```jldoctest
julia> @bitfield mutable struct MyBits
           a:2
           _ # padding
           b:3
       end

julia> FieldFlags.fieldsize(MyBits, :a)
2

julia> FieldFlags.fieldsize(MyBits, :b)
3
```
"""
function fieldsize end

"""
    cast_or_extend(T::DataType, x) -> T

Takes an object `x` of a primitive type and either bitcasts it to `T`
(if their sizes are egal) or zero-extends the bitrepresentation of `x`
to the size of `T`. `sizeof(x) <= sizeof(T)` must hold.

Returns a `T`.

See also [`FieldFlags.cast_extend_truncate`](@ref).
"""
function cast_or_extend(T::DataType, x)
    if sizeof(T) === sizeof(x)
        Core.Intrinsics.bitcast(T, x)
    else # can only be larger - if sizeof(T) is zero, we threw
        Core.Intrinsics.zext_int(T, x)
    end
end

"""
    cast_extend_truncate(T::DataType, x) -> T

Takes an object `x` of a primitive type and either bitcasts it to type `T`
(if their sizes are egal), zero extends the bitrepresentation of `x` to the
size of `T`, or truncates the bitrepresentation of `x` to `sizeof(T)`.

Returns a `T`.

See also [`FieldFlags.cast_or_extend`](@ref).
"""
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

"""
    bitfield(expr::Expr)

Takes an `Expr(:struct)` of the form

    struct MyStruct
        a:x
        b:y
        _
        _:z
    end

where `a`, `b` are potential field names, `x`, `y`, and `z` are desired bitwidths
for those fields, `_` is padding and returns the following expression:

    quote
        \$typedefs;
        \$typefuncs;
        \$conv;
        \$propsize;
        \$propoffset;
        \$getprop;
        \$setprop;
    end

Where `typedefs` are the new user-facing type definition and the internal type definitions,
`typefuncs` are type related functions from Base for the new types, `conv` are `convert` methods to those
types, `propsize` is the implementation for [`FieldFlags.fieldsize`](@ref), `propoffset` is the
implementation for [`FieldFlags.propertyoffset`](@ref), `getprop` is the definition for the
`getproperty` overload for the user facing type and `setprop` is the definition for the
`setproperty!` overloda for the user facing type.

See also [`FieldFlags.bitflags`](@ref).
"""
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
    # build constructor together with `getproperty`
    callargs = Any[T]
    bodyargs = Any[]
    # initialize return value of constructor
    push!(bodyargs, :(ret = cast_or_extend($Ti, 0x0)))
    running_offset = 0
    sizeexpr = origsize = Expr(:if)
    offsetexpr = origoffset = Expr(:if)
    getpropexpr = origgetprop = Expr(:if)
    setpropexpr = origsetprop = Expr(:if)
    for (fieldname,fieldsize) in fields
        # TODO: Invent some way to get an integer type of the correct bitsize without `@eval`, like Zigs' iX
        casttype = isone(fieldsize) ? Bool : UInt

        push!(sizeexpr.args, :(s === $(QuoteNode(fieldname))))
        push!(sizeexpr.args, :(return $fieldsize))
        nsize = Expr(:elseif)
        push!(sizeexpr.args, nsize)
        sizeexpr = nsize

        push!(offsetexpr.args, :(s === $(QuoteNode(fieldname))))
        push!(offsetexpr.args, :(return $running_offset))
        noffexpr = Expr(:elseif)
        push!(offsetexpr.args, noffexpr)
        offsetexpr = noffexpr

        running_offset += fieldsize
        fieldname === :_ && continue

        push!(getpropexpr.args, :(s === $(QuoteNode(fieldname))))
        ifbody = :(
            offsetshift = cast_extend_truncate($Ti, propertyoffset($T, s));
            shifted = Core.Intrinsics.lshr_int(data, offsetshift);
            maskshift = cast_extend_truncate($Ti, fieldsize($T, s));
            mask = Core.Intrinsics.not_int(Core.Intrinsics.shl_int(maskbase, maskshift));
            masked = Core.Intrinsics.and_int(shifted, mask);
            return cast_extend_truncate($casttype, masked);
        )
        push!(getpropexpr.args, ifbody)
        ngetprop = Expr(:elseif)
        push!(getpropexpr.args, ngetprop)
        getpropexpr = ngetprop

        # only build the expression if we actually need to
        if mutable
            push!(setpropexpr.args, :(s === $(QuoteNode(fieldname))))
            ifbody = :(
                offsetshift = cast_extend_truncate($Ti, propertyoffset($T, s));
                shifted = Core.Intrinsics.shl_int(val, offsetshift);
                mask = Core.Intrinsics.not_int(Core.Intrinsics.shl_int(maskbase, fieldsize($T, s)));
                mask = Core.Intrinsics.not_int(Core.Intrinsics.shl_int(mask, propertyoffset($T, s)));
                cleareddata = Core.Intrinsics.and_int(getfield(x, :fields), mask);
                newdata = Core.Intrinsics.or_int(cleareddata, shifted);
                setfield!(x, :fields, newdata);
                return maskeddata;
            )
            push!(setpropexpr.args, ifbody)
            nsetprop = Expr(:elseif)
            push!(setpropexpr.args, nsetprop)
            setpropexpr = nsetprop
        end

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
    end
    push!(bodyargs, Expr(:return, Expr(:call, :new, :ret)))

    nosuchfieldstr = "Objects of type `$typename` have no field `"
    fielderrstr = "type $typename has no field fields"
    errexpr = :(ArgumentError(LazyString($nosuchfieldstr, s, "`")))

    if !(:fields in fieldtuple)
        # we can just branch & error here, to disallow
        # property access to the internal field
        # this will unfortunately cause a false positive in report_package
        push!(getpropexpr.args, :(s === :fields))
        push!(getpropexpr.args, :(error($fielderrstr)))
        push!(getpropexpr.args, :(getfield(x, s)))

        push!(setpropexpr.args, :(s === :fields))
        push!(setpropexpr.args, :(error($fielderrstr)))
        # this will unfortunately cause a false positive in report_package
        push!(setpropexpr.args, :(setfield!(x, v, s)))
    else
        # there is a user defined field :fields
        # so just change the last else block from
        # another elseif to a call to getfield,
        # which will produce an error that JET.jl
        # reports even without mode=:sound
        getpropexpr.head = :call
        push!(getpropexpr.args, :getfield, :x, :s)
        setpropexpr.head = :call
        push!(setpropexpr.args, :setfield!, :x, :v, :s)
    end

    sizeexpr.head = :call
    push!(sizeexpr.args, :throw, errexpr)
    offsetexpr.head = :call
    push!(offsetexpr.args, :throw, errexpr)

    call = Expr(:call, callargs...)
    block = Expr(:block, bodyargs...)
    constr = Expr(:function, call, block)
    push!(mutstruct.args[3].args, constr)

    propsize = :(
        function FieldFlags.fieldsize(_::Type{$T}, s::Symbol)
            $origsize
        end
    )
    propoffset = :(
        function FieldFlags.propertyoffset(_::Type{$T}, s::Symbol)
            $origoffset
        end
    )
    getprop = :(
        function Base.getproperty(x::$T, s::Symbol)
            data = getfield(x, :fields)
            maskbase = Core.Intrinsics.not_int(cast_or_extend($Ti, 0x0))
            $origgetprop
        end
    )
    setprop = :(
        function Base.setproperty!(x::$T, s::Symbol, v::W) where W
            maskbase = Core.Intrinsics.not_int(cast_or_extend($Ti, 0x0))
            maskeddata = v & ~(~zero(W) << fieldsize($T, s))
            val = cast_extend_truncate($Ti, maskeddata)
            $origsetprop
        end
    )
    conv = :(
        function Base.convert(::Type{$T}, x::X) where X
            if !isprimitivetype(X)
                throw(ArgumentError(LazyString("Cannot convert objects of type ", X, " to objects of type ", $T,".")))
            else
                $T(cast_extend_truncate($Ti, x))
            end
        end
    )
    # TODO: should this mask out the actual field data?
    eqhash = :(
        Base.:(==)(x::$T, y::$T) = getfield(x, :fields) == getfield(y, :fields);
        Base.hash(x::$T, h::UInt) = hash(getfield(x, :fields), h)
    )
    shows = :(
        function Base.show(io::IO, x::$T)
            show(io, $T)
            write(io, '(')
            names = propertynames(x)
            for i in eachindex(names)
                show(io, getproperty(x, names[i]))
                i != lastindex(names) && write(io, ", ")
            end
            write(io, ')')
        end;
        function Base.show(io::IO, m::MIME"text/plain", x::$T)
            show(io, m, $T)
            write(io, '(')
            names = propertynames(x)
            for i in eachindex(names)
                prop = getproperty(x, names[i])
                write(io, names[i])
                write(io, ": ")
                truncshow(io, prop)
                i != lastindex(names) && write(io, ", ")
            end
            write(io, ')')
        end
    )

    ###

    return :(
        $typedefs;
        $typefuncs;
        $conv;
        $eqhash;
        $shows;
        $propsize;
        $propoffset;
        $getprop;
        $setprop;
    )
end

truncshow(io::IO, x) = show(io, MIME"text/plain"(), x)
function truncshow(io::IO, x::Unsigned)
    p = @view repr(x)[3:end]
    idx = something(findfirst(!=('0'), p), Some(lastindex(p)))
    write(io, "0x")
    _p = @view p[idx:end]
    write(io, _p)
    return nothing
end

"""
    @bitfield [mutable] struct MyBits
        a:2
        b:3
        _[:3] # padding; width is assumed 1 bit if the length is omitted
        c:1
    end

Construct a struct representing various fields, with their size specified in bits.
The struct can optionally be marked `mutable`.

See also [`@bitflags`](@ref).

# Extended Help

The fields are stored in a compact format where each field only takes up the specified number of bits.
Field access gives an unsigned integer, whose lower bits are the bits of the accessed field. The upper
bits are zeroed. As a special case, fields with size `1` return a `Bool`.
Explicit padding can be specified by naming a field `_`, with freely chosen width.
Field names (other than padding) need to be unique. The specified number of bits must be `>= 0`.

The order the fields are given in is the order the fields are stored in. The first field occupies
the least significant bits, followed by the second field, up to the last field, which is stored in
the most significant bits.

For example, the struct given above has this layout:

|MSB |    |    |    |    |    |    |    |LSB |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
|c   |_   |_   |_   |b   |b   |b   |a   |a   |

where `_` is padding, with undefined value.

The constructor created for structs defined with `@bitfield` takes any type in
`Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8, Bool}` and converts it
to the correct size by truncating the upper bits, before storing the truncated value in the object.
This truncation also occurs when writing to a field of a mutable object.

!!! warning "Struct size"
    Due to compiler limitations, the size of the resulting object will (currently) always be a
    multiple of 8 bits. The additional bits added due to this are considered padding and can
    not be relied on to exist. They may be removed in a future release without notice.
    If you need padding up to a given size, explicitly specify a trailing padding field.

!!! warning "Field type"
    As there are no variable sized integers in Julia, it is only guaranteed that the return type
    on field access is large enough to hold all bits required by that field. While currently
    field sizes larger than `1` return an `UInt`, this is in particular not guaranteed and
    may be changed in the future, so that e.g. a field of size `2` returns an `UInt8` instead.

# Examples

```jldoctest
julia> @bitfield struct MyBits
           a:2
           b:3
           _:3 # padding
           c:1
       end

julia> bits = MyBits(1,2,3)
MyBits(a: 0x1, b: 0x2, c: true)

julia> bits.a
0x0000000000000001

julia> bits.b
0x0000000000000002

julia> bits.c
true
```
"""
macro bitfield(expr::Expr)
    bitfield(expr)
end

#####
# Flagstructs
#####

"""
    bitflags(::Expr)

The given `Expr(:struct) has the following format

    struct MyFlags
        a
        b
        _
    end

which is turned into

    struct MyFlags
        a:1
        b:1
        _:1
    end

before being passed to `FieldFlags.bitfield`.

Some minimal expression filtering is performed.

See also [`@bitflags`](@ref), [`@bitfield`](@ref).
"""
function bitflags(expr::Expr)
    expr.head == :struct || throw(ArgumentError("`@bitflags` needs a struct definition!"))
    exprfields = expr.args[3].args
    fields = identity.(filter(s -> s isa Symbol, exprfields))
    isempty(fields) && throw(ArgumentError("`@bitflags` needs at least one field."))
    # TODO: Give a better error here, showing which fields are duplicated
    allunique(filter(!=(:_), fields)) || throw(ArgumentError("Fields need to be uniquely identifiable!"))
    # we do the heavy lifting in @bitfield, so that @bitflags is just an easier interface
    for n in eachindex(exprfields)
        arg = exprfields[n]
        arg isa Symbol || continue
        exprfields[n] = Expr(:call, :(:), arg, 1)
    end
    bitfield(expr)
end

"""
    @bitflags [mutable] struct MyFlags
        flagA
        flagB
        _ # padding
        flagC
    end

Construct a struct representing various boolean flags, stored in a compact format where each flag
takes up a single bit. Field access gives a `Bool`, explicit padding can be declared by naming a field
`_`. Field names (other than padding) need to be unique. The struct can optionally be marked `mutable`.

See also [`@bitfield`](@ref).

!!! warning "Struct size"
    Due to compiler limitations, the size of the resulting object will (currently) always be a
    multiple of 8 bits. The additional bits added due to this are considered padding and can
    not be relied on to exist. They may be removed in a future release without notice.

# Examples

```jldoctest
julia> @bitflags mutable struct MyFlags
           flagA
           flagB
           _ # padding
           flagC
       end

julia> flags = MyFlags(true, false, true)
MyFlags(flagA: true, flagB: false, flagC: true)

julia> flags.flagA
true

julia> flags.flagB
false

julia> flags.flagB = true
true

julia> flags.flagB
true

julia> sizeof(flags)
1
```
"""
macro bitflags(expr)
    bitflags(expr)
end

end # module FieldFlags
