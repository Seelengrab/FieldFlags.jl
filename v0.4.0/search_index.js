var documenterSearchIndex = {"docs":
[{"location":"api/#API-Reference","page":"API Reference","title":"API Reference","text":"","category":"section"},{"location":"api/#Public-API","page":"API Reference","title":"Public API","text":"","category":"section"},{"location":"api/","page":"API Reference","title":"API Reference","text":"The following symbols are considered API for the purposes of semver.","category":"page"},{"location":"api/#Macros","page":"API Reference","title":"Macros","text":"","category":"section"},{"location":"api/","page":"API Reference","title":"API Reference","text":"@bitflags\n@bitfield","category":"page"},{"location":"api/#FieldFlags.@bitflags","page":"API Reference","title":"FieldFlags.@bitflags","text":"@bitflags [mutable] struct MyFlags\n    flagA\n    flagB\n    _ # padding\n    flagC\nend\n\nConstruct a struct representing various boolean flags, stored in a compact format where each flag takes up a single bit. Field access gives a Bool, explicit padding can be declared by naming a field _. Field names (other than padding) need to be unique. The struct can optionally be marked mutable.\n\nSee also @bitfield.\n\nwarning: Struct size\nDue to compiler limitations, the size of the resulting object will (currently) always be a multiple of 8 bits. The additional bits added due to this are considered padding and can not be relied on to exist. They may be removed in a future release without notice.\n\nExamples\n\njulia> @bitflags mutable struct MyFlags\n           flagA\n           flagB\n           _ # padding\n           flagC\n       end\n\njulia> flags = MyFlags(true, false, true)\nMyFlags(flagA: true, flagB: false, flagC: true)\n\njulia> flags.flagA\ntrue\n\njulia> flags.flagB\nfalse\n\njulia> flags.flagB = true\ntrue\n\njulia> flags.flagB\ntrue\n\njulia> sizeof(flags)\n1\n\n\n\n\n\n","category":"macro"},{"location":"api/#FieldFlags.@bitfield","page":"API Reference","title":"FieldFlags.@bitfield","text":"@bitfield [mutable] struct MyBits\n    a:2\n    b:3\n    _[:3] # padding; width is assumed 1 bit if the length is omitted\n    c:1\nend\n\nConstruct a struct representing various fields, with their size specified in bits. The struct can optionally be marked mutable.\n\nSee also @bitflags.\n\nExtended Help\n\nThe fields are stored in a compact format where each field only takes up the specified number of bits. Field access gives an unsigned integer, whose lower bits are the bits of the accessed field. The upper bits are zeroed. As a special case, fields with size 1 return a Bool. Explicit padding can be specified by naming a field _, with freely chosen width. Field names (other than padding) need to be unique. The specified number of bits must be >= 0.\n\nThe order the fields are given in is the order the fields are stored in. The first field occupies the least significant bits, followed by the second field, up to the last field, which is stored in the most significant bits.\n\nFor example, the struct given above has this layout:\n\nMSB        LSB\nc _ _ _ b b b a a\n\nwhere _ is padding, with undefined value.\n\nThe constructor created for structs defined with @bitfield takes any type in Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8, Bool} and converts it to the correct size by truncating the upper bits, before storing the truncated value in the object. This truncation also occurs when writing to a field of a mutable object.\n\nwarning: Struct size\nDue to compiler limitations, the size of the resulting object will (currently) always be a multiple of 8 bits. The additional bits added due to this are considered padding and can not be relied on to exist. They may be removed in a future release without notice. If you need padding up to a given size, explicitly specify a trailing padding field.\n\nwarning: Field type\nAs there are no variable sized integers in Julia, it is only guaranteed that the return type on field access is large enough to hold all bits required by that field. While currently field sizes larger than 1 return an UInt, this is in particular not guaranteed and may be changed in the future, so that e.g. a field of size 2 returns an UInt8 instead.\n\nExamples\n\njulia> @bitfield struct MyBits\n           a:2\n           b:3\n           _:3 # padding\n           c:1\n       end\n\njulia> bits = MyBits(1,2,3)\nMyBits(a: 0x1, b: 0x2, c: true)\n\njulia> bits.a\n0x01\n\njulia> bits.b\n0x02\n\njulia> bits.c\ntrue\n\n\n\n\n\n","category":"macro"},{"location":"api/#Functions","page":"API Reference","title":"Functions","text":"","category":"section"},{"location":"api/","page":"API Reference","title":"API Reference","text":"These functions are explicitly not exported, to prevent confusion with Base.fieldoffset and similar field and property related functions.","category":"page"},{"location":"api/","page":"API Reference","title":"API Reference","text":"FieldFlags.propertyoffset\nFieldFlags.fieldsize","category":"page"},{"location":"api/#FieldFlags.propertyoffset","page":"API Reference","title":"FieldFlags.propertyoffset","text":"propertyoffset(::Type{T}, s::Symbol) -> Int\n\nGives the offset (in bits) the field s is placed at in objects of type T.\n\nSee also FieldFlags.fieldsize.\n\njulia> @bitflags mutable struct MyFlags\n           flagA\n           _ # padding\n           flagB\n       end\n\njulia> FieldFlags.propertyoffset(MyFlags, :flagA)\n0\n\njulia> FieldFlags.propertyoffset(MyFlags, :flagB)\n2\n\n\n\n\n\n","category":"function"},{"location":"api/#FieldFlags.fieldsize","page":"API Reference","title":"FieldFlags.fieldsize","text":"fieldsize(::Type{T}, s::Symbol) -> Int\n\nGives the size (in bits) the field s takes up in objects of type T.\n\nSee also FieldFlags.propertyoffset.\n\njulia> @bitfield mutable struct MyBits\n           a:2\n           _ # padding\n           b:3\n       end\n\njulia> FieldFlags.fieldsize(MyBits, :a)\n2\n\njulia> FieldFlags.fieldsize(MyBits, :b)\n3\n\n\n\n\n\n","category":"function"},{"location":"api/#Additional-Supported-API","page":"API Reference","title":"Additional Supported API","text":"","category":"section"},{"location":"api/","page":"API Reference","title":"API Reference","text":"These functions are listed because they are supported, but their docstrings can't be displayed without having an instance of a type created via @bitfield or @bitflags.","category":"page"},{"location":"api/","page":"API Reference","title":"API Reference","text":"Base.propertynames\nGives a tuple of the properties given in the original expression given to @bitfield or @bitflags.\nconvert(::T, x::Union{Bool, Base.BitInteger})\nConverts x to a T, originally created via the macros of this package. If the sizes don't match, x is either truncated or its bitrepresentation is zero-extended to fit the size of T.","category":"page"},{"location":"api/#Internal-API","page":"API Reference","title":"Internal API","text":"","category":"section"},{"location":"api/","page":"API Reference","title":"API Reference","text":"The following symbols are NOT considered API for the purposes of semver. They are documented here as a useful reference, not as a statement of semver guarantees.","category":"page"},{"location":"api/","page":"API Reference","title":"API Reference","text":"FieldFlags.bitflags\nFieldFlags.bitfield\nFieldFlags.cast_extend_truncate\nFieldFlags.cast_or_extend","category":"page"},{"location":"api/#FieldFlags.bitflags","page":"API Reference","title":"FieldFlags.bitflags","text":"bitflags(::Expr)\n\nThe given `Expr(:struct) has the following format\n\nstruct MyFlags\n    a\n    b\n    _\nend\n\nwhich is turned into\n\nstruct MyFlags\n    a:1\n    b:1\n    _:1\nend\n\nbefore being passed to FieldFlags.bitfield.\n\nSome minimal expression filtering is performed.\n\nSee also @bitflags, @bitfield.\n\n\n\n\n\n","category":"function"},{"location":"api/#FieldFlags.bitfield","page":"API Reference","title":"FieldFlags.bitfield","text":"bitfield(expr::Expr)\n\nTakes an Expr(:struct) of the form\n\nstruct MyStruct\n    a:x\n    b:y\n    _\n    _:z\nend\n\nwhere a, b are potential field names, x, y, and z are desired bitwidths for those fields as integer literals, _ is padding and returns the following expression:\n\nExpr(:block,\n    typedefs,\n    typefuncs,\n    conv,\n    eqhash,\n    shows,\n    propsize,\n    propoffset,\n    getprop,\n    setprop\n)\n\nWhere typedefs are the new user-facing type definition and the internal type definitions, typefuncs are type related functions from Base for the new types, conv are convert methods to those types, propsize is the implementation for FieldFlags.fieldsize, propoffset is the implementation for FieldFlags.propertyoffset, getprop is the definition for the getproperty overload for the user facing type and setprop is the definition for the setproperty! overloda for the user facing type.\n\nSee also FieldFlags.bitflags.\n\n\n\n\n\n","category":"function"},{"location":"api/#FieldFlags.cast_extend_truncate","page":"API Reference","title":"FieldFlags.cast_extend_truncate","text":"cast_extend_truncate(T::DataType, x) -> T\n\nTakes an object x of a primitive type and either bitcasts it to type T (if their sizes are egal), zero extends the bitrepresentation of x to the size of T, or truncates the bitrepresentation of x to sizeof(T).\n\nReturns a T.\n\nSee also FieldFlags.cast_or_extend.\n\n\n\n\n\n","category":"function"},{"location":"api/#FieldFlags.cast_or_extend","page":"API Reference","title":"FieldFlags.cast_or_extend","text":"cast_or_extend(T::DataType, x) -> T\n\nTakes an object x of a primitive type and either bitcasts it to T (if their sizes are egal) or zero-extends the bitrepresentation of x to the size of T. sizeof(x) <= sizeof(T) must hold.\n\nReturns a T.\n\nSee also FieldFlags.cast_extend_truncate.\n\n\n\n\n\n","category":"function"},{"location":"examples/#Examples","page":"Examples","title":"Examples","text":"","category":"section"},{"location":"examples/","page":"Examples","title":"Examples","text":"This page contains some examples on how to use @bitflags and @bitfields, though if you are familiar with regular structs, the usage should be familiar, as the interfaces are modeled after them.","category":"page"},{"location":"examples/#Basic-bitflags","page":"Examples","title":"Basic bitflags","text":"","category":"section"},{"location":"examples/","page":"Examples","title":"Examples","text":"Starting with @bitflags, which is used for tightly-packed boolean storage, which can be used like so:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"using FieldFlags\n@bitflags struct MyFlags\n    flagA\n    flagB\n    _ # padding\n    flagC\nend","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"The above defines a struct MyFlags with three fields, flagA, flagB and flagC. As the comment indicates, _ is for specifying padding. All fields specified in the struct take up a single bit - even the padding. The minimum size for the above is thus 4 bits. The fields are stored from least significant bit to most significant bit, starting with fieldA.","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"While the minimum bitsize for the above struct is 4 bits, due to an implementation detail/compiler requirement, all structsizes are rounded up to the next multiple of 8 bits. MyFlags is thus 8 bits, or 1 byte, large:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"sizeof(MyFlags)","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"That is, an instance of MyFlags has these bits:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"MSB    LSB\n5-8 flagC _ flagB flagA","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"With the 4 bits higher than flagC being implicit padding as well.","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"@bitflags gives us two default constructors; a zero-arg constructor as well as an n-arg constructor.","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"The zero-arg constructor allows us to construct an instance of MyFlags with all fields set to false:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"mf = MyFlags()\nmf.flagA == mf.flagB == mf.flagC == false","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"As can be seen above, individual fields can be accessed with regular dot-syntax.","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"note: Fields vs. Properties\nTechnically speaking, neither @bitflags nor @bitfield gives a struct with actual fields - dot-syntax access is only simulating fields, by overloading getproperty. That is, a call like getfield(mf, :flagA) cannot succeed - use getproperty(mf, :flagA) instead, which handles the field unpacking for you. This is a technicality though, and as such property and field are used interchangeably in this documentation.","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"In contrast, the n-arg constructor takes one argument for each field:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"mf = MyFlags(true, false, true)\nmf.flagA == mf.flagC == true\nmf.flagB == false","category":"page"},{"location":"examples/#Mutability","page":"Examples","title":"Mutability","text":"","category":"section"},{"location":"examples/","page":"Examples","title":"Examples","text":"While immutability can be useful, sometimes it is more convenient to mutate a flag in-place. This can be achieved by marking the struct given to @bitflags as mutable:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"using FieldFlags\n\n@bitflags mutable struct MutableFlags\n    a\n    _\n    b\n    _\n    c\nend","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"The above struct requires at least 5 bits, which means the bitlayout is like so:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"MSB     LSB\n6-8 c _ b _ a","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"The remaining upper 2 bits are once again implicit padding, while the overall size of the objects stay the same:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"sizeof(MutableFlags)","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"The available constructors are also once again the same:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"methods(MutableFlags)","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"The only difference is that we are now able to set individual fields in an object:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"mutf = MutableFlags(false, false, false)\nmutf.a == false\nmutf.a = true\nmutf.a == true","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"which we weren't able to do earlier:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"mf.flagA = true","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"warning: Allocations\nOne limitation of allowing fields to be set is that the object is declared as mutable, which has the same effect as with regular structs that are marked as mutable. For example, mutable structs aren't guaranteed to be stored inline in other objects like wrapper structs or arrays, which may require additional allocations. Setting/reading flags of mutable objects does not lead to allocations - these stay allocation-free.","category":"page"},{"location":"examples/#Subtyping","page":"Examples","title":"Subtyping","text":"","category":"section"},{"location":"examples/","page":"Examples","title":"Examples","text":"On top of mutability, we can also specify an abstract supertype as usual:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"using FieldFlags\nabstract type MyAbstract end\n@bitflags struct MyConcrete <: MyAbstract\n    foo\n    _\n    bar\n    baz\nend\nsupertype(MyConcrete) == MyAbstract","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"This allows for defining common fallback methods for @bitfield or @bitflags structs that may share some common fields or other invariants:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"@bitflags struct OtherConcrete <: MyAbstract\n    foo\n    _\n    bak\nend\nfallback(ma::MyAbstract) = ma.foo\n\nfallback(MyConcrete(true, false, false)) == true\nfallback(OtherConcrete(false, true)) == false","category":"page"},{"location":"examples/#[@bitfield](@ref)-structs","page":"Examples","title":"@bitfield structs","text":"","category":"section"},{"location":"examples/","page":"Examples","title":"Examples","text":"Structs defined with @bitfield are, in regards to mutability, bitsize and subtyping behavior, identical to those defined by @bitflags. The major difference is that while @bitflags structs only hold one bit per field, @bitfield can hold multiple bits per field:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"using FieldFlags\n\n@bitfield mutable struct MyField\n    a:1\n    _:2\n    b:3\n    _\n    c:2\nend","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"The above defines a struct MyField, with three fields a, b and c, with sizes (in bits) 1,  3 and 2 respectively. There are also two definitions of explicit padding between fields, the first being 2 bits in size and the second one being 1 bit in size; taken implicitly from _ not having a size annotated. The layout of the above struct is like so:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"MSB         LSB\n10-16 c c _ b b b _ _ a","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"With the additional padding bits, we come to a total of 9 bits. This is again rounded up to the next multiple of 8, which is 16 bits or 2 bytes:","category":"page"},{"location":"examples/","page":"Examples","title":"Examples","text":"sizeof(MyField)","category":"page"},{"location":"#FieldFlags.jl-Documentation","page":"FieldFlags.jl Documentation","title":"FieldFlags.jl Documentation","text":"","category":"section"},{"location":"","page":"FieldFlags.jl Documentation","title":"FieldFlags.jl Documentation","text":"FieldFlags.jl is a small package without dependencies, giving users the ability to create structs containing packed integers of various bitsizes.","category":"page"},{"location":"","page":"FieldFlags.jl Documentation","title":"FieldFlags.jl Documentation","text":"The two main exports of this package are the two macros @bitflags and  @bitfield, for creating bit packed boolean flag structs and bit packed integer field structs respectively.","category":"page"},{"location":"","page":"FieldFlags.jl Documentation","title":"FieldFlags.jl Documentation","text":"This package is heavily inspired by C-style bitfields, though there are some limitations.","category":"page"},{"location":"","page":"FieldFlags.jl Documentation","title":"FieldFlags.jl Documentation","text":"Pages=[\"index.md\", \"examples.md\", \"api.md\"]\nDepth=3","category":"page"},{"location":"#Goals","page":"FieldFlags.jl Documentation","title":"Goals","text":"","category":"section"},{"location":"","page":"FieldFlags.jl Documentation","title":"FieldFlags.jl Documentation","text":"Low/Negligible overhead\nI can get a bextract on my machine from extremely high level julia code\nHigh performance\nGood optimization by the compiler (constant folding, elimination of error paths)\nThe package should \"feel\" as if there were no special implementation\nGood debuggability with JET.jl","category":"page"},{"location":"#Limitations","page":"FieldFlags.jl Documentation","title":"Limitations","text":"","category":"section"},{"location":"","page":"FieldFlags.jl Documentation","title":"FieldFlags.jl Documentation","text":"Thread safety\nAccessing the objects produced by this package is not thread safe and atomic access is not planned to be supported. Users are advised to use proper locking to ensure safety.\nSize of the objects\nDue to a compiler limitation, the size of all objects created by this package is a multiple of 8 bits. This restriction may be removed in the future.\nType parameters cannot be supported - the size of a field needs to be known at definition time, so that the various bitshifts and masking operations done internally can be compiled away.\nThe widest a field can currently be is 8*sizeof(UInt) bits, as UInt is currently the default return type for fields (other than those of width 1, which return a Bool).","category":"page"},{"location":"#Planned-Features","page":"FieldFlags.jl Documentation","title":"Planned Features","text":"","category":"section"},{"location":"","page":"FieldFlags.jl Documentation","title":"FieldFlags.jl Documentation","text":"Custom field type annotations\nThis would look like a regular field type annotation for exact sizes (i.e. a::UInt16), or like e.g. a:3::UInt16 for objects that want to store the lower 3 bits of an UInt16 and want to get that type back out when accessing the field.\nDue to the nature of how these objects are stored internally, the types will need to be at least isbitstype, possibly even isprimitivetype, as it's unclear whether the padding potentially contained in an isbitstype is legal to observe (I suspect it isn't).\n<: Signed types will need to be at least 2 bits in size, to store the sign bit. \nSee #9 for the issue tracking this.\nNarrower field types\nCurrently, all field accesses (unless accessing a single bit field) return an UInt. This is not guaranteed, and may be narrowed in the future, such that a field annotated with width 2 returns an UInt8 by default, width 9 an UInt16 etc.\nSee #7 for the issue tracking this.","category":"page"}]
}
