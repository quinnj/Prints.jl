module Prints

using Ryu

export @printf, @sprintf, Format, format, @format, tofloat

const Ints = Union{Val{'d'}, Val{'i'}, Val{'u'}, Val{'x'}, Val{'X'}, Val{'o'}}
const Floats = Union{Val{'e'}, Val{'E'}, Val{'f'}, Val{'F'}, Val{'g'}, Val{'G'}, Val{'a'}, Val{'A'}}
const Chars = Union{Val{'c'}, Val{'C'}}
const Strings = Union{Val{'s'}, Val{'S'}}
const Pointer = Val{'p'}
const HexBases = Union{Val{'x'}, Val{'X'}, Val{'a'}, Val{'A'}}

struct Spec{T} # T => %type => Val{'type'}
    leftalign::Bool
    plus::Bool
    space::Bool
    zero::Bool
    hash::Bool
    width::Int
    precision::Int
end

Base.string(f::Spec{T}; modifier::String="") where {T} =
    string("%", f.leftalign ? "-" : "", f.plus ? "+" : "", f.space ? " " : "",
        f.zero ? "0" : "", f.hash ? "#" : "", f.width > 0 ? f.width : "",
        f.precision == 0 ? ".0" : f.precision > 0 ? ".$(f.precision)" : "", modifier, char(T))
Base.show(io::IO, f::Spec) = print(io, string(f))

ptrfmt(s::Spec{T}, x) where {T} =
    Spec{Val{'x'}}(s.leftalign, s.plus, s.space, s.zero, true, s.width, sizeof(x) == 8 ? 16 : 8)

struct Format{S, T}
    str::S
    substringranges::Vector{UnitRange{Int}}
    formats::T # Tuple of Specs
end

base(T) = T <: HexBases ? 16 : T <: Val{'o'} ? 8 : 10
char(::Type{Val{c}}) where {c} = c

# parse format string
function Format(f::AbstractString)
    isempty(f) && throw(ArgumentError("empty format string"))
    bytes = codeunits(f)
    len = length(bytes)
    pos = 1
    b = 0x00
    while true
        b = bytes[pos]
        pos += 1
        (pos > len || (b == UInt8('%') && pos <= len && bytes[pos] != UInt8('%'))) && break
    end
    strs = [1:pos - 1 - (b == UInt8('%'))]
    fmts = []
    while pos <= len
        b = bytes[pos]
        pos += 1
        # positioned at start of first format str %
        # parse flags
        leftalign = plus = space = zero = hash = false
        while true
            if b == UInt8('-')
                leftalign = true
            elseif b == UInt8('+')
                plus = true
            elseif b == UInt8(' ')
                space = true
            elseif b == UInt8('0')
                zero = true
            elseif b == UInt8('#')
                hash = true
            else
                break
            end
            pos > len && throw(ArgumentError("incomplete format string: '$f'"))
            b = bytes[pos]
            pos += 1
        end
        if leftalign
            zero = false
        end
        # parse width
        width = 0
        while b - UInt8('0') < 0x0a
            width = 10width + (b - UInt8('0'))
            b = bytes[pos]
            pos += 1
            pos > len && break
        end
        # parse precision
        precision = 0
        parsedprecdigits = false
        if b == UInt8('.')
            pos > len && throw(ArgumentError("incomplete format string: '$f'"))
            parsedprecdigits = true
            b = bytes[pos]
            pos += 1
            if pos <= len
                while b - UInt8('0') < 0x0a
                    precision = 10precision + (b - UInt8('0'))
                    b = bytes[pos]
                    pos += 1
                    pos > len && break
                end
            end
        end
        # parse length modifier (ignored)
        if b == UInt8('h') || b == UInt8('l')
            prev = b
            b = bytes[pos]
            pos += 1
            if b == prev
                pos > len && throw(ArgumentError("invalid format string: '$f'"))
                b = bytes[pos]
                pos += 1
            end
        elseif b in b"Ljqtz"
            b = bytes[pos]
            pos += 1
        end
        # parse type
        !(b in b"diouxXDOUeEfFgGaAcCsSpn") && throw(ArgumentError("invalid format string: '$f', invalid type specifier: '$(Char(b))'"))
        type = Val{Char(b)}
        if type <: Ints && precision > 0
            zero = false
        elseif (type <: Strings || type <: Chars) && !parsedprecdigits
            precision = -1
        elseif type <: Union{Val{'a'}, Val{'A'}} && !parsedprecdigits
            precision = -1
        elseif type <: Floats && !parsedprecdigits
            precision = 6
        end
        push!(fmts, Spec{type}(leftalign, plus, space, zero, hash, width, precision))
        start = pos
        prevperc = false
        while pos <= len
            b = bytes[pos]
            pos += 1
            if b == UInt8('%')
                pos > len && throw(ArgumentError("invalid format string: '$f'"))
                if bytes[pos] == UInt8('%')
                    pos += 1
                    pos > len && break
                    b = bytes[pos]
                    pos += 1
                else
                    break
                end
            end
        end
        push!(strs, start:pos - 1 - (b == UInt8('%')))
    end
    return Format(bytes, strs, Tuple(fmts))
end

macro format_str(str)
    Format(str)
end

const hex = b"0123456789abcdef"
const HEX = b"0123456789ABCDEF"

# write out a single arg according to format options
# char
@inline function writechar(buf, pos, c)
    u = bswap(reinterpret(UInt32, c))
    while true
        buf[pos] = u % UInt8
        pos += 1
        (u >>= 8) == 0 && break
    end
    return pos
end

@inline function fmt(buf, pos, arg, spec::Spec{T}) where {T <: Chars}
    leftalign, width = spec.leftalign, spec.width
    if !leftalign && width > 1
        for _ = 1:(width - 1)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    pos = writechar(buf, pos, arg isa String ? arg[1] : Char(arg))
    if leftalign && width > 1
        for _ = 1:(width - 1)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    return pos
end

# strings
@inline function fmt(buf, pos, arg, spec::Spec{T}) where {T <: Strings}
    leftalign, hash, width, prec = spec.leftalign, spec.hash, spec.width, spec.precision
    str = string(arg)
    op = p = prec == -1 ? (length(str) + (hash ? arg isa AbstractString ? 2 : 1 : 0)) : prec
    if !leftalign && width > p
        for _ = 1:(width - p)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    if hash
        if arg isa Symbol
            buf[pos] = UInt8(':')
            pos += 1
            p -= 1
        elseif arg isa AbstractString
            buf[pos] = UInt8('"')
            pos += 1
            p -= 1
        end
    end
    for c in str
        p == 0 && break
        pos = writechar(buf, pos, c)
        p -= 1
    end
    if hash && arg isa AbstractString && p > 0
        buf[pos] = UInt8('"')
        pos += 1
    end
    if leftalign && width > op
        for _ = 1:(width - op)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    return pos
end

# integers
@inline function fmt(buf, pos, arg, spec::Spec{T}) where {T <: Ints}
    leftalign, plus, space, zero, hash, width, prec =
        spec.leftalign, spec.plus, spec.space, spec.zero, spec.hash, spec.width, spec.precision
    bs = base(T)
    arg2 = arg isa AbstractFloat ? Integer(trunc(arg)) : arg
    n = i = ndigits(arg2, base=bs, pad=1)
    x, neg = arg2 < 0 ? (-arg2, true) : (arg2, false)
    arglen = n + (neg || (plus | space)) +
        (T == Val{'o'} && hash ? 2 : 0) +
        (T == Val{'x'} && hash ? 2 : 0) + (T == Val{'X'} && hash ? 2 : 0)
    arglen2 = arglen < width && prec > 0 ? arglen + min(max(0, prec - n), width - arglen) : arglen
    if !leftalign && !zero && arglen2 < width
        # pad left w/ spaces
        for _ = 1:(width - arglen2)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    if neg
        buf[pos] = UInt8('-'); pos += 1
    elseif plus # plus overrides space
        buf[pos] = UInt8('+'); pos += 1
    elseif space
        buf[pos] = UInt8(' '); pos += 1
    end
    if T == Val{'o'} && hash
        buf[pos] = UInt8('0')
        buf[pos + 1] = UInt8('o')
        pos += 2
    elseif T == Val{'x'} && hash
        buf[pos] = UInt8('0')
        buf[pos + 1] = UInt8('x')
        pos += 2
    elseif T == Val{'X'} && hash
        buf[pos] = UInt8('0')
        buf[pos + 1] = UInt8('X')
        pos += 2
    end
    if zero && arglen2 < width
        for _ = 1:(width - arglen2)
            buf[pos] = UInt8('0')
            pos += 1
        end
    elseif n < prec
        for _ = 1:(prec - n)
            buf[pos] = UInt8('0')
            pos += 1
        end
    elseif arglen < arglen2
        for _ = 1:(arglen2 - arglen)
            buf[pos] = UInt8('0')
            pos += 1
        end
    end
    while i > 0
        @inbounds buf[pos + i - 1] = bs == 16 ?
            (T == Val{'x'} ? hex[(x & 0x0f) + 1] : HEX[(x & 0x0f) + 1]) :
            (48 + (bs == 8 ? (x & 0x07) : rem(x, 10)))
        if bs == 8
            x >>= 3
        elseif bs == 16
            x >>= 4
        else
            x = oftype(x, div(x, 10))
        end
        i -= 1
    end
    pos += n
    if leftalign && arglen2 < width
        # pad right
        for _ = 1:(width - arglen2)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    return pos
end

# floats
"""
    Printf.tofloat(x)

Convert an argument to a Base float type for printf formatting.
By default, arguments are converted to `Float64` via `Float64(x)`.
Custom numeric types that have a conversion to a Base float type
that wish to hook into printf formatting can extend this method like:

```julia
Printf.tofloat(x::MyCustomType) = convert_my_custom_type_to_float(x)
```

For arbitrary precision numerics, you might extend the method like:

```julia
Printf.tofloat(x::MyArbitraryPrecisionType) = BigFloat(x)
```
"""
tofloat(x) = Float64(x)
tofloat(x::Base.IEEEFloat) = x
tofloat(x::BigFloat) = x

@inline function fmt(buf, pos, arg, spec::Spec{T}) where {T <: Floats}
    leftalign, plus, space, zero, hash, width, prec =
        spec.leftalign, spec.plus, spec.space, spec.zero, spec.hash, spec.width, spec.precision
    x = tofloat(arg)
    if x isa BigFloat && !isnan(x) && isfinite(x)
        ptr = pointer(buf, pos)
        newpos = ccall((:mpfr_snprintf, :libmpfr), Int32,
                (Ptr{UInt8}, Culong, Ptr{UInt8}, Ref{BigFloat}),
                ptr, length(buf), string(spec; modifier="R"), arg)
        newpos > 0 || error("invalid printf formatting for BigFloat")
        return pos + newpos
    elseif x isa BigFloat
        x = Float64(x)
    end
    if T <: Union{Val{'e'}, Val{'E'}}
        newpos = Ryu.writeexp(buf, pos, x, plus, space, hash, prec, char(T), UInt8('.'))
    elseif T <: Union{Val{'f'}, Val{'F'}}
        newpos = Ryu.writefixed(buf, pos, x, plus, space, hash, prec, UInt8('.'))
    elseif T <: Union{Val{'g'}, Val{'G'}}
        prec = prec == 0 ? 1 : prec
        x = round(x, sigdigits=prec)
        newpos = Ryu.writeshortest(buf, pos, x, plus, space, hash, prec, T == Val{'g'} ? UInt8('e') : UInt8('E'), true, UInt8('.'))
    elseif T <: Union{Val{'a'}, Val{'A'}}
        x, neg = x < 0 ? (-x, true) : (x, false)
        newpos = pos
        if neg
            buf[newpos] = UInt8('-')
            newpos += 1
        elseif plus
            buf[newpos] = UInt8('+')
            newpos += 1
        elseif space
            buf[newpos] = UInt8(' ')
            newpos += 1
        end
        if isnan(x)
            buf[newpos] = UInt8('N')
            buf[newpos + 1] = UInt8('a')
            buf[newpos + 2] = UInt8('N')
            newpos += 3
        elseif !isfinite(x)
            buf[newpos] = UInt8('I')
            buf[newpos + 1] = UInt8('n')
            buf[newpos + 2] = UInt8('f')
            newpos += 3
        else
            buf[newpos] = UInt8('0')
            newpos += 1
            buf[newpos] = T <: Val{'a'} ? UInt8('x') : UInt8('X')
            newpos += 1
            if arg == 0
                buf[newpos] = UInt8('0')
                newpos += 1
                if prec > 0
                    while prec > 0
                        buf[newpos] = UInt8('0')
                        newpos += 1
                        prec -= 1
                    end
                end
                buf[newpos] = T <: Val{'a'} ? UInt8('p') : UInt8('P')
                buf[newpos + 1] = UInt8('+')
                buf[newpos + 2] = UInt8('0')
            else
                if prec > -1
                    s, p = frexp(x)
                    sigbits = 4 * min(prec, 13)
                    s = 0.25 * round(ldexp(s, 1 + sigbits))
                    # ensure last 2 exponent bits either 01 or 10
                    u = (reinterpret(UInt64, s) & 0x003f_ffff_ffff_ffff) >> (52 - sigbits)
                    i = n = (sizeof(u) << 1) - (leading_zeros(u) >> 2)
                else
                    s, p = frexp(x)
                    s *= 2.0
                    u = (reinterpret(UInt64, s) & 0x001f_ffff_ffff_ffff)
                    t = (trailing_zeros(u) >> 2)
                    u >>= (t << 2)
                    i = n = 14 - t
                end
                frac = u > 9 || hash || prec > 0
                while i > 1
                    buf[newpos + i] = T == Val{'a'} ? hex[(u & 0x0f) + 1] : HEX[(u & 0x0f) + 1]
                    u >>= 4
                    i -= 1
                    prec -= 1
                end
                if frac
                    buf[newpos + 1] = UInt8('.')
                end
                buf[newpos] = T == Val{'a'} ? hex[(u & 0x0f) + 1] : HEX[(u & 0x0f) + 1]
                newpos += n + frac
                while prec > 0
                    buf[newpos] = UInt8('0')
                    newpos += 1
                    prec -= 1
                end
                buf[newpos] = T <: Val{'a'} ? UInt8('p') : UInt8('P')
                newpos += 1
                p -= 1
                buf[newpos] = p < 0 ? UInt8('-') : UInt8('+')
                p = p < 0 ? -p : p
                newpos += 1
                n = i = ndigits(p, base=10, pad=1)
                while i > 0
                    buf[newpos + i - 1] = 48 + rem(p, 10)
                    p = oftype(p, div(p, 10))
                    i -= 1
                end
                newpos += n
            end
        end
    end
    if newpos - pos < width
        # need to pad
        if leftalign
            # easy case, just pad spaces after number
            for _ = 1:(width - (newpos - pos))
                buf[newpos] = UInt8(' ')
                newpos += 1
            end
        else
            # right aligned
            n = width - (newpos - pos)
            if zero
                ex = (arg < 0 || (plus | space)) + (T <: Union{Val{'a'}, Val{'A'}} ? 2 : 0)
                so = pos + ex
                len = (newpos - pos) - ex
                copyto!(buf, so + n, buf, so, len)
                for i = so:(so + n - 1)
                    buf[i] = UInt8('0')
                end
                newpos += n
            else
                copyto!(buf, pos + n, buf, pos, newpos - pos)
                for i = pos:(pos + n - 1)
                    buf[i] = UInt8(' ')
                end
                newpos += n
            end
        end
    end
    return newpos
end

# pointers
fmt(buf, pos, arg, spec::Spec{Pointer}) = fmt(buf, pos, Int(arg), ptrfmt(spec, arg))

@inline function format(buf::Vector{UInt8}, pos::Integer, f::Format, args...)
    # write out first substring
    for i in f.substringranges[1]
        buf[pos] = f.str[i]
        pos += 1
    end
    # for each format, write out arg and next substring
    # unroll up to 8 formats
    N = length(f.formats)
    Base.@nexprs 8 i -> begin
        if N >= i
            pos = fmt(buf, pos, args[i], f.formats[i])
            for j in f.substringranges[i + 1]
                buf[pos] = f.str[j]
                pos += 1
            end
        end
    end
    if N > 8
        for i = 9:length(f.formats)
            pos = fmt(buf, pos, args[i], f.formats[i])
            for j in f.substringranges[i + 1]
                buf[pos] = f.str[j]
                pos += 1
            end
        end
    end
    return pos
end

plength(f::Spec{T}, x) where {T <: Chars} = max(f.width, 1) + (ncodeunits(x isa AbstractString ? x[1] : Char(x)) - 1)
plength(f::Spec{Pointer}, x) = max(f.width, 2 * sizeof(x) + 2)

function plength(f::Spec{T}, x) where {T <: Strings}
    str = string(x)
    p = f.precision == -1 ? (length(str) + (f.hash ? (x isa Symbol ? 1 : 2) : 0)) : f.precision
    return max(f.width, p) + (sizeof(str) - length(str))
end

function plength(f::Spec{T}, x) where {T <: Ints}
    x2 = x isa AbstractFloat ? Integer(trunc(x)) : x
    return max(f.width, f.precision + ndigits(x2, base=base(T), pad=1) + 5)
end

function plength(f::Spec{T}, x) where {T <: Floats}
    x2 = tofloat(x)
    if x2 isa BigFloat
        return Base.MPFR._calculate_buffer_size!(Base.StringVector(0), string(f), x) + 3
    else
        return max(f.width, f.precision + 309 + 17 + f.hash + 5)
    end
end

@inline function computelen(substringranges, formats, args)
    len = sum(length, substringranges)
    N = length(formats)
    # unroll up to 8 formats
    Base.@nexprs 8 i -> begin
        if N >= i
            len += plength(formats[i], args[i])
        end
    end
    if N > 8
        for i = 9:length(formats)
            len += plength(formats[i], args[i])
        end
    end
    return len
end

@noinline argmismatch(a, b) =
    throw(ArgumentError("mismatch between # of format specifiers and provided args: $a != $b"))

function format(io::IO, f::Format, args...) # => Nothing
    length(args) == length(f.formats) || argmismatch(length(args), length(f.formats))
    buf = Base.StringVector(computelen(f.substringranges, f.formats, args))
    pos = format(buf, 1, f, args...)
    write(io, resize!(buf, pos - 1))
    return
end

function format(f::Format, args...) # => String
    length(args) == length(f.formats) || argmismatch(length(args), length(f.formats))
    buf = Base.StringVector(computelen(f.substringranges, f.formats, args))
    pos = format(buf, 1, f, args...)
    return String(resize!(buf, pos - 1))
end

macro printf(io_or_fmt, args...)
    if io_or_fmt isa String
        io = stdout
        fmt = Format(io_or_fmt)
        return esc(:(Prints.format($io, $fmt, $(args...))))
    else
        io = io_or_fmt
        isempty(args) && throw(ArgumentError("must provide required format string"))
        fmt = Format(args[1])
        return esc(:(Prints.format($io, $fmt, $(Base.tail(args)...))))
    end
end

macro sprintf(fmt, args...)
    f = Format(fmt)
    return esc(:(Prints.format($f, $(args...))))
end

end # module
