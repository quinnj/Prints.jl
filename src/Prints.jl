module Prints

using Ryu

export @printf, @sprintf, Format, format, @format

isint(x) = x == 'd' || x == 'i' || x == 'u' || x == 'x' || x == 'X' || x == 'o'
isfloat(x) = x == 'e' || x == 'E' || x == 'f' || x == 'F' || x == 'g' || x == 'G' || x == 'a' || x == 'A'
ischar(x) = x == 'c' || x == 'C'
isstring(x) = x == 's' || x == 'S'
ishexbase(x) = x == 'x' || x == 'X' || x == 'a' || x == 'A'

struct Spec
    type::Char
    leftalign::Bool
    plus::Bool
    space::Bool
    zero::Bool
    hash::Bool
    width::Int
    precision::Int
end

Base.string(f::Spec; bigfloat::Bool=false) =
    string("%", f.leftalign ? "-" : "", f.plus ? "+" : "", f.space ? " " : "",
        f.zero ? "0" : "", f.hash ? "#" : "", f.width > 0 ? f.width : "",
        f.precision == 0 ? ".0" : f.precision > 0 ? ".$(f.precision)" : "", bigfloat ? "R" : "", f.type)
Base.show(io::IO, f::Spec) = print(io, '"', string(f), '"')

ptrfmt(s::Spec, x) =
    Spec('x', s.leftalign, s.plus, s.space, s.zero, true, s.width, sizeof(x) == 8 ? 16 : 8)

struct Format{S}
    str::S
    substringranges::Vector{UnitRange{Int}}
    formats::Vector{Spec}
end

base(x) = x in HEXBASES ? 16 : x == 'o' ? 8 : 10

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
    fmts = Spec[]
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
        type = Char(b)
        if isint(type) && precision > 0
            zero = false
        elseif (isstring(type) || ischar(type)) && !parsedprecdigits
            precision = -1
        elseif (type == 'a' || type == 'A') && !parsedprecdigits
            precision = -1
        elseif isfloat(type) && !parsedprecdigits
            precision = 6
        end
        push!(fmts, Spec(type, leftalign, plus, space, zero, hash, width, precision))
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
    return Format(bytes, strs, fmts)
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

function fmtchars(buf, pos, arg, spec)
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
function fmtstrings(buf, pos, arg, spec)
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
function fmtints(buf, pos, arg, spec)
    type, leftalign, plus, space, zero, hash, width, prec =
        spec.type, spec.leftalign, spec.plus, spec.space, spec.zero, spec.hash, spec.width, spec.precision
    bs = base(type)
    arg2 = arg isa AbstractFloat ? Integer(trunc(arg)) : arg
    n = i = ndigits(arg2, base=bs, pad=1)
    x, neg = arg2 < 0 ? (-arg2, true) : (arg2, false)
    arglen = n + (neg || (plus | space)) +
        (type == 'o' && hash ? 2 : 0) +
        (type == 'x' && hash ? 2 : 0) + (type == 'X' && hash ? 2 : 0)
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
    if type == 'o' && hash
        buf[pos] = UInt8('0')
        buf[pos + 1] = UInt8('o')
        pos += 2
    elseif type == 'x' && hash
        buf[pos] = UInt8('0')
        buf[pos + 1] = UInt8('x')
        pos += 2
    elseif type == 'X' && hash
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
    if bs == 16
        while i > 0
            buf[pos + i - 1] = type == 'x' ? hex[(x & 0x0f) + 1] : HEX[(x & 0x0f) + 1]
            x >>= 4
            i -= 1
        end
    elseif bs == 8
        while i > 0
            buf[pos + i - 1] = 48 + (x & 0x07)
            x >>= 3
            i -= 1
        end
    else
        while i > 0
            buf[pos + i - 1] = 48 + rem(x, 10)
            x = oftype(x, div(x, 10))
            i -= 1
        end
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
exp(a, b, c, d, e, f, g, h, i) = Ryu.writeexp(a, b, c, d, e, f, g, h, i)
fixed(a, b, c, d, e, f, g, h) = Ryu.writefixed(a, b, c, d, e, f, g, h)
shortest(a, b, c, d, e, f, g, h, i, j) = Ryu.writeshortest(a, b, c, d, e, f, g, h, i, j)

function fmtfloats(buf, pos, arg, spec)
    type, leftalign, plus, space, zero, hash, width, prec =
        spec.type, spec.leftalign, spec.plus, spec.space, spec.zero, spec.hash, spec.width, spec.precision
    x = Float64(arg)
    if arg isa BigFloat && !isnan(arg) && isfinite(arg)
        newpos = ccall((:mpfr_snprintf, :libmpfr), Int32,
                (Ptr{UInt8}, Culong, Ptr{UInt8}, Ref{BigFloat}),
                buf, length(buf), string(spec, bigfloat=true), arg)
        newpos > 0 || error("invalid printf formatting for BigFloat")
        return newpos + 1
    end
    if type == 'e' || type == 'E'
        newpos = exp(buf, pos, x, plus, space, hash, prec, UInt8(type), UInt8('.'))
    elseif type == 'f' || type == 'F'
        newpos = fixed(buf, pos, x, plus, space, hash, prec, UInt8('.'))
    elseif type == 'g' || type == 'G'
        prec = prec == 0 ? 1 : prec
        x = round(x, sigdigits=prec)
        newpos = shortest(buf, pos, x, plus, space, hash, prec, type == 'g' ? UInt8('e') : UInt8('E'), true, UInt8('.'))
    elseif type == 'a' || type == 'A'
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
            low = type == 'a'
            buf[newpos] = low ? UInt8('x') : UInt8('X')
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
                buf[newpos] = low ? UInt8('p') : UInt8('P')
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
                    buf[newpos + i] = low ? hex[(u & 0x0f) + 1] : HEX[(u & 0x0f) + 1]
                    u >>= 4
                    i -= 1
                    prec -= 1
                end
                if frac
                    buf[newpos + 1] = UInt8('.')
                end
                buf[newpos] = low ? hex[(u & 0x0f) + 1] : HEX[(u & 0x0f) + 1]
                newpos += n + frac
                while prec > 0
                    buf[newpos] = UInt8('0')
                    newpos += 1
                    prec -= 1
                end
                buf[newpos] = low ? UInt8('p') : UInt8('P')
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
                ex = (arg < 0 || (plus | space)) + (type == 'a' || type == 'A' ? 2 : 0)
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

@inline function format(buf::Vector{UInt8}, pos::Integer, f::Format, args)
    s = f.str
    fmts = f.formats
    for i = 1:length(args)
        # write out any prefix substring
        for j = f.substringranges[i]
            buf[pos] = s[j]
            pos += 1
        end
        # write out formatted arg
        fmt = fmts[i]
        arg = args[i]
        if isint(fmt.type)
            pos = fmtints(buf, pos, arg, fmt)
        elseif isfloat(fmt.type)
            pos = fmtfloats(buf, pos, arg, fmt)
        elseif isstring(fmt.type)
            pos = fmtstrings(buf, pos, arg, fmt)
        elseif ischar(fmt.type)
            pos = fmtchars(buf, pos, arg, fmt)
        elseif fmt.type == 'p'
            pos = fmtints(buf, pos, Int(arg), ptrfmt(fmt, arg))
        else
            throw(ArgumentError("invalid format type specifier: $(fmt)"))
        end
    end
    # write out any final substring postfix
    for i = f.substringranges[end]
        buf[pos] = s[i]
        pos += 1
    end
    return pos
end

@inline function computelen(substringranges, formats, args)
    # len = sum(length, substringranges)
    # for (f, arg) in zip(formats, args)
    #     if ischar(f.type)
    #         len += max(f.width, 1) + (ncodeunits(arg isa String ? arg[1] : Char(arg)) - 1)
    #     elseif isstring(f.type)
    #         str = string(arg)
    #         p = f.precision == -1 ? (length(str) + (f.hash ? (arg isa Symbol ? 1 : 2) : 0)) : f.precision
    #         len += max(f.width, p) + (sizeof(str) - length(str))
    #     elseif f.type == 'p'
    #         len += max(f.width, 2 * sizeof(arg) + 2)
    #     elseif isint(f.type)
    #         arg2 = arg isa AbstractFloat ? Integer(trunc(arg)) : arg
    #         y = max(f.width, f.precision + ndigits(arg2, base=base(f.type), pad=1) + plength(f.type))
    #         len += y
    #     elseif arg isa BigFloat
    #         y = Base.MPFR._calculate_buffer_size!(Base.StringVector(0), string(f), arg) + 3
    #         len += y
    #     elseif isfloat(f.type)
    #         len += max(f.width, f.precision + plength(arg) + f.hash + plength(f.type))
    #     else
    #         throw(ArgumentError("invalid format type specifier: $f"))
    #     end
    # end
    # return len
    return 10
end

plength(x::Float16) = 9 + 5
plength(x::Float32) = 39 + 9
plength(x) = 309 + 17
plength(c::Char) = (c == 'e' || c == 'E') ? 4 : (c == 'a' || c == 'A' ? 5 : 2)

@noinline argmismatch(a, b) =
    throw(ArgumentError("mismatch between # of format specifiers and provided args: $a != $b"))

function format(io::IO, f::Format, args...) # => Nothing
    length(args) == length(f.formats) || argmismatch(length(args), length(f.formats))
    buf = Base.StringVector(computelen(f.substringranges, f.formats, args))
    pos = format(buf, 1, f, args)
    write(io, resize!(buf, pos - 1))
    return
end

function format(f::Format, args...) # => String
    length(args) == length(f.formats) || argmismatch(length(args), length(f.formats))
    buf = Base.StringVector(computelen(f.substringranges, f.formats, args))
    pos = format(buf, 1, f, args)
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
