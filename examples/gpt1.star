fn add8(a, b int8) int8 {
    return a + b
}

fn neg_shift16(p *int16) int16 {
    *p = -*p    
    return *p * 2
}

fn mul_div32(x, y int32) int32 {
    return x * y / 3
}

fn accumulate64(acc *int64, v int64) int64 {
    *acc = *acc + v
    return *acc
}

fn inc_ptr(p *int32, delta int32) int32 {
    *p = *p + delta
    return *p
}

fn swap_ptrs(a, b *int16) {
    tmp int16 := *a
    *a = *b
    *b = tmp
}

fn sum3(x, y, z int8) int8 {
    return x + y + z
}

fn main() {
    x int8 := 30
    print_int8(x)
    print_int8(-(x / -3 * -2))

    a8 int8 := 12
    b8 int8 := -5
    r8 := add8(a8, b8)
    print_int8(r8)

    v16 int16 := 10
    r16 := neg_shift16(&v16)
    print_int16(r16)

    x32 int32 := 7
    y32 int32 := 5
    x32 = mul_div32(x32, y32)
    print_int32(x32)
    print_int32(y32)

    total64 int64 := 100
    add1 int64 := 23
    add2 int64 := -10
    r64a := accumulate64(&total64, add1)
    print_int64(r64a)
    r64b := accumulate64(&total64, add2)
    print_int64(r64b)

    tmp int8 := 3 * 4 + 2
    print_int8(tmp)

    print_int8(-100 / 50)
    print_int16(-100 / 50)
    print_int32(-100 / 50)
    print_int64(-100 / 50)

    z8 int8 := 100 / 5 - 3 * 2 + 1
    print_int8(z8)

    a16 int16 := 7
    b16 int16 := 3
    c16 := a16 * b16 - (a16 + b16)
    print_int16(c16)

    m32 int32 := -12
    n32 int32 := 4
    r32 := m32 / n32 + m32 * n32
    print_int32(r32)

    big int64 := -5000
    delta int64 := 1200
    print_int64(accumulate64(&big, delta))
    print_int64(accumulate64(&big, delta))
    print_int64(accumulate64(&big, -10000))

    x int16 := 5
    y := 2 + 4 - x 
    print_bool(true)

    ptr *int16 := &x
    value int16 := 40
    print_int16(-value / *ptr)
    print_int16(-(-*ptr - 3))
    print_bool(-value / *ptr + 100 > -(-*ptr - 3))

    tru1 := 2 <= 3
    fals1 := 2 >= 400
    print_bool(tru1)
    print_bool(fals1)
    print_bool(tru1 && !fals1)
    print_bool(false || false || false && true)

    x := false
    print_bool(!x)

    print_bool(2 > -1000 && true == false || -1000 < 2)
    print_bool((true || false) && (false || true))
    print_bool(true && false || true && true)
    print_bool(!(true && false) && (false || !false))

    a int8 := 100
    b int8 := -100
    print_int8(a + b)
    print_int8(a - b)
    print_int8(a * 2)

    c int16 := 30000
    print_int16(c + 10000)
    print_int16(-c - 5000)

    d int32 := 1000000000
    print_int32(d)
    print_int32(d - 1)

    e int64 := 2000000000
    print_int64(e)
    print_int64(e + 100)

    neg int8 := -128
    print_int8(neg + 1)

    zero int8 := 0
    print_bool(zero == 0)
    print_bool(zero != 1)

    z int32 := 10
    print_bool(z > 5 && z < 20)
    print_bool(z < 5 || z > 20)

    print_int32((2 + 3) * 4 - 10 / 2)
    print_int64((100 + 200) * (3 - 1) / 2)

    h int32 := 123456
    i int32 := -654321
    print_int32(h + i)

    j int64 := 999999999
    k int64 := 1
    print_int64(j * k - j / k + 1)

    print_bool(true == true && false == false)
    print_bool(!(true != false))

    a int8 := 1
    b int8 := 2
    c int8 := 3
    print_bool(a < b && b < c && a < c)

    x32 int32 := -5
    print_int32(x32 * x32 + x32 * 2 + 1)

    x64 int64 := 1000
    print_int64(accumulate64(&x64, 24))
    print_int64(accumulate64(&x64, -1000))
    print_int64(accumulate64(&x64, 10000))

    v int32 := 10
    print_int32(inc_ptr(&v, 5))
    print_int32(inc_ptr(&v, -3))
    print_int32(v)

    a16 int16 := 8
    b16 int16 := 99
    swap_ptrs(&a16, &b16)
    print_int16(a16)
    print_int16(b16)

    print_int8(sum3(10, 20, -5))
}
