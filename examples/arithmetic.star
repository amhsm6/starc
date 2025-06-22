
fn add8(a int8, b int8) int8 {
    return a + b
}

fn neg_shift16(p *int16) int16 {
    *p = -*p    
    return *p * 2
}

fn mul_div32(x int32, y int32) int32 {
    return x * y / 3
}

fn accumulate64(acc *int64, v int64) int64 {
    *acc = *acc + v
    return *acc
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
}

// 30
// -20
// 7
// -20
// 11
// 5
// 123
// 113
// 14
// -2
// -2
// -2
// -2
// 15
// 11
// -51
// -3800
// -2600
// -12600
