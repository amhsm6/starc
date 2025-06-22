// compute sum of two int8s
fn add8(a int8, b int8) int8 {
    return a + b
}

// negate and shift an int16 by pointer
fn neg_shift16(p *int16) int16 {
    *p = 0-*p       // negate in-place
    return *p * 2 // then shift left by 1
}

// multiply two int32s and divide by 3
fn mul_div32(x int32, y int32) int32 {
    return x * y / 3
}

// accumulate into int64 via pointer
fn accumulate64(acc *int64, v int64) int64 {
    *acc = *acc + v
    return *acc
}

fn main() {
    // int8
    a8 int8 := 12
    b8 int8 := 251
    r8 := add8(a8, b8)
    print_int8(r8)        // expect 7

    // int16 via pointer
    v16 int16 := 10
    r16 := neg_shift16(&v16)
    print_int16(r16)      // expect -10 <<1 = -20

    // int32 multiply/divide
    x32 int32 := 7
    y32 int32 := 5
    x32 = mul_div32(x32, y32)
    print_int32(x32)      // expect (7*5)/3 = 35/3 = 11
    print_int32(y32)      // expect (7*5)/3 = 35/3 = 11

    // int64 accumulation
    total64 int64 := 100
    add1 int64 := 23
    add2 int64 := 18446744073709551606
    r64a := accumulate64(&total64, add1)
    print_int64(r64a)     // expect 123
    r64b := accumulate64(&total64, add2)
    print_int64(r64b)     // expect 113

    // chaining some literals
    // tmp int8 := 3 * 4 + 2
    // print_int8(tmp)       // expect 14
}
