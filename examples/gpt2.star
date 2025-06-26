fn add(a int32, b int32) int32 {
    return a + b
}

fn negate(x *int16) int16 {
    *x = -*x
    return *x
}

fn scale(val int64, factor int64) int64 {
    return val * factor
}

fn complex_calc(a int8, b int8, c int8) int8 {
    return a * b + c - (a + b)
}

fn logicals(x int32, y int32) bool {
    return (x > y) || (x == 10) && !(y < 5)
}

fn test_shadow(x int64) int64 {
    x int64 := x + 10
    if x > 20 {
        x int64 := x * 2
        return x
    } else {
        return x - 2
    }
}

fn big_expression(a int64, b int64, c int64) int64 {
    return ((a + b) * (c - a)) / 3 + (a * b - c)
}

fn mutate(ptr *int32) int32 {
    *ptr = *ptr + 50
    return *ptr
}

fn tricky(x int32) int32 {
    if x > 10 {
        if x < 50 {
            if x + x > 30 {
                return x / 2
            } else {
                print_int32(x)
                return x * 3
            }
        } else {
            return x - 10
        }
    }
    return -x
}

fn nested(a int64, b int64) int64 {
    if a > b {
        a int64 := a - b
        if a > 5 {
            return a * 2
        }
    }
    return b - a
}

fn arithmetic_chain(x int32) int32 {
    return x + 1 - 2 + 3 - 4 + 5 - 6 + 7 - 8 + 9
}

fn bool_check(a bool, b bool) bool {
    return !(a && b) || (a || b)
}

fn test_all() {
    // print_int32(add(10, 20))

    // x int16 := 5
    // x_ptr *int16 := &x
    // print_int16(negate(x_ptr))

    // print_int64(scale(12, 5))

    // a int8 := 2
    // b int8 := 3
    // c int8 := 4
    // print_int8(complex_calc(a, b, c))

    // print_bool(logicals(10, 20))
    // print_bool(logicals(11, 5))

    // print_int64(test_shadow(5))
    // print_int64(test_shadow(15))

    // print_int64(big_expression(5, 7, 9))

    // v int32 := 30
    // v_ptr *int32 := &v
    // print_int32(mutate(v_ptr))
    // print_int32(v)

    // print_int32(tricky(11))
    print_int32(tricky(52))
    print_int32(tricky(8))

    print_int64(nested(10, 5))
    print_int64(nested(5, 10))

    print_int32(arithmetic_chain(0))
    print_bool(bool_check(true, true))
    print_bool(bool_check(false, true))
}

fn main() {
    x int64 := 1000
    y int64 := 100

    if x < y {
        print_int64(x)
        print_int64(y)
        x int64 := x + y
        print_int64(x)
    } else if x == y {
        print_int64(x)
        x = x + y
    } else {
        print_int64(999)
    }

    print_int64(x)

    test_all()

    a int32 := 3
    b int32 := 4
    c int32 := 5

    result int32 := a + b * c - (a + c / b)
    print_int32(result)

    d int16 := 7
    e int16 := -3
    d_ptr *int16 := &d
    e_ptr *int16 := &e
    f int16 := negate(d_ptr) + negate(e_ptr)
    print_int16(f)

    arr int64 := 0
    p *int64 := &arr
    *p = 42
    print_int64(*p)

    g int8 := 10
    h int8 := 5
    i int8 := complex_calc(g, h, g)
    print_int8(i)

    // j int64 := scale(i, 2)
    // print_int64(j)
// 
    // b1 bool := true
    // b2 bool := false
    // print_bool(bool_check(b1, b2))
// 
    // if b1 {
        // x int64 := 1234
        // if x > 1000 {
            // print_int64(x)
        // }
    // } else {
        // print_int64(0)
    // }
// 
    // print_int32(0)
    // print_int32(1)
    // print_int32(2)
    // print_int32(3)
    // print_int32(4)
// 
    // z int32 := tricky(33)
    // print_int32(z)
// 
    // u int64 := test_shadow(3)
    // print_int64(u)
// 
    // print_int64(big_expression(12, 8, 6))
}
