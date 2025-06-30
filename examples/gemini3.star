fn fib(n int32) int32 {
    if n < 1 {
        return 0
    }
    if n == 1 {
        return 1
    }
    return fib(n - 1) + fib(n - 2)
}

fn collatz(n int32) int32 {
    if n == 1 {
        return 0
    }
    if n / 2 * 2 == n {
        return 1 + collatz(n / 2)
    } else {
        return 1 + collatz(3 * n + 1)
    }
}

fn ackermann(m int32, n int32) int32 {
    if m == 0 {
        return n + 1
    }
    if n == 0 {
        return ackermann(m - 1, 1)
    }
    return ackermann(m - 1, ackermann(m, n - 1))
}

fn modify_and_check(ptr *int16, val int16) bool {
    old_val := *ptr
    *ptr = val
    return old_val < *ptr
}

fn main() {
    // Test basic arithmetic and type inference
    a := 10 + 20 * 3 // Should be int64
    print_int64(a)


    b int32 := 5
    // c := a + b // This should fail to compile, but let's see what the compiler does
    // Assuming it compiles, the type of c would be ambiguous.
    // For this test, we'll assume it defaults to the larger type, int64.
    // print_int64(c)

    // Test recursion
    fib_res := fib(10)
    print_int32(fib_res)

    collatz_res := collatz(7)
    print_int32(collatz_res)

    // This will take a long time to compute, so we'll use small numbers
    ackermann_res := ackermann(2, 2)
    print_int32(ackermann_res)

    // Test pointers and complex expressions
    p_val int16 := 100
    p_ptr *int16 := &p_val
    res_bool := modify_and_check(p_ptr, 200)
    print_bool(res_bool)
    print_int16(p_val)

    // Test nested control flow
    x := 50
    if x > 20 {
        y := x / 2
        if y > 10 {
            z := y - 5
            if z > 5 {
                w := z * 2
                print_int64(w)
            } else {
                print_int64(z)
            }
        } else {
            print_int64(y)
        }
    } else {
        print_int64(x)
    }

    // More complex expressions
    a int32 := 70
    expr1 := (a + fib_res) * 2 - (collatz_res + ackermann_res) * 3
    print_int32(expr1)

    // Shadowing
    shadow := 10
    if true {
        shadow := 20
        print_int64(shadow)
    }
    print_int64(shadow)
}
