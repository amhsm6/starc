
fn arithmetic_test(a int32, b int32) int32 {
    return (a + b) * 2 - a / b
}

fn logical_test(x bool, y bool) bool {
    return (x || y) && !(x && y)
}

fn factorial_recursive(n int32) int32 {
    if n <= 1 {
        return 1
    }
    return n * factorial_recursive(n - 1)
}

fn main() {
    // Variable declaration and arithmetic
    x int32 := 10
    y int32 := 5
    res int32 := arithmetic_test(x, y)
    print_int32(res)

    // Pointer manipulation
    p *int32 := &x
    *p = *p + 3
    print_int32(x) 

    // Conditional statements
    if x > y {
        print_int32(1)
    } else {
        print_int32(0)
    }

    // Boolean logic
    a bool := true
    b bool := false
    print_bool(logical_test(a, b))
    print_bool(logical_test(true, true))

    // Recursion
    fact5 int32 := factorial_recursive(5)
    print_int32(fact5)

    // Shadowing
    z int32 := 20
    if true {
        z int32 := 30
        print_int32(z)
    }
    print_int32(z)
}
