fn complex_recursive_func(n, depth int32) int32 {
    if n <= 0 {
        print_int32(n)
        return 1
    }
    if depth <= 0 {
        print_int32(n)
        return n
    }

    res := n + complex_recursive_func(n - 1, depth - 1) - complex_recursive_func(n - 2, depth - 1)
    return res
}

fn main() {
    a int16 := 100
    b int64 := 3000000000
    c := a + 200
    print_int16(c)
    print_int64(b)

    x := (100 * 5 / 2) + 10 - (a + 50) * 2
    print_int16(x)

    val int32 := 5
    max_d int32 := 3
    result := complex_recursive_func(val, max_d)

    // The print statements from the recursive function will be executed before this
    print_int32(result)

    if result > 10 {
        print_int32(1)
        if result > 20 {
            print_int32(2)
        } else {
            print_int32(3)
            y := result * 2
            print_int32(y)
        }
    } else {
        print_int32(0)
        is_small bool := true
        if is_small && (result > 5) {
            print_int32(4)
            z int32 := result + 10
            print_int32(z)
        } else {
            print_int32(5)
        }
    }
}
