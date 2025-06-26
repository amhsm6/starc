fn fib(n int64) int64 {
    if n < 1 {
        return 0
    }

    if n == 1 {
        return 1
    }
    
    if n == 2 {
        return 1
    }

    return fib(n - 1) + fib(n - 2)
}

fn factorial(x int32) int32 {
    if x <= 1 {
        return 1
    }

    return x * factorial(x - 1)
}

fn main() {
    print_int32(factorial(1))
    print_int32(factorial(2))
    print_int32(factorial(3))
    print_int32(factorial(4))
    print_int32(factorial(5))
    print_int32(factorial(6))

    print_int64(fib(1))
    print_int64(fib(2))
    print_int64(fib(3))
    print_int64(fib(4))
    print_int64(fib(5))
    print_int64(fib(6))
    print_int64(fib(7))
    print_int64(fib(8))
    print_int64(fib(9))
}
