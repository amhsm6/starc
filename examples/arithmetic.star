fn one(x *int64) int64 {
    result := *x + 1
    *x = *x * 2
    return result
}

fn two(x, y int64) int64 {
    return x * y
}

fn main() {
    x := 10
    one(&x)
    y := one(&x)
    print_int64(x + y)
}
