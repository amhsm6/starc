fn one(x *int32) int32 {
    result := *x + 1
    *x = *x + 2
    return result
}

fn main() {
    x int32 := 10
    one(&x)
    y := one(&x)
    print_int32(x + y)
}
