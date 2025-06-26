fn main() {
    x := 1000
    y := 100
    if x < y {
        print_int64(x)
        print_int64(y)
        x := x + y
        print_int64(x)
    } else if x == y {
        print_int64(x)
        x = x + y
    } else {
        print_int64(999)
    }
    print_int64(x)
}
