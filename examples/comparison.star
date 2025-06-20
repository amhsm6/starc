fn main() {
    x := 5
    y bool := x == 5
    y_ptr *bool := &y
    y := y == false
    print_bool(y)
    print_bool(*y_ptr)
}
