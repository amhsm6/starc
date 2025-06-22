fn main() {
    x int16 := 5
    y := 2 + 4 - x != 1
    print_bool(y != false == false == false != false)

    ptr *int16 := &x
    value int16 := 40
    print_int16(-value / *ptr)
    print_int16(-(-*ptr - 3))
    print_bool(-value / *ptr + 100 > -(-*ptr - 3))

    tru1 := 2 <= 3
    fals1 := 2 >= 400
    print_bool(tru1)
    print_bool(fals1)
    print_bool(tru1 && !fals1)
    print_bool(false || false || false && true)

    x := false
    print_bool(!x)
    
    print_bool(2 > -1000 && true == false || -1000 < 2)
}
