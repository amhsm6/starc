fn main(arg1, arg2 int32, arg3 bool) {
    y := 5
    x := &y
    z := &x
    //variable := ((*x + y) * 2 > 10) == (***(&z) < 3 + 4)
    print_int64(**z)
}
