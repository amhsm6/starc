fn one(x int64) int64 {
    return x + 1
}

fn two(x, y int64) int64 {
    return x * y
}

fn main(arg1, arg2 int32, arg3 bool) {
    y := 5
    x := &y
    z := &x
    //variable := ((*x + y) * 2 > 10) == (***(&z) < 3 + 4)
    one_result := one(**z) * one(**z) * one(**z) // 6*6*6 = 216
    // foo := &two_result
    a := 3
    b := 0
    print_int64(one_result)
}
