fn main() {
    y int8 := 20
    z int8 := 30
    slice := []int8(&z, 2)

    print_int8(slice[0])
    print_int8(slice[1])
    print_int8(slice[0] * slice[1])
}


// []T - slice, has ptr and len
// slice[x] - copy element
// &slice[y] - ptr to element
// >slice[x:y] - slice with new ptr and len

// &slice - ptr to slice if slice is an lvalue
// >&slice[x:y] - nope, slice[x:y] is an rvalue

// construction []T(ptr, len)
// get ptr      ptr(slice)
// get len      len(slice)

// [N]T - array, has slice
// array can be coerced to slice!

// c int32 := 3
// b int32 := 2
// a int32 := 1
// a_ptr := &a

// x []int32 := []int32(a_ptr, 3)
// x_ptr *[]int32 := &x
// first_value := *x_ptr[0]
// second_value_ptr := &*x_ptr[0]

// type Vec struct {
//     slice []int32
// }

// fn push(v *Vec, x int32) {
//     realloc and stuff
//     *v.slice = []int32(ptr_new, len(*v.slice) + 1)
// }
