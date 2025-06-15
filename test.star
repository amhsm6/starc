fn main(x, y int32, z bool) {
    foo := 5
    bar := &foo
    bar := &bar
    baz := **bar
}
