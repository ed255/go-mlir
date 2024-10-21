package main

type Data struct {
	a uint8
	b uint8
}

func entrypoint(a, b uint8) uint8 {
	d := Data{a: a, b: b}
	d.a = d.a + 1
	r := add(d)
	return r
}

func add(d Data) uint8 {
	return d.a + d.b
}
