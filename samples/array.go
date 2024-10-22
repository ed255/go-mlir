package main

func entrypoint(a uint8) uint8 {
	var x [3]uint8
	x[0] = a + 1
	return x[0]
}
