package main

func entrypoint(a, b uint8) uint8 {
	var x, y = add(a, b)
	return x + y
}

func add(a, b uint8) (uint8, uint8) {
	return a, a + b
}
