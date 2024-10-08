package main

func entrypoint(a, b uint8) (uint8, uint8) {
	return add(a, b)
}

func add(a, b uint8) (uint8, uint8) {
	return a, a + b
}
