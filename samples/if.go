package main

func entrypoint(a, b uint8) uint8 {
	var x uint8 = 0
	if a == 0 {
		x = b
	}
	return x
}
