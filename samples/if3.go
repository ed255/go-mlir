package main

func entrypoint(
	a uint8,
) uint8 {
	var x uint8 = 0
	if a > 5 {
		// x = x + b
		if a > 10 {
			x = x + 2
		} else {
			x = x + 3
		}
	}
	return x
}
