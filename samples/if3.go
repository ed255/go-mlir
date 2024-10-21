package main

func entrypoint(
	a uint8,
) uint8 {
	var x uint8 = 0
	if a/2 == 0 {
		// x = x + b
		if a/3 == 0 {
			x = x + 2
		} else if x == 0 {
			x = x + 3
		}
	}
	return x
}
