package main

func entrypoint(
	a uint8,
) uint8 {
	var x uint8 = a
	if a/2 == 0 {
		x = x + 1
	} else if a/3 == 0 {
		x = x + 2
	} else if a/5 == 0 {
		x = x + 3
	} else {
		x = x + 4
	}
	return x
}
