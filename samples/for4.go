package main

func entrypoint(a uint8) uint8 {
	i := 0
	if a > 128 {
		i = 1
	} else {
		i = 5
	}
	for ; i < 7; i++ {
		a = a + 1
	}
	return a
}
