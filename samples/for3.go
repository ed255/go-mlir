package main

func entrypoint(a uint8) uint8 {
	i := 0
	for {
		a = a + 1
		if i > 4 {
			break
		}
		i = i + 1
	}
	return a
}
