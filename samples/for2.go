package main

func entrypoint(a uint8) uint8 {
	for i := 0; i < 10; i++ {
		a = a + 1
		if a > 19 {
			break
		}
		a = a + 1
	}
	return a
}
