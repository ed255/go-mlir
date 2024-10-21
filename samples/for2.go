package main

func entrypoint(a uint8) uint8 {
	for i := 0; i < 10; i++ {
		a = a + a
		if a > 7 {
			break
		}
	}
	return a
}
