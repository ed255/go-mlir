package main

func entrypoint(a, b uint8) uint8 {
	var x uint8 = 0
	{
		b = 44
		var b uint8 = 32
		x = a + b
		b = 55
	}
	b = b + 1
	return x
}
