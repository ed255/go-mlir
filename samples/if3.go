package main

func entrypoint(a, b uint8) uint8 {
	var x uint8 = 0
	if a == 0 {
		// x = x + b
		if a == 5 {
			{
				x = x + 2
			}
		}
	}
	return x
}
