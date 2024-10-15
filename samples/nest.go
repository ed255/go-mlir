package main

func entrypoint(a, b uint8) (uint8, uint16) {
	var x uint8 = 0
	var y uint16 = 0
	{
		b = 44
		var b uint8 = 32
		{
			b = 88
			var b uint16 = 33
			y = b + 2
		}
		x = a + b
		b = 55
	}
	b = b + 1
	return x, y
}
