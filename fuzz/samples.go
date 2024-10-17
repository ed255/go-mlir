package fuzz


func entrypoint_add2(a, b uint8) uint8 {
	return a + b + b
}

func entrypoint_add(a, b uint8) uint8 {
	return a + b
}

func entrypoint_assign(a uint8) uint8 {
	a = a + 1
	a = a + 2
	a = a + 3
	return a
}

func entrypoint_if2(a, b uint8) uint8 {
	var x uint8 = 0
	if a > 5 {
		if a > 10 {
			x = x + 2
		}
	}
	return x
}

func entrypoint_if3(
	a uint8,
) uint8 {
	var x uint8 = 0
	if a/2 == 0 {
		// x = x + b
		if a/3 == 0 {
			x = x + 2
		} else {
			x = x + 3
		}
	}
	return x
}

func entrypoint_if(a, b uint8) uint8 {
	var x uint8 = 0
	if a == 0 {
		x = x + b
		x = x + 2
		var x uint8
		x = 7
		b = x
	}
	return x
}

func entrypoint_nest(a, b uint8) (uint8, uint16) {
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

func entrypoint_return(a uint8) uint8 {
	return a
}
