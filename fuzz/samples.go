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

func entrypoint_define(a uint8) uint8 {
	b := a + 1
	return b
}

func entrypoint_if2(a, b uint8) uint8 {
	var x uint8 = a
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
	var x uint8 = a
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

func entrypoint_if4(
	a uint8,
) uint8 {
	var i uint8
	i = 1
	if a/2 == 0 {
		var j uint8
		if a/3 == 0 {
			i = i + 2
			j = i
			if a/4 == 0 {
				i = i + 4 + j
			}
		} else {
			i = i + 3
		}
		i += 4
	}
	i += 5
	return i
}

func entrypoint_if5(
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

func entrypoint_for(a uint8) uint8 {
	for i := 0; i < 10; i++ {
		a = a + 1
	}
	return a
}

func entrypoint_for2(a uint8) uint8 {
	for i := 0; i < 4; i++ {
		a = a + 1
		if a > 19 {
			break
		}
		a = a + 1
	}
	return a
}
