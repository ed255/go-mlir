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

func entrypoint_func(a, b uint8) uint8 {
	return add_func(a, b)
}

func add_func(a, b uint8) uint8 {
	return a + b
}

func entrypoint_func2(a, b uint8) uint8 {
	var x, y = add_func2(a, b)
	return x + y
}

func add_func2(a, b uint8) (uint8, uint8) {
	return a, a + b
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
		} else if x == 0 {
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

type Data_struct struct {
	a uint8
	b uint8
}

func entrypoint_struct(a, b uint8) uint8 {
	d := Data_struct{a: a, b: b}
	d.a = d.a + 1
	r := add_struct(d)
	return r
}

func add_struct(d Data_struct) uint8 {
	return d.a + d.b
}

func entrypoint_var(a uint8) uint8 {
	var b = uint8(0)
	b = a + 1
	return b
}

func entrypoint_for(a uint8) uint8 {
	for i := 0; i < 10; i++ {
		a = a + 1
	}
	return a
}

func entrypoint_for2(a uint8) uint8 {
	for i := 0; i < 10; i++ {
		a = a + 1
		if a > 19 {
			break
		}
	}
	return a
}
