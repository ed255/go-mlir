package main

func entrypoint(
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
