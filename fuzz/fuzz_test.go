package fuzz

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

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

func entrypoint_if3_t1(
	a uint8,
) (
	_out0 uint8,
) {
	var x_b1 uint8
	x_b1 = 0
	var _0_b1 bool
	_0_b1 = (a / 2) == 0
	// (bid=2) if (a / 2) == 0
	var _1_b2 bool
	_1_b2 = (a / 3) == 0
	// (bid=3) if (a / 3) == 0
	var x_b1_c3 uint8
	x_b1_c3 = x_b1 + 2
	// (bid=4) else
	var x_b1_c4 uint8
	x_b1_c4 = x_b1 + 3
	var x_b1_c2 uint8
	if _1_b2 {
		x_b1_c2 = x_b1_c3
	} else {
		x_b1_c2 = x_b1_c4
	}
	if _0_b1 {
		x_b1 = x_b1_c2
	} else {
		x_b1 = x_b1
	}
	_out0 = x_b1
	return
}

func FuzzIf3(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out := entrypoint_if3(a)
		out_t1 := entrypoint_if3_t1(a)
		assert.Equal(t, out, out_t1)
	})
}
