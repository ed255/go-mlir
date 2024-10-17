package fuzz

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func FuzzAdd2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_add2(a, b)
		out0_t1 := entrypoint_add2_t1(a, b)
		
		assert.Equal(t, out0, out0_t1)
	})
}

func FuzzAdd(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_add(a, b)
		out0_t1 := entrypoint_add_t1(a, b)
		
		assert.Equal(t, out0, out0_t1)
	})
}

func FuzzAssign(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_assign(a)
		out0_t1 := entrypoint_assign_t1(a)
		
		assert.Equal(t, out0, out0_t1)
	})
}

func FuzzIf2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_if2(a, b)
		out0_t1 := entrypoint_if2_t1(a, b)
		
		assert.Equal(t, out0, out0_t1)
	})
}

func FuzzIf3(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if3(a)
		out0_t1 := entrypoint_if3_t1(a)
		
		assert.Equal(t, out0, out0_t1)
	})
}

func FuzzIf(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_if(a, b)
		out0_t1 := entrypoint_if_t1(a, b)
		
		assert.Equal(t, out0, out0_t1)
	})
}

func FuzzNest(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0, out1 := entrypoint_nest(a, b)
		out0_t1, out1_t1 := entrypoint_nest_t1(a, b)
		
		assert.Equal(t, out0, out0_t1)
		assert.Equal(t, out1, out1_t1)
	})
}

func FuzzReturn(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_return(a)
		out0_t1 := entrypoint_return_t1(a)
		
		assert.Equal(t, out0, out0_t1)
	})
}

