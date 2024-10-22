package fuzz

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func FuzzTranslateAdd2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_add2(a, b)
		out0_translate := entrypoint_add2_translate(a, b)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateAdd(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_add(a, b)
		out0_translate := entrypoint_add_translate(a, b)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateAssign(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_assign(a)
		out0_translate := entrypoint_assign_translate(a)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateAssign2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_assign2(a, b)
		out0_translate := entrypoint_assign2_translate(a, b)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateDefine(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_define(a)
		out0_translate := entrypoint_define_translate(a)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateFunc(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_func(a, b)
		out0_translate := entrypoint_func_translate(a, b)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateFunc2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_func2(a, b)
		out0_translate := entrypoint_func2_translate(a, b)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateIf2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_if2(a, b)
		out0_translate := entrypoint_if2_translate(a, b)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateIf3(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if3(a)
		out0_translate := entrypoint_if3_translate(a)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateIf(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_if(a, b)
		out0_translate := entrypoint_if_translate(a, b)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateNest(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0, out1 := entrypoint_nest(a, b)
		out0_translate, out1_translate := entrypoint_nest_translate(a, b)
		
		assert.Equal(t, out0, out0_translate)
		assert.Equal(t, out1, out1_translate)
	})
}

func FuzzTranslateReturn(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_return(a)
		out0_translate := entrypoint_return_translate(a)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateStruct(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_struct(a, b)
		out0_translate := entrypoint_struct_translate(a, b)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateVar(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_var(a)
		out0_translate := entrypoint_var_translate(a)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateFor(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_for(a)
		out0_translate := entrypoint_for_translate(a)
		
		assert.Equal(t, out0, out0_translate)
	})
}

func FuzzTranslateFor2(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_for2(a)
		out0_translate := entrypoint_for2_translate(a)
		
		assert.Equal(t, out0, out0_translate)
	})
}

