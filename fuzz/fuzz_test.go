package fuzz

import (
	"fmt"
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
func FuzzUnrollAdd2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_add2(a, b)
		out0_unroll := entrypoint_add2_unroll(a, b)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesAdd2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_add2(a, b)
		out0_rm_branches := entrypoint_add2_rm_branches(a, b)

		assert.Equal(t, out0, out0_rm_branches)
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
func FuzzUnrollAdd(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_add(a, b)
		out0_unroll := entrypoint_add_unroll(a, b)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesAdd(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_add(a, b)
		out0_rm_branches := entrypoint_add_rm_branches(a, b)

		assert.Equal(t, out0, out0_rm_branches)
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
func FuzzUnrollAssign(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_assign(a)
		out0_unroll := entrypoint_assign_unroll(a)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesAssign(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_assign(a)
		out0_rm_branches := entrypoint_assign_rm_branches(a)

		assert.Equal(t, out0, out0_rm_branches)
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
func FuzzUnrollDefine(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_define(a)
		out0_unroll := entrypoint_define_unroll(a)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesDefine(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_define(a)
		out0_rm_branches := entrypoint_define_rm_branches(a)

		assert.Equal(t, out0, out0_rm_branches)
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
func FuzzUnrollIf2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_if2(a, b)
		out0_unroll := entrypoint_if2_unroll(a, b)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesIf2(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_if2(a, b)
		out0_rm_branches := entrypoint_if2_rm_branches(a, b)

		assert.Equal(t, out0, out0_rm_branches)
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
func FuzzUnrollIf3(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if3(a)
		out0_unroll := entrypoint_if3_unroll(a)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesIf3(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if3(a)
		out0_rm_branches := entrypoint_if3_rm_branches(a)

		assert.Equal(t, out0, out0_rm_branches)
	})
}

func FuzzTranslateIf4(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if4(a)
		out0_translate := entrypoint_if4_translate(a)

		assert.Equal(t, out0, out0_translate)
	})
}
func FuzzUnrollIf4(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if4(a)
		out0_unroll := entrypoint_if4_unroll(a)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesIf4(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if4(a)
		out0_rm_branches := entrypoint_if4_rm_branches(a)

		assert.Equal(t, out0, out0_rm_branches)
	})
}

func FuzzTranslateIf5(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if5(a)
		out0_translate := entrypoint_if5_translate(a)

		assert.Equal(t, out0, out0_translate)
	})
}
func FuzzUnrollIf5(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if5(a)
		out0_unroll := entrypoint_if5_unroll(a)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesIf5(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_if5(a)
		out0_rm_branches := entrypoint_if5_rm_branches(a)

		assert.Equal(t, out0, out0_rm_branches)
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
func FuzzUnrollIf(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_if(a, b)
		out0_unroll := entrypoint_if_unroll(a, b)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesIf(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0 := entrypoint_if(a, b)
		out0_rm_branches := entrypoint_if_rm_branches(a, b)

		assert.Equal(t, out0, out0_rm_branches)
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
func FuzzUnrollNest(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0, out1 := entrypoint_nest(a, b)
		out0_unroll, out1_unroll := entrypoint_nest_unroll(a, b)

		assert.Equal(t, out0, out0_unroll)
		assert.Equal(t, out1, out1_unroll)
	})
}
func FuzzRmBranchesNest(f *testing.F) {
	f.Add(uint8(128), uint8(128))
	f.Fuzz(func(t *testing.T, a uint8, b uint8) {
		out0, out1 := entrypoint_nest(a, b)
		out0_rm_branches, out1_rm_branches := entrypoint_nest_rm_branches(a, b)

		assert.Equal(t, out0, out0_rm_branches)
		assert.Equal(t, out1, out1_rm_branches)
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
func FuzzUnrollReturn(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_return(a)
		out0_unroll := entrypoint_return_unroll(a)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesReturn(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_return(a)
		out0_rm_branches := entrypoint_return_rm_branches(a)

		assert.Equal(t, out0, out0_rm_branches)
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
func FuzzUnrollFor(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_for(a)
		out0_unroll := entrypoint_for_unroll(a)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesFor(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_for(a)
		out0_rm_branches := entrypoint_for_rm_branches(a)

		assert.Equal(t, out0, out0_rm_branches)
	})
}

func TestFor2(t *testing.T) {
	for i := 0; i < 256; i++ {
		a := uint8(i)
		v0 := entrypoint_for2(a)
		v1 := entrypoint_for2_unroll(a)
		v2 := entrypoint_for2_rm_branches(a)
		fmt.Printf("%v: %v %v %v\n", a, v0, v1, v2)
	}
}

func FuzzTranslateFor2(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_for2(a)
		out0_translate := entrypoint_for2_translate(a)

		assert.Equal(t, out0, out0_translate)
	})
}
func FuzzUnrollFor2(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_for2(a)
		out0_unroll := entrypoint_for2_unroll(a)

		assert.Equal(t, out0, out0_unroll)
	})
}
func FuzzRmBranchesFor2(f *testing.F) {
	f.Add(uint8(128))
	f.Fuzz(func(t *testing.T, a uint8) {
		out0 := entrypoint_for2(a)
		out0_rm_branches := entrypoint_for2_rm_branches(a)

		assert.Equal(t, out0, out0_rm_branches)
	})
}
