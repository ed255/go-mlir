package ast

import (
	"fmt"
	"os"
	"testing"

	"github.com/davecgh/go-spew/spew"
	test_assert "github.com/stretchr/testify/assert"
)

func TestTranslate(t *testing.T) {
	spew.Config.DisablePointerAddresses = true
	spew.Config.SortKeys = true
	spew.Config.DisableCapacities = true

	pkg, err := TranslateFile("../samples/for.go", nil)
	test_assert.Nil(t, err)
	// spew.Dump(f)
	p := NewPrinterGo(os.Stdout)
	p.Package(&pkg)
}

func TestVerilogTranslate(t *testing.T) {
	spew.Config.DisablePointerAddresses = true
	spew.Config.SortKeys = true
	spew.Config.DisableCapacities = true

	pkg, err := TranslateFile("../samples/add.go", nil)
	test_assert.Nil(t, err)
	// spew.Dump(f)
	p := NewPrinterVerilog(os.Stdout)
	p.Package(&pkg)
}

func TestUnroll(t *testing.T) {
	spew.Config.DisablePointerAddresses = true
	spew.Config.SortKeys = true
	spew.Config.DisableCapacities = true

	pkg, err := TranslateFile("../samples/for4.go", nil)
	test_assert.Nil(t, err)
	p := NewPrinterGo(os.Stdout)
	fmt.Printf("// Translate\n\n")
	p.Package(&pkg)

	pkg, err = Unroll(&pkg)
	test_assert.Nil(t, err)
	// spew.Dump(f)
	p = NewPrinterGo(os.Stdout)
	fmt.Printf("\n// Unroll\n\n")
	p.Package(&pkg)
}
