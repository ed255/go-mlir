package ast

import (
	"os"
	"testing"

	"github.com/davecgh/go-spew/spew"
	test_assert "github.com/stretchr/testify/assert"
)

func TestGoTranslate(t *testing.T) {
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

	pkg, err := TranslateFile("../samples/struct.go", nil)
	test_assert.Nil(t, err)
	// spew.Dump(f)
	p := NewPrinterVerilog(os.Stdout)
	p.Package(&pkg)
}
