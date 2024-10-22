package ast

import (
	"fmt"
	"os"
	"testing"

	"github.com/davecgh/go-spew/spew"
	"github.com/stretchr/testify/assert"
)

func TestTranslate(t *testing.T) {
	spew.Config.DisablePointerAddresses = true
	spew.Config.SortKeys = true
	spew.Config.DisableCapacities = true

	pkg, err := TranslateFile("../samples/array.go", nil)
	assert.Nil(t, err)
	// spew.Dump(f)
	p := NewPrinter(os.Stdout, PrinterOpts{GoCompat: true})
	p.Package(&pkg)
}

func TestUnroll(t *testing.T) {
	spew.Config.DisablePointerAddresses = true
	spew.Config.SortKeys = true
	spew.Config.DisableCapacities = true

	pkg, err := TranslateFile("../samples/for2.go", nil)
	assert.Nil(t, err)
	p := NewPrinter(os.Stdout, PrinterOpts{GoCompat: true})
	fmt.Printf("// Translate\n\n")
	p.Package(&pkg)

	pkg, err = Unroll(&pkg)
	assert.Nil(t, err)
	// spew.Dump(f)
	p = NewPrinter(os.Stdout, PrinterOpts{GoCompat: true})
	fmt.Printf("\n// Unroll\n\n")
	p.Package(&pkg)
}
