package ast

import (
	"os"
	"testing"

	"github.com/davecgh/go-spew/spew"
	"github.com/stretchr/testify/assert"
)

func TestTranslate(t *testing.T) {
	spew.Config.DisablePointerAddresses = true
	spew.Config.SortKeys = true
	spew.Config.DisableCapacities = true

	pkg, err := TranslateFile("../samples/struct.go", nil)
	assert.Nil(t, err)
	// spew.Dump(f)
	p := NewPrinter(os.Stdout, PrinterOpts{GoCompat: true})
	p.Package(&pkg)
}
