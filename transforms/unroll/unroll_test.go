package unroll

import (
	"fmt"
	"gocircuit/ast"
	"os"
	"testing"

	"github.com/davecgh/go-spew/spew"
	test_assert "github.com/stretchr/testify/assert"
)

func TestUnroll(t *testing.T) {
	spew.Config.DisablePointerAddresses = true
	spew.Config.SortKeys = true
	spew.Config.DisableCapacities = true

	pkg, err := ast.TranslateFile("../../samples/for5.go", nil)
	test_assert.Nil(t, err)
	p := ast.NewPrinterGo(os.Stdout)
	fmt.Printf("// Translate\n\n")
	p.Package(&pkg)

	pkg, err = Unroll(&pkg)
	test_assert.Nil(t, err)
	// spew.Dump(f)
	p = ast.NewPrinterGo(os.Stdout)
	fmt.Printf("\n// Unroll\n\n")
	p.Package(&pkg)
}
