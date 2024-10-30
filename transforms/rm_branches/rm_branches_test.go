package rm_branches

import (
	"fmt"
	"gocircuit/ast"
	"gocircuit/transforms/unroll"
	"os"
	"testing"

	"github.com/davecgh/go-spew/spew"
	test_assert "github.com/stretchr/testify/assert"
)

func TestRmBranches(t *testing.T) {
	spew.Config.DisablePointerAddresses = true
	spew.Config.SortKeys = true
	spew.Config.DisableCapacities = true

	pkg, err := ast.TranslateFile("../../samples/if5.go", nil)
	test_assert.Nil(t, err)
	p := ast.NewPrinterGo(os.Stdout)
	fmt.Printf("// Translate\n\n")
	p.Package(&pkg)

	pkg, err = unroll.Unroll(&pkg)
	test_assert.Nil(t, err)
	p = ast.NewPrinterGo(os.Stdout)
	fmt.Printf("\n// Unroll\n\n")
	p.Package(&pkg)

	pkg, err = RmBranches(&pkg)
	test_assert.Nil(t, err)
	p = ast.NewPrinterGo(os.Stdout)
	fmt.Printf("\n// RmBranches\n\n")
	p.Package(&pkg)
}
