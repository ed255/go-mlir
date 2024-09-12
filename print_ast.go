package main

import (
	"go/parser"
	"go/token"
	"log"

	"github.com/davecgh/go-spew/spew"
)

func main() {
	// parse file
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, "samples/assign.go", nil, parser.ParseComments|parser.SkipObjectResolution)
	if err != nil {
		log.Fatal(err)
	}

	spew.Config.DisablePointerAddresses = true
	spew.Config.DisableCapacities = true
	spew.Config.DisableMethods = true
	spew.Config.SortKeys = true
	spew.Dump(node)
}
