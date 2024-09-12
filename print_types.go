package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"slices"
	"strings"
)

func main() {
	// parse file
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, "samples/add2.go", nil, parser.ParseComments|parser.SkipObjectResolution)
	if err != nil {
		log.Fatal(err)
	}

	// Type Check test

	info := types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
	}

	var conf types.Config
	pkg, err := conf.Check("main", fset, []*ast.File{node}, &info)
	if err != nil {
		log.Fatal(err)
	}

	// For each named object, print the line and
	// column of its definition and each of its uses.
	fmt.Println("Defs and Uses of each named object:")
	usesByObj := make(map[types.Object][]string)
	for id, obj := range info.Uses {
		posn := fset.Position(id.Pos())
		lineCol := fmt.Sprintf("%d:%d", posn.Line, posn.Column)
		usesByObj[obj] = append(usesByObj[obj], lineCol)
	}
	var items []string
	for obj, uses := range usesByObj {
		slices.Sort(uses)
		item := fmt.Sprintf("%s:\n  defined at %s\n  used at %s",
			types.ObjectString(obj, types.RelativeTo(pkg)),
			fset.Position(obj.Pos()),
			strings.Join(uses, ", "))
		items = append(items, item)
	}
	slices.Sort(items) // sort by line:col, in effect
	fmt.Println(strings.Join(items, "\n"))
	fmt.Println()

	fmt.Println("Types and Values of each expression:")
	items = nil
	for expr, tv := range info.Types {
		var buf strings.Builder
		posn := fset.Position(expr.Pos())
		tvstr := tv.Type.String()
		if tv.Value != nil {
			tvstr += " = " + tv.Value.String()
		}
		// line:col | expr | mode : type = value
		fmt.Fprintf(&buf, "%2d:%2d | %-19s | %-7v : %s",
			posn.Line, posn.Column, types.ExprString(expr),
			tv, tvstr)
		items = append(items, buf.String())
	}
	slices.Sort(items)
	fmt.Println(strings.Join(items, "\n"))
}
