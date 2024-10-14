package ast

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"runtime/debug"
	"strconv"
)

type Translator struct {
	typeInfo    types.Info
	funcResults []Field
}

func NewTranslator(typeInfo types.Info) Translator {
	return Translator{
		typeInfo: typeInfo,
	}
}

func (t *Translator) errcheck(err error) {
	if err != nil {
		panic(err)
	}
}

var (
	Bool   = NewPrimType(false, 1)
	Int    = NewPrimType(true, 32)
	Int8   = NewPrimType(true, 8)
	Int16  = NewPrimType(true, 16)
	Int32  = NewPrimType(true, 32)
	Int64  = NewPrimType(true, 64)
	Uint   = NewPrimType(false, 32)
	Uint8  = NewPrimType(false, 8)
	Uint16 = NewPrimType(false, 16)
	Uint32 = NewPrimType(false, 32)
	Uint64 = NewPrimType(false, 64)
)

func (t *Translator) TypeFromExpr(e ast.Expr) Type {
	typ := t.typeInfo.TypeOf(e)
	if types.Identical(typ, types.Typ[types.Bool]) || types.Identical(typ, types.Typ[types.UntypedBool]) {
		return &Bool
	} else if types.Identical(typ, types.Typ[types.Int]) {
		return &Int
	} else if types.Identical(typ, types.Typ[types.Int8]) {
		return &Int8
	} else if types.Identical(typ, types.Typ[types.Int16]) {
		return &Int16
	} else if types.Identical(typ, types.Typ[types.Int32]) {
		return &Int32
	} else if types.Identical(typ, types.Typ[types.Int64]) {
		return &Int64
	} else if types.Identical(typ, types.Typ[types.Uint]) {
		return &Uint
	} else if types.Identical(typ, types.Typ[types.Uint8]) {
		return &Uint8
	} else if types.Identical(typ, types.Typ[types.Uint16]) {
		return &Uint16
	} else if types.Identical(typ, types.Typ[types.Uint32]) {
		return &Uint32
	} else if types.Identical(typ, types.Typ[types.Uint64]) {
		return &Uint64
	} else {
		panic(fmt.Errorf("unsupported Type %+#v", typ))
	}
}

func (t *Translator) Op(op token.Token) Op {
	switch op {
	case token.ADD:
		return ADD
	case token.SUB:
		return SUB
	case token.MUL:
		return MUL
	case token.QUO:
		return QUO
	case token.REM:
		return REM
	case token.AND:
		return AND
	case token.OR:
		return OR
	case token.XOR:
		return XOR
	case token.SHL:
		return SHL
	case token.SHR:
		return SHR
	default:
		panic(fmt.Errorf("unsupported Token: %v", op))
	}
}

func (t *Translator) BasicLit(basicLit *ast.BasicLit) BasicLit {
	typ := t.TypeFromExpr(basicLit).(*PrimType)
	var value int64
	if *typ == Bool {
		if basicLit.Value == "false" {
			value = 0
		} else {
			value = 1
		}
	} else {
		var err error
		value, err = strconv.ParseInt(basicLit.Value, 0, 64)
		t.errcheck(err)
	}
	return BasicLit{
		Type:  typ,
		Value: value,
	}
}

func (t *Translator) Expr(expr ast.Expr) []Expr {
	var e Expr
	switch expr := expr.(type) {
	case *ast.BinaryExpr:
		e = &BinaryExpr{
			X:  t.Expr(expr.X)[0],
			Op: t.Op(expr.Op),
			Y:  t.Expr(expr.Y)[0],
		}
	case *ast.Ident:
		e = &Ident{
			Name: expr.Name,
		}
	case *ast.BasicLit:
		bl := t.BasicLit(expr)
		e = &bl
	default:
		panic(fmt.Errorf("unsupported Expr: %+T", expr))
	}
	return []Expr{e}
}

func (t *Translator) VarRefFromExpr(expr ast.Expr) VarRef {
	switch expr := expr.(type) {
	case *ast.Ident:
		return VarRef{
			Name: expr.Name,
		}
	default:
		panic(fmt.Errorf("unsupported Expr for VarRef: %+T", expr))
	}
}

func (t *Translator) AssignStmt(assignStmt *ast.AssignStmt) []Stmt {
	var stmts []Stmt
	switch assignStmt.Tok {
	case token.DEFINE:
		i := 0
		for _, rhs := range assignStmt.Rhs {
			rs := t.Expr(rhs)
			for _, r := range rs {
				lhs := t.VarRefFromExpr(assignStmt.Lhs[i])
				declStmt := DeclStmt{
					Decl: &VarDecl{
						Name: lhs.Name,
						Type: t.TypeFromExpr(assignStmt.Lhs[i]),
					},
				}
				assignStmt := AssignStmt{
					Lhs: lhs,
					Rhs: r,
				}
				stmts = append(stmts, &declStmt, &assignStmt)
				i += 1
			}
		}
	case token.ASSIGN:
		i := 0
		for _, rhs := range assignStmt.Rhs {
			rs := t.Expr(rhs)
			for _, r := range rs {
				stmt := AssignStmt{
					Lhs: t.VarRefFromExpr(assignStmt.Lhs[i]),
					Rhs: r,
				}
				stmts = append(stmts, &stmt)
				i += 1
			}
		}
	case token.ADD_ASSIGN:
		panic("TODO")
	default:
		panic("TODO")
	}
	return stmts
}

func (t *Translator) ReturnStmt(returnStmt *ast.ReturnStmt) []AssignStmt {
	i := 0
	var stmts []AssignStmt
	for _, result := range returnStmt.Results {
		rs := t.Expr(result)
		for _, r := range rs {
			stmt := AssignStmt{
				Lhs: VarRef{
					Name: t.funcResults[i].Name,
				},
				Rhs: r,
			}
			stmts = append(stmts, stmt)
		}
	}
	return stmts
}

func (t *Translator) BlockStmt(blockStmt *ast.BlockStmt) BlockStmt {
	var list []Stmt
	for _, stmt := range blockStmt.List {
		switch stmt := stmt.(type) {
		case *ast.ReturnStmt:
			stmts := t.ReturnStmt(stmt)
			for _, stmt := range stmts {
				list = append(list, &stmt)
			}
		case *ast.AssignStmt:
			stmts := t.AssignStmt(stmt)
			list = append(list, stmts...)
		// TODO
		// case *ast.DeclStmt:
		// 	t.DeclStmt(s)
		// TODO
		// case *ast.IfStmt:
		// 	t.IfStmt(s)
		// TODO
		// case *ast.BlockStmt:
		// 	t.PushBlock()
		// 	t.indentLvl += 1
		// 	t.BlockStmt(s)
		// 	t.indentLvl -= 1
		// 	t.PopBlock()
		default:
			panic(fmt.Errorf("unsupported Stmt: %+T", stmt))
		}
	}

	return BlockStmt{
		List: list,
	}
}

func (t *Translator) FuncDecl(funcDecl *ast.FuncDecl) *FuncDecl {
	var params []Field
	for _, field := range funcDecl.Type.Params.List {
		for _, name := range field.Names {
			param := Field{
				Name: name.Name,
				Type: t.TypeFromExpr(field.Type),
			}
			params = append(params, param)
		}
	}

	var results []Field
	for i, field := range funcDecl.Type.Results.List {
		if len(field.Names) == 0 {
			result := Field{
				Name: fmt.Sprintf("_out%v", i),
				Type: t.TypeFromExpr(field.Type),
			}
			results = append(results, result)
		} else {
			for _, name := range field.Names {
				result := Field{
					Name: name.Name,
					Type: t.TypeFromExpr(field.Type),
				}
				results = append(results, result)
			}
		}
	}
	t.funcResults = results

	return &FuncDecl{
		Name: funcDecl.Name.Name,
		Type: FuncType{
			Params:  params,
			Results: results,
		},
		Body: t.BlockStmt(funcDecl.Body),
	}
}

func (t *Translator) File(file *ast.File) File {
	var f File
	for _, n := range file.Decls {
		var d Decl
		switch x := n.(type) {
		case *ast.FuncDecl:
			d = t.FuncDecl(x)
		}
		f.Decls = append(f.Decls, d)
	}
	return f
}

func translate(node ast.Node, typeInfo types.Info) File {
	t := NewTranslator(typeInfo)
	return t.File(node.(*ast.File))
}

func Translate(node ast.Node, typeInfo types.Info) (f File, err error) {
	defer func() {
		if r := recover(); r != nil {
			var ok bool
			err, ok = r.(error)
			if !ok {
				panic(r)
			}

			fmt.Println("DEBUG: Error backtrace:")
			trace := debug.Stack()
			traceLines := bytes.Split(trace, []byte("\n"))
			trace = bytes.Join(traceLines[7:], []byte("\n"))
			fmt.Println(string(trace))
		}
	}()
	f = translate(node, typeInfo)
	return f, nil
}

func TranslateFile(filePath string) (f File, err error) {
	// parse file
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, filePath, nil, parser.ParseComments|parser.SkipObjectResolution)
	if err != nil {
		log.Fatal(err)
	}

	typeInfo := types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
	}

	var typeCfg types.Config
	_, err = typeCfg.Check("main", fset, []*ast.File{node}, &typeInfo)
	if err != nil {
		log.Fatal(err)
	}

	return Translate(node, typeInfo)
}
