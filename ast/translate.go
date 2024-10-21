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

type Var struct {
	SrcName string
	Type    Type
}

type Translator struct {
	typeInfo    types.Info
	funcResults []Field
	lvl         int
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
	} else if types.Identical(typ, types.Typ[types.Int64]) || types.Identical(typ, types.Typ[types.UntypedInt]) {
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
	case token.EQL:
		return EQL
	case token.LSS:
		return LSS
	case token.GTR:
		return GTR
	case token.NEQ:
		return NEQ
	case token.LEQ:
		return LEQ
	case token.GEQ:
		return GEQ
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

func (t *Translator) VarRef(name string) VarRef {
	return VarRef{
		Name: name,
	}
}

func (t *Translator) VarRefFromExpr(expr ast.Expr) VarRef {
	switch expr := expr.(type) {
	case *ast.Ident:
		return t.VarRef(expr.Name)
	default:
		panic(fmt.Errorf("unsupported Expr for VarRef: %+T", expr))
	}
}

func (t *Translator) GenVarDecl(name string, typ Type) *VarDecl {
	return &VarDecl{
		Name: name,
		Type: typ,
	}
}

func (t *Translator) AssignStmt(assignStmt *ast.AssignStmt) []Stmt {
	var ss Stmts
	switch assignStmt.Tok {
	case token.DEFINE:
		i := 0
		for _, rhs := range assignStmt.Rhs {
			rs := t.Expr(rhs)
			for _, r := range rs {
				lhs := assignStmt.Lhs[i].(*ast.Ident)
				declStmt := DeclStmt{
					Decl: t.GenVarDecl(
						lhs.Name,
						t.TypeFromExpr(assignStmt.Lhs[i]),
					),
				}
				assignStmt := AssignStmt{
					Lhs: t.VarRef(lhs.Name),
					Rhs: r,
				}
				ss.Push(&declStmt, &assignStmt)
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
				ss.Push(&stmt)
				i += 1
			}
		}
	case token.ADD_ASSIGN:
		panic("TODO")
	default:
		panic("TODO")
	}
	return ss.List
}

func (t *Translator) ReturnStmt(returnStmt *ast.ReturnStmt) []Stmt {
	var ss Stmts
	i := 0
	for _, result := range returnStmt.Results {
		rs := t.Expr(result)
		for _, r := range rs {
			stmt := AssignStmt{
				Lhs: t.VarRef(t.funcResults[i].Name),
				Rhs: r,
			}
			i += 1
			ss.Push(&stmt)
		}
	}
	ss.Push(&ReturnStmt{})
	return ss.List
}

func (t *Translator) ValueSpec(valueSpec *ast.ValueSpec) []Stmt {
	var ss Stmts
	var lhss []string
	for _, name := range valueSpec.Names {
		lhs := name.Name
		lhss = append(lhss, lhs)
	}

	if valueSpec.Values == nil {
		for _, lhs := range lhss {
			declStmt := DeclStmt{
				Decl: t.GenVarDecl(
					lhs,
					t.TypeFromExpr(valueSpec.Type),
				),
			}
			ss.Push(&declStmt)
		}
	} else {
		i := 0
		for _, value := range valueSpec.Values {
			rs := t.Expr(value)
			for _, r := range rs {
				declStmt := DeclStmt{
					Decl: t.GenVarDecl(
						lhss[i],
						t.TypeFromExpr(value),
					),
				}
				assignStmt := AssignStmt{
					Lhs: t.VarRef(lhss[i]),
					Rhs: r,
				}
				ss.Push(&declStmt, &assignStmt)
				i += 1
			}
		}
	}
	return ss.List
}

func (t *Translator) DeclStmt(declStmt *ast.DeclStmt) []Stmt {
	var ss Stmts
	switch declStmt := declStmt.Decl.(type) {
	case *ast.GenDecl:
		switch declStmt.Tok {
		case token.VAR:
			for _, spec := range declStmt.Specs {
				valueSpec := spec.(*ast.ValueSpec)
				ss.Push(t.ValueSpec(valueSpec)...)
			}
		default:
			panic(fmt.Errorf("unsupported GenDecl token: %v", declStmt.Tok))
		}

	default:
		panic(fmt.Errorf("unsupported Decl: %T", declStmt))
	}
	return ss.List
}

func (t *Translator) IfStmt(ifStmt *ast.IfStmt) *IfStmt {
	if ifStmt.Init != nil {
		panic(fmt.Errorf("unsupported IfStmt.Init"))
	}
	cond := t.Expr(ifStmt.Cond)[0]
	body := t.BlockStmt(ifStmt.Body)
	var es Stmt
	switch elseStmt := ifStmt.Else.(type) {
	case *ast.BlockStmt:
		es = t.BlockStmt(elseStmt)
	case *ast.IfStmt:
		es = t.IfStmt(elseStmt)
	case nil:
	default:
		panic("unreachable")
	}
	return &IfStmt{
		Cond: cond,
		Body: body,
		Else: es,
	}
}

type Stmts struct {
	List []Stmt
}

func (ss *Stmts) Push(s ...Stmt) {
	ss.List = append(ss.List, s...)
}

func (t *Translator) Stmt(stmt ast.Stmt) []Stmt {
	var ss Stmts
	switch stmt := stmt.(type) {
	case *ast.ReturnStmt:
		ss.Push(t.ReturnStmt(stmt)...)
	case *ast.AssignStmt:
		ss.Push(t.AssignStmt(stmt)...)
	case *ast.DeclStmt:
		ss.Push(t.DeclStmt(stmt)...)
	case *ast.IfStmt:
		ss.Push(t.IfStmt(stmt))
	case *ast.BlockStmt:
		ss.Push(t.BlockStmt(stmt))
	default:
		panic(fmt.Errorf("unsupported Stmt: %+T", stmt))
	}
	return ss.List
}

func (t *Translator) BlockStmt(blockStmt *ast.BlockStmt) *BlockStmt {
	var ss Stmts
	for _, stmt := range blockStmt.List {
		ss.Push(t.Stmt(stmt)...)
	}
	return &BlockStmt{
		List: ss.List,
	}
}

func (t *Translator) FuncDecl(funcDecl *ast.FuncDecl) *FuncDecl {
	var params []Field
	for _, field := range funcDecl.Type.Params.List {
		for _, name := range field.Names {
			typ := t.TypeFromExpr(field.Type)
			param := Field{Name: name.Name, Type: typ}
			params = append(params, param)
		}
	}

	var results []Field
	for i, field := range funcDecl.Type.Results.List {
		if len(field.Names) == 0 {
			typ := t.TypeFromExpr(field.Type)
			name := fmt.Sprintf("_out%v", i)
			result := Field{Name: name, Type: typ}
			results = append(results, result)
		} else {
			for _, name := range field.Names {
				typ := t.TypeFromExpr(field.Type)
				result := Field{Name: name.Name, Type: typ}
				results = append(results, result)
			}
		}
	}
	t.funcResults = results

	bs := t.BlockStmt(funcDecl.Body)
	return &FuncDecl{
		Name: funcDecl.Name.Name,
		Type: FuncType{
			Params:  params,
			Results: results,
		},
		Body: bs,
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

func TranslateFile(filePath string, src any) (f File, err error) {
	// parse file
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, filePath, src, parser.ParseComments|parser.SkipObjectResolution)
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
