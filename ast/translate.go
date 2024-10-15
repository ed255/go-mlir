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
	BlockId int
}

func (v *Var) Name() string {
	if v.BlockId == 0 {
		return v.SrcName
	} else {
		return fmt.Sprintf("%v_b%v", v.SrcName, v.BlockId)
	}
}

type Block struct {
	id int
	// Map from src var name to block Var
	vars map[string]*Var
	// true if we're in a branch block
	branch bool
}

type Translator struct {
	typeInfo    types.Info
	funcResults []Field
	blocks      []Block
	curFuncBody []Stmt
	lvl         int
	blockCnt    int
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

func (t *Translator) CurBlock() *Block {
	return &t.blocks[len(t.blocks)-1]
}

func (t *Translator) PushStmt(s ...Stmt) {
	t.curFuncBody = append(t.curFuncBody, s...)
}

func (t *Translator) PushBlock() {
	id := t.blockCnt
	t.lvl += 1
	t.PushStmt(&MetaStmt{
		Meta: &LvlDelta{Delta: +1},
	})
	t.blockCnt += 1
	t.blocks = append(t.blocks, Block{id: id, vars: make(map[string]*Var)})
}

func (t *Translator) PopBlock() Block {
	t.lvl -= 1
	t.PushStmt(&MetaStmt{
		Meta: &LvlDelta{Delta: -1},
	})
	block := t.blocks[len(t.blocks)-1]
	t.blocks = t.blocks[:len(t.blocks)-1]
	return block
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
		v := t.GetVar(expr.Name)
		e = &Ident{
			Name: v.Name(),
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
	v := t.GetVar(name)
	return VarRef{
		Name: v.Name(),
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

func (t *Translator) AddVar(name string) Var {
	curBlock := t.CurBlock()
	if _, ok := curBlock.vars[name]; ok {
		panic(fmt.Errorf("Var %v already exists in current block", name))
	}
	v := Var{
		SrcName: name,
		BlockId: curBlock.id,
	}
	if _, ok := curBlock.vars[v.Name()]; ok {
		panic(fmt.Errorf("Var.Name() %v already exists in current block", v.Name()))
	}
	curBlock.vars[name] = &v
	return v
}

func (t *Translator) GetVar(name string) *Var {
	for i := 0; i < len(t.blocks); i++ {
		depth := len(t.blocks) - 1 - i
		block := t.blocks[depth]
		v, ok := block.vars[name]
		if ok {
			return v
		}
	}
	panic(fmt.Errorf("Var %v not found in any block", name))
}

func (t *Translator) GenVarDecl(name string, typ Type) *VarDecl {
	v := t.AddVar(name)
	return &VarDecl{
		Name: v.Name(),
		Type: typ,
	}
}

func (t *Translator) AssignStmt(assignStmt *ast.AssignStmt) {
	switch assignStmt.Tok {
	case token.DEFINE:
		i := 0
		for _, rhs := range assignStmt.Rhs {
			rs := t.Expr(rhs)
			for _, r := range rs {
				lhs := t.VarRefFromExpr(assignStmt.Lhs[i])
				declStmt := DeclStmt{
					Decl: t.GenVarDecl(
						lhs.Name,
						t.TypeFromExpr(assignStmt.Lhs[i]),
					),
				}
				assignStmt := AssignStmt{
					Lhs: lhs,
					Rhs: r,
				}
				t.PushStmt(&declStmt, &assignStmt)
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
				t.PushStmt(&stmt)
				i += 1
			}
		}
	case token.ADD_ASSIGN:
		panic("TODO")
	default:
		panic("TODO")
	}
}

func (t *Translator) ReturnStmt(returnStmt *ast.ReturnStmt) {
	i := 0
	for _, result := range returnStmt.Results {
		rs := t.Expr(result)
		for _, r := range rs {
			stmt := AssignStmt{
				Lhs: t.VarRef(t.funcResults[i].Name),
				Rhs: r,
			}
			t.PushStmt(&stmt)
		}
	}
}

func (t *Translator) ValueSpec(valueSpec *ast.ValueSpec) {
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
			t.PushStmt(&declStmt)
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
				t.PushStmt(&declStmt, &assignStmt)
				i += 1
			}
		}
	}
}

func (t *Translator) DeclStmt(declStmt *ast.DeclStmt) {
	switch declStmt := declStmt.Decl.(type) {
	case *ast.GenDecl:
		switch declStmt.Tok {
		case token.VAR:
			for _, spec := range declStmt.Specs {
				valueSpec := spec.(*ast.ValueSpec)
				t.ValueSpec(valueSpec)
			}
		default:
			panic(fmt.Errorf("unsupported GenDecl token: %v", declStmt.Tok))
		}

	default:
		panic(fmt.Errorf("unsupported Decl: %T", declStmt))
	}
}

func (t *Translator) IfStmt(ifStmt *ast.IfStmt) {
	// if ifStmt.Init != nil {
	// 	panic(fmt.Errorf("unsupported IfStmt.Init"))
	// }
	// cond := t.Expr(ifStmt.Cond)

}

func (t *Translator) BlockStmt(blockStmt *ast.BlockStmt) {
	for _, stmt := range blockStmt.List {
		switch stmt := stmt.(type) {
		case *ast.ReturnStmt:
			t.ReturnStmt(stmt)
		case *ast.AssignStmt:
			t.AssignStmt(stmt)
		case *ast.DeclStmt:
			t.DeclStmt(stmt)
		case *ast.IfStmt:
			t.IfStmt(stmt)
		case *ast.BlockStmt:
			t.PushBlock()
			t.BlockStmt(stmt)
			t.PopBlock()
		default:
			panic(fmt.Errorf("unsupported Stmt: %+T", stmt))
		}
	}
}

func (t *Translator) FuncDecl(funcDecl *ast.FuncDecl) *FuncDecl {
	t.PushBlock()
	defer t.PopBlock()
	var params []Field
	for _, field := range funcDecl.Type.Params.List {
		for _, name := range field.Names {
			v := t.AddVar(name.Name)
			param := Field{
				Name: v.Name(),
				Type: t.TypeFromExpr(field.Type),
			}
			params = append(params, param)
		}
	}

	var results []Field
	for i, field := range funcDecl.Type.Results.List {
		if len(field.Names) == 0 {
			v := t.AddVar(fmt.Sprintf("_out%v", i))
			result := Field{
				Name: v.Name(),
				Type: t.TypeFromExpr(field.Type),
			}
			results = append(results, result)
		} else {
			for _, name := range field.Names {
				v := t.AddVar(name.Name)
				result := Field{
					Name: v.Name(),
					Type: t.TypeFromExpr(field.Type),
				}
				results = append(results, result)
			}
		}
	}
	t.funcResults = results

	t.curFuncBody = []Stmt{}
	t.PushBlock()
	t.BlockStmt(funcDecl.Body)
	t.PopBlock()
	return &FuncDecl{
		Name: funcDecl.Name.Name,
		Type: FuncType{
			Params:  params,
			Results: results,
		},
		Body: t.curFuncBody,
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
