package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"io"
	"log"
	"os"
	"runtime/debug"
	"strings"
)

type VarOpt int

const (
	VarNew VarOpt = iota
	VarGet
	VarNext
)

type Var struct {
	name   string
	size   int
	signed bool
}

type Block struct {
	// Mapping of variable name -> SSA version
	vars       map[string]int
	returnVars []Var
}

type Translator struct {
	indentLvl int
	out       io.Writer
	varCnt    int
	constCnt  int
	typeInfo  types.Info
	// Mapping of block depth -> Block
	blocks []Block
}

func NewTranslator(out io.Writer, typeInfo types.Info) Translator {
	return Translator{
		indentLvl: 0,
		varCnt:    0,
		constCnt:  0,
		out:       out,
		typeInfo:  typeInfo,
		blocks:    []Block{},
	}
}

func (t *Translator) PushBlock() {
	t.blocks = append(t.blocks, Block{vars: make(map[string]int)})
}

func (t *Translator) PopBlock() {
	t.blocks = t.blocks[:len(t.blocks)-1]
}

func (t *Translator) DecodeType(nTyp ast.Expr) (bool, int) {
	typ := t.typeInfo.TypeOf(nTyp)
	if types.Identical(typ, types.Typ[types.Bool]) {
		return false, 1
	} else if types.Identical(typ, types.Typ[types.Int]) {
		return true, 32
	} else if types.Identical(typ, types.Typ[types.Int8]) {
		return true, 8
	} else if types.Identical(typ, types.Typ[types.Int16]) {
		return true, 16
	} else if types.Identical(typ, types.Typ[types.Int32]) {
		return true, 32
	} else if types.Identical(typ, types.Typ[types.Int64]) {
		return true, 64
	} else if types.Identical(typ, types.Typ[types.Uint]) {
		return false, 32
	} else if types.Identical(typ, types.Typ[types.Uint8]) {
		return false, 8
	} else if types.Identical(typ, types.Typ[types.Uint16]) {
		return false, 16
	} else if types.Identical(typ, types.Typ[types.Uint32]) {
		return false, 32
	} else if types.Identical(typ, types.Typ[types.Uint64]) {
		return false, 64
	} else {
		panic(fmt.Errorf("unsupported Type %+#v", typ))
	}
}

func (t *Translator) curBlock() *Block {
	return &t.blocks[len(t.blocks)-1]
}

func (t *Translator) NewVar(name string, nTyp ast.Expr) *Var {
	t.curBlock().vars[name] = 0

	signed, size := t.DecodeType(nTyp)
	return &Var{
		name:   fmt.Sprintf("%v0", name),
		signed: signed,
		size:   size,
	}
}

func (t *Translator) GetVar(name string) *Var {
	ver, ok := t.curBlock().vars[name]
	if !ok {
		panic(fmt.Errorf("Var \"%v\" not found in current block", name))
	}
	return &Var{
		name: fmt.Sprintf("%v%v", name, ver),
	}
}

func (t *Translator) GetVarNextVer(name string) *Var {
	block := t.curBlock()
	ver, ok := block.vars[name]
	if !ok {
		panic(fmt.Errorf("Var \"%v\" not found in current block", name))
	}
	block.vars[name] = ver + 1
	return &Var{
		name: fmt.Sprintf("%v%v", name, ver+1),
	}
}

func (t *Translator) NewAnonVar(nTyp ast.Expr) *Var {
	signed, size := t.DecodeType(nTyp)
	v := Var{
		name:   fmt.Sprintf("_%v_", t.varCnt),
		signed: signed,
		size:   size,
	}
	t.varCnt += 1
	return &v
}

func (t *Translator) NewAnonConst() *Var {
	v := Var{
		name: fmt.Sprintf("c%v", t.constCnt),
	}
	t.constCnt += 1
	return &v
}

func (t *Translator) errcheck(err error) {
	if err != nil {
		panic(err)
	}
}

func (t *Translator) Printf(format string, a ...any) {
	indent := strings.Repeat("  ", t.indentLvl)
	_, err := fmt.Fprintf(t.out, "%v"+format, append([]any{indent}, a...)...)
	t.errcheck(err)
}

func (t *Translator) EmitExpr(x ast.Expr, vrFn func() *Var, varOpt VarOpt) Var {
	var vr *Var
	switch s := x.(type) {
	case *ast.BinaryExpr:
		vx := t.EmitExpr(s.X, nil, VarGet)
		vy := t.EmitExpr(s.Y, nil, VarGet)
		if vrFn != nil {
			vr = vrFn()
		} else {
			vr = t.NewAnonVar(s)
			t.Printf("%v %v;\n", typeFmt(vr.signed, vr.size), vr.name)
		}
		op := t.TranslateOp(s.Op)
		t.Printf("assign %v = %v %v %v;\n", vr.name, vx.name, op, vy.name)
		return *vr
	case *ast.Ident:
		if vrFn != nil {
			vr = vrFn()
		} else {
			switch varOpt {
			case VarNew:
				vr = t.NewVar(s.Name, nil)
			case VarGet:
				vr = t.GetVar(s.Name)
			case VarNext:
				vr = t.GetVarNextVer(s.Name)
			}
		}
		return *vr
	case *ast.BasicLit:
		ty := t.TranslateType(x)
		if vrFn != nil {
			vr = vrFn()
		} else {
			vr = t.NewAnonConst()
		}
		t.Printf("%%%v = arith.constant %v : %v\n", vr.name, s.Value, ty)
		return *vr
	default:
		panic(fmt.Errorf("unsupported Expr: %+T", x))
	}
}

func (t *Translator) EmitReturnStmt(x *ast.ReturnStmt) {
	if len(x.Results) == 0 {
		return
	} else if len(x.Results) != 1 {
		panic(fmt.Errorf("unsupported"))
	}
	block := t.curBlock()
	for i, r := range x.Results {
		v := t.EmitExpr(r, nil, VarGet)
		t.Printf("assign %v = %v;\n", block.returnVars[i].name, v.name)
	}
}

func (t *Translator) EmitAssignStmt(x *ast.AssignStmt) {
	if len(x.Lhs) != 1 || len(x.Rhs) != 1 {
		panic(fmt.Errorf("unsupported"))
	}
	lhs, rhs := x.Lhs[0], x.Rhs[0]
	if x.Tok == token.DEFINE {
		vlhs := t.EmitExpr(lhs, nil, VarNew)
		_ = vlhs
		panic("TODO")
	}
	vlhsFn := func() *Var { vr := t.EmitExpr(lhs, nil, VarNext); return &vr }
	switch x.Tok {
	case token.ASSIGN:
		t.EmitExpr(rhs, vlhsFn, VarGet)
	case token.ADD_ASSIGN:
		panic("TODO")
	default:
		panic("TODO")
	}
}

func (t *Translator) EmitBlockStmt(x *ast.BlockStmt) {
	for _, stmt := range x.List {
		switch s := stmt.(type) {
		case *ast.ReturnStmt:
			t.EmitReturnStmt(s)
		case *ast.AssignStmt:
			t.EmitAssignStmt(s)
		default:
			panic(fmt.Errorf("unsupported Stmt: %+T", stmt))
		}
	}
	return
}

func (t *Translator) TranslateOp(op token.Token) string {
	switch op {
	case token.ADD:
		return "+"
	case token.SUB:
		return "-"
	case token.MUL:
		return "*"
	case token.QUO:
		// TODO: Case for signed based on expression type
		return "/"
	case token.REM:
		// TODO: Case for signed based on expression type
		return "%"
	case token.AND:
		return "&"
	case token.OR:
		return "|"
	case token.XOR:
		return "^"
	case token.SHL:
		return "<<"
	case token.SHR:
		// TODO: Case for signed based on expression type
		return ">>"
	default:
		panic(fmt.Errorf("unsupported Token: %v", op))
	}
}

func (t *Translator) TranslateType(n ast.Expr) string {
	typ := t.typeInfo.TypeOf(n)
	if types.Identical(typ, types.Typ[types.Bool]) {
		return "i1"
	} else if types.Identical(typ, types.Typ[types.Int]) {
		return "i32"
	} else if types.Identical(typ, types.Typ[types.Int8]) {
		return "i8"
	} else if types.Identical(typ, types.Typ[types.Int16]) {
		return "i16"
	} else if types.Identical(typ, types.Typ[types.Int32]) {
		return "i32"
	} else if types.Identical(typ, types.Typ[types.Int64]) {
		return "i64"
	} else if types.Identical(typ, types.Typ[types.Uint]) {
		return "i32"
	} else if types.Identical(typ, types.Typ[types.Uint8]) {
		return "i8"
	} else if types.Identical(typ, types.Typ[types.Uint16]) {
		return "i16"
	} else if types.Identical(typ, types.Typ[types.Uint32]) {
		return "i32"
	} else if types.Identical(typ, types.Typ[types.Uint64]) {
		return "i64"
	} else {
		panic(fmt.Errorf("unsupported Type %+#v", typ))
	}
}

func typeFmt(signed bool, size int) string {
	if signed {
		return fmt.Sprintf("signed wire [%v:0]", size)
	} else {
		return fmt.Sprintf("wire [%v:0]", size)
	}
}

func (t *Translator) EmitFuncDecl(x *ast.FuncDecl) {
	t.PushBlock()
	block := t.curBlock()
	t.Printf("module %v (\n", x.Name)
	for _, field := range x.Type.Params.List {
		for _, name := range field.Names {
			v := t.NewVar(name.Name, field.Type)
			t.Printf("  input %v %v,\n", typeFmt(v.signed, v.size), v.name)
		}
	}
	for i, field := range x.Type.Results.List {
		if len(field.Names) != 0 {
			panic(fmt.Errorf("unsupported return parameter names"))
		}
		v := t.NewVar(fmt.Sprintf("_out%v_", i), field.Type)
		t.Printf("  output %v %v,\n", typeFmt(v.signed, v.size), v.name)
		block.returnVars = append(block.returnVars, *v)
	}
	t.Printf(");\n")

	t.indentLvl += 1
	t.EmitBlockStmt(x.Body)
	t.indentLvl -= 1

	t.Printf("endmodule\n\n")
	t.PopBlock()
}

func (t *Translator) emit(node ast.Node) {
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			t.EmitFuncDecl(x)
		}
		return true
	})
}

func (t *Translator) Emit(node ast.Node) (err error) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("DEBUG: Error backtrace:")
			trace := debug.Stack()
			traceLines := bytes.Split(trace, []byte("\n"))
			trace = bytes.Join(traceLines[7:], []byte("\n"))
			fmt.Println(string(trace))
			err = r.(error)
		}
	}()
	t.emit(node)
	return nil
}

func main() {
	// parse file
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, "samples/add.go", nil, parser.ParseComments|parser.SkipObjectResolution)
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

	t := NewTranslator(os.Stdout, typeInfo)
	err = t.Emit(node)
	if err != nil {
		log.Fatal(err)
	}
}
