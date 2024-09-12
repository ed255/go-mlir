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
	name string
}

type Translator struct {
	indentLvl int
	out       io.Writer
	varCnt    int
	constCnt  int
	typeInfo  types.Info
	// Mapping of block depth -> variable name -> SSA version
	blockVars []map[string]int
}

func NewTranslator(out io.Writer, typeInfo types.Info) Translator {
	return Translator{
		indentLvl: 0,
		varCnt:    0,
		constCnt:  0,
		out:       out,
		typeInfo:  typeInfo,
		blockVars: []map[string]int{},
	}
}

func (t *Translator) PushBlock() {
	t.blockVars = append(t.blockVars, make(map[string]int))
}

func (t *Translator) PopBlock() {
	t.blockVars = t.blockVars[:len(t.blockVars)-1]
}

func (t *Translator) NewVar(name string) *Var {
	curBlock := t.blockVars[len(t.blockVars)-1]
	curBlock[name] = 0
	return &Var{
		name: fmt.Sprintf("%v0", name),
	}
}

func (t *Translator) GetVar(name string) *Var {
	curBlock := t.blockVars[len(t.blockVars)-1]
	ver, ok := curBlock[name]
	if !ok {
		panic(fmt.Errorf("Var \"%v\" not found in current block", name))
	}
	return &Var{
		name: fmt.Sprintf("%v%v", name, ver),
	}
}

func (t *Translator) GetVarNextVer(name string) *Var {
	curBlock := t.blockVars[len(t.blockVars)-1]
	ver, ok := curBlock[name]
	if !ok {
		panic(fmt.Errorf("Var \"%v\" not found in current block", name))
	}
	curBlock[name] = ver + 1
	return &Var{
		name: fmt.Sprintf("%v%v", name, ver+1),
	}
}

func (t *Translator) NewAnonVar() *Var {
	v := Var{
		name: fmt.Sprintf("%v", t.varCnt),
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
			vr = t.NewAnonVar()
		}
		op := t.TranslateOp(s.Op)
		ty := t.TranslateType(x)
		t.Printf("%%%v = %v %%%v, %%%v : %v\n", vr.name, op, vx.name, vy.name, ty)
		return *vr
	case *ast.Ident:
		if vrFn != nil {
			vr = vrFn()
		} else {
			switch varOpt {
			case VarNew:
				vr = t.NewVar(s.Name)
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
		t.Printf("return\n")
		return
	} else if len(x.Results) != 1 {
		panic(fmt.Errorf("unsupported"))
	}
	v := t.EmitExpr(x.Results[0], nil, VarGet)
	ty := t.TranslateType(x.Results[0])
	t.Printf("return %%%v : %v\n", v.name, ty)
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
		return "arith.addi"
	case token.SUB:
		return "arith.subi"
	case token.MUL:
		return "arith.muli"
	case token.QUO:
		// TODO: Case for signed based on expression type
		return "arith.divui"
	case token.REM:
		// TODO: Case for signed based on expression type
		return "arith.remui"
	case token.AND:
		return "arith.andi"
	case token.OR:
		return "arith.ori"
	case token.XOR:
		return "arith.xori"
	case token.SHL:
		return "arith.shli"
	case token.SHR:
		// TODO: Case for signed based on expression type
		return "arith.shrui"
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

func (t *Translator) EmitFuncDecl(x *ast.FuncDecl) {
	t.PushBlock()
	line := fmt.Sprintf("func.func @%v(", x.Name)
	i := 0
	for _, field := range x.Type.Params.List {
		for _, name := range field.Names {
			if i != 0 {
				line += ", "
			}
			v := t.NewVar(name.Name)
			typeName := t.TranslateType(field.Type)
			line += fmt.Sprintf("%%%v: %v", v.name, typeName)
			i += 1
		}
	}
	line += fmt.Sprintf(") ")
	if len(x.Type.Results.List) > 0 {
		if len(x.Type.Results.List) != 1 {
			panic("unsupported")
		}
		typeName := t.TranslateType(x.Type.Results.List[0].Type)
		line += fmt.Sprintf("-> %v", typeName)
	}
	line += " {\n"
	t.Printf("%v", line)

	t.indentLvl += 1
	t.EmitBlockStmt(x.Body)
	t.indentLvl -= 1

	t.Printf("}\n")
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
	node, err := parser.ParseFile(fset, "samples/assign.go", nil, parser.ParseComments|parser.SkipObjectResolution)
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
