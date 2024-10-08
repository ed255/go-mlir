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
	"strconv"
	"strings"
)

type VarOpt int

const (
	VarNew VarOpt = iota
	VarGet
	VarNext
)

type VarsType int

const (
	VarsSingle VarOpt = iota
	VarsTuple
	VarsStruct
)

type Vars struct {
	Type VarsType
}

type Var struct {
	name   string
	size   int
	signed bool
	value  *int64
}

func (v *Var) EmitDeclare(t *Translator) {
	if v.value == nil {
		t.Printf("%v %v;\n", typeFmt(v.signed, v.size), v.name)
	} else {
		t.Printf("parameter [%v:0] %v = %v;\n", v.size-1, v.name, *v.value)
	}
}

type VarVer struct {
	size    int
	signed  bool
	version int
}

type Block struct {
	// Mapping of variable name -> SSA version
	vars       map[string]VarVer
	returnVars []Var
}

type Translator struct {
	indentLvl int
	out       io.Writer
	varCnt    int
	constCnt  int
	typeInfo  types.Info
	funcs     map[string]FuncArgs
	structs   map[string]Struct
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
		funcs:     make(map[string]FuncArgs),
		structs:   make(map[string]Struct),
		blocks:    []Block{},
	}
}

func (t *Translator) PushBlock() {
	t.blocks = append(t.blocks, Block{vars: make(map[string]VarVer)})
}

func (t *Translator) PopBlock() {
	t.blocks = t.blocks[:len(t.blocks)-1]
}

func (t *Translator) DecodeType(nTyp ast.Expr) (bool, int) {
	return t.decodeType(t.typeInfo.TypeOf(nTyp))
}

func (t *Translator) decodeType(typ types.Type) (bool, int) {
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

func (t *Translator) NewVar(name string, signed bool, size int) Var {
	t.curBlock().vars[name] = VarVer{signed: signed, size: size, version: 0}
	return Var{
		name:   fmt.Sprintf("%v0", name),
		signed: signed,
		size:   size,
	}
}

func (t *Translator) NewVarExpr(name string, nTyp ast.Expr) Var {
	signed, size := t.DecodeType(nTyp)
	t.curBlock().vars[name] = VarVer{signed: signed, size: size, version: 0}
	return Var{
		name:   fmt.Sprintf("%v0", name),
		signed: signed,
		size:   size,
	}
}

func (t *Translator) GetVar(name string) Var {
	v, ok := t.curBlock().vars[name]
	if !ok {
		panic(fmt.Errorf("Var \"%v\" not found in current block", name))
	}
	return Var{
		name:   fmt.Sprintf("%v%v", name, v.version),
		signed: v.signed,
		size:   v.size,
	}
}

func (t *Translator) GetVarNextVer(name string) Var {
	block := t.curBlock()
	v, ok := block.vars[name]
	if !ok {
		panic(fmt.Errorf("Var \"%v\" not found in current block", name))
	}
	v.version += 1
	block.vars[name] = v
	return Var{
		name:   fmt.Sprintf("%v%v", name, v.version),
		signed: v.signed,
		size:   v.size,
	}
}

func (t *Translator) NewAnonVar(nTyp ast.Expr) Var {
	signed, size := t.DecodeType(nTyp)
	v := Var{
		name:   fmt.Sprintf("_%v_", t.varCnt),
		signed: signed,
		size:   size,
	}
	t.varCnt += 1
	return v
}

func (t *Translator) newAnonVar(typ types.Type) Var {
	signed, size := t.decodeType(typ)
	v := Var{
		name:   fmt.Sprintf("_%v_", t.varCnt),
		signed: signed,
		size:   size,
	}
	t.varCnt += 1
	return v
}

func (t *Translator) NewAnonConst(nTyp ast.Expr, value int64) Var {
	signed, size := t.DecodeType(nTyp)
	v := Var{
		name:   fmt.Sprintf("c%v", t.constCnt),
		signed: signed,
		size:   size,
		value:  &value,
	}
	t.constCnt += 1
	return v
}

func (t *Translator) newAnonConst(typ types.Type, value int64) Var {
	signed, size := t.decodeType(typ)
	v := Var{
		name:   fmt.Sprintf("c%v", t.constCnt),
		signed: signed,
		size:   size,
		value:  &value,
	}
	t.constCnt += 1
	return v
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

func (t *Translator) VarVars(name string, s ast.Expr, varOpt VarOpt) []Var {
	_, isStruct := t.typeInfo.TypeOf(s).Underlying().(*types.Struct)
	var vars []Var

	if isStruct {
		typeName := t.typeInfo.TypeOf(s).(*types.Named).Obj().Name()
		fields := t.structs[typeName].fields
		switch varOpt {
		case VarNew:
			for _, field := range fields {
				vr := t.NewVar(fmt.Sprintf("%v_%v", name, field.name), field.signed, field.size)
				vars = append(vars, vr)
			}
		case VarGet:
			for _, field := range fields {
				vr := t.GetVar(fmt.Sprintf("%v_%v", name, field.name))
				vars = append(vars, vr)
			}
		// case VarNext:
		// 	vr = t.GetVarNextVer(name, s)
		// 	vr.EmitDeclare(t)
		default:
			panic("TODO")
		}
	} else {
		switch varOpt {
		case VarNew:
			vars = append(vars, t.NewVarExpr(name, s))
		case VarGet:
			vars = append(vars, t.GetVar(name))
		case VarNext:
			vars = append(vars, t.GetVarNextVer(name))
		}
	}
	return vars
}

func (t *Translator) IdentVars(s *ast.Ident, varOpt VarOpt) []Var {
	return t.VarVars(s.Name, s, varOpt)
}

func (t *Translator) EmitExpr(x ast.Expr, varOpt VarOpt) []Var {
	switch s := x.(type) {
	case *ast.BinaryExpr:
		vx := t.EmitExpr(s.X, VarGet)[0]
		vy := t.EmitExpr(s.Y, VarGet)[0]
		vr := t.NewAnonVar(s)
		vr.EmitDeclare(t)
		op := t.TranslateOp(s.Op)
		t.Printf("assign %v = %v %v %v;\n", vr.name, vx.name, op, vy.name)
		return []Var{vr}
	case *ast.Ident:
		vars := t.IdentVars(s, varOpt)
		if varOpt == VarNew || varOpt == VarNext {
			for _, vr := range vars {
				vr.EmitDeclare(t)
			}
		}
		return vars
	case *ast.BasicLit:
		value, err := strconv.ParseInt(s.Value, 10, 64)
		t.errcheck(err)
		vr := t.NewAnonConst(s, value)
		vr.EmitDeclare(t)
		return []Var{vr}
	case *ast.CallExpr:
		ident := s.Fun.(*ast.Ident)
		fn := t.funcs[ident.Name]

		var inputs []Var
		for _, arg := range s.Args {
			inputs = append(inputs, t.EmitExpr(arg, VarGet)...)
		}
		var outputs []Var
		if len(fn.outputs) == 1 {
			vr := t.NewAnonVar(s)
			vr.EmitDeclare(t)
			outputs = append(outputs, vr)
		} else if len(fn.outputs) > 1 {
			tuple := t.typeInfo.TypeOf(s).(*types.Tuple)
			for i := 0; i < tuple.Len(); i++ {
				vr := t.newAnonVar(tuple.At(i).Type())
				vr.EmitDeclare(t)
				outputs = append(outputs, vr)
			}
		}

		t.Printf("%v _%v_ (\n", ident.Name, t.varCnt)
		t.varCnt += 1
		for i, input := range fn.inputs {
			t.Printf("  .%v(%v),\n", input, inputs[i].name)
		}
		for i, output := range fn.outputs {
			t.Printf("  .%v(%v),\n", output, outputs[i].name)
		}
		t.Printf(");\n")
		return outputs
	case *ast.CompositeLit:
		// fmt.Printf("DBG %+#v\n", s.Type.(*ast.Ident).Name)
		// fmt.Printf("DBG typeof %+#v\n", t.typeInfo.TypeOf(s).Underlying())
		underlying := t.typeInfo.TypeOf(s).Underlying()
		switch underlying.(type) {
		case *types.Struct:
			// Return all the struct fields in order (with 0 for defaults)
			kvs := make(map[string]Var)
			for _, elt := range s.Elts {
				kv := elt.(*ast.KeyValueExpr)
				value := t.EmitExpr(kv.Value, VarGet)[0]
				kvs[kv.Key.(*ast.Ident).Name] = value
			}
			st := t.structs[s.Type.(*ast.Ident).Name]
			var vars []Var
			for _, field := range st.fields {
				vr, ok := kvs[field.name]
				if ok {
					vars = append(vars, vr)
				} else {
					vr := t.newAnonConst(types.Typ[types.Bool], 0)
					vr.EmitDeclare(t)
					vr.signed = field.signed
					vr.size = field.size
					vars = append(vars)
				}
			}
			return vars
		default:
			panic(fmt.Errorf("unsupported CompositeLit type: %+T", underlying))
		}

		// switch t.typeInfo.TypeOf(s.Type) {
		// }
		// for _, elt := range s.Elts {
		// 	vars = append(vars, t.EmitExpr(elt, GetVar))
		// }
		// if st, ok := t.structs[s.Type.(*ast.Ident).Name]; ok {
		// 	fmt.Printf("DBG %+#v\n", s)
		// 	var vars []Var
		// 	for _, elt := range s.Elts {
		// 		vars = append(vars, t.EmitExpr(elt, GetVar))
		// 	}
		// } else {
		// 	panic("unimplemented")
		// }
	case *ast.KeyValueExpr:
		vl := t.EmitExpr(s.Key, VarNew)[0]
		vr := t.EmitExpr(s.Value, VarGet)[0]
		t.Printf("assign %v = %v;\n", vl.name, vr.name)
		return []Var{vl}
	case *ast.SelectorExpr:
		vars := t.EmitExpr(s.X, VarGet)
		_, isStruct := t.typeInfo.TypeOf(s.X).Underlying().(*types.Struct)
		if !isStruct {
			panic("unsupported")
		}
		typeName := t.typeInfo.TypeOf(s.X).(*types.Named).Obj().Name()
		st := t.structs[typeName]
		for i, field := range st.fields {
			if field.name == s.Sel.Name {
				return []Var{vars[i]}
			}
		}
		panic("unreachable")
	default:
		panic(fmt.Errorf("unsupported Expr: %+T", x))
	}
}

func (t *Translator) EmitReturnStmt(x *ast.ReturnStmt) {
	block := t.curBlock()
	i := 0
	for _, r := range x.Results {
		vs := t.EmitExpr(r, VarGet)
		for _, v := range vs {
			t.Printf("assign %v = %v;\n", block.returnVars[i].name, v.name)
			i += 1
		}
	}
}

func (t *Translator) EmitAssignStmt(x *ast.AssignStmt) {
	switch x.Tok {
	case token.DEFINE:
		i := 0
		for _, rhs := range x.Rhs {
			results := t.EmitExpr(rhs, VarGet)
			j := 0
			for j < len(results) {
				vls := t.EmitExpr(x.Lhs[i], VarNew)
				i += 1
				for _, vl := range vls {
					t.Printf("assign %v = %v;\n", vl.name, results[j].name)
					j += 1
				}
			}
		}
	case token.ASSIGN:
		i := 0
		for _, rhs := range x.Rhs {
			results := t.EmitExpr(rhs, VarGet)
			for _, r := range results {
				vl := t.EmitExpr(x.Lhs[i], VarNext)[0]
				t.Printf("assign %v = %v;\n", vl.name, r.name)
				i += 1
			}
		}
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
		return fmt.Sprintf("signed wire [%v:0]", size-1)
	} else {
		return fmt.Sprintf("wire [%v:0]", size-1)
	}
}

func (t *Translator) EmitFuncDecl(x *ast.FuncDecl) {
	t.PushBlock()
	block := t.curBlock()
	t.Printf("module %v (\n", x.Name)
	for _, field := range x.Type.Params.List {
		for _, name := range field.Names {
			vars := t.VarVars(name.Name, field.Type, VarNew)
			// v := t.NewVarExpr(name.Name, field.Type)
			for _, v := range vars {
				t.Printf("  input %v %v,\n", typeFmt(v.signed, v.size), v.name)
			}
		}
	}
	for i, field := range x.Type.Results.List {
		if len(field.Names) != 0 {
			panic(fmt.Errorf("unsupported return parameter names"))
		}
		v := t.NewVarExpr(fmt.Sprintf("_out%v_", i), field.Type)
		t.Printf("  output %v %v,\n", typeFmt(v.signed, v.size), v.name)
		block.returnVars = append(block.returnVars, v)
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
	t.PassInit(node)
	t.emit(node)
	return nil
}

type FuncArgs struct {
	inputs  []string
	outputs []string
}

type Struct struct {
	name   string
	fields []Var
}

func (t *Translator) ParseType(spec ast.Spec) {
	switch s := spec.(type) {
	case *ast.TypeSpec:
		switch ts := s.Type.(type) {
		case *ast.StructType:
			st := Struct{name: s.Name.Name}
			for _, f := range ts.Fields.List {
				signed, size := t.DecodeType(f.Type)
				for _, name := range f.Names {
					st.fields = append(st.fields, Var{
						name:   name.Name,
						signed: signed,
						size:   size,
					})
				}
			}
			t.structs[s.Name.Name] = st
		default:
			panic(fmt.Errorf("unsupported Spec.Type: %v", s.Type))
		}
	default:
		panic(fmt.Errorf("unsupported Spec: %v", spec))
	}
}

// PassInit performs some initial passes:
// - Find all struct declarations
// - Find all function declarations
func (t *Translator) PassInit(node ast.Node) {
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.GenDecl:
			if x.Tok == token.TYPE {
				for _, spec := range x.Specs {
					t.ParseType(spec)
				}
			}
		}
		return true
	})

	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			var inputs []string
			for _, field := range x.Type.Params.List {
				for _, name := range field.Names {
					if _, ok := t.typeInfo.TypeOf(field.Type).Underlying().(*types.Struct); ok {
						typeName := t.typeInfo.TypeOf(field.Type).(*types.Named).Obj().Name()
						for _, field := range t.structs[typeName].fields {
							inputs = append(inputs, fmt.Sprintf("%v_%v0", name.Name, field.name))
						}
					} else {
						inputs = append(inputs, fmt.Sprintf("%v0", name.Name))
					}
				}
			}
			var outputs []string
			for i, field := range x.Type.Results.List {
				if len(field.Names) != 0 {
					panic(fmt.Errorf("unsupported return parameter names"))
				}
				outputs = append(outputs, fmt.Sprintf("_out%v_0", i))
			}
			t.funcs[x.Name.Name] = FuncArgs{inputs: inputs, outputs: outputs}
		}
		return true
	})
}

func main() {
	// parse file
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, "samples/struct.go", nil, parser.ParseComments|parser.SkipObjectResolution)
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
