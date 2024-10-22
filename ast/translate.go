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

// Translator can take go source code and translate it into our internal
// simplified AST with minimum transformations.
type Translator struct {
	typeInfo    types.Info
	funcResults []Field
	structs     map[string]*StructDecl
	structList  []*StructDecl
	funcList    []*FuncDecl
	blockCnt    int
	lvl         int
}

func NewTranslator(typeInfo types.Info) Translator {
	return Translator{
		typeInfo: typeInfo,
		structs:  make(map[string]*StructDecl),
	}
}

func (t *Translator) NewBlockStmt(ss []Stmt) *BlockStmt {
	bs := &BlockStmt{
		Id:   t.blockCnt,
		List: ss,
	}
	t.blockCnt += 1
	return bs
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

func (t *Translator) Type(typ types.Type) Type {
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
	}

	switch typ := typ.(type) {
	case *types.Named:
		underlying := typ.Underlying()
		switch underlying := underlying.(type) {
		case *types.Struct:
			sd, ok := t.structs[typ.Obj().Name()]
			if !ok {
				panic("unreachable")
			}
			return sd
		default:
			panic(fmt.Errorf("unsupported Underlying Type %+#v", underlying))
		}
	case *types.Array:
		return &ArrayType{
			Len:  typ.Len(),
			Type: t.Type(typ.Elem()),
		}
	default:
		panic(fmt.Errorf("unsupported Type %+#v", typ))
	}

}

func (t *Translator) TypeFromExpr(expr ast.Expr) Type {
	typ := t.typeInfo.TypeOf(expr)
	return t.Type(typ)
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

func (t *Translator) SelectorExpr(selExpr *ast.SelectorExpr) Expr {
	return &SelectorExpr{
		X:   t.Expr(selExpr.X),
		Sel: selExpr.Sel.Name,
	}
}

func (t *Translator) CompositeLit(compLit *ast.CompositeLit) Expr {
	underlying := t.typeInfo.TypeOf(compLit).Underlying()
	switch underlying.(type) {
	case *types.Struct:
		// Return all the struct fields in order (with 0 for defaults)
		kvs := make(map[string]Expr)
		for _, elt := range compLit.Elts {
			kv := elt.(*ast.KeyValueExpr)
			kvs[kv.Key.(*ast.Ident).Name] = t.Expr(kv.Value)
		}
		structName := compLit.Type.(*ast.Ident).Name
		st := t.structs[structName]
		sl := StructLit{Name: structName}
		for _, field := range st.Fields {
			var kvExpr KeyValueExpr
			e, ok := kvs[field.Name]
			if ok {
				kvExpr = KeyValueExpr{
					Key:   field.Name,
					Value: e,
				}
			} else {
				panic("WIP")
				// vr := t.newAnonConst(types.Typ[types.Bool], 0)
				// vr.EmitDeclareConst(t, 0)
				// vr.signed = field.signed
				// vr.size = field.size
				// vars = append(vars)
			}
			sl.KeyValues = append(sl.KeyValues, kvExpr)
		}
		return &sl
	default:
		panic(fmt.Errorf("unsupported CompositeLit type: %+T", underlying))
	}
}

func (t *Translator) CallExpr(callExpr *ast.CallExpr) *CallExpr {
	var funName string
	switch fun := callExpr.Fun.(type) {
	case *ast.Ident:
		funName = fun.Name
	default:
		panic(fmt.Errorf("unsupported Expr for Fun: %+T", callExpr.Fun))
	}

	var args []Expr
	for _, arg := range callExpr.Args {
		args = append(args, t.Expr(arg))
	}
	return &CallExpr{
		Fun:  funName,
		Args: args,
	}
}

func (t *Translator) BasicLit(basicLit *ast.BasicLit) *BasicLit {
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
	return &BasicLit{
		Type:  *typ,
		Value: value,
	}
}

func (t *Translator) Expr(expr ast.Expr) Expr {
	var e Expr
	switch expr := expr.(type) {
	case *ast.BinaryExpr:
		e = &BinaryExpr{
			X:  t.Expr(expr.X),
			Op: t.Op(expr.Op),
			Y:  t.Expr(expr.Y),
		}
	case *ast.Ident:
		e = &Ident{
			Name: expr.Name,
		}
	case *ast.BasicLit:
		e = t.BasicLit(expr)
	case *ast.CallExpr:
		e = t.CallExpr(expr)
	case *ast.CompositeLit:
		e = t.CompositeLit(expr)
	case *ast.SelectorExpr:
		e = t.SelectorExpr(expr)
	default:
		panic(fmt.Errorf("unsupported Expr: %+T", expr))
	}
	return e
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
	case *ast.SelectorExpr:
		parent := t.VarRefFromExpr(expr.X)
		return VarRef{Parent: &parent, Name: expr.Sel.Name}
	case *ast.IndexExpr:
		parent := t.VarRefFromExpr(expr.X)
		return VarRef{Parent: &parent, Index: t.Expr(expr.Index)}
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

func (t *Translator) ExprLen(e ast.Expr) int {
	rhsType := t.typeInfo.TypeOf(e)
	exprLen := 1
	if rhsType, ok := rhsType.(*types.Tuple); ok {
		exprLen = rhsType.Len()
	}
	return exprLen
}

func (t *Translator) ExprTypes(e ast.Expr) []Type {
	rhsType := t.typeInfo.TypeOf(e)
	var typs []Type
	if rhsType, ok := rhsType.(*types.Tuple); ok {
		for i := 0; i < rhsType.Len(); i++ {
			typs = append(typs, t.Type(rhsType.At(i).Type()))
		}
	} else {
		typs = append(typs, t.TypeFromExpr(e))
	}
	return typs
}

func (t *Translator) BranchStmt(branchStmt *ast.BranchStmt) Stmt {
	var tok BranchToken
	switch branchStmt.Tok {
	case token.BREAK:
		tok = BREAK
	case token.CONTINUE:
		tok = CONTINUE
	default:
		panic(fmt.Errorf("unsupported BranchStmt.Tok %v", branchStmt.Tok))
	}
	return &BranchStmt{
		Tok: tok,
	}
}

func (t *Translator) IncDecStmt(incDecStmt *ast.IncDecStmt) Stmt {
	var op Op
	switch incDecStmt.Tok {
	case token.INC:
		op = ADD
	case token.DEC:
		op = SUB
	default:
		panic("unreachable")
	}

	one := &BasicLit{
		Type:  *t.TypeFromExpr(incDecStmt.X).(*PrimType),
		Value: 1,
	}
	return &AssignStmt{
		Lhs: []VarRef{t.VarRefFromExpr(incDecStmt.X)},
		Rhs: &BinaryExpr{
			X:  t.Expr(incDecStmt.X),
			Op: op,
			Y:  one,
		},
	}
}

func (t *Translator) AssignStmt(assignStmt *ast.AssignStmt) []Stmt {
	var ss Stmts
	switch assignStmt.Tok {
	case token.DEFINE:
		i := 0
		for _, rhs := range assignStmt.Rhs {
			r := t.Expr(rhs)
			var lhss []VarRef
			for j := 0; j < t.ExprLen(rhs); j++ {
				lhs := assignStmt.Lhs[i].(*ast.Ident)
				ss.Push(&DeclStmt{
					Decl: t.GenVarDecl(
						lhs.Name,
						t.TypeFromExpr(assignStmt.Lhs[i]),
					),
				})
				lhss = append(lhss, t.VarRef(lhs.Name))
				i += 1
			}
			ss.Push(&AssignStmt{
				Lhs: lhss,
				Rhs: r,
			})
		}
	case token.ASSIGN:
		if len(assignStmt.Rhs) > 1 {
			panic(fmt.Errorf("unsupported assign with multiple rhs"))
		}
		i := 0
		for _, rhs := range assignStmt.Rhs {
			r := t.Expr(rhs)
			var lhss []VarRef
			for j := 0; j < t.ExprLen(rhs); j++ {
				lhss = append(lhss, t.VarRefFromExpr(assignStmt.Lhs[i]))
				i += 1
			}
			ss.Push(&AssignStmt{
				Lhs: lhss,
				Rhs: r,
			})
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
		r := t.Expr(result)
		var lhss []VarRef
		for j := 0; j < t.ExprLen(result); j++ {
			lhss = append(lhss, t.VarRef(t.funcResults[i].Name))
			i += 1
		}
		ss.Push(&AssignStmt{
			Lhs: lhss,
			Rhs: r,
		})
	}
	ss.Push(&ReturnStmt{})
	return ss.List
}

func (t *Translator) ValueSpec(valueSpec *ast.ValueSpec) []Stmt {
	var ss Stmts
	if valueSpec.Values == nil {
		for _, name := range valueSpec.Names {
			declStmt := DeclStmt{
				Decl: t.GenVarDecl(
					name.Name,
					t.TypeFromExpr(valueSpec.Type),
				),
			}
			ss.Push(&declStmt)
		}
	} else {
		i := 0
		for _, value := range valueSpec.Values {
			r := t.Expr(value)
			var lhss []VarRef
			for _, typ := range t.ExprTypes(value) {
				declStmt := DeclStmt{
					Decl: t.GenVarDecl(
						valueSpec.Names[i].Name,
						typ,
					),
				}
				ss.Push(&declStmt)
				lhss = append(lhss, t.VarRef(valueSpec.Names[i].Name))
				i += 1
			}
			ss.Push(&AssignStmt{
				Lhs: lhss,
				Rhs: r,
			})
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

func (t *Translator) ForStmt(forStmt *ast.ForStmt) Stmt {
	var init, post []Stmt
	if forStmt.Init != nil {
		init = t.Stmt(forStmt.Init)
	}
	if forStmt.Post != nil {
		post = t.Stmt(forStmt.Post)
	}
	var cond Expr
	if forStmt.Cond != nil {
		cond = t.Expr(forStmt.Cond)
	}
	var loopBody Stmts
	loopBody.Push(t.BlockStmt(forStmt.Body))
	loopBody.Push(post...)
	return &LoopStmt{
		Init: init,
		Cond: cond,
		Body: t.NewBlockStmt(loopBody.List),
	}
}

func (t *Translator) IfStmt(ifStmt *ast.IfStmt) *IfStmt {
	if ifStmt.Init != nil {
		panic(fmt.Errorf("unsupported IfStmt.Init"))
	}
	cond := t.Expr(ifStmt.Cond)
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
	case *ast.ForStmt:
		ss.Push(t.ForStmt(stmt))
	case *ast.IncDecStmt:
		ss.Push(t.IncDecStmt(stmt))
	case *ast.BranchStmt:
		ss.Push(t.BranchStmt(stmt))
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
	return t.NewBlockStmt(ss.List)
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

func (t *Translator) FindFuncs(file *ast.File) {
	for _, n := range file.Decls {
		switch x := n.(type) {
		case *ast.FuncDecl:
			t.funcList = append(t.funcList, t.FuncDecl(x))
		}
	}
}

func (t *Translator) AddSpec(spec ast.Spec) {
	switch spec := spec.(type) {
	case *ast.TypeSpec:
		switch ts := spec.Type.(type) {
		case *ast.StructType:
			sd := &StructDecl{Name: spec.Name.Name}
			for _, f := range ts.Fields.List {
				for _, name := range f.Names {
					sd.Fields = append(sd.Fields, Field{
						Name: name.Name,
						Type: t.TypeFromExpr(f.Type),
					})
				}
			}
			t.structs[spec.Name.Name] = sd
			t.structList = append(t.structList, sd)
		default:
			panic(fmt.Errorf("unsupported Spec.Type: %v", spec.Type))
		}
	default:
		panic(fmt.Errorf("unsupported Spec: %v", spec))
	}
}

func (t *Translator) FindStructs(file *ast.File) {
	for _, n := range file.Decls {
		switch x := n.(type) {
		case *ast.GenDecl:
			if x.Tok == token.TYPE {
				for _, spec := range x.Specs {
					t.AddSpec(spec)
				}
			}
		}
	}
}

func translate(node ast.Node, typeInfo types.Info) Package {
	t := NewTranslator(typeInfo)
	file := node.(*ast.File)
	t.FindStructs(file)
	t.FindFuncs(file)
	return Package{
		Structs:  t.structList,
		Funcs:    t.funcList,
		BlockCnt: t.blockCnt,
	}
}

func Translate(node ast.Node, typeInfo types.Info) (p Package, err error) {
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
	p = translate(node, typeInfo)
	return p, nil
}

func TranslateFile(filePath string, src any) (p Package, err error) {
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
