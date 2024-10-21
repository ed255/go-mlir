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
	"slices"
	"sort"
	"strconv"
)

type Var struct {
	SrcName  string
	BlockId  int
	BranchId int
	Type     Type
	// Variable from which this Var is branching.  This is an intermediate
	// branch variable that needs updating after the branchin block is
	// completed.
	Parent *Var
	// Variable from which this Var branch initiates.  This is the value
	// that the original variable takes if the branch doesn't apply.
	Ancestor *Var
}

func (v *Var) Name() string {
	s := fmt.Sprintf("%v", v.SrcName)
	if v.BlockId != 0 {
		s += fmt.Sprintf("_b%v", v.BlockId)
	}
	if v.BranchId != 0 {
		s += fmt.Sprintf("_c%v", v.BranchId)
	}
	return s
}

type Block struct {
	id int
	// Map from src var name to block Var
	vars map[string]*Var
	// List of branching vars
	branchVars []*Var
	// true if we're in a branch block
	branch bool
}

func (b *Block) AddVar(name string, typ Type) *Var {
	v := &Var{
		SrcName: name,
		BlockId: b.id,
		Type:    typ,
	}
	if _, ok := b.vars[v.Name()]; ok {
		panic(fmt.Errorf("Var.Name() %v already exists in current block", v.Name()))
	}
	b.vars[name] = v
	return v
}

type Translator struct {
	typeInfo    types.Info
	funcResults []Field
	blocks      []Block
	curFuncBody []Stmt
	lvl         int
	blockCnt    int
	varCnt      int
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
func (t *Translator) PushComment(s string) {
	t.PushStmt(&MetaStmt{Meta: &Comment{Value: s}})
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

func (t *Translator) DeclAssignAnon(expr Expr, typ Type) Expr {
	name := fmt.Sprintf("_%v", t.varCnt)
	t.varCnt += 1
	vd := t.GenVarDecl(name, typ)
	declStmt := DeclStmt{Decl: vd}
	assignStmt := AssignStmt{
		Lhs: VarRef{Name: vd.Name},
		Rhs: expr,
	}
	t.PushStmt(&declStmt, &assignStmt)
	return &Ident{
		Name: vd.Name,
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
	depth, branchBlocks, v := t.getVar(name)
	if len(branchBlocks) > 0 && depth != len(t.blocks)-1 {
		ancestor := v
		parent := ancestor
		var child *Var
		// We build a linked list from Ancestor to the deepest
		// branchBlock, chained by the Parent field
		for i, block := range branchBlocks {
			isLast := i == len(branchBlocks)-1
			// Now we only add the var we're about to use.  The
			// other vars in the chain will be added when emitting
			// their CondExpr.
			if isLast {
				child = block.AddVar(name, v.Type)
			} else {
				child = &Var{}
			}
			*child = *ancestor
			child.BranchId = block.id
			child.Parent = parent
			child.Ancestor = ancestor
			if isLast {
				t.PushStmt(&DeclStmt{Decl: &VarDecl{Name: child.Name(), Type: child.Type}})
			}
			block.branchVars = append(block.branchVars, child)
			parent = child
		}
		return VarRef{
			Name: child.Name(),
		}
	} else {
		return VarRef{
			Name: v.Name(),
		}
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

// getVar returns:
// - depth (block depth were var is declared)
// - list of branching blocks in the path (from shallow to deepest, excluding
// the block where the var is defined)
// - var
func (t *Translator) getVar(name string) (int, []*Block, *Var) {
	var branchBlocks []*Block
	for i := 0; i < len(t.blocks); i++ {
		depth := len(t.blocks) - 1 - i
		block := &t.blocks[depth]
		v, ok := block.vars[name]
		// collect branching blocks in the path, excluding the block
		// where the var is defined
		if !ok && block.branch == true {
			branchBlocks = append(branchBlocks, block)
		}
		if ok {
			slices.Reverse(branchBlocks)
			return depth, branchBlocks, v
		}
	}
	panic(fmt.Errorf("Var %v not found in any block", name))
}

func (t *Translator) GetVar(name string) *Var {
	_, _, v := t.getVar(name)
	return v
}

func (t *Translator) GenVarDecl(name string, typ Type) *VarDecl {
	v := t.CurBlock().AddVar(name, typ)
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
			i += 1
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
	if ifStmt.Init != nil {
		panic(fmt.Errorf("unsupported IfStmt.Init"))
	}
	cond := t.Expr(ifStmt.Cond)[0]
	condStr := SprintExpr(cond)
	cond = t.DeclAssignAnon(cond, &Bool)

	// True case
	t.PushBlock()
	curBlock := t.CurBlock()
	curBlock.branch = true
	t.PushComment(fmt.Sprintf("(bid=%v) if %v", curBlock.id, condStr))
	t.BlockStmt(ifStmt.Body)
	trueBlockBranchVars := t.PopBlock().branchVars

	// False case
	var falseBlockBranchVars []*Var
	if ifStmt.Else != nil {
		t.PushBlock()
		curBlock := t.CurBlock()
		curBlock.branch = true
		t.PushComment(fmt.Sprintf("(bid=%v) else", curBlock.id))
		switch elseStmt := ifStmt.Else.(type) {
		case *ast.BlockStmt:
			t.BlockStmt(elseStmt)
		case *ast.IfStmt:
			t.IfStmt(elseStmt)
		default:
			panic("unreachable")
		}
		falseBlockBranchVars = t.PopBlock().branchVars
	}

	// Arrange vars that have branched in the true and false case by name
	branchVars := make(map[string][2]*Var)
	for _, v := range trueBlockBranchVars {
		branchVars[v.SrcName] = [2]*Var{v, nil}
	}
	for _, v := range falseBlockBranchVars {
		pair, ok := branchVars[v.SrcName]
		if ok {
			pair[1] = v
			branchVars[v.SrcName] = pair
		} else {
			branchVars[v.SrcName] = [2]*Var{nil, v}
		}
	}

	curBlock = t.CurBlock()
	keys := make([]string, 0, len(branchVars))
	for k := range branchVars {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, key := range keys {
		pair := branchVars[key]
		trueCaseVar := pair[0]
		falseCaseVar := pair[1]
		// Find the parent from either true/false case
		v := trueCaseVar
		if v == nil {
			v = falseCaseVar
		}
		ancestor := v.Ancestor
		parent := v.Parent

		// We haven't reached the ancestor yet, we need to add and
		// declare the var
		if parent.Parent != nil {
			parentVar := curBlock.AddVar(parent.SrcName, parent.Type)
			*parentVar = *parent
			t.PushStmt(&DeclStmt{Decl: &VarDecl{Name: parent.Name(), Type: parent.Type}})
		}

		// oldParent := *parent
		caseTrueName := ancestor.Name()
		caseFalseName := ancestor.Name()
		if trueCaseVar != nil {
			caseTrueName = trueCaseVar.Name()
		}
		if falseCaseVar != nil {
			caseFalseName = falseCaseVar.Name()
		}
		t.PushStmt(&AssignStmt{
			Lhs: VarRef{
				Name: parent.Name(),
			},
			Rhs: &CondExpr{
				Cond:      cond,
				CaseTrue:  &Ident{Name: caseTrueName},
				CaseFalse: &Ident{Name: caseFalseName},
			},
		})
	}
}

func (t *Translator) Stmt(stmt ast.Stmt) {
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

func (t *Translator) BlockStmt(blockStmt *ast.BlockStmt) {
	for _, stmt := range blockStmt.List {
		t.Stmt(stmt)
	}
}

func (t *Translator) FuncDecl(funcDecl *ast.FuncDecl) *FuncDecl {
	t.PushBlock()
	defer t.PopBlock()
	var params []Field
	for _, field := range funcDecl.Type.Params.List {
		for _, name := range field.Names {
			typ := t.TypeFromExpr(field.Type)
			v := t.CurBlock().AddVar(name.Name, typ)
			param := Field{Name: v.Name(), Type: typ}
			params = append(params, param)
		}
	}

	var results []Field
	for i, field := range funcDecl.Type.Results.List {
		if len(field.Names) == 0 {
			typ := t.TypeFromExpr(field.Type)
			v := t.CurBlock().AddVar(fmt.Sprintf("_out%v", i), typ)
			result := Field{Name: v.Name(), Type: typ}
			results = append(results, result)
		} else {
			for _, name := range field.Names {
				typ := t.TypeFromExpr(field.Type)
				v := t.CurBlock().AddVar(name.Name, typ)
				result := Field{Name: v.Name(), Type: typ}
				results = append(results, result)
			}
		}
	}
	t.funcResults = results

	t.curFuncBody = []Stmt{}
	t.PushBlock()
	t.BlockStmt(funcDecl.Body)
	// spew.Dump(t.CurBlock().vars)
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
