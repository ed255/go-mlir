package rm_branches

import (
	"bytes"
	"fmt"
	"gocircuit/ast"
	"runtime/debug"
	"slices"
	"sort"
)

type Var struct {
	SrcName  string
	Type     ast.Type
	BranchId int
}

func (v *Var) Name() string {
	s := fmt.Sprintf("%v", v.SrcName)
	if v.BranchId != 0 {
		s += fmt.Sprintf("_c%v", v.BranchId)
	}
	return s
}

type Block struct {
	Id int
	bs *ast.BlockStmt
	// id int
	// Map from src var name to block Var
	vars map[string]*Var
	// List of branching vars
	branchVars []*Var
	// true if we're in a branch block
	branch bool
	// Pointer to parent Block
	Parent *Block
}

func (b *Block) AddVar(name string, typ ast.Type) *Var {
	v := &Var{
		SrcName: name,
		Type:    typ,
	}
	b.vars[name] = v
	return v
}

func NewBlock(bs *ast.BlockStmt, parent *Block, branch bool) *Block {
	return &Block{vars: make(map[string]*Var), bs: bs, Parent: parent, branch: branch}
}

type TransformRmBranches struct {
	blocks []*Block
}

func NewTransformRmBranches() TransformRmBranches {
	return TransformRmBranches{}
}

// getVar returns:
// - depth (block depth were var is declared)
// - list of branching blocks in the path (from shallow to deepest, excluding
// the block where the var is defined)
// - var
func (t *TransformRmBranches) getVar(name string) (int, []*Block, *Var) {
	var branchBlocks []*Block
	for i := 0; i < len(t.blocks); i++ {
		depth := len(t.blocks) - 1 - i
		block := t.blocks[depth]
		v, ok := block.vars[name]
		if ok {
			slices.Reverse(branchBlocks)
			return depth, branchBlocks, v
		}
		// collect branching blocks in the path, excluding the block
		// where the var is defined
		if block.branch == true {
			branchBlocks = append(branchBlocks, block)
		}
	}
	panic(fmt.Errorf("Var %v not found in any block", name))
}

func (t *TransformRmBranches) GetVar(name string) *Var {
	_, _, v := t.getVar(name)
	return v
}

func (t *TransformRmBranches) NewBlockStmt(id int) *ast.BlockStmt {
	bs := &ast.BlockStmt{
		Id: id,
	}
	return bs
}

func (t *TransformRmBranches) Expr(e ast.Expr) ast.Expr {
	switch e := e.(type) {
	case *ast.CallExpr:
		panic("TODO")
	case *ast.BinaryExpr:
		return &ast.BinaryExpr{X: t.Expr(e.X), Op: e.Op, Y: t.Expr(e.Y)}
	case *ast.Ident:
		v := t.GetVar(e.Name)
		return &ast.Ident{Name: v.Name()}
	case *ast.StructLit:
		panic("TODO")
	case *ast.SelectorExpr:
		panic("TODO")
	case *ast.IndexExpr:
		panic("TODO")
	case *ast.BasicLit:
		return e
	default:
		panic(fmt.Errorf("TODO: %+#T", e))
		// panic("unreachable")
	}
}

func (t *TransformRmBranches) EndBlock(eb *ast.EndBlock) []ast.Stmt {
	// TODO
	return []ast.Stmt{eb}
}

func (t *TransformRmBranches) IfStmt(is *ast.IfStmt) []ast.Stmt {
	block := t.CurBlock()
	cond := t.Expr(is.Cond)
	condStr := ast.SprintGoExpr(cond)

	var ss ast.Stmts
	ss.Push(&ast.MetaStmt{Meta: &ast.Comment{
		Value: fmt.Sprintf("(bid=%v) if %v", block.Id, condStr)}})
	// True case
	body := t.BlockStmt(is.Body, true)
	ss.Push(body)
	trueBlockBranchVars := block.branchVars
	block.branchVars = nil

	// False case
	var falseBlockBranchVars []*Var
	if is.Else != nil {
		elseStmt := t.Stmt(is.Else, true)
		ss.Push(elseStmt...)
		falseBlockBranchVars = block.branchVars
		block.branchVars = nil
	}

	// fmt.Printf("DBG true %+#v\n", trueBlockBranchVars)
	// fmt.Printf("DBG false %+#v\n", falseBlockBranchVars)

	// Arrange vars that have branched in the true and false case by name
	branchVars := make(map[string][2]*Var)
	for _, v := range trueBlockBranchVars {
		branchVars[v.SrcName] = [2]*Var{v, nil}
	}
	for _, v := range falseBlockBranchVars {
		if pair, ok := branchVars[v.SrcName]; ok {
			pair[1] = v
			branchVars[v.SrcName] = pair
		} else {
			branchVars[v.SrcName] = [2]*Var{nil, v}
		}
	}

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
		src := t.GetVar(v.SrcName)

		caseTrueName := src.Name()
		caseFalseName := src.Name()
		if trueCaseVar != nil {
			caseTrueName = trueCaseVar.Name()
		}
		if falseCaseVar != nil {
			caseFalseName = falseCaseVar.Name()
		}
		ss.Push(&ast.AssignStmt{
			Lhs: []ast.VarRef{{
				Name: src.Name(),
			}},
			Rhs: &ast.CondExpr{
				Cond:      cond,
				CaseTrue:  &ast.Ident{Name: caseTrueName},
				CaseFalse: &ast.Ident{Name: caseFalseName},
			},
		})
	}

	return ss
}

func (t *TransformRmBranches) VarRef(name string) ast.VarRef {
	depth, branchBlocks, v := t.getVar(name)
	if len(branchBlocks) > 0 && depth != len(t.blocks)-1 {
		var lastVar *Var
		for _, block := range branchBlocks {
			branchVar := &Var{SrcName: name, Type: v.Type, BranchId: block.bs.Id}
			// For each branch block we declare a corresponding
			// branching variable in the parent block that is
			// initialized with the value before branching.
			block.Parent.bs.List.Push(
				&ast.DeclStmt{Decl: &ast.VarDecl{
					Name: branchVar.Name(),
					Type: v.Type,
				}},
				&ast.AssignStmt{
					Lhs: []ast.VarRef{{Name: branchVar.Name()}},
					Rhs: &ast.Ident{
						Name: v.Name(),
					},
				},
			)
			block.Parent.branchVars = append(block.Parent.branchVars, branchVar)
			// Then we make the branching variable available in the
			// scope of the branching block
			block.vars[name] = branchVar
			lastVar = branchVar
		}
		return ast.VarRef{
			Name: lastVar.Name(),
		}
	} else {
		return ast.VarRef{
			Name: v.Name(),
		}
	}
}

func (t *TransformRmBranches) AssignStmt(as *ast.AssignStmt) ast.Stmt {
	var lhs []ast.VarRef
	for _, v := range as.Lhs {
		if v.Parent != nil {
			panic("TODO")
		}
		lhs = append(lhs, t.VarRef(v.Name))
	}
	return &ast.AssignStmt{Lhs: lhs, Rhs: t.Expr(as.Rhs)}
}

func (t *TransformRmBranches) Stmt(s ast.Stmt, branch bool) []ast.Stmt {
	switch s := s.(type) {
	case *ast.BlockStmt:
		return []ast.Stmt{t.BlockStmt(s, branch)}
	case *ast.DeclStmt:
		vd := s.Decl.(*ast.VarDecl)
		t.CurBlock().AddVar(vd.Name, vd.Type)
		return []ast.Stmt{s}
	case *ast.AssignStmt:
		return []ast.Stmt{t.AssignStmt(s)}
	case *ast.IfStmt:
		return t.IfStmt(s)
	case *ast.EndBlock:
		return t.EndBlock(s)
	case *ast.LoopStmt:
		panic("unsupported")
	case *ast.BranchStmt:
		panic("unsupported")
	case *ast.MetaStmt:
		return []ast.Stmt{s}
	default:
		panic(fmt.Errorf("TODO: %+T", s))
		// panic("unreachable")
	}
}

func (t *TransformRmBranches) PushBlock(bs *ast.BlockStmt, branch bool) {
	var parent *Block
	if len(t.blocks) > 0 {
		parent = t.CurBlock()
	}
	t.blocks = append(t.blocks, NewBlock(bs, parent, branch))
}

func (t *TransformRmBranches) CurBlock() *Block {
	return t.blocks[len(t.blocks)-1]
}

func (t *TransformRmBranches) PopBlock() *Block {
	block := t.blocks[len(t.blocks)-1]
	t.blocks = t.blocks[:len(t.blocks)-1]
	return block
}

func (t *TransformRmBranches) BlockStmt(bs *ast.BlockStmt, branch bool) *ast.BlockStmt {
	bs1 := t.NewBlockStmt(bs.Id)
	t.PushBlock(bs1, branch)
	defer t.PopBlock()
	for _, stmt := range bs.List {
		bs1.List.Push(t.Stmt(stmt, false)...)
	}
	return bs1
}

func (t *TransformRmBranches) FuncDecl(fd *ast.FuncDecl) *ast.FuncDecl {
	t.PushBlock(nil, false)
	defer t.PopBlock()
	block := t.CurBlock()
	for _, f := range fd.Type.Params {
		block.AddVar(f.Name, f.Type)
	}
	for _, f := range fd.Type.Results {
		block.AddVar(f.Name, f.Type)
	}

	return &ast.FuncDecl{
		Name: fd.Name,
		Type: fd.Type,
		Body: t.BlockStmt(fd.Body, false),
	}
}

func (t *TransformRmBranches) Transform(pkg *ast.Package) ast.Package {
	var funcs []*ast.FuncDecl
	for _, f := range pkg.Funcs {
		funcs = append(funcs, t.FuncDecl(f))
	}
	return ast.Package{
		Structs: pkg.Structs,
		Funcs:   funcs,
	}
}

func rmBranches(pkg *ast.Package) ast.Package {
	t := NewTransformRmBranches()
	return t.Transform(pkg)
}

func RmBranches(pkg *ast.Package) (p ast.Package, err error) {
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
	p = rmBranches(pkg)
	return p, nil
}
