package ast

import (
	"bytes"
	"fmt"
	"runtime/debug"

	"github.com/davecgh/go-spew/spew"
)

type Evaluator struct {
	blocks []Block
}

func (e *Evaluator) CurBlock() *Block {
	return &e.blocks[len(e.blocks)-1]
}

func (e *Evaluator) PushBlock() {
	e.blocks = append(e.blocks, Block{vars: make(map[string]Value)})
}

func (e *Evaluator) PopBlock() *Block {
	block := e.CurBlock()
	e.blocks = e.blocks[:len(e.blocks)-1]
	return block
}

func (e *Evaluator) AddVar(name string, typ Type, known bool) {
	e.CurBlock().vars[name] = NewValue(typ, known)
}

func (e *Evaluator) GetVar(name string) Value {
	for i := 0; i < len(e.blocks); i++ {
		depth := len(e.blocks) - 1 - i
		block := e.blocks[depth]
		v, ok := block.vars[name]
		if ok {
			return v
		}
	}
	panic(fmt.Errorf("Var \"%v\" not found in any block", name))
}

func (e *Evaluator) SetVar(name string, v Value) {
	for i := 0; i < len(e.blocks); i++ {
		depth := len(e.blocks) - 1 - i
		block := e.blocks[depth]
		_, ok := block.vars[name]
		if ok {
			block.vars[name] = v
			return
		}
	}
	spew.Dump(e.blocks)
	panic(fmt.Errorf("Var \"%v\" not found in any block", name))
}

func (e *Evaluator) EvalBinaryExpr(be *BinaryExpr) Value {
	xValue := e.Eval(be.X)[0].(*PrimValue)
	yValue := e.Eval(be.Y)[0].(*PrimValue)
	known := true
	if !xValue.Known || !yValue.Known {
		known = false
	}
	x, y := xValue.V, yValue.V
	var z int64
	isBool := false
	typ := xValue.Type
	signed := typ.signed

	switch be.Op {
	case ADD:
		z = x + y
	case SUB:
		z = x - y
	case MUL:
		z = x * y
	case QUO:
		z = x / y
	case REM:
		z = x % y
	case EQL:
		if x == y {
			z = 1
		}
		isBool = true
	case LSS:
		if (signed && x < y) || (!signed && uint64(x) < uint64(y)) {
			z = 1
		}
		isBool = true
	default:
		panic(fmt.Errorf("TODO Op: %v", ops[be.Op]))
	}

	if isBool {
		typ = Bool
	}
	return &PrimValue{
		Known: known,
		V:     z,
		Type:  typ,
	}
}

func (e *Evaluator) Eval(ex Expr) []Value {
	switch ex := ex.(type) {
	case *BinaryExpr:
		return []Value{e.EvalBinaryExpr(ex)}
	case *BasicLit:
		return []Value{&PrimValue{
			Type:  *ex.Type.(*PrimType),
			V:     ex.Value,
			Known: true,
		}}
	case *Ident:
		return []Value{e.GetVar(ex.Name)}
	default:
		panic("TODO")
	}
}

type Block struct {
	vars map[string]Value
}

type Value interface {
	valueNode()
}

// If known is true the value will be set to 0 (default go value)
func NewValue(typ Type, known bool) Value {
	switch typ := typ.(type) {
	case *PrimType:
		return &PrimValue{Known: known, V: 0, Type: *typ}
	case *StructDecl:
		v := make(map[string]Value)
		for _, field := range typ.Fields {
			v[field.Name] = NewValue(field.Type, known)
		}
		return &StructValue{V: v, Type: typ}
	default:
		panic("unreachable")
	}
}

func (*PrimValue) valueNode()   {}
func (*StructValue) valueNode() {}

type PrimValue struct {
	Known bool
	V     int64
	Type  PrimType
}

type StructValue struct {
	V    map[string]Value
	Type *StructDecl
}

type TransformUnroll struct {
	eval        Evaluator
	cfg         UnrollConfig
	blockCnt    int
	loopBlockId int
	iterBlockId int
}

type UnrollConfig struct {
	MaxIter int
}

func NewTransformUnroll() TransformUnroll {
	return TransformUnroll{
		cfg: UnrollConfig{
			MaxIter: 16,
		},
	}
}

func (t *TransformUnroll) NewBlockStmt() *BlockStmt {
	bs := &BlockStmt{
		Id: t.blockCnt,
	}
	t.blockCnt += 1
	return bs
}

func (t *TransformUnroll) BlockStmt(bs *BlockStmt) *BlockStmt {
	block := t.NewBlockStmt()
	t.eval.PushBlock()
	defer t.eval.PopBlock()
	var ss Stmts
	for _, stmt := range bs.List {
		ss.Push(t.Stmt(stmt)...)
	}
	block.List = ss.List
	return block
}

// TODO: Add init in LoopStmt
func (t *TransformUnroll) LoopStmt(ls *LoopStmt) Stmt {
	bs := t.NewBlockStmt()
	t.loopBlockId = bs.Id
	var ss Stmts
	for _, s := range ls.Init {
		ss.Push(t.Stmt(s)...)
	}
	i := 0
	for ; i < t.cfg.MaxIter; i++ {
		cond := t.eval.Eval(ls.Cond)[0].(*PrimValue)
		if !cond.Known {
			panic(fmt.Errorf("Unable to evaluate loop condition"))
		}
		if cond.V == 0 {
			break
		}
		t.iterBlockId = t.blockCnt
		ss.Push(t.BlockStmt(ls.Body))
	}
	if i == t.cfg.MaxIter {
		panic(fmt.Errorf("Reached MaxIter"))
	}
	bs.List = ss.List
	return bs
}

func (t *TransformUnroll) Stmt(s Stmt) []Stmt {
	switch s := s.(type) {
	case *LoopStmt:
		return []Stmt{t.LoopStmt(s)}
	case *BlockStmt:
		return []Stmt{t.BlockStmt(s)}
	case *DeclStmt:
		varDecl := s.Decl.(*VarDecl)
		t.eval.AddVar(varDecl.Name, varDecl.Type, true)
		return []Stmt{s}
	case *AssignStmt:
		values := t.eval.Eval(s.Rhs)
		for i, l := range s.Lhs {
			if l.Parent != nil {
				panic("TODO")
			}
			t.eval.SetVar(l.Name, values[i])
		}
		return []Stmt{s}
	case *IfStmt:
		return []Stmt{&IfStmt{
			Cond: s.Cond,
			Body: t.BlockStmt(s.Body),
			Else: t.Stmt(s.Else)[0],
		}}
	case *BranchStmt:
		switch s.Tok {
		case BREAK:
			return []Stmt{&EndBlock{
				Id: t.loopBlockId,
			}}
		case CONTINUE:
			return []Stmt{&EndBlock{
				Id: t.iterBlockId,
			}}
		default:
			panic("unreachable")
		}
	default:
		return []Stmt{s}
	}
}

func (t *TransformUnroll) FuncDecl(fd *FuncDecl) *FuncDecl {
	t.eval.PushBlock()
	for _, f := range fd.Type.Params {
		t.eval.AddVar(f.Name, f.Type, false)
	}
	for _, f := range fd.Type.Results {
		t.eval.AddVar(f.Name, f.Type, true)
	}
	defer t.eval.PopBlock()
	return &FuncDecl{
		Name: fd.Name,
		Type: fd.Type,
		Body: t.BlockStmt(fd.Body),
	}
}

func (t *TransformUnroll) Transform(pkg *Package) Package {
	var funcs []*FuncDecl
	for _, f := range pkg.Funcs {
		funcs = append(funcs, t.FuncDecl(f))
	}
	return Package{
		Structs:  pkg.Structs,
		Funcs:    funcs,
		BlockCnt: t.blockCnt,
	}
}

func unroll(pkg *Package) Package {
	t := NewTransformUnroll()
	t.blockCnt = pkg.BlockCnt
	return t.Transform(pkg)
}

func Unroll(pkg *Package) (p Package, err error) {
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
	p = unroll(pkg)
	return p, nil
}
