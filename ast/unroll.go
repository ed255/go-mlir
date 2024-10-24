package ast

import (
	"bytes"
	"fmt"
	"runtime/debug"
)

type Evaluator struct {
	blocks []Block
}

func (e *Evaluator) CurBlock() *Block {
	return &e.blocks[len(e.blocks)-1]
}

func (e *Evaluator) PushBlock(id int) {
	e.blocks = append(e.blocks, Block{id: id, vars: make(map[string]Value)})
}

func (e *Evaluator) PopBlock() *Block {
	block := e.CurBlock()
	e.blocks = e.blocks[:len(e.blocks)-1]
	return block
}

func (e *Evaluator) AddVar(name string, typ Type, known bool) {
	e.CurBlock().vars[name] = NewValue(typ, known)
}

func (e *Evaluator) getVar(name string) (int, Value) {
	for i := 0; i < len(e.blocks); i++ {
		depth := len(e.blocks) - 1 - i
		block := e.blocks[depth]
		v, ok := block.vars[name]
		if ok {
			return depth, v
		}
	}
	panic(fmt.Errorf("Var \"%v\" not found in any block", name))
}

func (e *Evaluator) GetVar(name string) Value {
	_, v := e.getVar(name)
	return v
}

// SetVar finds the closest scope where variable "name" is defined and sets it to v.
// If the blocks path involves a branching block, the value is set to unknown.
func (e *Evaluator) SetVar(name string, v Value) Value {
	branchVar := false
	for i := 0; i < len(e.blocks); i++ {
		depth := len(e.blocks) - 1 - i
		block := e.blocks[depth]
		_, ok := block.vars[name]
		// If we traverse a branching block which is not the block
		// where the var is defined, then we no longer know what value
		// the var will take.
		if !branchVar && block.branch && !ok {
			branchVar = true
			v.SetUnknown()
		}
		if ok {
			block.vars[name] = v
			return v
		}
	}
	panic(fmt.Errorf("Var \"%v\" not found in any block", name))
}

func (e *Evaluator) EvalBinaryExpr(be *BinaryExpr) Value {
	xValue := e.Eval(be.X)[0].(*PrimValue)
	yValue := e.Eval(be.Y)[0].(*PrimValue)
	known := true
	if !xValue.Known() || !yValue.Known() {
		known = false
	}
	x, y := xValue.v, yValue.v
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
	case GTR:
		if (signed && x > y) || (!signed && uint64(x) > uint64(y)) {
			z = 1
		}
		isBool = true
	default:
		panic(fmt.Errorf("TODO Op: %v", ops[be.Op]))
	}
	// Mask according to the bit size
	if typ.size < 64 {
		z = z & ((1 << typ.size) - 1)
	}

	if isBool {
		typ = Bool
	}
	return &PrimValue{
		known: known,
		v:     z,
		Type:  typ,
	}
}

func (e *Evaluator) Eval(ex Expr) []Value {
	switch ex := ex.(type) {
	case *BinaryExpr:
		return []Value{e.EvalBinaryExpr(ex)}
	case *BasicLit:
		return []Value{&PrimValue{
			Type:  ex.Type,
			v:     ex.Value,
			known: true,
		}}
	case *Ident:
		return []Value{e.GetVar(ex.Name)}
	default:
		panic(fmt.Sprintf("TODO %+T", ex))
	}
}

type Block struct {
	id     int
	vars   map[string]Value
	branch bool
}

type Value interface {
	valueNode()
	SetUnknown()
	Known() bool
}

// If known is true the value will be set to 0 (default go value)
func NewValue(typ Type, known bool) Value {
	switch typ := typ.(type) {
	case *PrimType:
		return &PrimValue{known: known, v: 0, Type: *typ}
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
	known bool
	v     int64
	Type  PrimType
}

func (v *PrimValue) V() int64 {
	if !v.known {
		panic("unreachable: value is unknown")
	}
	return v.v
}

func (v *PrimValue) SetUnknown() {
	v.known = false
}

func (v *PrimValue) Known() bool {
	return v.known
}

func (v *PrimValue) String() string {
	if !v.known {
		return "unknown"
	} else if v.Type.signed {
		return fmt.Sprintf("%v", v.v)
	} else {
		return fmt.Sprintf("%v", uint64(v.v))
	}
}

type StructValue struct {
	V    map[string]Value
	Type *StructDecl
}

func (v *StructValue) SetUnknown() {
	for _, value := range v.V {
		value.SetUnknown()
	}
}

func (v *StructValue) Known() bool {
	known := true
	for _, value := range v.V {
		known = known && value.Known()
	}
	return known
}

type TransformUnroll struct {
	eval        Evaluator
	cfg         UnrollConfig
	blockCnt    int
	loopBlockId int
	iterBlockId int
	// Set to true by a break that unconditionally terminates a loop
	breakLoop bool
	// Map from blockIds of input Package to output Package
	blockIdMap map[int]int
}

type UnrollConfig struct {
	MaxIter int
}

func NewTransformUnroll() TransformUnroll {
	return TransformUnroll{
		cfg: UnrollConfig{
			MaxIter: 16,
		},
		blockIdMap: make(map[int]int),
	}
}

func (t *TransformUnroll) NewBlockStmt() *BlockStmt {
	bs := &BlockStmt{
		Id: t.blockCnt,
	}
	t.blockCnt += 1
	return bs
}

func (t *TransformUnroll) BlockStmt(bs *BlockStmt, branch bool) *BlockStmt {
	block := t.NewBlockStmt()
	t.blockIdMap[bs.Id] = block.Id
	t.eval.PushBlock(block.Id)
	t.eval.CurBlock().branch = branch
	defer t.eval.PopBlock()
	for _, stmt := range bs.List {
		block.List.Push(t.Stmt(stmt, false)...)
	}
	return block
}

func (t *TransformUnroll) LoopStmt(ls *LoopStmt) Stmt {
	bs := t.NewBlockStmt()
	t.loopBlockId = bs.Id
	for _, s := range ls.Init {
		bs.List.Push(t.Stmt(s, false)...)
	}
	i := 0
	for ; i < t.cfg.MaxIter; i++ {
		if ls.Cond != nil {
			cond := t.eval.Eval(ls.Cond)[0].(*PrimValue)
			if cond.Known() && cond.V() == 0 {
				break
			}
		}
		t.iterBlockId = t.blockCnt
		bs.List.Push(t.BlockStmt(ls.Body, false))
		if t.breakLoop {
			t.breakLoop = false // reset
			break
		}
	}
	if i == t.cfg.MaxIter {
		panic(fmt.Errorf("Unroll: reached MaxIter=%v", t.cfg.MaxIter))
	}
	return bs
}

func (t *TransformUnroll) Stmt(s Stmt, branch bool) []Stmt {
	switch s := s.(type) {
	case *LoopStmt:
		return []Stmt{t.LoopStmt(s)}
	case *BlockStmt:
		return []Stmt{t.BlockStmt(s, branch)}
	case *DeclStmt:
		varDecl := s.Decl.(*VarDecl)
		t.eval.AddVar(varDecl.Name, varDecl.Type, true)
		return []Stmt{s}
	case *AssignStmt:
		values := t.eval.Eval(s.Rhs)
		var ss Stmts
		for i, l := range s.Lhs {
			if l.Parent != nil {
				panic("TODO")
			}
			if l.Name == "" {
				panic("TODO")
			}
			v := t.eval.SetVar(l.Name, values[i])
			if v.Known() {
				ss.Push(&MetaStmt{
					&Comment{Value: fmt.Sprintf("%v = %v",
						l.Name, v)},
				})
			}
		}
		ss.Push(s)
		return ss
	case *IfStmt:
		cond := t.eval.Eval(s.Cond)[0].(*PrimValue)
		// If we can evaluate the if condition, we remove the if already
		if cond.Known() {
			if cond.V() == 1 {
				return []Stmt{t.BlockStmt(s.Body, branch)}
			} else if s.Else != nil {
				return t.Stmt(s.Else, branch)
			} else {
				return []Stmt{}
			}
		}
		return []Stmt{&IfStmt{
			Cond: s.Cond,
			Body: t.BlockStmt(s.Body, true),
			Else: t.Stmt(s.Else, true)[0],
		}}
	case *EndBlock:
		return []Stmt{&EndBlock{
			Id: t.blockIdMap[s.Id],
		}}
	case *BranchStmt:
		switch s.Tok {
		case BREAK:
			branchBreak := false
			for i := 0; i < len(t.eval.blocks); i++ {
				depth := len(t.eval.blocks) - 1 - i
				block := t.eval.blocks[depth]
				if block.id == t.loopBlockId {
					break
				}
				if block.branch {
					branchBreak = true
					break
				}
			}
			if !branchBreak {
				t.breakLoop = true
			}
			// TODO: If this happens in a non-branch block we
			// propagate up to the for block
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
	t.eval.PushBlock(t.blockCnt)
	t.blockCnt += 1
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
		Body: t.BlockStmt(fd.Body, false),
	}
}

func (t *TransformUnroll) Transform(pkg *Package) Package {
	var funcs []*FuncDecl
	for _, f := range pkg.Funcs {
		funcs = append(funcs, t.FuncDecl(f))
	}
	return Package{
		Structs: pkg.Structs,
		Funcs:   funcs,
		// BlockCnt: t.blockCnt,
	}
}

func unroll(pkg *Package) Package {
	t := NewTransformUnroll()
	// t.blockCnt = pkg.BlockCnt
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
