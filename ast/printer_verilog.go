package ast

import (
	"fmt"
	"io"
	"strings"
)

type PrinterVerilog struct {
	lvl int
	out io.Writer
	// dirtyLine is true when we have printed something in the current line and we haven't addad a new line yet
	dirtyLine    bool
	usedBlockIds map[int]bool
	wireCnt      int
}

func NewPrinterVerilog(out io.Writer) PrinterVerilog {
	return PrinterVerilog{
		lvl:          0,
		out:          out,
		usedBlockIds: make(map[int]bool),
	}
}

func (p *PrinterVerilog) NextWire() int {
	w := p.wireCnt
	p.wireCnt += 1
	return w
}

func (p *PrinterVerilog) errcheck(err error) {
	if err != nil {
		panic(err)
	}
}

func (p *PrinterVerilog) Printf(format string, a ...any) {
	indent := ""
	if !p.dirtyLine {
		indent = strings.Repeat("  ", p.lvl)
	}
	_, err := fmt.Fprintf(p.out, "%v"+format, append([]any{indent}, a...)...)
	p.errcheck(err)
	p.dirtyLine = true
}

func (p *PrinterVerilog) Printfln(format string, a ...any) {
	p.Printf(format+"\n", a...)
	p.dirtyLine = false
}

func (p *PrinterVerilog) VarDecl(vd *VarDecl) {
	p.Printfln("%v %v;", p.Type(vd.Type), vd.Name)
}

func (p *PrinterVerilog) DeclStmt(ds *DeclStmt) {
	switch d := ds.Decl.(type) {
	case *VarDecl:
		p.VarDecl(d)
	default:
		panic("TODO")
	}
}

func (p *PrinterVerilog) FprintBasicLit(o io.Writer, bl *BasicLit) {
	v := bl.Value
	if bl.Type.signed {
		fmt.Fprintf(o, "(-")
		v = -v
	}
	fmt.Fprintf(o, "%v'h%x", bl.Type.size, v)
	if bl.Type.signed {
		fmt.Fprintf(o, ")")
	}
}

// Create a wire and assign it the Expr
func (p *PrinterVerilog) wireExpr(e Expr, t Type) int {
	w := p.NextWire()
	p.Printfln("%v _%v_", p.Type(t), w)
	p.Printfln("assign _%v_ = %v;", w, p.SprintExpr(e))
	return w
}

func (p *PrinterVerilog) structFieldOffsetSize(t *StructDecl, field string) (int, int) {
	offset := 0
	size := -1
	for _, f := range t.Fields {
		// TODO: cache offsets and sizes
		if f.Name == field {
			size = f.Type.BitSize()
			break
		}
		offset += f.Type.BitSize()
	}
	assert(size != -1)
	return offset, size
}

func (p *PrinterVerilog) FprintSelectorExpr(o io.Writer, se *SelectorExpr) {
	w := p.wireExpr(se.X, se.Type)
	offset, size := p.structFieldOffsetSize(se.Type, se.Sel)
	fmt.Fprintf(o, "_%v_[%v:%v]", w, offset+size-1, offset)
}

func (p *PrinterVerilog) FprintStructLit(o io.Writer, sl *StructLit) {
	var wires []int
	for i, f := range sl.Type.Fields {
		w := p.wireExpr(sl.Values[i], f.Type)
		wires = append(wires, w)
	}
	fmt.Fprintf(o, "{")
	for i := range wires {
		if i != 0 {
			fmt.Fprintf(o, ", ")
		}
		// Reverse the wires because Verilog concatenates in big endian
		w := wires[len(wires)-1-i]
		fmt.Fprintf(o, "_%v_", w)
	}
	fmt.Fprintf(o, "}")
}

func (p *PrinterVerilog) FprintExpr(o io.Writer, e Expr, parens bool) {
	if parens {
		fmt.Fprintf(o, "(")
	}
	switch e := e.(type) {
	case *BinaryExpr:
		p.FprintExpr(o, e.X, printExprNeedsParens(e.X))
		fmt.Fprintf(o, " %v ", ops[e.Op])
		p.FprintExpr(o, e.Y, printExprNeedsParens(e.Y))
	case *Ident:
		fmt.Fprintf(o, "%v", e.Name)
	case *BasicLit:
		p.FprintBasicLit(o, e)
	case *CondExpr:
		p.FprintExpr(o, e.Cond, printExprNeedsParens(e.Cond))
		fmt.Fprintf(o, " ? ")
		p.FprintExpr(o, e.CaseTrue, printExprNeedsParens(e.CaseTrue))
		fmt.Fprintf(o, " : ")
		p.FprintExpr(o, e.CaseFalse, printExprNeedsParens(e.CaseFalse))
	case *CallExpr:
		fmt.Fprintf(o, "%v(", e.Fun)
		for i, arg := range e.Args {
			if i != 0 {
				fmt.Fprintf(o, ", ")
			}
			p.FprintExpr(o, arg, false)
		}
		fmt.Fprintf(o, ")")
	case *StructLit:
		p.FprintStructLit(o, e)
	case *SelectorExpr:
		p.FprintSelectorExpr(o, e)
	case *IndexExpr:
		p.FprintExpr(o, e.X, printExprNeedsParens(e.X))
		fmt.Fprintf(o, "[")
		p.FprintExpr(o, e.Index, false)
		fmt.Fprintf(o, "]")
	default:
		panic("TODO")
	}
	if parens {
		fmt.Fprintf(o, ")")
	}
}

func (p *PrinterVerilog) SprintExpr(e Expr) string {
	var exprStr strings.Builder
	p.FprintExpr(&exprStr, e, false)
	return exprStr.String()
}

func (p *PrinterVerilog) MetaStmt(m *MetaStmt) {
	switch m := m.Meta.(type) {
	case *LvlDelta:
		p.lvl += m.Delta
	case *Comment:
		p.Printfln("// %v", m.Value)
	default:
		panic("unreachable")
	}
}

func (p *PrinterVerilog) BranchStmt(bs *BranchStmt) {
	switch bs.Tok {
	case BREAK:
		p.Printfln("break")
	case CONTINUE:
		p.Printfln("continue")
	default:
		panic("unreachable")
	}
}

func (p *PrinterVerilog) varRefAssignAux(vr *VarRef, e Expr) (string, string) {
	dst := vr.Name
	child := vr
	parent := child.Parent
	if vr.Parent == nil {
		return dst, p.SprintExpr(e)
	}
	begin, end := 0, 0
	prevSize := vr.Type.BitSize()
	for parent != nil {
		dst = parent.Name
		if parent.Name != "" {
			// Struct field selector
			offset, size := p.structFieldOffsetSize(
				parent.Type.(*StructDecl), child.Name)
			begin = begin*prevSize + offset
			end = end*prevSize + offset + size
			prevSize = parent.Type.BitSize()
		} else {
			// Array index
			panic("TODO")
		}
		child = parent
		parent = parent.Parent
	}
	assert(dst != "")

	var rhs strings.Builder
	fmt.Fprintf(&rhs, "{")
	if end != prevSize-1 {
		fmt.Fprintf(&rhs, "%v[%v:%v], ", dst, prevSize-1, end)
	}
	fmt.Fprintf(&rhs, p.SprintExpr(e))
	if begin != 0 {
		fmt.Fprintf(&rhs, ", %v[%v:0], ", dst, begin-1)
	}
	fmt.Fprintf(&rhs, "}")
	return dst, rhs.String()
}

func (p *PrinterVerilog) AssignStmt(as *AssignStmt) {
	assert(len(as.Lhs) == 1, "unsupported by verilog")
	dst, rhs := p.varRefAssignAux(&as.Lhs[0], as.Rhs)
	p.Printfln("assign %v = %v;", dst, rhs)
}

func (p *PrinterVerilog) Stmt(s Stmt) {
	switch s := s.(type) {
	case *DeclStmt:
		p.DeclStmt(s)
	case *AssignStmt:
		p.AssignStmt(s)
	case *IfStmt:
		panic("unsupported by verilog")
	case *BlockStmt:
		p.BlockStmt(s, true)
	case *MetaStmt:
		p.MetaStmt(s)
	case *LoopStmt:
		panic("unsupported by verilog")
	case *BranchStmt:
		panic("unsupported by verilog")
	case *EndBlock:
		panic("unsupported by verilog")
	default:
		panic("TODO")
	}
}

func (p *PrinterVerilog) BlockStmt(bs *BlockStmt, newline bool) {
	p.Printfln("{ // b%v", bs.Id)
	p.lvl += 1
	for _, s := range bs.List {
		p.Stmt(s)
	}
	p.lvl -= 1
	if newline {
		p.Printfln("}")
	} else {
		p.Printf("}")
	}
	if p.usedBlockIds[bs.Id] {
		lvl := p.lvl
		p.lvl = 0
		p.Printfln("_endblock%v:", bs.Id)
		p.lvl = lvl
	}
}

func (p *PrinterVerilog) FuncDecl(fd *FuncDecl) {
	p.Printfln("module %v (", fd.Name)
	for _, f := range fd.Type.Params {
		p.Printfln("  input %v %v,", p.Type(f.Type), f.Name)
	}
	for _, f := range fd.Type.Results {
		p.Printfln("  output %v %v,", p.Type(f.Type), f.Name)
	}
	p.Printfln(");")
	p.BlockStmt(fd.Body, true)
	p.Printfln("endmodule")
	p.Printfln("")
}

func (p *PrinterVerilog) Type(t Type) string {
	switch t := t.(type) {
	case *PrimType:
		if t.size == 1 {
			return "wire"
		} else {
			var s string
			if t.signed {
				s = "signed "
			}
			s += fmt.Sprintf("wire [%v:0]", t.size-1)
			return s
		}
	case *StructDecl:
		return fmt.Sprintf("wire [%v:0]", t.BitSize()-1)
	case *ArrayType:
		return fmt.Sprintf("wire [%v:0]", t.BitSize()-1)
	default:
		panic("TODO")
	}
}

func (p *PrinterVerilog) Decl(d Decl) {
	switch d := d.(type) {
	case *FuncDecl:
		p.FuncDecl(d)
	case *VarDecl:
		p.VarDecl(d)
	default:
		panic("TODO")
	}
}

func (p *PrinterVerilog) Package(pkg *Package) {
	for _, f := range pkg.Funcs {
		p.FuncDecl(f)
	}
}
