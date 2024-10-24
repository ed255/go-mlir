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
}

func NewPrinterVerilog(out io.Writer) PrinterVerilog {
	return PrinterVerilog{
		lvl:          0,
		out:          out,
		usedBlockIds: make(map[int]bool),
	}
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
	p.Printfln("%v %v,", p.Type(vd.Type), vd.Name)
}

func (p *PrinterVerilog) DeclStmt(ds *DeclStmt) {
	switch d := ds.Decl.(type) {
	case *VarDecl:
		p.VarDecl(d)
	default:
		panic("TODO")
	}
}

func printVerilogExpr(o io.Writer, e Expr, parens bool) {
	if parens {
		fmt.Fprintf(o, "(")
	}
	switch e := e.(type) {
	case *BinaryExpr:
		printVerilogExpr(o, e.X, printExprNeedsParens(e.X))
		fmt.Fprintf(o, " %v ", ops[e.Op])
		printVerilogExpr(o, e.Y, printExprNeedsParens(e.Y))
	case *Ident:
		fmt.Fprintf(o, "%v", e.Name)
	case *BasicLit:
		if e.Type.signed {
			fmt.Fprintf(o, "%v", e.Value)
		} else {
			fmt.Fprintf(o, "%v", uint64(e.Value))
		}
	case *CondExpr:
		printVerilogExpr(o, e.Cond, printExprNeedsParens(e.Cond))
		fmt.Fprintf(o, " ? ")
		printVerilogExpr(o, e.CaseTrue, printExprNeedsParens(e.CaseTrue))
		fmt.Fprintf(o, " : ")
		printVerilogExpr(o, e.CaseFalse, printExprNeedsParens(e.CaseFalse))
	case *CallExpr:
		fmt.Fprintf(o, "%v(", e.Fun)
		for i, arg := range e.Args {
			if i != 0 {
				fmt.Fprintf(o, ", ")
			}
			printVerilogExpr(o, arg, false)
		}
		fmt.Fprintf(o, ")")
	case *StructLit:
		panic("unsupported by verilog")
	case *SelectorExpr:
		panic("unsupported by verilog")
	case *IndexExpr:
		printVerilogExpr(o, e.X, printExprNeedsParens(e.X))
		fmt.Fprintf(o, "[")
		printVerilogExpr(o, e.Index, false)
		fmt.Fprintf(o, "]")
	default:
		panic("TODO")
	}
	if parens {
		fmt.Fprintf(o, ")")
	}
}

func SprintVerilogExpr(e Expr) string {
	var exprStr strings.Builder
	printVerilogExpr(&exprStr, e, false)
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

func (p *PrinterVerilog) IfStmt(is *IfStmt) {
	p.Printf("if %v ", SprintVerilogExpr(is.Cond))
	newline := false
	if is.Else == nil {
		newline = true
	}
	p.BlockStmt(is.Body, newline)
	switch es := is.Else.(type) {
	case *BlockStmt:
		p.Printf("else ")
		p.BlockStmt(es, true)
	case *IfStmt:
		p.Printf("else ")
		p.IfStmt(es)
	case nil:
	default:
		panic("unreachable")
	}
}

func (p *PrinterVerilog) LoopStmt(ls *LoopStmt) {
	p.Printfln("{")
	p.lvl += 1
	for _, s := range ls.Init {
		p.Stmt(s)
	}
	p.Printf("for %v ", SprintVerilogExpr(ls.Cond))
	p.BlockStmt(ls.Body, true)
	p.lvl -= 1
	p.Printfln("}")
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

func sprintVerilogVarRefChild(vr *VarRef) string {
	str := ""
	if vr.Name != "" {
		if vr.Parent != nil {
			str += "."
		}
		str += vr.Name
	} else {
		str += fmt.Sprintf("[%v]", SprintVerilogExpr(vr.Index))
	}
	return str
}

func SprintVerilogVarRef(vr *VarRef) string {
	str := sprintVerilogVarRefChild(vr)
	parent := vr.Parent
	for parent != nil {
		str = sprintVerilogVarRefChild(parent) + str
		parent = parent.Parent
	}
	return str
}

func (p *PrinterVerilog) AssignStmt(as *AssignStmt) {
	var exprStr strings.Builder
	printVerilogExpr(&exprStr, as.Rhs, false)
	if len(as.Lhs) != 1 {
		panic("unsupported by verilog")
	}
	p.Printfln("assign %v = %v;", SprintVerilogVarRef(&as.Lhs[0]), exprStr.String())
}

func (p *PrinterVerilog) Stmt(s Stmt) {
	switch s := s.(type) {
	case *DeclStmt:
		p.DeclStmt(s)
	case *AssignStmt:
		p.AssignStmt(s)
	case *IfStmt:
		p.IfStmt(s)
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
		panic("TODO")
	case *ArrayType:
		panic("TODO")
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
