package ast

import (
	"fmt"
	"io"
	"strings"
)

type PrinterOpts struct {
	GoCompat bool
}

type Printer struct {
	lvl  int
	out  io.Writer
	opts PrinterOpts
	// dirtyLine is true when we have printed something in the current line and we haven't addad a new line yet
	dirtyLine    bool
	usedBlockIds map[int]bool
}

func NewPrinter(out io.Writer, opts PrinterOpts) Printer {
	return Printer{
		lvl:          0,
		out:          out,
		opts:         opts,
		usedBlockIds: make(map[int]bool),
	}
}

func (p *Printer) errcheck(err error) {
	if err != nil {
		panic(err)
	}
}

func (p *Printer) Printf(format string, a ...any) {
	indent := ""
	if !p.dirtyLine {
		indent = strings.Repeat("  ", p.lvl)
	}
	_, err := fmt.Fprintf(p.out, "%v"+format, append([]any{indent}, a...)...)
	p.errcheck(err)
	p.dirtyLine = true
}

func (p *Printer) Printfln(format string, a ...any) {
	p.Printf(format+"\n", a...)
	p.dirtyLine = false
}

func (p *Printer) VarDecl(vd *VarDecl) {
	p.Printfln("var %v %v", vd.Name, p.Type(vd.Type))
}

func (p *Printer) DeclStmt(ds *DeclStmt) {
	switch d := ds.Decl.(type) {
	case *VarDecl:
		p.VarDecl(d)
	default:
		panic("TODO")
	}
}

func printExprNeedsParens(e Expr) bool {
	switch e.(type) {
	case *BinaryExpr:
		return true
	case *CondExpr:
		return true
	default:
		return false
	}
}

func printCondExprGo(o io.Writer, ce *CondExpr, lhs string) {
	fmt.Fprintf(o, "if ")
	printExpr(o, ce.Cond, false)
	fmt.Fprintf(o, " { %v = ", lhs)
	printExpr(o, ce.CaseTrue, false)
	fmt.Fprintf(o, " } else { %v = ", lhs)
	printExpr(o, ce.CaseFalse, false)
	fmt.Fprintf(o, " }")
}

func printExpr(o io.Writer, e Expr, parens bool) {
	if parens {
		fmt.Fprintf(o, "(")
	}
	switch e := e.(type) {
	case *BinaryExpr:
		printExpr(o, e.X, printExprNeedsParens(e.X))
		fmt.Fprintf(o, " %v ", ops[e.Op])
		printExpr(o, e.Y, printExprNeedsParens(e.Y))
	case *Ident:
		fmt.Fprintf(o, "%v", e.Name)
	case *BasicLit:
		if e.Type.size == 1 {
			if e.Value == 0 {
				fmt.Fprintf(o, "false")
			} else {
				fmt.Fprintf(o, "true")
			}
		} else {
			fmt.Fprintf(o, "%v", e.Value)
		}
	case *CondExpr:
		printExpr(o, e.Cond, printExprNeedsParens(e.Cond))
		fmt.Fprintf(o, " ? ")
		printExpr(o, e.CaseTrue, printExprNeedsParens(e.CaseTrue))
		fmt.Fprintf(o, " : ")
		printExpr(o, e.CaseFalse, printExprNeedsParens(e.CaseFalse))
	case *CallExpr:
		fmt.Fprintf(o, "%v(", e.Fun)
		for i, arg := range e.Args {
			if i != 0 {
				fmt.Fprintf(o, ", ")
			}
			printExpr(o, arg, false)
		}
		fmt.Fprintf(o, ")")
	case *StructLit:
		fmt.Fprintf(o, "%v{", e.Name)
		for i, kv := range e.KeyValues {
			if i != 0 {
				fmt.Fprintf(o, ", ")
			}
			fmt.Fprintf(o, "%v: ", kv.Key)
			printExpr(o, kv.Value, false)
		}
		fmt.Fprintf(o, "}")
	case *SelectorExpr:
		printExpr(o, e.X, printExprNeedsParens(e.X))
		fmt.Fprintf(o, ".%v", e.Sel)
	default:
		panic("TODO")
	}
	if parens {
		fmt.Fprintf(o, ")")
	}
}

func SprintExpr(e Expr) string {
	var exprStr strings.Builder
	printExpr(&exprStr, e, false)
	return exprStr.String()
}

func (p *Printer) MetaStmt(m *MetaStmt) {
	switch m := m.Meta.(type) {
	case *LvlDelta:
		p.lvl += m.Delta
	case *Comment:
		p.Printfln("// %v", m.Value)
	default:
		panic("unreachable")
	}
}

func (p *Printer) ReturnStmt(rs *ReturnStmt) {
	p.Printfln("return")
}

func (p *Printer) IfStmt(is *IfStmt) {
	p.Printf("if %v ", SprintExpr(is.Cond))
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

func (p *Printer) LoopStmt(ls *LoopStmt) {
	p.Printfln("{")
	p.lvl += 1
	for _, s := range ls.Init {
		p.Stmt(s)
	}
	p.Printf("for %v ", SprintExpr(ls.Cond))
	p.BlockStmt(ls.Body, true)
	p.lvl -= 1
	p.Printfln("}")
}

func (p *Printer) BranchStmt(bs *BranchStmt) {
	switch bs.Tok {
	case BREAK:
		p.Printfln("break")
	case CONTINUE:
		p.Printfln("continue")
	default:
		panic("unreachable")
	}
}

func SprintVarRef(vr *VarRef) string {
	str := vr.Name
	parent := vr.Parent
	for parent != nil {
		str = parent.Name + "." + str
		parent = parent.Parent
	}
	return str
}

func (p *Printer) AssignStmt(as *AssignStmt) {
	var exprStr strings.Builder
	condExpr, ok := as.Rhs.(*CondExpr)
	if ok && p.opts.GoCompat {
		if len(as.Lhs) != 1 {
			panic("unreachable")
		}
		// go-friendly ternary operator
		printCondExprGo(&exprStr, condExpr, SprintVarRef(&as.Lhs[0]))
		p.Printfln("%v", exprStr.String())
	} else {
		printExpr(&exprStr, as.Rhs, false)
		for i, l := range as.Lhs {
			if i != 0 {
				p.Printf(", ")
			}
			p.Printf("%v", SprintVarRef(&l))
		}
		p.Printfln(" = %v", exprStr.String())
	}
}

func (p *Printer) Stmt(s Stmt) {
	switch s := s.(type) {
	case *DeclStmt:
		p.DeclStmt(s)
	case *AssignStmt:
		p.AssignStmt(s)
	case *IfStmt:
		p.IfStmt(s)
	case *BlockStmt:
		p.BlockStmt(s, true)
	case *ReturnStmt:
		p.ReturnStmt(s)
	case *MetaStmt:
		p.MetaStmt(s)
	case *LoopStmt:
		p.LoopStmt(s)
	case *BranchStmt:
		p.BranchStmt(s)
	case *EndBlock:
		p.Printfln("goto _endblock%v", s.Id)
		p.usedBlockIds[s.Id] = true
	default:
		panic("TODO")
	}
}

func (p *Printer) BlockStmt(bs *BlockStmt, newline bool) {
	p.Printfln("{")
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

func (p *Printer) FuncDecl(fd *FuncDecl) {
	p.Printfln("func %v (", fd.Name)
	for _, param := range fd.Type.Params {
		p.Printfln("  %v %v,", param.Name, p.Type(param.Type))
	}
	p.Printfln(") (")
	for _, result := range fd.Type.Results {
		p.Printfln("  %v %v,", result.Name, p.Type(result.Type))
	}
	p.Printf(") ")
	p.BlockStmt(fd.Body, true)
	p.Printfln("")
}

func (p *Printer) Type(t Type) string {
	switch t := t.(type) {
	case *PrimType:
		if t.size == 1 {
			return "bool"
		} else {
			s := ""
			if t.signed {
				s += "int"
			} else {
				s += "uint"
			}
			s += fmt.Sprintf("%v", t.size)
			return s
		}
	case *StructDecl:
		return t.Name
	default:
		panic("TODO")
	}
}

func (p *Printer) Decl(d Decl) {
	switch d := d.(type) {
	case *FuncDecl:
		p.FuncDecl(d)
	case *VarDecl:
		p.VarDecl(d)
	default:
		panic("TODO")
	}
}

func (p *Printer) Package(pkg *Package) {
	for _, f := range pkg.Funcs {
		p.FuncDecl(f)
	}
}
