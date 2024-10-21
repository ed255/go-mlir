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
	dirtyLine bool
}

func NewPrinter(out io.Writer, opts PrinterOpts) Printer {
	return Printer{
		lvl:  0,
		out:  out,
		opts: opts,
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
		typ := e.Type.(*PrimType)
		if typ.size == 1 {
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
	p.Printfln("if %v {", SprintExpr(is.Cond))
	p.BlockStmt(is.Body)
	switch es := is.Else.(type) {
	case *BlockStmt:
		p.Printfln("} else {")
		p.BlockStmt(es)
		p.Printfln("}")
	case *IfStmt:
		p.Printf("} else ")
		p.IfStmt(es)
	case nil:
		p.Printfln("}")
	default:
		panic("unreachable")
	}
}

func (p *Printer) AssignStmt(as *AssignStmt) {
	var exprStr strings.Builder
	condExpr, ok := as.Rhs.(*CondExpr)
	if ok && p.opts.GoCompat {
		// go friendly ternary operator
		printCondExprGo(&exprStr, condExpr, as.Lhs.Name)
		p.Printfln("%v", exprStr.String())
	} else {
		printExpr(&exprStr, as.Rhs, false)
		p.Printfln("%v = %v", as.Lhs.Name, exprStr.String())
	}
}

func (p *Printer) BlockStmt(bs *BlockStmt) {
	p.lvl += 1
	for _, s := range bs.List {
		switch s := s.(type) {
		case *DeclStmt:
			p.DeclStmt(s)
		case *AssignStmt:
			p.AssignStmt(s)
		case *IfStmt:
			p.IfStmt(s)
		case *BlockStmt:
			p.Printfln("{")
			p.BlockStmt(s)
			p.Printfln("}")
		case *ReturnStmt:
			p.ReturnStmt(s)
		case *MetaStmt:
			p.MetaStmt(s)
		default:
			panic("TODO")
		}
	}
	p.lvl -= 1
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
	p.Printfln(") {")
	p.BlockStmt(fd.Body)
	p.Printfln("}")
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

func (p *Printer) File(f *File) {
	for _, d := range f.Decls {
		p.Decl(d)
	}
}
