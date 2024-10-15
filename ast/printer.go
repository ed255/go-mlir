package ast

import (
	"fmt"
	"io"
	"strings"
)

type Printer struct {
	lvl int
	out io.Writer
}

func NewPrinter(out io.Writer) Printer {
	return Printer{
		lvl: 0,
		out: out,
	}
}

func (p *Printer) errcheck(err error) {
	if err != nil {
		panic(err)
	}
}

func (p *Printer) Printf(format string, a ...any) {
	indent := strings.Repeat("  ", p.lvl)
	_, err := fmt.Fprintf(p.out, "%v"+format, append([]any{indent}, a...)...)
	p.errcheck(err)
}

func (p *Printer) Printfln(format string, a ...any) {
	p.Printf(format+"\n", a...)
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

func (p *Printer) ExprNeedsParens(e Expr) bool {
	switch e.(type) {
	case *BinaryExpr:
		return true
	default:
		return false
	}
}

func (p *Printer) Expr(o io.Writer, e Expr, parens bool) {
	if parens {
		fmt.Fprintf(o, "(")
	}
	switch e := e.(type) {
	case *BinaryExpr:
		p.Expr(o, e.X, p.ExprNeedsParens(e.X))
		fmt.Fprintf(o, " %v ", ops[e.Op])
		p.Expr(o, e.Y, p.ExprNeedsParens(e.Y))
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
	}
	if parens {
		fmt.Fprintf(o, ")")
	}
}

func (p *Printer) MetaStmt(m *MetaStmt) {
	switch m := m.Meta.(type) {
	case *LvlDelta:
		p.lvl += m.Delta
	case *Comment:
		p.Printfln("// %v", m.Value)
	}
}

func (p *Printer) AssignStmt(as *AssignStmt) {
	var exprStr strings.Builder
	p.Expr(&exprStr, as.Rhs, false)
	p.Printfln("%v = %v", as.Lhs.Name, exprStr.String())
}

func (p *Printer) FuncDecl(fd *FuncDecl) {
	p.Printfln("func %v (", fd.Name)
	for _, param := range fd.Type.Params {
		p.Printfln("  in %v %v,", param.Name, p.Type(param.Type))
	}
	for _, result := range fd.Type.Results {
		p.Printfln("  out %v %v,", result.Name, p.Type(result.Type))
	}
	p.Printfln(") {")
	for _, s := range fd.Body {
		switch s := s.(type) {
		case *DeclStmt:
			p.DeclStmt(s)
		case *AssignStmt:
			p.AssignStmt(s)
		case *MetaStmt:
			p.MetaStmt(s)
		default:
			panic("TODO")
		}
	}
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
				s += "i"
			} else {
				s += "u"
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
