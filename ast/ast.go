// Package ast defines an internal ast that is a reduced version of go's ast.
// The aim of this ast is to represent go source code in a simple way to be
// transformed into a static circuit.  Several transformations of this ast are
// implemented to achieve that.
package ast

type Package struct {
	Structs []*StructDecl
	Funcs   []*FuncDecl
}

type Decl interface {
	declNode()
}

func (*FuncDecl) declNode() {}

func (*StructDecl) declNode() {}

// func (*ConstDecl) declNode()  {}
func (*VarDecl) declNode() {}

// func (*TypeDecl) declNode()   {}

type Type interface {
	typeNode()
}

type PrimType struct {
	signed bool
	size   int
}

func (*PrimType) typeNode()   {}
func (*StructDecl) typeNode() {}

func NewPrimType(signed bool, size int) PrimType {
	return PrimType{signed: signed, size: size}
}

type FuncDecl struct {
	Name string
	Type FuncType
	Body *BlockStmt
}

type FuncType struct {
	Params  []Field
	Results []Field
}

type Field struct {
	Name string
	Type Type
}

type StructDecl struct {
	Name   string
	Fields []Field
}

// TODO
// type ConstDecl struct{}

// TODO
type VarDecl struct {
	Name string
	Type Type
}

// TODO
// type TypeDecl struct{}

type Stmt interface {
	stmtNode()
}

func (*DeclStmt) stmtNode()   {}
func (*AssignStmt) stmtNode() {}
func (*IfStmt) stmtNode()     {}
func (*BlockStmt) stmtNode()  {}
func (*ReturnStmt) stmtNode() {}
func (*LoopStmt) stmtNode()   {}
func (*BranchStmt) stmtNode() {}
func (*MetaStmt) stmtNode()   {}

type BlockStmt struct {
	List []Stmt
}

type ReturnStmt struct{}

type LoopStmt struct {
	Cond Expr
	Body *BlockStmt
}

type BranchStmt struct {
	Tok BranchToken
}

type MetaStmt struct {
	Meta Meta
}

type DeclStmt struct {
	Decl Decl
}

type IfStmt struct {
	Cond Expr
	Body *BlockStmt
	Else Stmt // BlockStmt, IfStmt or nil
}

// Variable Reference for assignment
type VarRef struct {
	// If Parent != nil then it contains the reference struct and Name is
	// the field
	Parent *VarRef
	Name   string
}

type AssignStmt struct {
	Lhs []VarRef
	Rhs Expr
}

type Expr interface {
	exprNode()
}

func (*BinaryExpr) exprNode()   {}
func (*CondExpr) exprNode()     {}
func (*Ident) exprNode()        {}
func (*BasicLit) exprNode()     {}
func (*CallExpr) exprNode()     {}
func (*StructLit) exprNode()    {}
func (*SelectorExpr) exprNode() {}

// func (*CompositeLit) exprNode() {}

type CondExpr struct {
	Cond      Expr
	CaseTrue  Expr
	CaseFalse Expr
}

type BinaryExpr struct {
	X  Expr
	Op Op
	Y  Expr
}

type Ident struct {
	Name string
}

type BasicLit struct {
	Type  Type // TODO: Replace with PrimType
	Value int64
}

type KeyValueExpr struct {
	Key   string
	Value Expr
}

type StructLit struct {
	Name      string
	KeyValues []KeyValueExpr
}

type SelectorExpr struct {
	X   Expr
	Sel string
}

type Meta interface {
	metaNode()
}

func (*LvlDelta) metaNode() {}
func (*Comment) metaNode()  {}

type LvlDelta struct {
	Delta int
}

type Comment struct {
	Value string
}

type CallExpr struct {
	Fun  string
	Args []Expr
}

// TODO
// type CompositeLit struct{}

type BranchToken int

const (
	BREAK BranchToken = iota
	CONTINUE
)

type Op int

const (
	ILLEGAL Op = iota

	ADD // +
	SUB // -
	MUL // *
	QUO // /
	REM // %

	AND // &
	OR  // |
	XOR // ^
	SHL // <<
	SHR // >>

	EQL // ==
	LSS // <
	GTR // >
	NEQ // !=
	LEQ // <=
	GEQ // >=
)

var ops = [...]string{
	ILLEGAL: "ILLEGAL",

	ADD: "+",
	SUB: "-",
	MUL: "*",
	QUO: "/",
	REM: "%",

	AND: "&",
	OR:  "|",
	XOR: "^",
	SHL: "<<",
	SHR: ">>",

	EQL: "==",
	LSS: "<",
	GTR: ">",
	NEQ: "!=",
	LEQ: "<=",
	GEQ: ">=",
}
