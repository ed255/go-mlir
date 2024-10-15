package ast

type File struct {
	Decls []Decl
}

type Decl interface {
	declNode()
}

func (*FuncDecl) declNode() {}

// func (*StructDecl) declNode() {}
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

func (*PrimType) typeNode() {}

func NewPrimType(signed bool, size int) PrimType {
	return PrimType{signed: signed, size: size}
}

type FuncDecl struct {
	Name string
	Type FuncType
	Body []Stmt
}

type FuncType struct {
	Params  []Field
	Results []Field
}

type Field struct {
	Name string
	Type Type
}

// type StructDecl struct{}

// TODO
// type ConstDecl struct{}

// TODO
type VarDecl struct {
	Name string
	Type Type
}

// TODO
// type TypeDecl struct{}

// type BlockStmt struct {
// 	List []Stmt
// }

type Stmt interface {
	stmtNode()
}

func (*DeclStmt) stmtNode()   {}
func (*AssignStmt) stmtNode() {}
func (*MetaStmt) stmtNode()   {}

// func (*ReturnStmt) stmtNode() {}

type MetaStmt struct {
	Meta Meta
}

type DeclStmt struct {
	Decl Decl
}

// Variable Reference for assignment
type VarRef struct {
	Name string
}

type AssignStmt struct {
	Lhs VarRef
	Rhs Expr
}

// type ReturnStmt struct {
// 	Results []Expr
// }

type Expr interface {
	exprNode()
}

func (*BinaryExpr) exprNode() {}
func (*CondExpr) exprNode()   {}
func (*Ident) exprNode()      {}
func (*BasicLit) exprNode()   {}

// func (*CallExpr) exprNode()     {}
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

// TODO
type BasicLit struct {
	Type  Type
	Value int64
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

// TODO
// type CallExpr struct{}

// TODO
// type CompositeLit struct{}

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
