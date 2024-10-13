package ast

type File struct {
	Decls []Decl
}

type Decl interface {
	declNode()
}

func (*FuncDecl) declNode()   {}
func (*StructDecl) declNode() {}
func (*ConstDecl) declNode()  {}
func (*VarDecl) declNode()    {}
func (*TypeDecl) declNode()   {}

type Type interface {
	typeNode()
}

type PrimType struct {
	size   int
	signed bool
}

func (*PrimType) typeNode() {}

type FuncDecl struct {
	Name string
	Type FuncType
	Body BlockStmt
}

type FuncType struct {
	Params  []Field
	Results []Field
}

type Field struct {
	Name string
	Type Type
}

type StructDecl struct{}

// TODO
type ConstDecl struct{}

// TODO
type VarDecl struct {
	Name string
	Type Type
}

// TODO
type TypeDecl struct{}

type BlockStmt struct {
	List []Stmt
}

type Stmt interface {
	stmtNode()
}

func (*DeclStmt) stmtNode()   {}
func (*AssignStmt) stmtNode() {}
func (*ReturnStmt) stmtNode() {}

type DeclStmt struct {
	Decl Decl
}

type AssignStmt struct {
}

type ReturnStmt struct{}
