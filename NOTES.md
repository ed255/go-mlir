# Go to Verilog

## Supported features

- Bool 
    - All boolean operators
- All integer arithmetic types (signed and unsigned)
    - All arithmetic operators
- Function calls
    - Multiple arguments
    - Multiple returned values
- Structs
    - as function arguments
    - as function return value
    - construction
    - field get
    - TODO: field assignment
    - TODO: Nested structs
- TODO: Arrays
    - TODO: Indexing based on wire
    - TODO: Indexing based on constant
    - TODO: len
    - TODO: capacity
- Struct & Array combinations
    - TODO: Array of Structs
    - TODO: Struct of Array
- TODO: If/Else
- TODO: for loop
    - TODO: break
    - TODO: continue

## Unsupported features

- Pointer types
- Assign & Operation operator (like `+=`)
- Interfaces
- Map
    - delete
- Channels
    - select
    - close
- Strings
- floats
- complex
- anonymous functions
- anonymous structs
- goto
- defer
- make
- clear
- new
- panic
- println
- import

---

# AST

File = Decl*
Decl = FuncDecl | StructDecl | Const | Var | Type
FuncDecl = Ident FuncType BlockStmt
BlockStmt = Stmt*
Stmt = DeclStmt | AssignStmt | ReturnStmt
DeclStmt = Decl
