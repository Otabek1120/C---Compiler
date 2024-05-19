#ifndef __CODE_GEN_H__
#define __CODE_GEN_H__

#include "ast.h"

// Enumeration defining types of operands, (x + y: x and y are operands)
typedef enum
{
   INTCONSTANT,
   SYMB_TB_PTR,
   // Add more operand types as needed
} OperandType;

// Enumeration defining types of operations
typedef enum
{
   PLUS,
   MINUS,
   MULTIPLY,
   DIVIDE,
   MOV,
   LABEL,
   ENTER,
   LEAVE,
   RET,
   STMTLIST,
   PARAM,
   CALL,
   RETRIEVE,
   ASSGN,
   IF_EQ,
   IF_NE,
   IF_LE,
   IF_LT,
   IF_GE,
   IF_GT,
   GOTO,
   ELSE,
   U_MINUS, 
   LOG_AND, 
   LOG_OR
} OpType;

// Define a structure for each symbol entry
typedef struct symbol_table
{
   char                 name[64];
   int                  val;           
   int                  type;       // 0 for int variable, 1 for function
   int                  scope;      // 0 for global scope, 1 for local scope
   int                  param_c;    // only for functions
   int                  offset;         
   struct symbol_table *param_head; // pointer to symbol table of parameters
   struct symbol_table *next;       // Pointer to the next symbol in the table
} SymbolTable;

// Define a structure for scope
typedef struct scope
{
   SymbolTable       *symbol_table;
   int               level;         // 0 for global scope, 1 for local scope, 2 
   struct scope      *next;
   struct scope      *prev;
} Scope;

////////////////////////////////////////////////////////////////////////////////////////////////

// Structure representing an operand
typedef struct
{
   OperandType       operand_type;
   struct
   {
      int            iconst;          // Integer constant
      SymbolTable    *st_ptr; // Symbol table pointer
      int            offset;
   } val;
} Operand;

// Structure representing a quadruple
typedef struct quad
{
   OpType         op;         // Type of operation
   Operand        *src1;      // Source operand 1
   Operand        *src2;      // Source operand 2
   SymbolTable    *dest;      // symbol table entry to store the intermediate result
   struct quad    *next;
} Quad;

typedef struct ast_node
{
   ASTNodeType          ntype;   /* what type of node */
   char                 *name;
   SymbolTable          *st_ref; /* symbol table ref for variables, function calls */
   int                  num;     /* integer onstacnts */
   struct ast_node      *child0, *child1, *child2;
   Quad                 *code;
   SymbolTable          *place;
   int                  tmp_arg_c;
} ASTNode;

#endif /* __SCANNER_H__ */