/*
 * File:        parser.c
 * Author:      Otabek Abduraimov
 * Purpose:     This file implements the parser for
 *              G1 subset of C-- grammar
 *              (https://d2l.arizona.edu/content/enforced/1407520-293-2241-1CSC453001/G1.html)
 * References:  Lecture Slides and StackOverflow
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "scanner.h"
#include "ast.h"
#include "code_gen.h"
////////////////////////////////////////////////////////////////////////////////////////////////
// PROPTOTYPES
void match(Token expected);
void PROG();
void TYPE();
void FUNC_OR_VAR();
ASTNode *FUNC_DEF_REST();
void VAR_DECL();
void VAR_DECL_REST();
ASTNode *ID_LIST();
SymbolTable *OPT_FORMALS();
SymbolTable *FORMALS();
void OPT_VAR_DECLS();
ASTNode *OPT_STMT_LIST();
ASTNode *STMT();
void FN_CALL_REST(ASTNode *ast_node);
ASTNode *OPT_EXPR_LIST(ASTNode *node);
void IF_STMT(ASTNode *ast_node);
void WHILE_STMT(ASTNode *ast_node);
void RETURN_STMT(ASTNode *ast_node);
void ASSG_STMT_REST(ASTNode *ast);
ASTNode *ARITH_EXP();
ASTNode *add_subB_expr(ASTNode *ast_node);
ASTNode *mul_div_expr();
ASTNode *mul_div_Helper(ASTNode *ast_node);
ASTNode *subU_expr();
ASTNode *basic_expr();
void ARITH_OP(ASTNode *ast_node);
ASTNode *EXPRS_LIST(ASTNode *ast_node);
ASTNode *BOOL_EXP();
ASTNode *or_expr_Helper(ASTNode *ast_node);
ASTNode *and_expr();
ASTNode *and_expr_Helper(ASTNode *ast_node);
ASTNode *rel_expr();

ASTNode *RELOP(ASTNode *ast);
SymbolTable *add_symbol_tb(Scope *scope,
                           char *name,
                           int type,
                           int scope_type,
                           int arg_count,
                           SymbolTable *p_head);
Scope *add_scope(int level);
void chk_sem(char *name, int scope);
void chk_func_decl(char *name, int param_count);
void chk_func_call(char *name, int arg_c);
void chk_var_decl(char *name);
void rmv_scope();
SymbolTable *get_st_ref(char *func_name);
void print_token(Token tok, char *lexeme); // TODO: TESTING
extern void gen_code(ASTNode *ast_node);
extern int get_token();

////////////////////////////////////////////////////////////////////////////////////////////////
#define  VAR   0
#define FUNC   1
#define GLOBAL 0
#define LOCAL  1
////////////////////////////////////////////////////////////////////////////////////////////////
// Globals
extern char    *lexeme;
extern int     line_number;
extern int     chk_decl_flag;
extern int     print_ast_flag;
extern int     gen_code_flag;
Token          curr_tok;
int            err_code;
char           *name;
char           *func_name;
Scope          *curr_scope;
Scope          *global_scope;
char           *token_name[];
int            param_c;
int            scope_index;
////////////////////////////////////////////////////////////////////////////////////////////////
int parse()
{
   name = malloc(sizeof(char *));
   func_name = malloc(sizeof(char *));

   if (chk_decl_flag)
   {
      scope_index = 0;
      curr_scope = global_scope = add_scope(scope_index); // add global scope
      add_symbol_tb(global_scope, "println", FUNC, scope_index, 1, NULL);
      
   }
   
   while ((curr_tok = get_token()) != EOF && !err_code)
   {
      PROG();
   }
   return err_code;
}
////////////////////////////////////////////////////////////////////////////////////////////////
// prog : func_defn prog | var_decl prog | <epsilon>;
void PROG()
{
   if (curr_tok == kwINT)
   {
      TYPE();
      strcpy(name, lexeme); // saving ID, or name, into var name
      strcpy(func_name, lexeme);
      match(ID);
      FUNC_OR_VAR();
      if (!err_code)
         PROG(); // if err_code, don't continue. Stop and exit
   }
   else if (curr_tok != kwINT && curr_tok != -1 && curr_tok != UNDEF)
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {prog}\n", line_number);
      return;
   }
   return;
}

// type	:	kwINT
void TYPE()
{
   match(kwINT);
}

void FUNC_OR_VAR()
{
   if (curr_tok == LPAREN)
      FUNC_DEF_REST();
   else if (curr_tok == SEMI)
   {
      if (chk_decl_flag)
      {
         chk_sem(name, GLOBAL);
         add_symbol_tb(global_scope, name, VAR, GLOBAL, -1, NULL);
      }
      match(SEMI);
   }
   else if (curr_tok == COMMA)
      VAR_DECL_REST();
   else
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {func_or_var}\n", line_number);
      return;
   }
}

// func_defn : type ID LPAREN opt_formals RPAREN LBRACE opt_var_decls opt_stmt_list RBRACE
ASTNode *FUNC_DEF_REST()
{
   // TYPE();
   // match(ID);

   ASTNode *ast_node = (ASTNode *)malloc(sizeof(ASTNode));
   ast_node->ntype = FUNC_DEF;
   ast_node->name = strdup(func_name);

   match(LPAREN);
   // switch to local scope
   if (chk_decl_flag)
   {
      scope_index++;
      curr_scope = add_scope(scope_index);
   }
   param_c = 0;
   SymbolTable *params_head = OPT_FORMALS();
   // get arg_count for the function from OPT_FORMALS()
   if (chk_decl_flag)
   {
      chk_sem(func_name, GLOBAL); // adding the current function to the global scope
      ast_node->st_ref = add_symbol_tb(global_scope,
                                       func_name,
                                       FUNC,
                                       GLOBAL,
                                       param_c,
                                       params_head);
   }

   match(RPAREN);
   match(LBRACE);
   OPT_VAR_DECLS();

   // AST CHILD or FUNCTION BODY
   ast_node->child0 = OPT_STMT_LIST(); // TODO: fill in code and place
                                       // AST CHILD or FUNCTION BODY

   match(RBRACE);
   if (print_ast_flag)
   {
      print_ast(ast_node);
   }

   if (gen_code_flag)
   {
      gen_code(ast_node);
   }

   // EXIT&REMOVE THE LOCAL SCOPE

   // if (chk_decl_flag)
   //    rmv_scope();
   return ast_node;
}

// var_decl : type id_list SEMI;
void VAR_DECL()
{
   TYPE();
   ID_LIST();
   match(SEMI);
}
void VAR_DECL_REST()
{
   // TYPE();
   if (chk_decl_flag)
   {
      chk_sem(name, GLOBAL);
      add_symbol_tb(curr_scope, name, VAR, GLOBAL, -1, NULL);
   }
   match(COMMA);
   ID_LIST();
   match(SEMI);
}

// id_list : ID | ID COMMA id_list;
ASTNode *ID_LIST()
{
   // assuming token is always ID
   // since both rules start with ID
   ASTNode *ast_node = (ASTNode *)malloc(sizeof(ASTNode));
   strcpy(name, lexeme);
   ast_node->ntype = IDENTIFIER;
   ast_node->name = strdup(name);

   match(ID);
   if (chk_decl_flag)
   {
      chk_sem(name, GLOBAL);
      ast_node->st_ref = add_symbol_tb(curr_scope, name, VAR, scope_index, -1, NULL);
   }

   if (curr_tok == COMMA)
   {
      match(COMMA);
      ast_node->child0 = ID_LIST();
   }
   return ast_node;
}

// opt_formals : <epsilon> | formals;
SymbolTable *OPT_FORMALS()
{
   SymbolTable *tb = NULL;
   if (curr_tok == kwINT)
   {
      tb = FORMALS();
   }
   return tb;
}

// formals : type ID COMMA formals | type ID;
SymbolTable *FORMALS()
{

   SymbolTable *tb = NULL;
   TYPE();
   strcpy(name, lexeme);
   match(ID);
   param_c++;
   if (chk_decl_flag)
   {
      // formals are always local
      chk_sem(name, LOCAL);
      tb = add_symbol_tb(curr_scope, name, VAR, LOCAL, -1, NULL);
   }
   if (curr_tok == COMMA)
   {
      match(COMMA);
      FORMALS();
   }
   return tb;
}

// opt_var_decls : <espilon> | var_decl opt_var_decls;
void OPT_VAR_DECLS()
{
   if (curr_tok == kwINT)
   {
      VAR_DECL();
      OPT_VAR_DECLS();
   }
   return;
}

// opt_stmt_list :	stmt opt_stmt_list | ε
ASTNode *OPT_STMT_LIST()
{
   if (curr_tok == ID ||
       curr_tok == kwIF ||
       curr_tok == kwRETURN ||
       curr_tok == LBRACE ||
       curr_tok == SEMI ||
       curr_tok == kwWHILE)
   {
      ASTNode *ast = (ASTNode *)malloc(sizeof(ASTNode));
      ast->ntype = STMT_LIST;
      ast->child0 = STMT();
      ast->child1 = OPT_STMT_LIST();
      return ast;
   }
   return NULL;
}

// stmt:	   fn_call SEMI
//       |	while_stmt | if_stmt | assg_stmt | return_stmt
//       |  LBRACE opt_stmt_list RBRACE
//       |  SEMI;
ASTNode *STMT()
{
   ASTNode *ast_node = (ASTNode *)malloc(sizeof(ASTNode));
   // storing ID for FN_CALL_REST & ASSG_STMT_REST
   if (curr_tok == ID)
   {
      strcpy(name, lexeme);
      match(ID);

      if (curr_tok == LPAREN)
      {
         strcpy(func_name, name);
         ast_node->ntype = FUNC_CALL;
         ast_node->name = strdup(func_name);
         ast_node->st_ref = get_st_ref(func_name);
         FN_CALL_REST(ast_node);
         match(SEMI);
      }
      else if (curr_tok == opASSG)
      {
         ast_node->ntype = ASSG;
         ASSG_STMT_REST(ast_node);
      }
   }
   else if (curr_tok == kwWHILE)
   {
      ast_node->ntype = WHILE;
      WHILE_STMT(ast_node);
   }
   else if (curr_tok == kwIF)
   {
      ast_node->ntype = IF;
      IF_STMT(ast_node);
   }
   else if (curr_tok == kwRETURN)
   {
      ast_node->ntype = RETURN;
      RETURN_STMT(ast_node);
   }
   else if (curr_tok == LBRACE)
   {
      match(LBRACE);
      ast_node->ntype = STMT_LIST;
      ast_node->child0 = OPT_STMT_LIST();
      ast_node = ast_node->child0;
      match(RBRACE);
   }
   else if (curr_tok == SEMI)
   {
      match(SEMI);
   }
   else
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {stmt}\n", line_number);
      return NULL;
   }
   return ast_node;
}

// if_stmt :   kwIF LPAREN bool_exp RPAREN stmt
//          |  kwIF LPAREN bool_exp RPAREN stmt kwELSE stmt;
void IF_STMT(ASTNode *ast_node)
{
   match(kwIF);
   match(LPAREN);
   ast_node->child0 = BOOL_EXP();
   match(RPAREN);
   ast_node->child1 = STMT();
   if (curr_tok == kwELSE)
   {
      match(kwELSE);
      ast_node->child2 = STMT();
   }
}

// while_stmt : kwWHILE LPAREN bool_exp RPAREN stmt
void WHILE_STMT(ASTNode *ast_node)
{
   match(kwWHILE);
   match(LPAREN);
   ast_node->child0 = BOOL_EXP();
   match(RPAREN);
   ast_node->child1 = STMT();
}

// return_stmt : kwRETURN SEMI | kwRETURN arith_exp SEMI;
void RETURN_STMT(ASTNode *ast_node)
{
   match(kwRETURN);
   if (curr_tok == SEMI)
   {
      match(SEMI);
   }
   else if (curr_tok == ID || curr_tok == INTCON || curr_tok == opSUB || curr_tok == LPAREN)
   {
      ast_node->child0 = ARITH_EXP();
      match(SEMI);
   }
}

// assg_stmt : ID opASSG arith_exp SEMI
ASTNode *ASSG_STMT()
{
   ASTNode *ast_node = (ASTNode *)malloc(sizeof(ASTNode));
   ast_node->ntype = ASSG;

   ASTNode *ast_id = (ASTNode *)malloc(sizeof(ASTNode));
   strcpy(name, lexeme);
   ast_id->ntype = IDENTIFIER;
   ast_id->name = strdup(name);

   ast_node->child0 = ast_id;
   match(ID);
   // check ID for declaration
   if (chk_decl_flag)
   {
      chk_sem(name, LOCAL);
   }
   match(opASSG);
   ast_node->child1 = ARITH_EXP();
   match(SEMI);
   return ast_node;
}
void ASSG_STMT_REST(ASTNode *ast)
{
   // match(ID);
   ASTNode *ast_id = (ASTNode *)malloc(sizeof(ASTNode));
   ast_id->ntype = IDENTIFIER;
   ast_id->name = strdup(name);
   ast_id->st_ref = get_st_ref(name);
   ast->child0 = ast_id;
   if (chk_decl_flag)
   {
      // printf("{asgn_stmt_rest} CHECKING VAR DECL: %s\n", name);
      chk_var_decl(name);
   }
   match(opASSG);
   ast->child1 = ARITH_EXP();
   match(SEMI);
}

// int arg_c;
// fn_call	:	ID   LPAREN   opt_expr_list   RPAREN
void FN_CALL_REST(ASTNode *ast_node)
{
   // match(ID);
   ast_node->tmp_arg_c = 0;
   match(LPAREN);
   ast_node->child0 = OPT_EXPR_LIST(ast_node);
   // check if it has been declared
   if (chk_decl_flag)
   {
      if (strcmp(ast_node->name, "main") != 0)
      {
         chk_func_call(ast_node->name, ast_node->tmp_arg_c);
      }
   }
   match(RPAREN);
}

int chk_agr_c;
void chk_func_call(char *name, int arg_c)
{
   chk_agr_c = 0;
   SymbolTable *st = get_st_ref(name);

   // if (strcmp(st->name, "println") == 0)
   // {
   //    return;
   // }

   if (st == NULL)
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {chk_func_call}\n", line_number);
      return;
   }

   if (st->param_c != arg_c)
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {chk_func_call}\n", line_number);
      return;
   }
}

// opt_expr_list : ε | expr_list
ASTNode *OPT_EXPR_LIST(ASTNode *node)
{
   ASTNode *ast = (ASTNode *)malloc(sizeof(ASTNode));
   if (  curr_tok == ID    || curr_tok == INTCON 
      || curr_tok == opSUB || curr_tok == LPAREN)
   {
      node->tmp_arg_c++;
      ast->ntype = EXPR_LIST;
      ast->child0 = EXPRS_LIST(node);
   }
   
   return ast;
}
// expr_list	:	arith_exp   COMMA   expr_list | arith_exp
ASTNode *EXPRS_LIST(ASTNode *node)
{  
   ASTNode *ast = (ASTNode *)malloc(sizeof(ASTNode));
   ast->ntype = EXPR_LIST;
   ast->child0 = ARITH_EXP();
   
   if (curr_tok == COMMA)
   {
      node->tmp_arg_c++;
      match(COMMA);
      ast->child1 = EXPRS_LIST(node);
   }
   return ast;
}

// ORIGINAL GRAMMAR:
//  bool_exp	         :	arith_exp   relop   arith_exp
//                      |	bool_exp   logical_op   bool_exp

// bool_exp : or_expr_Helper;
ASTNode *BOOL_EXP()
{
   ASTNode *left = and_expr();
   ASTNode *right = or_expr_Helper(left);
   if (right == left)
   {
      return left;
   }
   return right;
}

// or_expr_Helper : opOR and_expr or_expr_Helper
//                | ;
ASTNode *or_expr_Helper(ASTNode *ast_node)
{
   ASTNode *right;
   ASTNode *ast0 = (ASTNode *)malloc(sizeof(ASTNode));

   if (curr_tok == opOR)
   {
      match(opOR);
      right = and_expr();
      ast0->child0 = ast_node;
      ast0->ntype = OR;
      ast0->child1 = right;

      return or_expr_Helper(ast0);
   }
   return ast_node;
}

// and_expr : rel_expr and_expr_Helper;
ASTNode *and_expr()
{
   ASTNode *left = rel_expr();
   ASTNode *right = and_expr_Helper(left);
   if (right == NULL)
   {
      return left;
   }
   return right;
}

// and_expr_Helper   : opAND rel_expr and_expr_Helper
//                   | ;
ASTNode *and_expr_Helper(ASTNode *ast_node)
{
   ASTNode *right;
   ASTNode *ast0 = (ASTNode *)malloc(sizeof(ASTNode));

   if (curr_tok == opAND)
   {
      match(opAND);
      right = rel_expr();
      ast0->child0 = ast_node;
      ast0->ntype = AND;
      ast0->child1 = right;

      return and_expr_Helper(ast0);
   }
   return ast_node;
}

// rel_expr          : arith_exp relop arith_exp
//                   | arith_exp
ASTNode *rel_expr()
{
   ASTNode *ast0 = (ASTNode *)malloc(sizeof(ASTNode));
   ast0->child0 = ARITH_EXP();

   if (  curr_tok == opEQ || curr_tok == opNE 
      || curr_tok == opLE || curr_tok == opLT 
      || curr_tok == opGE || curr_tok == opGT)
   {
      RELOP(ast0);
      ast0->child1 = ARITH_EXP();
      return ast0;
   }
   else
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {rel_expr}\n", line_number);
      return NULL;
   }
   return ast0;
}

// relop	:	opEQ | opNE | opLE | opLT | opGE | opGT
ASTNode *RELOP(ASTNode *ast_node)
{
   if (curr_tok == opEQ)
   {
      ast_node->ntype = EQ;
      match(opEQ);
   }
   else if (curr_tok == opNE)
   {
      ast_node->ntype = NE;
      match(opNE);
   }
   else if (curr_tok == opLE)
   {
      ast_node->ntype = LE;
      match(opLE);
   }
   else if (curr_tok == opLT)
   {
      ast_node->ntype = LT;
      match(opLT);
   }
   else if (curr_tok == opGE)
   {
      ast_node->ntype = GE;
      match(opGE);
   }
   else if (curr_tok == opGT)
   {
      ast_node->ntype = GT;
      match(opGT);
   }
   else
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {relop}\n", line_number);
      return NULL;
   }
   return ast_node;
}


// arith_exp      : add_subB_expr;
ASTNode *ARITH_EXP()
{
   ASTNode *left = mul_div_expr();
   ASTNode *right = add_subB_expr(left);
   if (right == left)
   {
      return left;
   }
   return right;
}

// add_subB_Helper   : opADD mul_div_expr add_subB_Helper
//                   | opSUB mul_div_expr add_subB_Helper
//                   | ;
ASTNode *add_subB_expr(ASTNode *ast_node)
{
   ASTNode *right;
   ASTNode *ast0 = (ASTNode *)malloc(sizeof(ASTNode));
   if (curr_tok == opADD)
   {
      match(opADD);
      right = mul_div_expr();
      ast0->child0 = ast_node;
      ast0->ntype = ADD;
      ast0->child1 = right;
      return add_subB_expr(ast0);
   }
   else if (curr_tok == opSUB)
   {
      match(opSUB);
      right = mul_div_expr();
      ast0->child0 = ast_node;
      ast0->ntype = SUB;
      ast0->child1 = right;
      return add_subB_expr(ast0);
   }
   {
      // err_code = 1;
      // fprintf(stderr, "ERROR LINE %d: {add_subB_expr}\n", line_number);
      return ast_node;
   }
}

// mul_div_expr      : subU_expr mul_div_Helper;
ASTNode *mul_div_expr()
{
   ASTNode *left = subU_expr();
   ASTNode *right = mul_div_Helper(left);

   if (right == NULL)
   {
      return left;
   }
   return right;
}

// mul_div_Helper    : opMUL subU_expr mul_div_Helper
//                   | opDIV subU_expr mul_div_Helper
//                   | ;
ASTNode *mul_div_Helper(ASTNode *ast_node)
{
   ASTNode *right;
   ASTNode *ast0 = (ASTNode *)malloc(sizeof(ASTNode));
   if (curr_tok == opMUL)
   {
      match(opMUL);
      right = subU_expr();
      ast0->child0 = ast_node;
      ast0->ntype = MUL;
      ast0->child1 = right;
      return mul_div_Helper(ast0);
   }
   else if (curr_tok == opDIV)
   {
      match(opDIV);
      right = subU_expr();
      ast0->child0 = ast_node;
      ast0->ntype = DIV;
      ast0->child1 = right;
      return mul_div_Helper(ast0);
   }
   else
   {
      // err_code = 1;
      // fprintf(stderr, "ERROR LINE %d: {mul_div_Helper}\n", line_number);
      return ast_node;
   }
}

// subU_expr         : opSUB subU_expr | basic_expr;
ASTNode *subU_expr()
{
   ASTNode *ast0 = (ASTNode *)malloc(sizeof(ASTNode));
   if (curr_tok == opSUB)
   {
      match(opSUB);
      ast0->ntype = UMINUS;
      ast0->child0 = subU_expr();
      return ast0;
   }
   else if (curr_tok == ID || curr_tok == INTCON || curr_tok == LPAREN)
   {
      ast0 = basic_expr();
      return ast0;
   }
   else
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d:  {subU_expr}\n", line_number);
      return NULL;
   }
}

// basic_expr        : ID
//                   | INTCON
//                   | LPAREN arith_exp RPAREN
//                   | fn_call;
ASTNode *basic_expr()
{
   ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
   if (curr_tok == ID)
   {
      strcpy(name, lexeme);
      match(ID);

      if (curr_tok == LPAREN)
      {
         strcpy(func_name, name);
         node->ntype = FUNC_CALL;
         node->name = strdup(func_name);
         node->st_ref = get_st_ref(func_name);
         FN_CALL_REST(node);
      }
      else 
      {  
         node->ntype = IDENTIFIER;
         node->name = strdup(name);
         node->st_ref = get_st_ref(name);
         if (chk_decl_flag)
         {
            chk_var_decl(name);
         }
      }
      return node;
   }
   else if (curr_tok == INTCON)
   {

      node->ntype = INTCONST;
      node->num = atoi(lexeme);
      match(INTCON);
      return node;
   }
   else if (curr_tok == LPAREN)
   {
      match(LPAREN);
      node = ARITH_EXP();
      match(RPAREN);
      return node;
   }
   else
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {basic_expr}\n", line_number);
      return NULL;
   }
}

// arithop : opADD | opSUB | opMUL | opDIV;
void ARITH_OP(ASTNode *ast_node)
{
   if (curr_tok == opADD)
   {
      ast_node->ntype = ADD;
      match(opADD);
   }
   else if (curr_tok == opSUB)
   {
      ast_node->ntype = SUB;
      match(opSUB);
   }
   else if (curr_tok == opMUL)
   {
      ast_node->ntype = MUL;
      match(opMUL);
   }
   else if (curr_tok == opDIV)
   {
      ast_node->ntype = DIV;
      match(opDIV);
   }
   else
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {arith_op}\n", line_number);
      return;
   }
}

// logical_op : opAND | opOR;
void LOGICAL_OP(ASTNode *ast_node)
{
   if (curr_tok == opAND)
   {
      ast_node->ntype = AND;
      match(opAND);
   }
   else if (curr_tok == opOR)
   {
      ast_node->ntype = OR;
      match(opOR);
   }
   else
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {logical_op}\n", line_number);
      return;
   }
}

////////////////////////////////////////////////////////////////////////////////////////////////
/*
 * Function to add a symbol to the symbol table to a scope
 */
SymbolTable *add_symbol_tb(Scope *scope,
                           char *name,
                           int type,
                           int scope_num,
                           int param_c,
                           SymbolTable *params_head)
{
   // Create a new symbol entry
   SymbolTable *new_symbol = (SymbolTable *)malloc(sizeof(SymbolTable));
   if (new_symbol == NULL)
   {
      fprintf(stderr, "Memory allocation failed\n");
      return NULL;
   }

   // Fill in the details of the new symbol
   strcpy(new_symbol->name, name);
   new_symbol->type = type; // VAR or FUNC
   new_symbol->scope = scope_num;
   if (type == FUNC)
   {
      new_symbol->param_c = param_c;
      new_symbol->param_head = params_head; // could be NULL
   }
   new_symbol->next = NULL;

   // Add the symbol to the symbol table of the scope
   if (scope->symbol_table == NULL)
      scope->symbol_table = new_symbol;
   else
   {
      // Find the  last symbol in the table and link the new symbol
      SymbolTable *current = scope->symbol_table;
      while (current->next != NULL)
      {
         current = current->next;
      }
      current->next = new_symbol;
   }
   return new_symbol;
}

/*
 * Function to add a new scope to the list of scopes
 */
Scope *add_scope(int level)
{
   // Create a new scope
   Scope *new_scope = (Scope *)malloc(sizeof(Scope));
   if (new_scope == NULL)
   {
      fprintf(stderr, "Memory allocation failed\n");
      exit(EXIT_FAILURE);
   }

   // Initialize the symbol table of the scope
   // new_scope->level = level;
   new_scope->symbol_table = NULL;
   new_scope->next = NULL;
   new_scope->prev = NULL;

   // Add the new scope to the list of scopes
   if (curr_scope == NULL)
   {
      curr_scope = new_scope;
   }
   else
   {
      // Find the last scope in the list and link the new scope
      Scope *current = curr_scope;
      current->next = new_scope;
      new_scope->prev = current;
      curr_scope = new_scope;
   }
   return new_scope;
}

/*
 * Removes the currrent scope from the stack
 */
void rmv_scope()
{
   if (curr_scope == NULL)
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d:  {rmv_scope} \n", line_number);
      return;
   }

   if (curr_scope->prev == NULL)
   {
      free(curr_scope);
      return;
   }
   else
   {
      Scope *curr_scope = curr_scope;
      while (curr_scope != NULL)
      {
         SymbolTable *curr_tb = curr_scope->symbol_table;
         while (curr_tb != NULL)
         {
            SymbolTable *tmp = curr_tb;
            curr_tb = curr_tb->next;
            free(tmp);
         }
         Scope *tmp2 = curr_scope;
         curr_scope = curr_scope->prev;
         free(tmp2);
      }
   }
}

/*
 * Function to check if a variable is declared twice
 */
void chk_sem(char *name, int scope)
{
   SymbolTable *curr = curr_scope->symbol_table;
   while (curr != NULL)
   {
      if (strcmp(curr->name, name) == 0 && curr->scope == scope)
      {
         err_code = 1;
         fprintf(stderr, "ERROR LINE %d: {chk_sem}\n", line_number);
         return;
      }
      curr = curr->next;
   }
}

/*
 * Function to check function definition and signature
 */
int func_exists_flag;
void chk_func_decl(char *name, int arg_c)
{
   func_exists_flag = 0;
   Scope *curr_scope_tmp = curr_scope;

   while (curr_scope_tmp != NULL)
   {
      SymbolTable *curr_symb_tb = curr_scope_tmp->symbol_table;

      while (curr_symb_tb != NULL)
      { 
         if (strcmp(curr_symb_tb->name, "main") == 0 
         || strcmp(curr_symb_tb->name, "println") == 0)
         {
            continue;
         }

         if (  strcmp(curr_symb_tb->name, name) == 0 
            && curr_symb_tb->type == FUNC
            && curr_symb_tb->param_c == arg_c)
         {
            // make sure the function exists globally
            func_exists_flag = 1;
            break;
         }
         curr_symb_tb = curr_symb_tb->next;
      }
      curr_scope_tmp = curr_scope_tmp->prev;
   }

   if (func_exists_flag == 0)
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {chk_func_decl}\n", line_number);
      return;
   }
}

/*
 * Checks where the variable is declared before usage.
 */
int var_decl_flag;
void chk_var_decl(char *name)
{
   var_decl_flag = 0;
   // printf("VAR NAME: %s\n", name);
   Scope *curr_scope_tmp = curr_scope;

   while (curr_scope_tmp != NULL)
   {
      SymbolTable *curr_tb = curr_scope_tmp->symbol_table;
      while (curr_tb != NULL)
      {
         if (strcmp(curr_tb->name, name) == 0 && curr_tb->type == VAR)
         {
            var_decl_flag = 1;
            break;
         }
         curr_tb = curr_tb->next;
      }
      curr_scope_tmp = curr_scope_tmp->prev;
   }

   if (var_decl_flag == 0)
   {
      err_code = 1;
      fprintf(stderr, "ERROR LINE %d: {chk_var_decl} \n", line_number);
      return;
   }
}

SymbolTable *get_st_ref(char *func_name)
{
   Scope *curr = curr_scope;
   while (curr != NULL)
   {
      SymbolTable *st = curr->symbol_table;

      while (st != NULL)
      {
         if (strcmp(st->name, func_name) == 0)
         {
            return st;
         }
         st = st->next;
      }
      curr = curr->prev;
   }
   return NULL;
}

////////////////////////////////////////////////////////////////////////////////////////////////
// AST PRINT FUNCTIONS
/*
 * ptr: an arbitrary non-NULL AST pointer; ast_node_type() returns the node type
 * for the AST node ptr points to.
 */
ASTNodeType ast_node_type(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   return ast->ntype;
}

/*
 * ptr: pointer to an AST for a function definition; func_def_name() returns
 * a pointer to the function name (a string) of the function definition AST that
 * ptr points to.
 */
char *func_def_name(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   return ast->name;
}

/*
 * ptr: pointer to an AST for a function definition; func_def_nargs() returns
 * the number of formal parameters for that function.
 */
int func_def_nargs(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   return ast->st_ref->param_c;
}

/*
 * ptr: pointer to an AST for a function definition, n is an integer. If n > 0
 * and n <= no. of arguments for the function, then func_def_argname() returns
 * a pointer to the name (a string) of the nth formal parameter for that function;
 * the first formal parameter corresponds to n == 1.  If the value of n is outside
 * these parameters, the behavior of this function is undefined.
 */
char *func_def_argname(void *ptr, int n)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);

   if (n > 0 && n <= ast->st_ref->param_c)
   {
      SymbolTable *p_head = ast->st_ref->param_head;
      int i = 1;
      while (i < n)
      {
         p_head = p_head->next;
         i++;
      }
      return p_head->name;
   }
   else
   {
      printf("Error: n is outside the valid range of parameters\n");
      exit(EXIT_FAILURE);
   }
}

/*
 * ptr: pointer to an AST for a function definition; func_def_body() returns
 * a pointer to the AST that is the function body of the function that ptr
 * points to.
 */
void *func_def_body(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child0 != NULL && ast->child0->ntype != 0)
      return ast->child0;
   return NULL;
}

/*
 * ptr: pointer to an AST node for a function call; func_call_callee() returns
 * a pointer to a string that is the name of the function being called.
 */
char *func_call_callee(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   return ast->name;
}

/*
 * ptr: pointer to an AST node for a function call; func_call_args() returns
 * a pointer to the AST that is the list of arguments to the call.
 */
void *func_call_args(void *ptr)
{
   ASTNode *ast = ptr;
   // printf("HERE\n");
   assert(ast != NULL);
   if (ast->child0 != NULL && ast->child0->ntype != 0)
      return ast->child0;
   return NULL;
}

/*
 * ptr: pointer to an AST node for a statement list; stmt_list_head() returns
 * a pointer to the AST of the statement at the beginning of this list.
 */
void *stmt_list_head(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child0 != NULL && ast->child0->ntype != 0)
      return ast->child0;
   return NULL;
}

/*
 * ptr: pointer to an AST node for a statement list; stmt_list_rest() returns
 * a pointer to the AST of the rest of this list (i.e., the pointer to the
 * next node in the list).
 */
void *stmt_list_rest(void *ptr)
{
   // printf("HERE\n");
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child1 != NULL && ast->child1->ntype != 0)
      return ast->child1;
   return NULL;
}

/*
 * ptr: pointer to an AST node for an expression list; expr_list_head() returns
 * a pointer to the AST of the expression at the beginning of this list.
 */
void *expr_list_head(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child0 != NULL && ast->child0->ntype != 0)
      return ast->child0;
   return NULL;
}

/*
 * ptr: pointer to an AST node for an expression list; expr_list_rest() returns
 * a pointer to the AST of the rest of this list (i.e., the pointer to the
 * next node in the list).
 */
void *expr_list_rest(void *ptr)
{
   // THIS IS THE BOOL_EXPR
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child1 != NULL && ast->child1->ntype != 0)
      return ast->child1;
   return NULL;
}

/*
 * ptr: pointer to an AST node for an IDENTIFIER; expr_id_name() returns a
 * pointer to the name of the identifier (a string).
 */
char *expr_id_name(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   return ast->name;
}

/*
 * ptr: pointer to an AST node for an INTCONST; expr_intconst_val() returns the
 * integer value of the constant.
 */
int expr_intconst_val(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   return ast->num;
}

/*
 * ptr: pointer to an AST node for an arithmetic or boolean expression.
 * expr_operand_1() returns a pointer to the AST of the first operand.
 */
void *expr_operand_1(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child0 != NULL && ast->child0->ntype != 0)
      return ast->child0;
   return NULL;
}

/*
 * ptr: pointer to an AST node for an arithmetic or boolean expression.
 * expr_operand_2() returns a pointer to the AST of the second operand.
 */
void *expr_operand_2(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child1 != NULL && ast->child1->ntype != 0)
      return ast->child1;
   return NULL;
}

/*
 * ptr: pointer to an AST node for an IF statement.  stmt_if_expr() returns
 * a pointer to the AST for the expression tested by the if statement.
 */
void *stmt_if_expr(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child0 != NULL && ast->child0->ntype != 0)
      return ast->child0;
   return NULL;
}

/*
 * ptr: pointer to an AST node for an IF statement.  stmt_if_then() returns
 * a pointer to the AST for the then-part of the if statement, i.e., the
 * statement to be executed if the condition is true.
 */
void *stmt_if_then(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child1 != NULL && ast->child1->ntype != 0)
      return ast->child1;
   return NULL;
}

/*
 * ptr: pointer to an AST node for an IF statement.  stmt_if_else() returns
 * a pointer to the AST for the else-part of the if statement, i.e., the
 * statement to be executed if the condition is false.
 */
void *stmt_if_else(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child2 != NULL && ast->child2->ntype != 0)
      return ast->child2;
   return NULL;
}

/*
 * ptr: pointer to an AST node for an assignment statement.  stmt_assg_lhs()
 * returns a pointer to the name of the identifier on the LHS of the
 * assignment.
 */
char *stmt_assg_lhs(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   return ast->child0->name;
}

/*
 * ptr: pointer to an AST node for an assignment statement.  stmt_assg_rhs()
 * returns a pointer to the AST of the expression on the RHS of the assignment.
 */
void *stmt_assg_rhs(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   return ast->child1;
}

/*
 * ptr: pointer to an AST node for a while statement.  stmt_while_expr()
 * returns a pointer to the AST of the expression tested by the while statement.
 */
void *stmt_while_expr(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child0 != NULL && ast->child0->ntype != 0)
      return ast->child0;
   return NULL;
}

/*
 * ptr: pointer to an AST node for a while statement.  stmt_while_body()
 * returns a pointer to the AST of the body of the while statement.
 */
void *stmt_while_body(void *ptr)
{
   ASTNode *ast = ptr;
   assert(ast != NULL);
   if (ast->child1 != NULL && ast->child1->ntype != 0)
      return ast->child1;
   return NULL;
}

/*
 * ptr: pointer to an AST node for a return statement.  stmt_return_expr()
 * returns a pointer to the AST of the expression whose value is returned.
 */
void *stmt_return_expr(void *ptr)
{
   ASTNode *ast = ptr;
   return ast->child0;
}

////////////////////////////////////////////////////////////////////////////////////////////////
// TODO: TESTING

void match(Token expected)
{
   if (expected == EOF)
      return;

   if (curr_tok == expected)
   {
      // print_token(curr_tok, lexeme);
      curr_tok = get_token();
      if (curr_tok == UNDEF)
      {
         curr_tok = get_token();
      }
   }
   else
   {
      // generate the error message
      err_code = 1;
      // printf("EXPECTED: %s\n", token_name[expected]);
      // print_token(curr_tok, lexeme);
      fprintf(stderr, "ERROR LINE %d: {match} \n", line_number);
      return;
   }
}

// TODO: remove this
/*
 * token_name is an array of strings that gives, for each token value,
 * a string indicating the type of token.
 */
char *token_name[] = {
    "UNDEF",
    "ID",
    "INTCON",
    "LPAREN",
    "RPAREN",
    "LBRACE",
    "RBRACE",
    "COMMA",
    "SEMI",
    "kwINT",
    "kwIF",
    "kwELSE",
    "kwWHILE",
    "kwRETURN",
    "opASSG",
    "opADD",
    "opSUB",
    "opMUL",
    "opDIV",
    "opEQ",
    "opNE",
    "opGT",
    "opGE",
    "opLT",
    "opLE",
    "opAND",
    "opOR",
    "opNOT",
};

void print_token(Token tok, char *lexeme)
{
   if (tok < UNDEF || tok > opNOT)
      printf("TOKEN VALUE OUT OF BOUNDS: %d\n", tok);
   else
      printf("LINE: {%d}     %s : %s\n", line_number, token_name[tok], lexeme);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////