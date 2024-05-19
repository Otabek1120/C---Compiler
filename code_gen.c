#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "code_gen.h"
#include "ast.h"

SymbolTable *newtemp(int t);
Quad *newinstr(OpType opType, void *src1, void *src2, void *dest);
extern SymbolTable *add_symbol_tb(Scope *scope,
                                  char *name,
                                  int type,
                                  int scope_type,
                                  int arg_count,
                                  SymbolTable *p_head);
extern SymbolTable *get_st_ref(char *func_name);
void codeGen_stmt(ASTNode *s);
void codeGen_expr(ASTNode *e);
void codeGen_func_call(ASTNode *f_call);
void _codeGen_arg_list(ASTNode *arg_list);
void codeGen_assgn(ASTNode *assgn_node);
void print_3ac(Quad *head);
void genMips_code(Quad *head);
void codeGen_opc(ASTNode *node, char *op);
void codeGen_uminus(ASTNode *node);
void codeGen_id(ASTNode *node);
void codeGen_iconst(ASTNode *iconst);
void codeGen_Bool(ASTNode *expr, ASTNode *s);
int getNumOf_locals();
void print_scope_and_symbs();
void _printlnCode();
void _print_globals();
Quad *newlabel();

void codeGen_IF(ASTNode *s);
void codeGen_IF_EXPR(ASTNode *expr, ASTNode *s, OpType op_type);
void _mips_enter(Quad *head);
void codeGen_WHILE(ASTNode *s);
void codeGen_stmt_list(ASTNode *s);
void codeGen_RETURN(ASTNode *s);
void codeGen_AND(ASTNode *expr, ASTNode *s);
////////////////////////////////////////////////////////////////////////////////////////////////

extern int     scope_index;
extern Scope   *curr_scope;
extern Scope   *global_scope;
extern int     print_ast_flag;
static int     tmp_num = 0;
Quad           *curr_instr;
Quad           *intrs_head;
int            space;

void gen_code(ASTNode *ast_node)
{
   if (ast_node->ntype == FUNC_DEF)
   {
      space = 1;

      // ENTER func
      Quad *instrEnter = newinstr(ENTER, NULL, NULL, ast_node->st_ref);
      ast_node->code = instrEnter;
      curr_instr = intrs_head = instrEnter;

      // code for the body of the function
      ASTNode *stmt_node = func_def_body(ast_node);
      while (stmt_node != NULL)
      {
         codeGen_stmt(stmt_node->child0);
         stmt_node = stmt_list_rest(stmt_node);
      }

      space = getNumOf_locals();

      // LEAVE func
      Quad *instr_leave = newinstr(LEAVE, NULL, NULL, ast_node->st_ref);
      curr_instr->next = instr_leave;
      curr_instr = curr_instr->next;

      // RETURN
      Quad *instr_ret = newinstr(RET, NULL, NULL, ast_node->place);
      curr_instr->next = instr_ret;
      curr_instr = curr_instr->next;

      if (print_ast_flag)
      {
         printf("# ******************PRINTING 3-ADDRESS CODE************************\n");
         print_3ac(intrs_head);
         printf("\n\n");

         printf("# *****************PRINTING SYMBOL TABLE ENTRIES******************\n");
         print_scope_and_symbs();
         printf("\n\n");
      }

      printf("# ***********************PRINTING MIPS CODE*********************\n");
      genMips_code(intrs_head);
   }
   else
   {
      printf("AST NOT A FUNCTION\n");
   }
}

int getNumOf_locals()
{
   int offset = -4;
   int ret = 0;
   SymbolTable *curr = curr_scope->symbol_table;
   while (curr != NULL)
   {
      // printf("SYM_TB_ENTRY_NAME: %s\t\t", curr->name);
      curr->offset = offset;
      offset -= 4;
      // printf("SYM_TB_ENTRY_OFFSET: %d\n", curr->offset);
      curr = curr->next;
      ret++;
   }
   // printf("SPACE: %d\n", ret);
   return ret;
}

ASTNode *list_hd;
void codeGen_stmt(ASTNode *s)
{

   // printf("[codeGen_stmt] STMT->NTYPE: %d\n", s->ntype);
   switch (s->ntype)
   {
   case FUNC_CALL:
      codeGen_func_call(s);
      break;
   case ASSG:
      codeGen_assgn(s);
      break;
   case IF:
      codeGen_IF(s);
      break;
   case WHILE:
      codeGen_WHILE(s);
      break;
   case STMT_LIST:
      codeGen_stmt_list(s);
      break;
   case RETURN:
      codeGen_RETURN(s);
      break;
   default:
      break;
   }
}

void codeGen_func_call(ASTNode *ast_f_call)
{
   ASTNode *arg_list = func_call_args(ast_f_call);

   // SymbolTable *tb = get_st_ref(ast_f_call->name);
   // printf("ARG_LISTHEAD_NAME: %s\n", tb->name);

   // E.place = newtemp(f_call.returnType);
   // ast_f_call->place = newtemp(0);

   // E.code = arg_list.code;
   if (arg_list != NULL)
      ast_f_call->code = arg_list->code;

   // PARAMs:
   // newinstr(PARAM, argk, NULL, NULL);
   // newinstr(PARAM, arg1, NULL, NULL);
   _codeGen_arg_list(arg_list);
   // glues together the intrs

   // CALL: newinstr(CALL, f, k, NULL);
   Operand *src1 = (Operand *)malloc(sizeof(Operand));
   src1->operand_type = SYMB_TB_PTR;
   src1->val.st_ptr = get_st_ref(ast_f_call->name);

   Operand *src2 = (Operand *)malloc(sizeof(Operand));
   src2->operand_type = SYMB_TB_PTR;
   if (arg_list != NULL)
      src2->val.st_ptr = arg_list->st_ref;

   Quad *call = newinstr(CALL, src1, src2, NULL);

   curr_instr->next = call;
   curr_instr = curr_instr->next;

   // RETRIEVE: newinstr(RETRIEVE, NULL, NULL, E.place)
   // Quad *retreive = newinstr(RETRIEVE, NULL, NULL, ast_f_call->place);
   // curr_instr->next = retreive;
   // curr_instr = curr_instr->next;
}
void _codeGen_arg_list(ASTNode *arg_list)
{
   // printf("CODE_GEN_ARG_LIST \t\t");
   // printf("ARG_LIST TYPE {%d} \t\t", arg_list->ntype);

   // if arglist or arglis.child0 is null, skip;
   int pos = 1;
   if (arg_list != NULL && arg_list->child0 != NULL)
   {
      // ASTNode *curr = arg_list->child0->child0;
      ASTNode *curr = expr_list_head(arg_list);
      // printf("CURR->CHILD0: %d\n", curr->child0->ntype);
      while (curr != NULL)
      {
         Operand *src1 = (Operand *)malloc(sizeof(Operand));
         if (curr->child0->ntype == INTCONST)
         {
            // printf("CURR NODE VALUE {%d}\n", curr->child0->num);
            src1->operand_type = INTCONSTANT;
            src1->val.iconst = curr->child0->num;
            src1->val.offset = (pos + 1) * 4;
            // space++;

            SymbolTable *tmp = newtemp(0);
            src1->val.st_ptr = tmp;
            Quad *intrs_assgn = newinstr(ASSGN, src1, NULL, tmp);
            curr_instr->next = intrs_assgn;
            curr_instr = curr_instr->next;
         }
         else
         {
            // printf("CURR NODE NAME {%s}\n", curr->child0->name);
            src1->operand_type = SYMB_TB_PTR;
            src1->val.st_ptr = curr->child0->st_ref;
            src1->val.iconst = curr->child0->num;
            src1->val.offset = (pos + 1) * 4;
            // space++;
         }
         pos++;

         Quad *param = newinstr(PARAM, src1, NULL, NULL);
         curr->code = param;

         curr_instr->next = param;
         curr_instr = curr_instr->next;
         curr = expr_list_rest(curr);
      }
   }
}

void codeGen_assgn(ASTNode *assgn_node)
{
   // codeGen_expr(LHS);
   ASTNode *lhs = assgn_node->child0;
   // printf("LHS : %s\t\t", lhs->name);
   codeGen_expr(lhs);

   // codeGen_expr(RHS);
   ASTNode *rhs = stmt_assg_rhs(assgn_node);
   codeGen_expr(rhs);

   // S.code = LHS.code
   //          + RHS.code
   //          + newinstr(ASSG, RHS.place, NULL, LHS.place)
   if (lhs->code != NULL)
   {
      curr_instr->next = lhs->code;
      curr_instr = curr_instr->next;
      assgn_node->code = curr_instr;
   }
   if (rhs->code != NULL)
   {
      curr_instr->next = rhs->code;
      curr_instr = curr_instr->next;
      assgn_node->code = curr_instr;
   }

   Operand *src1 = (Operand *)malloc(sizeof(Operand));
   src1->operand_type = INTCONSTANT;
   src1->val.iconst = rhs->num;
   src1->val.st_ptr = rhs->place;

   // printf("RHS : %s\n", rhs->place->name);

   Quad *intrs_assgn = newinstr(ASSGN, src1, NULL, lhs->place);
   space++;
   curr_instr->next = intrs_assgn;
   curr_instr = curr_instr->next;
}

void codeGen_IF(ASTNode *s)
{
   ASTNode *expr = stmt_if_expr(s);
   codeGen_Bool(expr, s);
}

void codeGen_WHILE(ASTNode *s)
{
   // printf("HERE\n");
   // Ltop = newlabel();
   Quad *L_top = newlabel();

   // codeGen_bool(B, Lbody, Lafter);
   ASTNode *bool_expr = stmt_while_expr(s);
   codeGen_Bool(bool_expr, s);

   // codeGen_stmt(S1);
   ASTNode *body = stmt_while_body(s);
   // printf("WHILE_BODY: %d\n", body->ntype);
   codeGen_stmt(body);

   // S.code = Ltop
   s->code = L_top;
   curr_instr->next = s->code;
   curr_instr = curr_instr->next;
   // printf("CURR_INSTR: %d\n", curr_instr->op);

   //          + B.code
   if (bool_expr->code != NULL)
   {
      curr_instr->next = bool_expr->code;
      curr_instr = curr_instr->next;
      // printf("BOOL_EXPR_CODE: %d\n", curr_instr->op);
   }

   // Lbody = newlabel();
   Quad *L_body = newlabel();
   //          + Lbody
   curr_instr->next = L_body;
   curr_instr = curr_instr->next;

   //          + S1.code
   if (body->code != NULL)
   {
      curr_instr->next = body->code;
      curr_instr = curr_instr->next;
   }

   //          + newinstr(GOTO, NULL, NULL, Ltop)

   curr_instr->next = newinstr(GOTO, NULL, NULL, L_top->dest);
   curr_instr = curr_instr->next;
   // printf("L_top = %d\n", L_top->dest->val);
   // printf("L_body = %d\n", L_body->dest->val);
   // printf("L_after = %d\n", L_after->dest->val);

   // Lafter = newlabel();
   Quad *L_after = newlabel();
   //          + Lafter;
   curr_instr->next = L_after;
   curr_instr = curr_instr->next;
}

void codeGen_stmt_list(ASTNode *s)
{
   while (s != NULL)
   {
      list_hd = stmt_list_head(s);
      s = stmt_list_rest(s);
      codeGen_stmt(list_hd);
   }
}

void codeGen_RETURN(ASTNode *s)
{
   // printf("codeGen_RETURN: %d\n", s->ntype);
   ASTNode *expr = stmt_return_expr(s);
   if (expr != NULL)
   {
      Operand *src1 = (Operand *)malloc(sizeof(Operand));
      if (expr->ntype == INTCONST)
      {
         SymbolTable *expr_iconst = newtemp(0);
         // printf("NEW_TMPL: %s\n", expr_iconst->name);
         src1->operand_type = INTCONSTANT;
         src1->val.iconst = expr->num;
         src1->val.st_ptr = expr_iconst;
         // printf("ST_REF_VAL: %s\n", src1->val.st_ptr->name);
         // printf("ST_REF_OFFSET: %d\n", expr_iconst->offset);
      }
      else
      {
         src1->operand_type = SYMB_TB_PTR;
         src1->val.st_ptr = expr->st_ref;
         // printf("ST_REF: %s\n", expr->st_ref->name);
      }
      s->code = newinstr(RET, src1, NULL, NULL);
   }
   else
   {
      printf("EXPR: %d\n", expr->num);
      s->code = newinstr(RET, NULL, NULL, NULL);
   }
   curr_instr->next = s->code;
   curr_instr = curr_instr->next;
}

void codeGen_Bool(ASTNode *expr, ASTNode *s)
{
   // printf("[codeGen_Bool] STMT->NTYPE: %d\n", expr->ntype);
   switch (expr->ntype)
   {
   case EQ:
      codeGen_IF_EXPR(expr, s, IF_EQ);
      break;
   case NE:
      codeGen_IF_EXPR(expr, s, IF_NE);
      break;
   case LE:
      codeGen_IF_EXPR(expr, s, IF_LE);
      break;
   case LT:
      codeGen_IF_EXPR(expr, s, IF_LT);
      break;
   case GE:
      codeGen_IF_EXPR(expr, s, IF_GE);
      break;
   case GT:
      codeGen_IF_EXPR(expr, s, IF_GT);
      break;
   case AND:
      codeGen_AND(expr, s);
      break;
   case OR:
      break;   
   default:
      break;
   }
}

void codeGen_AND(ASTNode *expr, ASTNode *s)
{
   // L1 = newlabel();
   Quad *L1 = newlabel();

   // codeGen_bool(B1, L1, falseDst);
   ASTNode *b1 = expr_operand_1(expr);
   codeGen_Bool(b1, s);

   curr_instr->next = L1;
   curr_instr = curr_instr->next;
   
   // codeGen_bool(B2, trueDst, falseDst);
   ASTNode *b2 = expr_operand_2(expr);
   codeGen_Bool(b2, s);

   // B.code = B1.code + L1 + B2.code;

}

void codeGen_IF_EXPR(ASTNode *expr, ASTNode *s, OpType op_type)
{

   ASTNode *operand_1 = expr_operand_1(expr);

   // codeGen_expr(E1);
   codeGen_expr(operand_1);

   // codeGen_expr(E2);
   ASTNode *operand_2 = expr_operand_2(expr);
   codeGen_expr(operand_2);

   // TESTTING
   // printf("[codeGen_Bool] STMT->NTYPE: %d\n", expr->ntype);
   // printf("EXPRS_1: %s\n", operand_1->name);
   // printf("EXPRS_2: %s\n", operand_2->name);

   // E.code = E1.code
   //          + E2.code
   //          + newinstr(IF_GT, E1.place, E2.place, trueDst)
   //          + newinstr(GOTO, NULL, NULL, falseDst

   if (operand_1->code != NULL)
   {
      s->code = operand_1->code;
      curr_instr->next = operand_1->code;
      curr_instr = curr_instr->next;

      if (operand_2->code != NULL)
      {
         curr_instr->next = operand_2->code;
         curr_instr = curr_instr->next;
      }
   }

   else
   {
      if (operand_2->code != NULL)
      {
         s->code = operand_2->code;
         curr_instr->next = operand_2->code;
         curr_instr = curr_instr->next;
      }
   }
   Operand *src1 = (Operand *)malloc(sizeof(Operand));
   src1->val.st_ptr = operand_1->place;

   Operand *src2 = (Operand *)malloc(sizeof(Operand));
   src2->val.st_ptr = operand_2->place;

   Quad *trueDst = newlabel();

   if (s->code == NULL)
   {
      s->code = newinstr(op_type, src1, src2, trueDst->dest);
      curr_instr->next = s->code;
      curr_instr = curr_instr->next;
   }
   else
   {
      curr_instr->next = newinstr(op_type, src1, src2, trueDst->dest);
      curr_instr = curr_instr->next;
   }

   // else then
   ASTNode *else_body = stmt_if_else(s);
   if (else_body != NULL)
      codeGen_stmt(else_body);

   // LABEL _L1
   Quad *falseDst = newlabel();
   curr_instr->next = newinstr(GOTO, NULL, NULL, falseDst->dest);
   curr_instr = curr_instr->next;

   // LABEL _L0
   curr_instr->next = trueDst;
   curr_instr = curr_instr->next;

   // if then
   ASTNode *body = stmt_if_then(s);
   if (body != NULL)
      codeGen_stmt(body);

   // LABEL _L1
   curr_instr->next = falseDst;
   curr_instr = curr_instr->next;
}

void codeGen_expr(ASTNode *e)
{
   // printf("[codeGen_expr] EXPR->NTYPE: %d\n", e->ntype);
   switch (e->ntype)
   {
   case ADD:
      codeGen_opc(e, "ADD");
      break;
   case SUB:
      codeGen_opc(e, "SUB");
      break;
   case MUL:
      codeGen_opc(e, "MUL");
      break;
   case DIV:
      codeGen_opc(e, "DIV");
      break;
   case UMINUS:
      codeGen_uminus(e);
      break;
   case IDENTIFIER:
      codeGen_id(e);
      break;
   case INTCONST:
      codeGen_iconst(e);
      break;
   default:
      printf( "{codeGen_expr} Unknown case type");
      break;
   }
}

void codeGen_opc(ASTNode *node, char *op_name)
{
   // codeGen_expr(E1);
   ASTNode *operand_1 = expr_operand_1(node);
   codeGen_expr(operand_1);

   // codeGen_expr(E2);
   ASTNode *operand_2 = expr_operand_2(node);
   codeGen_expr(operand_2);

   // E.place = newtemp(E.type);
   node->place = newtemp(0);

   // E.code = E1.code 
   Operand *src1 = (Operand *)malloc(sizeof(Operand));
   src1->val.st_ptr = operand_1->place;

   // + E2.code
   Operand *src2 = (Operand *)malloc(sizeof(Operand));
   src2->val.st_ptr = operand_2->place;

   // + newinstr(PLUS, E1.place, E2.place, E.place);
   OpType op;
   if (strcmp(op_name, "ADD") == 0)
      op = PLUS;
   else if (strcmp(op_name, "SUB") == 0)
      op = MINUS;
   else if (strcmp(op_name, "MUL") == 0)
      op = MULTIPLY;
   else if (strcmp(op_name, "DIV") == 0)
      op = DIVIDE;
   else
   {
      fprintf(stderr, "Unknown op_name");
      return;
   }
   Quad *intrs_add = newinstr(op, src1, src2, node->place);
   space++; space++;
   curr_instr->next = intrs_add;
   curr_instr = curr_instr->next;
}


void codeGen_uminus(ASTNode *u_minusNode)
{
   // codeGen_expr(E1);
   ASTNode *operand_1 = expr_operand_1(u_minusNode);
   codeGen_expr(operand_1);

   // E.place = newtemp(E.type);
   u_minusNode->place = newtemp(0);

   // E.code = E1.code 
   Operand *src1 = (Operand *)malloc(sizeof(Operand));
   src1->val.st_ptr = operand_1->place;

   // +newinstr(UMINUS, E1.place, NULL, E.place);
   Quad *intrs_uMinus = newinstr(U_MINUS, src1, NULL, u_minusNode->place);
   space++;
   curr_instr->next = intrs_uMinus;
   curr_instr = curr_instr->next;
}


void codeGen_id(ASTNode *identifier)
{
   // E.place = id.loc; /* location: from symbol table */
   // printf("[codeGen_id] ID->NAME : %s\n", id->st_ref->name);
   identifier->place = identifier->st_ref;
   identifier->place->val = identifier->num;

   // E.code = NULL;
   identifier->code = NULL;
}


void codeGen_iconst(ASTNode *iconst)
{
   // E.place = newtemp(E.type);
   iconst->place = newtemp(0);

   // E.code = newinstr(ASSG, intcon.val, NULL, E.place);
   Operand *src1 = (Operand *)malloc(sizeof(Operand));
   src1->operand_type = INTCONSTANT;
   src1->val.iconst = iconst->num;
   src1->val.st_ptr = iconst->place;

   Quad *insts_assgn = newinstr(ASSGN, src1, NULL, iconst->place);
   iconst->code = insts_assgn;
   curr_instr->next = insts_assgn;
   curr_instr = curr_instr->next;
   // printf("[codeGen_iconst] ICONST->OP : %d\n", iconst->code->op);
}



////////////////////////////////////////////////////////////////////////////////////////////////
/*
 * Creates a symbol table entry for a new temporary
 * and returns a pointer to this ST entry.
 */
SymbolTable *newtemp(int t)
{
   // ntmp->name => create a new name that doesn’t conflict
   char *name = (char *)malloc(15 * sizeof(char));
   sprintf(name, "tmp%d", tmp_num++);
   int type = t;                // ntmp->type = t;
   int scope_num = scope_index; // local scope
   int param_c = 0;             // cuz they are all vars

   // insert ntmp into the function's local symbol table
   struct symbol_table *nth_tmp = add_symbol_tb(curr_scope,
                                                name,
                                                type,
                                                scope_num,
                                                param_c,
                                                NULL);
   return nth_tmp;
}

/*
 * Creates a new instruction,
 * fills in the arguments supplied and
 * returns a pointer to the result
 */
Quad *newinstr(OpType opType, void *src1, void *src2, void *dest)
{
   Quad *new_instr = (Quad *)malloc(sizeof(Quad));

   new_instr->op = opType;

   Operand *src1_ = src1;
   new_instr->src1 = src1_;

   Operand *src2_ = src2;
   new_instr->src2 = src2_;

   SymbolTable *dest_ = dest;
   new_instr->dest = dest_;

   return new_instr;
}

/*
 * Returns a new label instruction
 */
static int label_num = 0;
Quad *newlabel()
{
   SymbolTable *l = (SymbolTable *)malloc(sizeof(SymbolTable));
   l->val = label_num;
   label_num++;
   return newinstr(LABEL, NULL, NULL, l);
}

////////////////////////////////////////////////////////////////////////
// 3_ADRRESS CODE
void print_3ac(Quad *head)
{
   while (head != NULL)
   {
      switch (head->op)
      {
      case ENTER:
         printf("enter %s\n", head->dest->name);
         break;
      case PARAM:
         if (head->src1->operand_type == INTCONSTANT)
            printf("param %d\n", head->src1->val.iconst);
         else
            printf("param %s\n", head->src1->val.st_ptr->name);
         break;
      case CALL:
         printf("call %s %d\n", head->src1->val.st_ptr->name, head->src1->val.st_ptr->param_c);
         break;
      case LEAVE:
         printf("leave %s\n", head->dest->name);
         break;
      case RET:
         printf("return %s\n", head->dest->name);
         break;
      case RETRIEVE:
         printf("retrieve %s\n", head->dest->name);
         break;
      case IF_EQ:
         printf("if %s == %s goto _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         break;
      case IF_NE:
         printf("if %s != %s goto _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         break;
      case IF_LE:
         printf("if %s <= %s goto _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         break;
      case IF_LT:
         printf("if %s < %s goto _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         break;
      case IF_GE:
         printf("if %s >= %s goto _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         break;
      case IF_GT:
         printf("if %s > %s goto _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         break;
      case GOTO:
         printf("goto _L%d\n", head->dest->val);
         break;
      default:
         break;
      }
      head = head->next;
   }
}

////////////////////////////////////////////////////////////////////////////////////////////////
// MIPS CODE
int set_up_flag = 0;
int print_main_flag = 0;
SymbolTable *args_head;
int is_param;
int param_pos;

void genMips_code(Quad *head)
{
   if (!set_up_flag)
   {
      _printlnCode();
      _print_globals();
      set_up_flag = 1;
   }

   while (head != NULL)
   {
      is_param = 0;
      param_pos = 0;
      switch (head->op)
      {
      case ENTER:
         _mips_enter(head);
         break;
      case PARAM:
         if (head->src1->operand_type == INTCONSTANT)
         {
            printf("  # PARAM _%s\n", head->src1->val.st_ptr->name);
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);
            printf("  la $sp, -4($sp)\n");
            printf("  sw $t0, 0($sp)\n");
            printf("\n");
            break;
         }
         else
         {
            SymbolTable *curr = args_head;
            while (curr != NULL)
            {
               printf("# ARGS: %s\n", curr->name);
               if (strcmp(curr->name, head->src1->val.st_ptr->name) == 0)
               {
                  is_param = 1;
                  break;
               }

               curr = curr->next;
               param_pos++;
            }

            // printf("PARAM_POSSSSSSSS: %d\n", param_pos);
            printf("  # PARAM _%s\n", head->src1->val.st_ptr->name);
            if (head->src1->val.st_ptr->scope == 0)
               printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
            else
            {
               if (is_param)
                  printf("  lw $t0, %d($fp)\n", (param_pos + 2) * 4);
               else
                  printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);
            }
            printf("  la $sp, -4($sp)\n");
            printf("  sw $t0, 0($sp)\n");
            printf("\n");
            break;
         }
      case ASSGN:
         if (strncmp(head->dest->name, "tmp", 3) == 0)
         {
            printf("  # ASSGN _%s = %d\n", head->dest->name, head->src1->val.iconst);
            printf("  li $t0, %d\n", head->src1->val.iconst);
            printf("  sw $t0, %d($fp)\n", head->dest->offset);
         }
         else
         {
            SymbolTable *curr = args_head;
            while (curr != NULL)
            {
               printf("# ARGS: %s\n", curr->name);
               if (strcmp(curr->name, head->src1->val.st_ptr->name) == 0)
               {
                  is_param = 1;
                  break;
               }

               curr = curr->next;
               param_pos++;
            }

            printf("  # ASSGN _%s = %s\n", head->dest->name, head->src1->val.st_ptr->name);
            if (head->src1->val.st_ptr->scope == 0)
               printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
            else if (is_param)
            {
               printf("  lw $t0, %d($fp)\n", (param_pos + 2) * 4);
            }
            else
            {
               printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);
            }

            if (head->dest->scope == 0)
               printf("  sw $t0, _%s\n", head->dest->name);
            else
               printf("  sw $t0, %d($fp)\n", head->dest->offset);
         }
         printf("\n");
         break;
      case CALL:
         printf("  # CALL _%s %d\n", head->src1->val.st_ptr->name, head->src1->val.st_ptr->param_c);
         printf("  jal _%s\n", head->src1->val.st_ptr->name);
         printf("  la $sp, %d($sp)\n", head->src1->val.st_ptr->param_c * 4);
         printf("\n");
         break;
      case RETRIEVE:
         printf("  # __tmp1 = 0\n");
         printf("  li $t0, 0\n");
         printf("  sw $t0, -4($fp)\n");
         printf("\n");
         break;
      case LEAVE:
         printf("  # LEAVE _%s\n", head->dest->name);
         printf("  # LEAVE is ignored for CSC 453\n");
         printf("\n");
         break;
      case RET:
         if (head->src1 != NULL)
         {
            if (head->src1->operand_type == INTCONSTANT)
            {
               printf("  # RETURN _%s\n", head->src1->val.st_ptr->name);
               printf("  lw $v0, %d($fp)\n", head->src1->val.st_ptr->offset);
            }
            else if (head->src1->val.st_ptr->scope == 0)
            {
               printf("  lw $v0, _%s($fp)\n", head->src1->val.st_ptr->name);
            }
            else
            {
               printf("  # RETURN _%s\n", head->src1->val.st_ptr->name);
               printf("  lw $v0, %d($fp)\n", head->src1->val.st_ptr->offset);
            }
         }
         printf("  la $sp, 0($fp)\n");
         printf("  lw $ra, 0($sp)\n");
         printf("  lw $fp, 4($sp)\n");
         printf("  la $sp, 8($sp)\n");
         printf("  jr $ra\n");
         printf("\n");
         break;
      case IF_EQ:
         // s->code = newinstr(op_type, src1, src2, trueDst->dest);
         // # IF (x == y) GOTO __label_0;
         printf("  # IF (%s == %s) GOTO _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         // lw $t0, _x; src1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);

         // lw $t1, -28($fp); src2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);

         // bne $t2, $t0, $t1, _L1
         printf("  beq $t0, $t1 _L%d\n", head->dest->val);
         printf("\n");
         break;
      case IF_NE:
         // s->code = newinstr(op_type, src1, src2, trueDst->dest);
         // # IF (x != y) GOTO __label_0;
         printf("  # IF (%s != %s) GOTO _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         // lw $t0, _x; src1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);

         // lw $t1, -28($fp); src2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);

         // bne $t2, $t0, $t1, _L1
         printf("  beq $t0, $t1 _L%d\n", head->dest->val);
         printf("\n");
         break;
      case IF_LE:
         // s->code = newinstr(op_type, src1, src2, trueDst->dest);
         // # IF (x <= y) GOTO __label_0;
         printf("  # IF (%s <= %s) GOTO _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         // lw $t0, _x; src1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);

         // lw $t1, -28($fp); src2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);

         // bgt $t2, $t0, $t1, _L1
         printf("  bgt $t1, $t0 _L%d\n", head->dest->val);
         printf("\n");
         break;
      case IF_LT:
         // s->code = newinstr(op_type, src1, src2, trueDst->dest);
         // # IF (x < y) GOTO __label_0;
         printf("  # IF (%s < %s) GOTO _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         // lw $t0, _x; src1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);

         // lw $t1, -28($fp); src2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);

         // blt $t2, $t0, $t1, _L1
         printf("  blt $t0, $t1 _L%d\n", head->dest->val);
         printf("\n");
         break;
      case IF_GE:
         // s->code = newinstr(op_type, src1, src2, trueDst->dest);
         // # IF (x >= y) GOTO __label_0;
         printf("  # IF (%s >= %s) GOTO _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         // lw $t0, _x; src1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);

         // lw $t1, -28($fp); src2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);

         // bge $t0, $t1, _L0
         printf("  bge $t0, $t1, _L%d\n", head->dest->val);
         printf("\n");
         break;
      case IF_GT:
         // s->code = newinstr(op_type, src1, src2, trueDst->dest);
         // # IF (x > y) GOTO __label_0;
         printf("  # IF (%s > %s) GOTO _L%d\n", head->src1->val.st_ptr->name,
                head->src2->val.st_ptr->name,
                head->dest->val);
         // lw $t0, _x; src1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);

         // lw $t1, -28($fp); src2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);

         // bgt t0, t1, _L0
         printf("  bgt $t0, $t1, _L%d\n", head->dest->val);
         printf("\n");
         break;
      case LABEL:
         // #LABEL __label_0
         printf("  # LABEL _L%d\n", head->dest->val);
         //       __label_0:
         printf("  _L%d:\n", head->dest->val);
         printf("\n");
         break;
      case GOTO:
         //  # GOTO _L1
         printf("  # GOTO _L%d\n", head->dest->val);
         //  j _L1
         printf("  j _L%d\n", head->dest->val);
         printf("\n");
         break;
      case U_MINUS:
         if (head->src1->operand_type == INTCONSTANT)
            printf("  # %s := -%d\n", head->dest->name, head->src1->val.iconst);
         else
            printf("  # %s := -%s\n", head->dest->name, head->src1->val.st_ptr->name);

         // load y into reg1
         
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
         {   
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);
         }
         // neg reg2, reg1
         printf("  neg $t1, $t0\n");
         // store reg2 into x
         printf("  sw $t1, %d($fp)\n", head->dest->offset);
         printf("\n");
         break;
      case PLUS:
         // tmp = 10 + 20; or tmp = a + b;
         if (head->src1->operand_type == INTCONSTANT)
            printf("  # %s := %d +", head->dest->name, head->src1->val.iconst);
         else
            printf("  # %s := %s +", head->dest->name, head->src1->val.st_ptr->name);

         if (head->src2->operand_type == INTCONSTANT)
            printf(" %d\n", head->src2->val.iconst);
         else
            printf(" %s\n", head->src2->val.st_ptr->name);

         // load y into reg1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
         {
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);
         }

         // load z into reg2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
         {
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);
         }

         // add reg3, reg1, reg2
         printf("  add $t2, $t0, $t1\n");

         // store reg3 into x
         printf("  sw $t2, %d($fp)\n", head->dest->offset);

         printf("\n");
         break;
      case MINUS:
         // tmp = 10 - 20; or tmp = a - b;
         if (head->src1->operand_type == INTCONSTANT)
            printf("  # %s := %d -", head->dest->name, head->src1->val.iconst);
         else
            printf("  # %s := %s -", head->dest->name, head->src1->val.st_ptr->name);

         if (head->src2->operand_type == INTCONSTANT)
            printf(" %d\n", head->src2->val.iconst);
         else
            printf(" %s\n", head->src2->val.st_ptr->name);

         // load y into reg1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
         {
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);
         }

         // load z into reg2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
         {
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);
         }

         // sub reg3, reg1, reg2
         printf("  sub $t2, $t0, $t1\n");

         // store reg3 into x
         printf("  sw $t2, %d($fp)\n", head->dest->offset);

         printf("\n");
         break;
      case MULTIPLY:
         // tmp = 10 * 20; or tmp = a * b;
         if (head->src1->operand_type == INTCONSTANT)
            printf("  # %s := %d *", head->dest->name, head->src1->val.iconst);
         else
            printf("  # %s := %s *", head->dest->name, head->src1->val.st_ptr->name);

         if (head->src2->operand_type == INTCONSTANT)
            printf(" %d\n", head->src2->val.iconst);
         else
            printf(" %s\n", head->src2->val.st_ptr->name);

         // load y into reg1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
         {
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);
         }

         // load z into reg2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
         {
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);
         }

         // mul reg3, reg1, reg2
         printf("  mul $t2, $t0, $t1\n");

         // store reg3 into x
         printf("  sw $t2, %d($fp)\n", head->dest->offset);

         printf("\n");
         break;
      case DIVIDE:
         // tmp = 10 / 20; or tmp = a / b;
         if (head->src1->operand_type == INTCONSTANT)
            printf("  # %s := %d /", head->dest->name, head->src1->val.iconst);
         else
            printf("  # %s := %s /", head->dest->name, head->src1->val.st_ptr->name);

         if (head->src2->operand_type == INTCONSTANT)
            printf(" %d\n", head->src2->val.iconst);
         else
            printf(" %s\n", head->src2->val.st_ptr->name);

         // load y into reg1
         if (head->src1->val.st_ptr->scope == 0)
            printf("  lw $t0, _%s\n", head->src1->val.st_ptr->name);
         else
         {
            printf("  lw $t0, %d($fp)\n", head->src1->val.st_ptr->offset);
         }

         // load z into reg2
         if (head->src2->val.st_ptr->scope == 0)
            printf("  lw $t1, _%s\n", head->src2->val.st_ptr->name);
         else
         {
            printf("  lw $t1, %d($fp)\n", head->src2->val.st_ptr->offset);
         }

         // div reg3, reg1, reg2
         printf("  div $t2, $t0, $t1\n");

         // store reg3 into x
         printf("  sw $t2, %d($fp)\n", head->dest->offset);

         printf("\n");
         break;
      
      default:
         break;
      }
      head = head->next;
   }

   if (print_main_flag)
   {
      printf(".align 2\n");
      printf(".text\n");
      printf("main:\n");
      printf("  j _main\n\n");
   }
}

void _print_globals()
{
   SymbolTable *curr_tb = global_scope->symbol_table;

   while (curr_tb != NULL)
   {
      if (curr_tb->type == 0)
      {
         printf("#GLOBAL _%s\n", curr_tb->name);
         printf(".data\n");
         printf("_%s: .space 4\n\n", curr_tb->name);
      }
      curr_tb = curr_tb->next;
   }
}

void _printlnCode()
{
   printf(".align 2\n");
   printf(".data\n");
   printf("_nl: .asciiz \"\\n\"\n");
   printf(".align 2\n");
   printf(".text\n\n");
   printf("# _println: print out an integer followed by a newline\n");
   printf("_println:\n");
   printf("  li $v0, 1\n");
   printf("  lw $a0, 0($sp)\n");
   printf("  syscall\n");
   printf("  li $v0, 4\n");
   printf("  la $a0, _nl\n");
   printf("  syscall\n");
   printf("  jr $ra\n");
   printf("\n");
}

void _mips_enter(Quad *head)
{
   if (strcmp(head->dest->name, "println") != 0)
      args_head = head->dest->param_head;

   if (strcmp(head->dest->name, "main") == 0)
      print_main_flag = 1;

   printf("# ENTER _main\n");
   printf(".align 2\n");
   printf(".text\n");
   printf("_main:\n");
   printf("  la $sp, -8($sp) # allocate space for old $fp and $ra\n");
   printf("  sw $fp, 4($sp) # save old $fp\n");
   printf("  sw $ra, 0($sp) # save return address\n");
   printf("  la $fp, 0($sp) # set up frame pointer\n");
   printf("  la $sp, -%d($sp) # allocate stack frame\n", (space + 1) * 4);
   printf("\n");
}

////////////////////////////////////////////////////////////////////////////////////////////////
// TESTING
void print_scope_and_symbs()
{
   Scope *curr_scope_tmp = curr_scope;
   while (curr_scope_tmp != NULL)
   {
      SymbolTable *curr_tb = curr_scope_tmp->symbol_table;
      printf("Scope: %d\n", curr_tb->scope);
      while (curr_tb != NULL)
      {
         printf("[NAME: %s, VAL: %d, TYPE: %d, SCOPE: %d, PARAM#: %d, OFFSET: %d] -->\n",
                curr_tb->name,
                curr_tb->val,
                curr_tb->type,
                curr_tb->scope,
                curr_tb->param_c,
                curr_tb->offset);
         curr_tb = curr_tb->next;
      }
      printf("\n");
      curr_scope_tmp = curr_scope_tmp->prev;
   }
}