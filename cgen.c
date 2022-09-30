/****************************************************/
/* File: cgen.c                                     */
/* The code generator implementation                */
/* for the TINY compiler                            */
/* (generates code for the TM machine)              */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "symtab.h"
#include "code.h"
#include "cgen.h"


static int tmpOffset = 0;


static void cGen(TreeNode *tree);

static void genStmt(TreeNode *tree)
{
  
   TreeNode *p1, *p2, *p3, *curCase;
   int savedLoc1, savedLoc2, currentLoc, jmpToNextLoc, lastPos;
   int loc;
   switch (tree->kind.stmt) 
   {
   case SwitchK:
      if (TraceCode)
         emitComment("-> switch");
      p1 = tree->child[0];
      p2 = tree->child[1];
      cGen(p1);
      cGen(p2);
      break; 
   case CaseK:
      curCase = tree;
      if (TraceCode)
         emitComment("-> ");
      emitRM("LDA", ac1, 0, ac, "tentando colocar o valor de ac em ac1");
      do
      {
         p1 = curCase->child[0];
         p2 = curCase->child[1];
         cGen(p1);
         emitRO("SUB", ac, ac1, ac, "op ==");   
         emitRM("JEQ", ac, 1, pc, "br if true"); 
         jmpToNextLoc = emitSkip(1);            
         cGen(p2);                                                             
         lastPos = emitSkip(0);                                                
         emitBackup(jmpToNextLoc);                                             
         emitRM("LDA", pc, (lastPos - jmpToNextLoc), pc, "unconditional jmp"); 
         emitRestore();

         curCase = curCase->sibling;
      } while (curCase != NULL);
      break; 
   case IfK:
      if (TraceCode)
         emitComment("-> if");
      p1 = tree->child[0];
      p2 = tree->child[1];
      p3 = tree->child[2];
      cGen(p1);
      savedLoc1 = emitSkip(1);
      emitComment("if: jump to else belongs here");
      cGen(p2);
      savedLoc2 = emitSkip(1);
      emitComment("if: jump to end belongs here");
      currentLoc = emitSkip(0);
      emitBackup(savedLoc1);
      emitRM_Abs("JEQ", ac, currentLoc, "if: jmp to else");
      emitRestore();
      cGen(p3);
      currentLoc = emitSkip(0);
      emitBackup(savedLoc2);
      emitRM_Abs("LDA", pc, currentLoc, "jmp to end");
      emitRestore();
      if (TraceCode)
         emitComment("<- if");
      break; 

   case RepeatK:
      if (TraceCode)
         emitComment("-> repeat");
      p1 = tree->child[0]; 
      p2 = tree->child[1]; 
      savedLoc1 = emitSkip(0); 
      emitComment("repeat: jump after body comes back here");
      cGen(p1);
      cGen(p2); 
      emitRM_Abs("JEQ", ac, savedLoc1, "repeat: jmp back to body");
      if (TraceCode)
         emitComment("<- repeat");
      break; 

   case WhileK:
      if (TraceCode)
         emitComment("-> while");
      p1 = tree->child[0];
      p2 = tree->child[1];
      currentLoc = emitSkip(0);
      cGen(p1); 
      savedLoc1 = emitSkip(1); 
      
      cGen(p2);
      emitRM_Abs("LDA", pc, currentLoc, "while: jump after body comes back here"); 
      currentLoc = emitSkip(0); 
      emitBackup(savedLoc1);
      emitRM_Abs("JEQ", ac, currentLoc, "while: jump back to body");
      emitRestore(); 
      if (TraceCode)
         emitComment("<- while");
      break; 

   case AssignK:
      if (TraceCode)
         emitComment("-> assign");
      /* generate code for rhs */
      cGen(tree->child[0]);
      /* now store value */
      loc = st_lookup(tree->attr.name);
      emitRM("ST", ac, loc, gp, "assign: store value");
      if (TraceCode)
         emitComment("<- assign");
      break; /* assign_k */

   case ReadK:
      emitRO("IN", ac, 0, 0, "read integer value");
      loc = st_lookup(tree->attr.name);
      emitRM("ST", ac, loc, gp, "read: store value");
      break;
   case WriteK:
      /* generate code for expression to write */
      cGen(tree->child[0]);
      /* now output it */
      emitRO("OUT", ac, 0, 0, "write ac");
      break;
   default:
      break;
   }
} /* genStmt */

/* Procedure genExp generates code at an expression node */
static void genExp(TreeNode *tree)
{
   int loc;
   TreeNode *p1, *p2;
   switch (tree->kind.exp)
   {

   case ConstK:
      if (TraceCode)
         emitComment("-> Const");
      /* gen code to load integer constant using LDC */
      emitRM("LDC", ac, tree->attr.val, 0, "load const");
      if (TraceCode)
         emitComment("<- Const");
      break; /* ConstK */

   case IdK:
      if (TraceCode)
         emitComment("-> Id");
      loc = st_lookup(tree->attr.name);
      emitRM("LD", ac, loc, gp, "load id value");
      if (TraceCode)
         emitComment("<- Id");
      break; /* IdK */

   case OpK:
      if (TraceCode)
         emitComment("-> Op");
      p1 = tree->child[0];
      p2 = tree->child[1];
      cGen(p1);
      /* gen code to push left operand */
      emitRM("ST", ac, tmpOffset--, mp, "op: push left");
      /* gen code for ac = right operand */
      cGen(p2);
      /* now load left operand */
      emitRM("LD", ac1, ++tmpOffset, mp, "op: load left");
      switch (tree->attr.op)
      {
      case PLUS:
         emitRO("ADD", ac, ac1, ac, "op +");
         break;
      case MINUS:
         emitRO("SUB", ac, ac1, ac, "op -");
         break;
      case TIMES:
         emitRO("MUL", ac, ac1, ac, "op *");
         break;
      case OVER:
         emitRO("DIV", ac, ac1, ac, "op /");
         break;
      case LT:
         emitRO("SUB", ac, ac1, ac, "op <");
         emitRM("JLT", ac, 2, pc, "br if true");
         emitRM("LDC", ac, 0, ac, "false case");
         emitRM("LDA", pc, 1, pc, "unconditional jmp");
         emitRM("LDC", ac, 1, ac, "true case");
         break;
      case EQ:
         emitRO("SUB", ac, ac1, ac, "op ==");
         emitRM("JEQ", ac, 2, pc, "br if true");
         emitRM("LDC", ac, 0, ac, "false case");
         emitRM("LDA", pc, 1, pc, "unconditional jmp");
         emitRM("LDC", ac, 1, ac, "true case");
         break;
      default:
         emitComment("BUG: Unknown operator");
         break;
      } /* case op */
      if (TraceCode)
         emitComment("<- Op");
      break; /* OpK */

   default:
      break;
   }
} /* genExp */

static void cGen(TreeNode *tree)
{
   if (tree != NULL)
   {
      switch (tree->nodekind)
      {
      case StmtK: // se for um nó de sentença
         genStmt(tree); // chama essa função para gerar código para esse nó
         break;
      case ExpK:  // se for expressão
         genExp(tree);
         break;
      default:
         break;
      }
      cGen(tree->sibling); // chama recursivamente cgen passando o nó irmão como argumento
   }
}

/**********************************************/
/* the primary function of the code generator */
/**********************************************/
/* Procedure codeGen generates code to a code
 * file by traversal of the syntax tree. The
 * second parameter (codefile) is the file name
 * of the code file, and is used to print the
 * file name as a comment in the code file
 */
void codeGen(TreeNode *syntaxTree, char *codefile)
{
   char *s = malloc(strlen(codefile) + 7);
   strcpy(s, "File: ");
   strcat(s, codefile);
   emitComment("TINY Compilation to TM Code");
   emitComment(s);
   /* generate standard prelude */
   emitComment("Standard prelude:");
   emitRM("LD", mp, 0, ac, "load maxaddress from location 0");
   emitRM("ST", ac, 0, ac, "clear location 0");
   emitComment("End of standard prelude.");
   /* generate code for TINY program */
   cGen(syntaxTree);
   /* finish */
   emitComment("End of execution.");
   emitRO("HALT", 0, 0, 0, "");
}
