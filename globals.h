/****************************************************/
/* File: globals.h                                  */
/* Global types and vars for TINY compiler          */
/* must come before other include files             */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#ifndef _GLOBALS_H_
#define _GLOBALS_H_

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/*Vetor de palavras reservadas, sempre que acrescentamos uma palavra reservada ele dever ser modificado*/
#define MAXRESERVED 10

typedef enum
/* book-keeping tokens */
{
   ENDFILE,
   ERROR,
   /* reserved words */
   IF,
   THEN,
   ELSE,
   ENDIF,
   REPEAT,
   UNTIL,
   READ,
   WRITE,
   SWITCH,
   CASE,
   ENDSWITCH,
   WHILE,
   ENDWHILE,
   /* multicharacter tokens */
   ID,
   NUM,
   /* special symbols */
   ASSIGN,
   EQ,
   LT,
   PLUS,
   MINUS,
   TIMES,
   OVER,
   SEMI,
   DDOT,
} TokenType;

extern FILE *source;  /* source code text file */
extern FILE *listing; /* listing output text file */
extern FILE *code;    /* code text file for TM simulator */

extern int lineno; /* source line number for listing */

/**************************************************/
/***********   Syntax tree for parsing ************/
/**************************************************/

/*Código que especifica um nó de arvore sintatica tiny */


typedef enum
{
   StmtK,
   ExpK
} NodeKind; // estrutura de enumeração que contém os dois tipos de formação em tiny (declaração (Stmtk) e expressão)(Expk)
typedef enum
{
   IfK,
   RepeatK,
   AssignK,
   ReadK,
   WriteK,
   SwitchK,
   CaseK,
   WhileK
} StmtKind; // corresponde as declarações existentes na linguagem
typedef enum
{
   OpK,
   ConstK,
   IdK
} ExpKind; // expressões da linguagem operadores aritiméticos, constantes e identificadores

/* usada para checagem de tipo */
typedef enum
{
   Void,
   Integer,
   Boolean
} ExpType; /* usada para checagem de tipo */

// Define a maior quantidade de filhos que um nó pode ter
#define MAXCHILDREN 3

typedef struct treeNode
{
   struct treeNode *child[MAXCHILDREN]; // vetor que contém ponteiros para os nós filhos
   struct treeNode *sibling; // ponteiro para o nó irmão
   int lineno; // guarda o número da linha no código fonte ao qual o nó se refere
   NodeKind nodekind;
   union
   {
      StmtKind stmt;
      ExpKind exp;
   } kind; // serve para dizer se é um nó de declaração ou expressão 
   union
   {
      TokenType op;
      int val;
      char *name;
   } attr;// Armazena o operador (*,+,-,...), armazena o valor inteiro no caso de 
             //uma constante númerica, armazena uma string no caso de um identidicador
   ExpType type; // armazena o tipo do nó que pode ser ou integer  ou boolean e serve para verificação de tipo e é usada caso
     // o nó seja do tipo expressão
} TreeNode;

/**************************************************/
/***********   Flags for tracing       ************/
/**************************************************/

/* EchoSource = TRUE causes the source program to
 * be echoed to the listing file with line numbers
 * during parsing
 */
extern int EchoSource;

/* TraceScan = TRUE causes token information to be
 * printed to the listing file as each token is
 * recognized by the scanner
 */
extern int TraceScan;

/* TraceParse = TRUE causes the syntax tree to be
 * printed to the listing file in linearized form
 * (using indents for children)
 */
extern int TraceParse;

/* TraceAnalyze = TRUE causes symbol table inserts
 * and lookups to be reported to the listing file
 */
extern int TraceAnalyze;

/* TraceCode = TRUE causes comments to be written
 * to the TM code file as code is generated
 */
extern int TraceCode;

/* Error = TRUE prevents further passes if an error occurs */
extern int Error;
#endif
