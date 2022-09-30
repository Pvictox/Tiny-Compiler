/****************************************************/
/* File: main.c                                     */
/* Main program for TINY compiler                   */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"

/* set NO_PARSE to TRUE to get a scanner-only compiler */
#define NO_PARSE FALSE
/* set NO_ANALYZE to TRUE to get a parser-only compiler */
#define NO_ANALYZE FALSE

/* set NO_CODE to TRUE to get a compiler that does not
 * generate code
 */
#define NO_CODE FALSE

#include "util.h"
#if NO_PARSE
#include "scan.h"
#else
#include "parse.h"
#if !NO_ANALYZE
#include "analyze.h"
#if !NO_CODE
#include "cgen.h"
#endif
#endif
#endif

/* allocate global variables */
int lineno = 0;
FILE *source;
FILE *listing;
FILE *code;

/* allocate and set tracing flags */
int EchoSource = TRUE;
int TraceScan = TRUE;
int TraceParse = TRUE;
int TraceAnalyze = TRUE;
int TraceCode = TRUE;

int Error = FALSE;

int main(int argc, char *argv[])
{

    TreeNode *syntaxTree;
    char pgm[120]; /* source code file name */
    if (argc != 2)
    {
        fprintf(stderr, "usage: %s <filename>\n", argv[0]);
        exit(1);
    }
    strcpy(pgm, argv[1]);
    if (strchr(pgm, '.') == NULL)
        strcat(pgm, ".tny");
    source = fopen(pgm, "r");
    if (source == NULL)
    {
        fprintf(stderr, "File %s not found\n", pgm);
        exit(1);
    }

    listing = stdout; /* send listing to screen */
    fprintf(listing, "\nTINY COMPILATION: %s\n", pgm);
#if NO_PARSE // se for setada como verdadeira a análise sintática não é realizada somente a lexica 
    while (getToken() != ENDFILE)
        ;
#else
    syntaxTree = parse();
    if (TraceParse)
    {
        fprintf(listing, "\nSyntax tree:\n");
        printTree(syntaxTree);
    }
#if !NO_ANALYZE // se for verdadeiro e não houver nenhuma condição de erro referente a análise sintática, então é construída a tabela de símbolos
    if (!Error)
    {
        if (TraceAnalyze)
            fprintf(listing, "\nBuilding Symbol Table...\n");
        buildSymtab(syntaxTree); // essa função constroi a tabela de simbolos para a árvore sintática que recebeu como argumento
        if (TraceAnalyze)
            fprintf(listing, "\nChecking Types...\n");
        typeCheck(syntaxTree); // faz a verificação de tipos para a árvore sintática
        if (TraceAnalyze)
            fprintf(listing, "\nType Checking Finished\n");
    }
#if !NO_CODE // se for verdadeiro e não houver nenhuma condição de erro em relação a semântica então gera código
    if (!Error)
    {
        char *codefile;
        int fnlen = strcspn(pgm, ".");
        codefile = (char *)calloc(fnlen + 4, sizeof(char));
        strncpy(codefile, pgm, fnlen);
        strcat(codefile, ".tm");
        code = fopen(codefile, "w");
        if (code == NULL)
        {
            printf("Unable to open %s\n", codefile);
            exit(1);
        }
        codeGen(syntaxTree, codefile); // recebe a árvore sintática e o árquivo que vai conter o código gerado
        fclose(code);
    }
#endif
#endif
#endif
    fclose(source);
    return 0;
}
