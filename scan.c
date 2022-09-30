/****************************************************/
/* File: scan.c                                     */
/* The scanner implementation for the TINY compiler */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"

typedef enum
{
  START,
  INASSIGN,
  INCOMMENT,
  INNUM,
  INID,
  DONE
} StateType;


char tokenString[MAXTOKENLEN + 1];


#define BUFLEN 256

static char lineBuf[BUFLEN]; 
static int linepos = 0;      
static int bufsize = 0;      
static int EOF_flag = FALSE;


static int getNextChar(void)
{
  if (!(linepos < bufsize))
  {
    lineno++;
    if (fgets(lineBuf, BUFLEN - 1, source))
    {
      if (EchoSource)
        fprintf(listing, "%4d: %s", lineno, lineBuf);
      bufsize = strlen(lineBuf);
      linepos = 0;
      return lineBuf[linepos++];
    }
    else
    {
      EOF_flag = TRUE;
      return EOF;
    }
  }
  else
    return lineBuf[linepos++];
}


static void ungetNextChar(void)
{
  if (!EOF_flag)
    linepos--;
}


static struct
{
  char *str;
  TokenType tok;
} reservedWords[MAXRESERVED] = {{"if", IF}, {"then", THEN}, {"else", ELSE}, {"endif", ENDIF}, {"repeat", REPEAT}, {"until", UNTIL}, {"read", READ}, {"write", WRITE}, {"while", WHILE}, {"endwhile", ENDWHILE}};


static TokenType reservedLookup(char *s)
{
  int i;
  for (i = 0; i < MAXRESERVED; i++)
    if (!strcmp(s, reservedWords[i].str))
      return reservedWords[i].tok;
  return ID; 
}



TokenType getToken(void)
{ 
  int tokenStringIndex = 0;
 
  TokenType currentToken;
 
  StateType state = START;

  int save;
  while (state != DONE)
  {
    int c = getNextChar(); // pega o caracter
    save = TRUE; // seta como true para entrar no switch
    switch (state)
    {
    case START: // corresponde ao primeiro estado do automato
      if (isdigit(c)) // se for um digito
        state = INNUM;
      else if (isalpha(c) || c == '_')
        state = INID;
      else if (c == ':')
        state = INASSIGN;
      else if ((c == ' ') || (c == '\t') || (c == '\n'))
        save = FALSE;
      else if (c == '{')
      {
        save = FALSE;
        state = INCOMMENT;
      }
      else
      {
        state = DONE;
        switch (c)
        {
        case EOF:
          save = FALSE;
          currentToken = ENDFILE;
          break;
        case '=':
          currentToken = EQ;
          break;
        case '<':
          currentToken = LT;
          break;
        case '+':
          currentToken = PLUS;
          break;
        case '-':
          currentToken = MINUS;
          break;
        case '*':
          currentToken = TIMES;
          break;
        case '/':
          currentToken = OVER;
          break;
        case ';':
          currentToken = SEMI;
          break;
        default:
          currentToken = ERROR;
          break;
        }
      }
      break;
    case INCOMMENT: // reconhecimento de comentario
      save = FALSE;
      if (c == EOF)
      {
        state = DONE;
        currentToken = ENDFILE;
      }
      else if (c == '}')
        state = START;
      break;
    case INASSIGN:
      state = DONE;
      if (c == '=')
        currentToken = ASSIGN;
      else
      { /* backup in the input */
        ungetNextChar();
        save = FALSE;
        currentToken = DDOT;
      }
      break;
    case INNUM: // reconhecimento de digito
      if (!isdigit(c))
      { /* backup in the input */
        ungetNextChar();
        save = FALSE;
        state = DONE;
        currentToken = NUM;
      }
      break;
    case INID: // aceitação de identificadores
      if (!isalpha(c) && !isdigit(c) && c != '_')
      { /* backup in the input */
        ungetNextChar();
        save = FALSE;
        state = DONE;
        currentToken = ID;
      }
      break;
    case DONE:
    default: /* should never happen */
      fprintf(listing, "Scanner Bug: state= %d\n", state);
      state = DONE;
      currentToken = ERROR;
      break;
    }

    if ((save) && (tokenStringIndex <= MAXTOKENLEN)) 
      tokenString[tokenStringIndex++] = (char)c; 
    if (state == DONE) 
    {
      tokenString[tokenStringIndex] = '\0';
      if (currentToken == ID) 
        currentToken = reservedLookup(tokenString); 
    }
  }
  if (TraceScan) 
  {
    fprintf(listing, "\t%d: ", lineno);
    printToken(currentToken, tokenString);
  }
  return currentToken;
} 
