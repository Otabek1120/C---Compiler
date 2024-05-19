
/*
 * File:        scanner.c
 * Author:      Otabek Abduraimov
 * Purpose:     The scanner.c implements the scanner function
 * References:  Lecture Slides and StackOverflow
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "scanner.h"

char *lexeme;
int line_number = 1;

int get_token()
{
   lexeme = (char *)malloc(sizeof(char) * 32);
   char ch;

   // Ignore whitespace
   while (isspace(ch = getchar()))
   {
      if (ch == '\n')
         line_number++;
   }

   // Return when reached the end of the string
   if (ch == EOF)
   {
      return EOF;
   }

   // Ignore comments
   // opDIV /*	/ : division */,
   // Comments:  /* and followed by */, and not containing any occurrence of */.
   if (ch == '/')
   {
      char ch2 = getchar();
      if (ch2 == '*')
      {
         // Multi-line comment, consume until '*/'
         // match_comments();
         int c;
         int prev = ' ';
         while ((c = getchar()) != EOF)
         {
            if (c == '\n')
               line_number++;

            if (prev == '*' && c == '/')
               break;
            prev = c;
         }

         ch = getchar();
         if (ch == '\n')
            line_number++;

         if (ch == '/')
         {
            lexeme[0] = ch;
            return opDIV;
         }
      }
      else
      {
         if (ch2 == '\n')
            line_number++;
         lexeme[0] = ch;
         ungetc(ch2, stdin);
         return opDIV;
      }
   }

   // Check for identifiers or keywords
   if (isalpha(ch))
   {
      int i = 0;
      lexeme[i++] = ch;
      while ((ch = getchar()) != '\0' && (isalnum(ch) || ch == '_'))
      {
         lexeme[i++] = ch;
      }
      ungetc(ch, stdin);
      if (lexeme != NULL)
      {
         if (strcmp(lexeme, "int") == 0)
            return kwINT;
         else if (strcmp(lexeme, "if") == 0)
            return kwIF;
         else if (strcmp(lexeme, "else") == 0)
            return kwELSE;
         else if (strcmp(lexeme, "while") == 0)
            return kwWHILE;
         else if (strcmp(lexeme, "return") == 0)
            return kwRETURN;
         else
            return ID;
      }
   }

   // INTCON /* integer constant: e.g., 12345 */,
   if (isdigit(ch))
   {
      int i = 0;
      lexeme[i++] = ch;
      while ((ch = getchar()) != '\0' && isdigit(ch))
      {
         lexeme[i++] = ch;
      }
      ungetc(ch, stdin);
      return INTCON;
   }

   // LPAREN /* '(' : Left parenthesis */,
   if (ch == '(')
   {
      lexeme[0] = ch;
      return LPAREN;
   }

   // RPAREN /* ')' : Right parenthesis */,
   if (ch == ')')
   {
      lexeme[0] = ch;
      return RPAREN;
   }

   // LBRACE /* '{' : Left curly brace */,
   if (ch == '{')
   {
      lexeme[0] = ch;
      return LBRACE;
   }

   // RBRACE /* '}' : Right curly brace */,
   if (ch == '}')
   {
      lexeme[0] = ch;
      return RBRACE;
   }

   // COMMA /* ',' : Comma */,
   if (ch == ',')
   {
      lexeme[0] = ch;
      return COMMA;
   }

   // SEMI /*	;  : Semicolon */,
   if (ch == ';')
   {
      lexeme[0] = ch;
      return SEMI;
   }

   // opASSG /*	= : Assignment */,
   // opEQ /*	== : Op: equals */,
   if (ch == '=')
   {
      lexeme[0] = ch;
      char ch2 = getchar();
      if (ch2 == '=')
      {
         lexeme[1] = ch2;
         return opEQ;
      }
      else
      {
         ungetc(ch2, stdin);
         return opASSG;
      }
   }

   // opADD /*	+ : addition */,
   if (ch == '+')
   {
      lexeme[0] = ch;
      return opADD;
   }

   // opSUB /*	– : subtraction */,
   if (ch == '-')
   {
      lexeme[0] = ch;
      return opSUB;
   }

   // opMUL /*	* : multiplication */,
   if (ch == '*')
   {
      lexeme[0] = ch;
      return opMUL;
   }

   // opNE /*	!= : op: not-equals */,
   // opNOT /* ! : Op: logical-not */
   if (ch == '!')
   {
      lexeme[0] = ch;
      char ch2 = getchar();
      if (ch2 == '=')
      {
         lexeme[1] = ch2;
         return opNE;
      }
      else
      {
         ungetc(ch2, stdin);
         return opNOT;
      }
   }

   // opGT /*	>  : Op: greater-than */,
   // opGE /*	>= : Op: greater-or-equal */,
   if (ch == '>')
   {
      lexeme[0] = ch;
      char ch2 = getchar();
      if (ch2 == '=')
      {
         lexeme[1] = ch2;
         return opGE;
      }
      else
      {
         ungetc(ch2, stdin);
         return opGT;
      }
   }

   // opLT /*	<  : Op: less-than */,
   // opLE /*	<= : Op: less-or-equal */,
   if (ch == '<')
   {
      lexeme[0] = ch;
      char ch2 = getchar();
      if (ch2 == '=')
      {
         lexeme[1] = ch2;
         return opLE;
      }
      else
      {
         ungetc(ch2, stdin);
         return opLT;
      }
   }

   // opAND /*	&& : Op: logical-and */,
   if (ch == '&')
   {
      lexeme[0] = ch;
      char ch2 = getchar();
      if (ch2 == '&')
      {
         lexeme[1] = ch2;
         return opAND;
      }
      else
      {
         lexeme[0] = getchar();
      }
   }

   // opOR /*	|| : Op: logical-or */,
   if (ch == '|')
   {
      lexeme[0] = ch;
      char ch2 = getchar();
      if (ch2 == '|')
      {
         lexeme[1] = ch2;
         return opOR;
      }
      else
      {
         lexeme[0] = getchar();
      }
   }

   return UNDEF;
}