/*** Bahadir Etka Kilinc 1901042701 hw2_part1 ***/
%{
      #include <stdio.h>
      void print_func(const char *);
      void err_func(const char *);
      
      int token_of_quote_flag = 0;
%}

OP_PLUS		"+"
OP_MINUS		"-"
OP_DIV		"/"
OP_MULT		"*"
OP_OP			"("
OP_CP			")"
OP_DBLMULT		"**"
OP_OC_CC		"\""
OP_COMMA		","

KW_AND		"and"
KW_OR			"or"
KW_NOT		"not"
KW_EQUAL		"equal"
KW_LESS		"less"
KW_NIL		"nil"
KW_LIST		"list"
KW_APPEND		"append"
KW_CONCAT		"concat"
KW_SET		"set"
KW_DEFFUN		"deffun"
KW_FOR		"for"
KW_IF			"if"
KW_EXIT		"exit"
KW_LOAD		"load"
KW_DISP		"disp"
KW_TRUE		"true"
KW_FALSE		"false"

COMMENT		";;".*
VALUEI      "0"|[1-9][0-9]*
VALUEF      [1-9]+[a-zA-Z][0-9]+
IDENTIFIER		[a-zA-Z][a-zA-Z0-9]*
SPACE			[[:space:]]

SYN_ERR_1		[0][[:digit:]]+
SYN_ERR_2		[[:digit:]]+[[:alpha:]]+
SYN_ERR_3		[[:alnum:]]*[[:alnum:][:punct:]]{-}[()"]+

%%

{KW_AND}          {print_func("KW_AND\n");}
{KW_OR}          {print_func("KW_OR\n");}
{KW_NOT}          {print_func("KW_NOT\n");}
{KW_EQUAL}          {print_func("KW_EQUAL\n");}
{KW_LESS}          {print_func("KW_LESS\n");}
{KW_NIL}          {print_func("KW_NIL");}
{KW_LIST}          {print_func("KW_LIST");}
{KW_APPEND}          {print_func("KW_APPEND");}
{KW_CONCAT}          {print_func("KW_CONCAT");}
{KW_SET}          {print_func("KW_SET");}
{KW_DEFFUN}          {print_func("KW_DEFFUN");}
{KW_FOR}          {print_func("KW_FOR");}
{KW_IF}          {print_func("KW_IF");}
{KW_EXIT}          {print_func("KW_EXIT");}
{KW_LOAD}          {print_func("KW_LOAD");}
{KW_DISP}          {print_func("KW_DISP");}
{KW_TRUE}          {print_func("KW_TRUE");}
{KW_FALSE}          {print_func("KW_FALSE");}
{OP_PLUS}          {print_func("OP_PLUS");}
{OP_MINUS}          {print_func("OP_MINUS");}
{OP_DIV}          {print_func("OP_DIV");}
{OP_MULT}          {print_func("OP_MULT");}
{OP_CP}          {print_func("OP_CP");}
{OP_OP}          {print_func("OP_OP");}
{OP_DBLMULT}          {print_func("OP_DBLMULT");}
{OP_OC_CC}          {print_func(token_of_quote_flag == 0 ? "OP_OC" : "OP_CC"); token_of_quote_flag = !token_of_quote_flag;}
{OP_COMMA}          {print_func("OP_COMMA");}
{COMMENT}         {print_func("COMMENT");}
{VALUEI}      { print_func("VALUEI"); }
{VALUEF}      { print_func("VALUEF"); }
{IDENTIFIER}          {print_func("IDENTIFIER");}
{SPACE}                 /*ignored*/
                       	
.		 			  {err_func("SYNTAX ERROR\n"); return 0;}	
"(exit)" {printf( " PROGRAM IS TERMINATING ...\n"); return 0; } //for terminating program.   
%%

int yywrap(){}
void print_func(const char *str)
{
      printf("%s : %s\n",yytext,str);
}
void err_func(const char *str)
{
      printf("SYNTAX ERROR:\n");
}
int main(int argc, char *argv[]){
    if (argc > 1) yyin = fopen(argv[1], "r");
	else yyin = stdin;
	yylex();  
}
