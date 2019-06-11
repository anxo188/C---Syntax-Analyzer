%{

    #include <stdlib.h>
	#include <stdio.h>
	#include <string.h>
	#include "cminus2.tab.h"
	unsigned int linea = 1;
%}
IDENTIFICADOR [A-Za-z_][A-Za-z_0-9]*
INT [[:digit:]]+|0[0-7]+|0[xX][0-9A-F]+
FLOAT [0-9]+[.][0-9]+|0[xX][0-9A-Fa-f]*[.][0-9A-Fa-f]+|0[xX][0-9A-Fa-f]*[.][0-9A-Fa-f]+
MANTISA {INT}[eE][+-]?{INT}|{FLOAT}[eE][+-]?{FLOAT}
OPERADOR "+"|"-"|"*"|"**"|"/"|"%"|"--"|"++"|"*="|"/="|"+="|"-="|"."|"->"|"&"|"["|"]"|"~"|"|"|"^"|"<<"|">>"|"<<="|">>="|"&="|"^="|"|="|"<"|">"|"<="|">="|"=="|"!="|"!"|"&&"|"||"|"="|"#"
SALTO_LINEA      \n
CARACTER '[^\n'\\]'|'(\\\'|\\\"|\\\?|\\\\|\\n|\\r|\\t|\\v)'|'\\(37[0-7]|3[0-6][0-7]|[0-2][0-7]{2}|[0-7]{1,2})'|'\\x[0-9a-fA-F]{1,2}'
CADENA \"([^\n"\\]|\\\'|\\\"|\\\?|\\\\|\\n|\\r|\\t|\\v|\\(37[0-7]|3[0-6][0-7]|[0-2][0-7]{2}|[0-7]{1,2})|\\x[0-9a-fA-F]{1,2}|\\\n)*\"
DIRECTORIO [^'\n'\"\/\>]+
ARCHIVO [^'\n'\"\/\>]+\.[:ascii:]
PATH \<\/?({DIRECTORIO}\/)*{ARCHIVO}\>
DELIMITADORES [?:;,(){}]
%x Comentario
%x Comentario_Linea
%%
"auto" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return AUTO; }
"break"	{printf("nlinea %d, Palabra reservada: %s",linea,yytext); return BREAK; }
"case" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return CASE; }
"char" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return CHAR; }
"continue" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return CONTINUE; }
"default" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return DEFAULT; }
"do" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return DO; }
"define" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return DEFINE; }
"double" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return DOUBLE; }
"else" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return ELSE; }
"enum" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return ENUM; }
"extern" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return EXTERN; }
"float" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return FLOAT; }
"for" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return FOR; }
"goto" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return GOTO; }
"if" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return IF; }
"include" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return INCLUDE; }
"int" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return INT; }
"long" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return LONG; }
"register" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return REGISTER; }
"return" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return RETURN; }
"short" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return SHORT; }
"signed" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return SIGNED; }
"static" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return STATIC; }
"struct" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return STRUCT; }
"switch" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return SWITCH; }
"typedef" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return TYPEDEF; }
"union" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return UNION; }
"unsigned" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return UNSIGNED; }
"void" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return VOID; }
"while" {printf("nlinea %d, Palabra reservada: %s",linea,yytext); return WHILE; }
"sizeof" {printf("\nlinea %d, Operador: %s",linea,yytext); return(SIZEOF);}
{IDENTIFICADOR} {printf("\nlinea %d, Identificador: ",linea); return(IDENTIFICADOR);}
{INT} {printf("\nlinea %d, Constante Entera: %s",linea,yytext); return(ENTERO);}
{FLOAT} {printf("\nlinea %d, Constante Real : %s",linea,yytext); return(REAL);}
{MANTISA} {printf("\nlinea %d, Constante Real : %s",linea,yytext); return(REAL);}
{DELIMITADORES} {printf("\nlinea %d, Delimitador: %s",linea,yytext); return *yytext;}
{PATH} {printf("\nlinea %d, PATH: %s",linea,yytext); return(PATH);}
"\+="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(SUMA_ASIG); }
"-="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(RESTA_ASIG); }
"\*="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(MULT_ASIG); }
"\/="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(DIV_ASIG); }
"%="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(MOD_ASIG);}
"\<\<="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(DESPI_ASIG); }
"\>\>="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(DESPD_ASIG); }
"&="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(AND_ASIG); }
"\|="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(OR_ASIG); }
"^="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(XOR_ASIG); }
"\+\+"	{printf("\nlinea %d, Operador: %s",linea,yytext); return(INC);}
"--"	{printf("\nlinea %d, Operador: %s",linea,yytext); return(DEC); }
"\<\<"	{printf("\nlinea %d, Operador: %s",linea,yytext); return(DESPI);} 
"\>\>"	{printf("\nlinea %d, Operador: %s",linea,yytext); return(DESPD); }
"\>="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(GE); }
"\<="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(LE); }
"=="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(EQ); }
"!="	{printf("\nlinea %d, Operador: %s",linea,yytext); return(NEQ); }
"&&"	{printf("\nlinea %d, Operador: %s",linea,yytext); return(AND); }
"\|\|"	{printf("\nlinea %d, Operador: %s",linea,yytext); return(OR); }
"-\>"	{printf("\nlinea %d, Operador: %s",linea,yytext); return(PTR_ACCESO);}
"**"	{printf("\nlinea %d, Operador: %s",linea,yytext); return(POTENCIA);}

{OPERADOR} {printf("\nlinea %d, Operador: ",linea); return(*yytext);}
{CARACTER} {printf("\nlinea %d, Caracter: %s",linea,yytext);  return(CARACTER);}
{CADENA} {printf("\nlinea %d, Caracter: %s",linea,yytext);  return(STRING);}
{SALTO_LINEA} linea++;
"/*" BEGIN(Comentario);
 "\/\/" BEGIN(Comentario_Linea);
<Comentario>\n {linea++;}
<Comentario>. ;
<Comentario>"*/" {BEGIN(INITIAL) ;}
<Comentario_Linea>{SALTO_LINEA}     { BEGIN(INITIAL); ++linea; }
".*" printf("\n Línea %d, token no reconocido: %s \n", linea , yytext);
%%

