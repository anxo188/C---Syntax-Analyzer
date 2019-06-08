%{

        #include <stdlib.h>
	#include <stdio.h>
	#include "cminus2.tab.h"
	unsigned int linea = 1;
%}
RESERVADA auto|break|case|char|continue|default|do|define|double|else|enum|extern|float|for|goto|if|include|int|long|register|return|short|signed|static|struct|switch|typedef|union|unsigned|void|while
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
{RESERVADA} {printf("nlinea %d, Palabra reservada: %s",linea,yytext);  return strupr(*yytext);}
"sizeof" {printf("\nlinea %d, Operador: %s",linea,yytext);  return(SIZEOF);}
{IDENTIFICADOR} {printf("\nlinea %d, Identificador: ",linea);  return(IDENTIFICADOR);}
{INT} {printf("\nlinea %d, Constante Entera: %s",linea,*yytext);  return(ENTERO);}
{FLOAT} {printf("\nlinea %d, Constante Real : %s",linea,*yytext);  return(REAL);}
{MANTISA} {printf("\nlinea %d, Constante Real : %s",linea,*yytext);  return(REAL);}
{DELIMITADORES} {printf("\nlinea %d, Delimitador: %s",linea,yytext); return *yytext};
{PATH} {printf("\nlinea %d, PATH: %s",linea,yytext); return(PATH)};

"\+="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(SUMA_ASIG); }
"-="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(RESTA_ASIG); }
"\*="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(MULT_ASIG); }
"\/="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(DIV_ASIG); }
"%="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(MOD_ASIG);}
"\<\<="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(DESPI_ASIG); }
"\>\>="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(DESPD_ASIG); }
"&="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(AND_ASIG); }
"\|="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(OR_ASIG); }
"^="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(XOR_ASIG); }
"\+\+"	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(INC);}
"--"	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(DEC); }
"\<\<"	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(DESPI);} 
"\>\>"	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(DESPD); }
"\>="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(GE); }
"\<="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(LE); }
"=="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(EQ); }
"!="	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(NEQ); }
"&&"	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(AND); }
"\|\|"	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(OR); }
"-\>"	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(PTR_ACCESO);}
"**"	{printf("\nlinea %d, Operador: %s",linea,yytext);  return(POTENCIA);}
{OPERADOR} {printf("\nlinea %d, Operador: ",linea); return(*yytext);}
{CARACTER} {printf("\nlinea %d, Caracter: %s",linea,yytext);  return(CARACTER);}
{CADENA} {printf("\nlinea %d, Caracter: %s",linea,yytext);  return(CADENA);}
{SALTO_LINEA} numeroLinea++;
"/*" BEGIN(Comentario);
 "\/\/" BEGIN(Comentario_Linea);
<Comentario>\n {numeroLinea++;}
<Comentario>. ;
<Comentario>"*/" {BEGIN(INITIAL) ;}
<Comentario_Linea>{SALTO_LINEA}     { BEGIN(INITIAL); ++numeroLinea; }
".*" printf("\n Línea %d, token no reconocido: %s \n", linea , yytext);
%%
/*Codigo C*/
/*yywrap() Se ejecuta al llegar al final del fichero y devuelve 1 para indicar que no queda texto por procesar*/
int yywrap(){	
	 return 1; 
} 
int main(char args[]) {
    yyin = fopen(args,"r");/*Abre el fichero pasado como argumento en modo lectura*/
/*  yylex() toma caracteres de la entrada hasta que coinciden con una exp. reg*/
    while (yylex()){
    	printf("Tratando linea %d",linea);
    }
    fclose(yyin);
}
