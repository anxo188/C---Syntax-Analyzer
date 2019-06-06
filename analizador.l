/*Declaraciones*/
RESERVADA "auto"|"break"|"case"|"char"|"continue"|"default"|"do"|"define"|"double"|"else"|"enum"|"extern"|"float"|"for"|"goto"|"if"|"include"|"int"|"long"|"register"|"return"|"short"|"signed"|"static"|"struct"|"switch"|"typedef"|"union"|"unsigned"|"void"|"while"
IDENTIFICADOR [A-Za-z_][A-Za-z_0-9]*

/*Constantes*/
ENTERO  [[:digit:]]+| 0[0-7]+ | "0x"[0-9A-Fa-f]+
REAL [0-9]*.[0-9]+| "0X"[0-9A-Fa-f]*.[0-9A-Fa-f]+ | "0x"[0-9A-Fa-f]*.[0-9A-Fa-f]+
MANTISA {ENTERO}[eE][+-]?{ENTERO}|{REAL}[eE][+-]?{REAL}

CARACTER '[^\n'\\]'|'(\\\'|\\\"|\\\?|\\\\|\\n|\\r|\\t|\\v)'|'\\(37[0-7]|3[0-6][0-7]|[0-2][0-7]{2}|[0-7]{1,2})'|'\\x[0-9a-fA-F]{1,2}'
CADENA \"([^\n"\\]|\\\'|\\\"|\\\?|\\\\|\\n|\\r|\\t|\\v|\\(37[0-7]|3[0-6][0-7]|[0-2][0-7]{2}|[0-7]{1,2})|\\x[0-9a-fA-F]{1,2}|\\\n)*\"

DIRECTORIO [^'\n'\"\/\>]+
ARCHIVO [^'\n'\"\/\>]+\.[:ascii:]
PATH \<\/?({DIRECTORIO}/)*{ARCHIVO}\>

DELIMITADORES [?:;,(){}]

OPERADOR "+" | "-" | "*" | "**" | "/" | "%" | "--" | "++" | "*=" | "/=" | "%=" | "+=" | "-=" | "." | "->" | "&" | "[" | "]" | "~" | "|" | "^" | "<<" | ">>" | "<<=" | ">>=" | "&=" | "^=" | "|=" | "<" | ">" | "<=" | ">=" | "==" | "!=" | "!" | "&&" | "||" | "=" | "#"

CLINEA \/\/
CMULTI \/\*
FIN_CMULTI \*\/
/* Condiciones de arranque */
%x CLINEA
%x CMULTI


%{
/*Declaraciones en C*/
	#include "analizador_sintactico.tab.h"
        #include <stdlib.h>
	#include <stdio.h>
	
	/*Variable que marca la línea actual*/
	unsigned int linea = 1;
%}
/*Reglas*/
%%







{RESERVADA} printf("nlinea %d, Palabra reservada: %s",linea,yytext); return strupr(*yytext);
{IDENTIFICADOR} printf("\nlinea %d, Identificador: ",linea);return IDENTIFICADOR;
sizeof printf("\nlinea %d, Operador: %s",linea,yytext);return SIZEOF;

{ENTERO} printf("\nlinea %d, Constante Entera: %s",linea,yytext); return ENTERO;
{REAL} printf("\nlinea %d, Constante Real : %s",linea,yytext); return REAL;
{MANTISA} printf("\nlinea %d, Constante Real : %s",linea,yytext); return REAL;




{DELIMITADORES} printf("\nlinea %d, Delimitador: %s",linea,yytext);return *yytext;
"# include"[[:blank:]]*{PATH} printf("\nlinea %d, PATH: %s",linea,yytext);return PATH;

/*Operadores*/
\+=	printf("\nlinea %d, Operador: %s",linea,yytext); return SUMA_ASIG; 
-=	printf("\nlinea %d, Operador: %s",linea,yytext); return RESTA_ASIG; 
\*=	printf("\nlinea %d, Operador: %s",linea,yytext); return MULT_ASIG; 
\/=	printf("\nlinea %d, Operador: %s",linea,yytext); return DIV_ASIG; 
%=	printf("\nlinea %d, Operador: %s",linea,yytext); return MOD_ASIG;
\<\<=	printf("\nlinea %d, Operador: %s",linea,yytext); return DESPI_ASIG; 
\>\>=	printf("\nlinea %d, Operador: %s",linea,yytext); return DESPD_ASIG; 
&=	printf("\nlinea %d, Operador: %s",linea,yytext); return AND_ASIG; 
\|=	printf("\nlinea %d, Operador: %s",linea,yytext); return OR_ASIG; 
^=	printf("\nlinea %d, Operador: %s",linea,yytext); return XOR_ASIG; 
\+\+	printf("\nlinea %d, Operador: %s",linea,yytext); return INC;
--	printf("\nlinea %d, Operador: %s",linea,yytext); return DEC; 
\<\<	printf("\nlinea %d, Operador: %s",linea,yytext); return DESPI; 
\>\>	printf("\nlinea %d, Operador: %s",linea,yytext); return DESPD; 
\>=	printf("\nlinea %d, Operador: %s",linea,yytext); return GE; 
\<=	printf("\nlinea %d, Operador: %s",linea,yytext); return LE; 
==	printf("\nlinea %d, Operador: %s",linea,yytext); return EQ; 
!=	printf("\nlinea %d, Operador: %s",linea,yytext); return NEQ; 
&&	printf("\nlinea %d, Operador: %s",linea,yytext); return AND; 
\|\|	printf("\nlinea %d, Operador: %s",linea,yytext); return OR; 
-\>	printf("\nlinea %d, Operador: %s",linea,yytext); return PTR_ACCESO;
**	printf("\nlinea %d, Operador: %s",linea,yytext); return POTENCIA;
{OPERADOR} printf("\nlinea %d, Operador: ",linea);return *yytext;

{CARACTER} printf("\nlinea %d, Caracter: %s",linea,yytext); return CARACTER;
{CADENA}	{	size_t entrada = yyleng;
			char* actual = yytext;
			size_t restante = tamEntrada;
			printf("\nlinea %d, Cadena: %s",linea,yytext);
			/* Buscar cuantas líneas ocupa la cadena */
			while (restante > 1) 
			{
				/* La posicion actual o la siguiente contiene un salto de línea */
				if ( ((*actual == '\\') && (*(actual + 1) == '\n')) || ((restante > 2) && (*(actual + 1) == '\\') && (*(actual + 2) == '\n')))
				{
					++linea;
				}
				/* Avanzar el puntero de la cadena dos caracteres hacia delante */
				actual += 2;
				restante -= 2;
			}
			return STRING;
		}

{CLINEA}	BEGIN(COMENTARIO_LINEA);
{CMULTI}	BEGIN(COMENTARIO_MULTI);

<COMENTARIO_LINEA>{SALTO_LINEA}		{ BEGIN(INITIAL); ++numLinea; }

<COMENTARIO_MULTI>{FIN_COMENTARIO_MULTI}  BEGIN(INITIAL);
<COMENTARIO_MULTI,INITIAL>{SALTO_LINEA}	  ++linea;

/* Esta regla, al coincidir con un solo caracter y estar al final, tendrá prioridad mínima para Flex */
<INITIAL>[^ \t\n]	{ mostrarErrorSintaxis(); yyterminate(); }
 /* Avisar si encontramos comentarios multilínea no finalizados */
 
<COMENTARIO_MULTI><<EOF>>  { puts("Aviso: comentario multilínea no finalizado en código fuente"); yyterminate(); }

{SALTO_LINEA} linea ++;
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
