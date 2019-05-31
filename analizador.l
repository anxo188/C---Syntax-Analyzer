/*Declaraciones*/
RESERVADA "auto"|"base"|"boolean"|"break"|"case"|"catch"|"class"|"char"|"continue"|"default"|"do"|"double"|"else"|"enum"|"extern"|"false"|"finally"|"float"|"for"|"goto"|"if"|"include"|"int"|"interface"|"long"|"namespace"|"new"|"override"|"private"|"protected"|"public"|"return"|"sealed"|"short"|"signed"|"sizeof"|"static"|"struct"|"switch"|"this"|"throw"|"true"|"typedef"|"union"|"unsigned"|"using"|"virtual"|"void"|"while"
DELIMITADORES [?:;,(){}]
COMENTARIO "//".*\n
MULTILINEA \/\*.*\n*.*\*\/
OPERADOR_A "\+"|"-"|"\*"|"/"|"&"|"--"|"++"|"*="|"%="|"+="|"-="
OPERADOR_M  "."|"->"|"&"|"\["|"\]"
OPERADOR_B "~"|"&"|"|"|"\^"|"<<"|">>"|"<<="|">>="|"&="|"^="|"|="
OPERADOR_R "<"|">"|"<="|">="|"=="|"!="
OPERADOR_L "!"|"&&"|"||"
OPERADOR_O "="|"sizeof"
IDENTIFICADOR [A-Za-z_][A-Za-z_0-9]*
ESPACIO [[:blank:]]
ENTERO  [[:digit:]]+|0[0-7]+|"0x"[0-9A-Fa-f]|"0X"[0-9A-Fa-f] 
REAL [0-9]*.[0-9]+|"0X"[0-9A-Fa-f]*.[0-9A-Fa-f]|"0x"[0-9A-Fa-f]*.[0-9A-Fa-f]
MANTISA {ENTERO}[eE][+-]?{ENTERO}|{REAL}[eE][+-]?{REAL}
CARACTER \'[^\n\'\\\"]\'|\'\'|\'\\[0-377]\'|\'\\x[0-9A-Fa-f][0-9A-Fa-f]?\'
CADENA \"[^\"]*\"
%{
/*Declaraciones en C*/
	#include <stdio.h>
	int linea = 1;
%}
/*Reglas*/
%%
[[:blank:]]*{COMENTARIO}[[:blank:]]*|[[:blank:]]*{MULTILINEA}[[:blank:]]* linea++; 
[[:blank:]]*{CADENA}[[:blank:]]* printf("\nlinea %d, Cadena: ",linea);ECHO;printf("\n");
[[:blank:]]*{RESERVADA}[[:blank:]]* printf("\nlinea %d, Palabra Reservada: ",linea);ECHO;printf("\n"); /*RECONOCE PALABRAS RESERVADAS*/
[[:blank:]]*{CARACTER}[[:blank:]]* printf("\nlinea %d, Caracter: ",linea);ECHO;printf("\n");
[[:blank:]]*{IDENTIFICADOR}[[:blank:]]* printf("\nlinea %d, Identificador: ",linea);ECHO;printf("\n");
[[:blank:]]*{DELIMITADORES}[[:blank:]]* printf("\nlinea %d, Delimitador: ",linea);ECHO;printf("\n");
[[:blank:]]*{ENTERO}[[:blank:]]* printf("\nlinea %d, Constante Entera: ",linea);ECHO;printf("\n");
[[:blank:]]*{OPERADOR_A}[[:blank:]]*|[[:blank:]]*{OPERADOR_B}[[:blank:]]*|[[:blank:]]*{OPERADOR_M}[[:blank:]]*|[[:blank:]]*{OPERADOR_R}[[:blank:]]*|[[:blank:]]*{OPERADOR_L}[[:blank:]]*|[[:blank:]]*{OPERADOR_O}[[:blank:]]* printf("\nlinea %d, Operador: ",linea);ECHO;printf("\n");
[[:blank:]]*{MANTISA}[[:blank:]]*|[[:blank:]]*{REAL}[[:blank:]]* printf("\nlinea %d, Constante Real : ",linea);ECHO;printf("\n");
\n linea++;
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
