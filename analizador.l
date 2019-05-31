/*Declaraciones*/
RESERVADA "auto"|"break"|"case"|"char"|"continue"|"default"|"do"|"define"|"double"|"else"|"enum"|"extern"|"float"|"for"|"goto"|"if"|"include"|"int"|"long"|"register"|"return"|"short"|"signed"|"static"|"struct"|"switch"|"typedef"|"union"|"unsigned"|"void"|"while"
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
CARACTER '[^\n'\]'|'(\'|\"|\?|\\|\n|\r|\t|\v)'|'\(3[0-6][0-7]|37[0-7]|[0-2][0-7]{2}|[0-7]{1,2})'|'\x[0-7a-fA-F]{1,2}'
CADENA "([^\n"\]|\'|\"|\?|\\|\n|\r|\t|\v|\(3[0-6][0-7]|37[0-7]|[0-2][0-7]{2}|[0-7]{1,2})|\x[0-7a-fA-F]{1,2}|\\n)*"
PATH "#include "\<.*\>

{SALTO_LINEA} numeroLinea++;
"/" BEGIN(Comentario);
 "//" BEGIN(Comentario_Linea);
<Comentario>\n numeroLinea++;
<Comentario>. ;
<Comentario>"/" BEGIN(INITIAL) ;
<Comentario_Linea>{SALTO_LINEA}     { BEGIN(INITIAL); ++numeroLinea; }

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
