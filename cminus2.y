%{
  #include <stdlib.h>
  #include <stdio.h>
  extern FILE *yyin;
  extern int linea;

  #define YYDEBUG 1

%}


%token IDENTIFICADOR ENTERO REAL STRING CARACTER SIZEOF PATH POTENCIA
%token PTR_ACCESO INC DEC DESPI DESPD LE GE EQ NEQ AND OR MULT_ASIG
%token DIV_ASIG MOD_ASIG SUMA_ASIG RESTA_ASIG DESPI_ASIG DESPD_ASIG
%token AND_ASIG XOR_ASIG OR_ASIG

%token INCLUDE DEFINE TYPEDEF EXTERN STATIC AUTO REGISTER CHAR SHORT
%token INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID STRUCT UNION ENUM

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%%

/************/
/* PROGRAMA */
/************/

lista_bloque :bloque           { printf ("  lista_bloque  -> bloque\n"); }
|lista_bloque bloque           { printf ("  lista_bloque  -> lista_bloque bloque \n"); }
;
//programa ::= [ bloque ]+
programa : lista_bloque { printf ("  programa  -> lista_bloque\n"); }
;
//bloque ::= definicion_funcion| declaracion| macros
bloque : definicion_funcion
| declaracion
| macros
;
lista_asteriscos: * { printf ("lista_asteriscos  -> *\n"); }
|lista_asteriscos * { printf ("lista_asteriscos  -> lista_asteriscos *\n"); }
;
//definicion_funcion : [ declaracion_tipo [ ’*’ ]* ]? IDENTIFICADOR bloque_instrucciones 8posible conflicto aqui , puede que haya que cambiar lista asteriscos)
definicion_funcion  : IDENTIFICADOR bloque_instrucciones { printf ("  definicion_funcion  -> IDENTIFICADOR bloque_instrucciones\n"); }
| declaracion_tipo  IDENTIFICADOR bloque_instrucciones { printf (" definicion_funcion  -> declaracion_tipo IDENTIFICADOR bloque_instrucciones\n"); }
|declaracion_tipo lista_asteriscos IDENTIFICADOR bloque_instrucciones { printf (" declaracion_tipo definicion_funcion  -> declaracion_tipo lista_asteriscos IDENTIFICADOR bloque_instrucciones\n"); }
;

//macros : ’#’ ’include’ PATH | ’#’ ’define’ IDENTIFICADOR constante

macros : '#' 'include' PATH { printf (" macros -> '#' 'include' PATH\n"); }
| '#' 'define' IDENTIFICADOR constante { printf (" macros -> '#' 'define' IDENTIFICADOR constante\n"); }
;
//constante ::= ENTERO | REAL | CADENA | CARACTER
constante : ENTERO { printf (" constante -> ENTERO\n"); }
| REAL  { printf (" constante -> REAL\n"); }
| CADENA  { printf (" constante -> CADENA\n"); }
| CARACTER  { printf (" constante -> CARACTER\n"); }
;

/*****************/
/* DECLARACIONES */
/*****************/
lista_nombre:nombre { printf (" lista_nombre -> nombre;\n"); }
lista_nombre ',' nombre { printf (" lista_nombre -> declaracion_tipo lista_nombre ;\n"); }
;

//declaracion : declaracion_tipo ( nombre )* [ ’#’ ]? ’;’ | ’typedef’ declaracion_tipo IDENTIFICADOR ’;’(posible problema con la lista nombre , preguntar a alex)
declaracion : declaracion_tipo lista_nombre ';' { printf (" declaracion -> declaracion_tipo lista_nombre ;\n"); }
|declaracion_tipo  # ';' { printf (" declaracion -> declaracion_tipo lista_nombre # ;\n"); }
|declaracion_tipo  # ';' { printf (" declaracion -> declaracion_tipo lista_nombre # ;\n"); }
|declaracion_tipo lista_nombre # ';' { printf (" declaracion -> declaracion_tipo lista_nombre # ;\n"); }
|'typedef' declaracion_tipo IDENTIFICADOR ';'  { printf (" declaracion -> 'typedef' declaracion_tipo IDENTIFICADOR ';'\n"); }
;


//declaracion_tipo ::= [ almacenamiento ]* tipo_basico_modificado| [ almacenamiento ]* definicion_struct_union | [ almacenamiento ]* definicion_enum


/*****************/
/* INSTRUCCIONES */
/*****************/


/***************/
/* EXPRESIONES */
/***************/



%%


int yyerror(char *s) {
  fflush(stdout);
  printf("Error linea %d, %s\n", linea,s);
  }

int yywrap() {
  return(1);
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./c2minus NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }

  }
