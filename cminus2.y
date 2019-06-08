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
lista_asteriscos: '*' { printf ("lista_asteriscos  -> *\n"); }
|lista_asteriscos '*' { printf ("lista_asteriscos  -> lista_asteriscos *\n"); }
;
//definicion_funcion : [ declaracion_tipo [ '*']* ]? IDENTIFICADOR bloque_instrucciones (posible conflicto aqui , puede que haya que cambiar lista asteriscos)
definicion_funcion  : IDENTIFICADOR bloque_instrucciones { printf ("  definicion_funcion  -> IDENTIFICADOR bloque_instrucciones\n"); }
| declaracion_tipo  IDENTIFICADOR bloque_instrucciones { printf (" definicion_funcion  -> declaracion_tipo IDENTIFICADOR bloque_instrucciones\n"); }
|declaracion_tipo lista_asteriscos IDENTIFICADOR bloque_instrucciones { printf (" declaracion_tipo definicion_funcion  -> declaracion_tipo lista_asteriscos IDENTIFICADOR bloque_instrucciones\n"); }
;

//macros : '#' INCLUDE PATH | '#' DEFINE IDENTIFICADOR constante

macros : '#' INCLUDE PATH { printf (" macros -> '#' 'include' PATH\n"); }
| '#' DEFINE IDENTIFICADOR constante { printf (" macros -> '#' 'define' IDENTIFICADOR constante\n"); }
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
lista_nombre: nombre { printf (" lista_nombre -> nombre;\n"); }
lista_nombre ',' nombre { printf (" lista_nombre -> declaracion_tipo lista_nombre ;\n"); }
;

//declaracion : declaracion_tipo ( nombre )* [ '#']? ';'| 'typedef'declaracion_tipo IDENTIFICADOR ';'(posible problema con la lista nombre , preguntar a alex)
declaracion : declaracion_tipo lista_nombre ';' { printf (" declaracion -> declaracion_tipo lista_nombre ;\n"); }
|declaracion_tipo  ';' { printf (" declaracion -> declaracion_tipo   ;\n"); }
|declaracion_tipo  '#' ';' { printf (" declaracion -> declaracion_tipo lista_nombre # ;\n"); }
|declaracion_tipo lista_nombre '#' ';' { printf (" declaracion -> declaracion_tipo lista_nombre # ;\n"); }
|TYPEDEF declaracion_tipo IDENTIFICADOR ';'  { printf (" declaracion -> 'typedef' declaracion_tipo IDENTIFICADOR ';'\n"); }
;

lista_almacenamiento: almacenamiento { printf (" lista_almacenamiento -> almacenamiento ';'\n"); }
|lista_almacenamiento almacenamiento { printf (" lista_almacenamiento -> lista_almacenamiento almacenamiento ';'\n"); }
;

//declaracion_tipo ::= [ almacenamiento ]* tipo_basico_modificado| [ almacenamiento ]* definicion_struct_union | [ almacenamiento ]* definicion_enum
declaracion_tipo : tipo_basico_modificado
  | lista_almacenamiento tipo_basico_modificado
  | definicion_struct_union
  | lista_almacenamiento  definicion_struct_union
  | definicion_enum
  | lista_almacenamiento definicion_enum
;

//almacenamiento ::= 'extern'| 'static'| 'auto'| 'register'
almacenamiento : EXTERN | STATIC | AUTO | REGISTER
;


longitud : SHORT
| LONG
;

signo : SIGNED
| UNSIGNED
;

tipo_basico : VOID
| CHAR
| INT 
| FLOAT
| DOUBLE
;

lista_declaraciones_struct: declaracion_struct
  | lista_declaraciones_struct declaracion_struct
;



//definicion_struct_union ::= struct_union [ IDENTIFICADOR ]? '{'[ declaracion_struct ]+ '}'| struct_union IDENTIFICADOR
definicion_struct_union : struct_union '{' lista_declaraciones_struct '}'
  | struct_union IDENTIFICADOR '{' lista_declaraciones_struct '}'
  | struct_union IDENTIFICADOR
;

//struct_union ::= ’struct’ | ’union’
struct_union: STRUCT
  | UNION
;

//declaracion_struct ::= tipo_basico_modificado ( nombre )+ ';'| definicion_struct_union ( nombre )+ ';'
declaracion_struct : tipo_basico_modificiado lista_nombre ';'
  | definicion_struct_union lista_comas_nombre ';'
;

//nombre ::= dato [ ’=’ elementos ]?
nombre: dato
  | dato '=' elementos
;

lista_expresiones: '[' ']'
  | '[' expresion ']'
  | lista_expresiones '[' ']'
  | lista_expresiones '[' expresion ']'
;

//dato ::= [ ’*’ ]* IDENTIFICADOR [ ’[’ [ expresion ]? ’]’ ]*
dato: IDENTIFICADOR
  | lista_asteriscos IDENTIFICADOR
  | lista_asteriscos IDENTIFICADOR lista_expresiones
;

lista_elementos: elemento
  |lista_elementos elemento
;

//elementos ::= expresion | ’{’ ( elementos )+ ’}’
elementos: expresion
  | '{' lista_elementos '}'
;

//definicion_enum ::= ’enum’ IDENTIFICADOR  [’:’ tipo_basico_modificado ]? cuerpo_enum
definicion_enum: ENUM IDENTIFICADOR cuerpo_enum
  | ENUM IDENTIFICADOR ';' tipo_basico_modificado cuerpo_enum
;


lista_declaraciones_miembro_enum: declaracion_miembro_enum
  | lista_declaraciones_miembro_enum declaracion_miembro_enum
;

//cuerpo_enum ::= ’{’ ( declaracion_miembro_enum )+ ’}’
cuerpo_enum: '{' lista_declaraciones_miembro_enum '}'
;


//declaracion_miembro_enum ::= IDENTIFICADOR [ ’=’ expresion ]?
declaracion_miembro_enum: IDENTIFICADOR
  | IDENTIFICADOR '=' expresion
;




/*****************/
/* INSTRUCCIONES */
/*****************/
/*
instruccion ::= bloque_instrucciones
  | instruccion_expresion
  | instruccion_bifurcacion
  | instruccion_bucle
  | instruccion_salto
  | instruccion_destino_salto
  | instruccion_retorno
  | ';'
*/
instruccion : bloque_instrucciones
  | instruccion_expresion
  | instruccion_bifurcacion
  | instruccion_bucle
  | instruccion_salto
  | instruccion_destino_salto
  | instruccion_retorno
  | ';'
;



lista_declaraciones: declaracion
  | lista_declaraciones declaracion;

lista_instrucciones: instruccion
  | lista_instrucciones instruccion;

//bloque_instrucciones ::= '{'[ declaracion ]* [ instruccion ]* '}'
bloque_instrucciones : '{' '}' 
  | '{' lista_declaraciones '}'
  | '{' lista_instrucciones '}'
  | '{' lista_declaraciones lista_instrucciones '}'
;


//instruccion_expresion ::= expresion_funcional ';'| asignacion ';'
instruccion_expresion : expresion_funcional ';'| asignacion ';'
;

//asignacion ::= expresion_indexada operador_asignacion expresion
asignacion : expresion_indexada operador_asignacion expresion
;


//operador_asignacion ::= '='| '*='| '/='| '%='| '+='| '-='| '<<='| '>>='| '&='| '^='| '|='
operador_asignacion : '=' 
  | MULT_ASIG 
  | DIV_ASIG 
  | MOD_ASIG 
  | SUMA_ASIG 
  | RESTA_ASIG 
  | DESPI_ASIG
  | DESPD_ASIG 
  | AND_ASIG 
  | XOR_ASIG 
  | OR_ASIG
;



lista_else: ELSE instruccion
  | ELSE instruccion lista_else
;

lista_instruccion_caso: instruccion_caso
  | lista_instruccion_caso instruccion_caso
;
 
/*
instruccion_bifurcacion ::= 'if''('expresion ')'instruccion [ 'else'instruccion ]?
  | SWITCH '('expresion ')''{'[ instruccion_caso ]+ '}'
*/
instruccion_bifurcacion : IF '(' expresion ')'
  | IF '(' expresion ')' instruccion lista_else 
  | SWITCH '('expresion ')''{'lista_instruccion_caso '}'
;

/*
instruccion_caso ::= 'case'expresion ':'instruccion
  | 'default'':'instruccion
*/
instruccion_caso : CASE expresion ':' instruccion
  | DEFAULT ':' instruccion
;  


lista_definicion_asignacion: definicion_asignacion
  | lista_definicion_asignacion','definicion_asignacion
;


/*
instruccion_bucle ::= 'while''('expresion ')'instruccion
  | 'do'instruccion 'while''( expresion ')'';'
  | 'for''('( definicion_asignacion )* ';'expresion ';'expresion ')'instruccion
  | 'for''('[ declaracion_tipo [ '*']* ]? IDENTIFICADOR ';'expresion ')'instruccioN
*/

instruccion_bucle : WHILE '('expresion ')' instruccion
  | DO instruccion WHILE '(' expresion ')' ';'
  | FOR '('lista_definicion_asignacion ';' expresion ';'expresion ')'instruccion
  | FOR '(' ';' expresion ';'expresion ')' instruccion
  | FOR '('IDENTIFICADOR ';'expresion ')' instruccion
  | FOR '('declaracion_tipo lista_asteriscos IDENTIFICADOR ';'expresion ')'instruccion //lista_asteriscos declarada arriba
;

/*
definicion_asignacion ::= asignacion
  | declaracion_tipo [ '*']* expresion_indexada '='expresion
*/
definicion_asignacion : asignacion
  | declaracion_tipo expresion_indexada '='expresion
  | declaracion_tipo lista_asteriscos expresion_indexada '='expresion
;

/*
instruccion_salto ::= 'goto'IDENTIFICADOR ';'| 'continue'';'| 'break'';'
*/
instruccion_salto : GOTO IDENTIFICADOR ';'| CONTINUE ';'| BREAK ';'
;

/*
instruccion_destino_salto ::= IDENTIFICADOR ':'instruccion ';'
*/
instruccion_destino_salto : IDENTIFICADOR ':'instruccion ';'
;

/*
instruccion_retorno ::= 'return'[ expresion ]? ';'
*/
instruccion_retorno :  RETURN ';'
  | RETURN expresion ';'
;


/***************/
/* EXPRESIONES */
/***************/
expresion_constante : 
ENTERO 
|REAL 
|CADENA 
|CARACTER
;

expresion_parentesis :
'(' expresion ')'
;

lista_expresion:expresion
lista_expresion expresion
;


//expresion_funcional : IDENTIFICADOR '(' ( expresion )* ')'
expresion_funcional : IDENTIFICADOR :'(' ')'
|'(' lista_expresion ')' 

;


expresion_indexada : IDENTIFICADOR
| expresion_indexada ’[’ expresion ’]’
| expresion_indexada ’.’ IDENTIFICADOR
| expresion_indexada PTR_ACCESO IDENTIFICADOR
;
expresion_postfija : expresion_constante
| expresion_parentesis
| expresion_funcional
| expresion_indexada
| expresion_postfija INC
| expresion_postfija DEC
;
expresion_prefija : expresion_postfija
| SIZEOF expresion_prefija
| SIZEOF ’(’ nombre_tipo ’)’
| operador_unario expresion_cast
;

operador_unario : INC | DEC | ’&’ | ’*’ | ’+’ | ’-’ | ’~’ | ’!’
;

expresion_cast : expresion_prefija
| ’(’ nombre_tipo ’)’ expresion_prefija
;

expresion_or_logico: expresion_or_logico OR expresion_and_logico
  | expresion_and_logico
;

expresion_and_logico: expresion_and_logico AND expresion_igual_distinto
  | expresion_igual_distinto
;

expresion_igual_distinto: expresion_igual_distinto EQ expresion_mayor_menor
  | expresion NEQ expresion_mayor_menor
  | expresion_mayor_menor
;

expresion_mayor_menor: expresion_mayor_menor '<' expresion_or
  | expresion_mayor_menor '>' expresion_or
  | expresion_mayor_menor LE expresion_or
  | expresion_mayor_menor GE expresion_or
  |expresion_or
;

expresion_or: expresion_or '|' expresion_xor
  | expresion_xor
;

expresion_xor: expresion_xor '^' expresion_and
   | expresion_and
;

expresion_and: expresion_and '&' expresion_desplazar
   | expresion desplazar
;
expresion_desplazar: expresion_desplazar DESPD expresion_suma_resta
  | expresion_suma_resta
;

expresion_suma_resta: expresion_suma_resta '+' expresion_multiplicacion
  | expresion_suma_resta '-' expresion_multiplicacion
  | expresion_multiplicacion
;

expresion_multiplicacion: expresion_multiplicacion '*' expresion_potencia
  | expresion_multiplicacion '/' expresion_potencia
  | expresion_multiplicacion '%' expresion_potencia
  | expresion_potencia
;

expresion_potencia: expresion_cast 
  | expresion_cast '**' expresion_potencia
 ;

//expresion ::= expresion_or_logico [ ’?’ expresion ’:’ expresion ]?
expresion: expresion_or_logico
  | expresion_or_logico '?' expresion ':' expresion
;

 






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
