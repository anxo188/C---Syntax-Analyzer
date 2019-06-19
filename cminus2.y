
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


//programa ::= [ bloque ]+
programa : lista_bloque { printf ("  programa  -> lista_bloque\n"); }
| error                        { yyerrok;}
;

lista_bloque : bloque           { printf ("  lista_bloque  -> bloque\n"); }
|lista_bloque bloque           { printf ("  lista_bloque  -> lista_bloque bloque \n"); }
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
//constante ::= ENTERO | REAL | STRING | CARACTER
constante : ENTERO { printf (" constante -> ENTERO\n"); }
| REAL  { printf (" constante -> REAL\n"); }
| STRING  { printf (" constante -> STRING\n"); }
| CARACTER  { printf (" constante -> CARACTER\n"); }
;

/*****************/
/* DECLARACIONES */
/*****************/
lista_nombre: nombre { printf (" lista_nombre -> nombre;\n"); }
|lista_nombre ',' nombre { printf (" lista_nombre -> declaracion_tipo lista_nombre ;\n"); }
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

declaracion_tipo : tipo_basico_modificado { printf (" declaracion_tipo -> tipo_basico_modificado \n"); }
  | lista_almacenamiento tipo_basico_modificado { printf (" declaracion_tipo -> lista_almacenamiento lista_almacenamiento \n"); }
  | definicion_struct_union { printf (" declaracion_tipo -> definicion_struct_union \n"); }
  | lista_almacenamiento  definicion_struct_union { printf (" declaracion_tipo -> lista_almacenamiento \n"); }
  | definicion_enum { printf (" declaracion_tipo -> definicion_enum \n"); }
  | lista_almacenamiento definicion_enum { printf (" declaracion_tipo -> lista_almacenamiento definicion_enum \n"); }
;
//tipo_basico_modificado ::= [ signo ]? [ longitud ]? tipo_basico| ’[’ IDENTIFICADOR ’]’
tipo_basico_modificado: tipo_basico { printf (" tipo_basico_modificado -> tipo_basico \n"); }
    | longitud tipo_basico { printf (" tipo_basico_modificado ->  longitud tipo_basico \n"); }
    | signo tipo_basico { printf (" tipo_basico_modificado -> signo tipo_basico \n"); }
    | signo longitud tipo_basico { printf (" tipo_basico_modificado -> signo longitud tipo_basico \n"); }
    | '[' IDENTIFICADOR ']' { printf (" tipo_basico_modificado -> '[' IDENTIFICADOR ']' \n"); }
  ;

//almacenamiento ::= 'extern'| 'static'| 'auto'| 'register'
almacenamiento : EXTERN { printf (" almacenamiento -> EXTERN \n"); }
    | STATIC { printf (" almacenamiento -> STATIC \n"); }
    | AUTO { printf (" almacenamiento -> AUTO \n"); }
    | REGISTER { printf (" almacenamiento -> REGISTER \n"); }
;

longitud : SHORT { printf (" longitud -> SHORT  \n"); }
| LONG { printf (" longitud -> LONG  \n"); }
;

signo : SIGNED { printf (" signo -> SIGNED  \n"); }
| UNSIGNED { printf (" signo -> UNSIGNED  \n"); }
;

tipo_basico : VOID { printf (" tipo_basico -> VOID  \n"); }
| CHAR { printf (" tipo_basico -> CHAR  \n"); }
| INT  { printf (" tipo_basico -> INT  \n"); }
| FLOAT { printf (" tipo_basico -> FLOAT  \n"); }
| DOUBLE { printf (" tipo_basico -> DOUBLE  \n"); }
;

lista_declaraciones_struct: declaracion_struct  { printf (" lista_declaraciones_struct -> declaracion_struct  \n"); }
  | lista_declaraciones_struct declaracion_struct { printf (" lista_declaraciones_struct -> declaracion_struct  \n"); }
;

//definicion_struct_union ::= struct_union [ IDENTIFICADOR ]? '{'[ declaracion_struct ]+ '}'| struct_union IDENTIFICADOR 
definicion_struct_union : struct_union '{' lista_declaraciones_struct '}'{ printf (" definicion_struct_union : struct_union '{' lista_declaraciones_struct '}' \n"); }
  | struct_union IDENTIFICADOR '{' lista_declaraciones_struct '}' { printf (" definicion_struct_union : struct_union IDENTIFICADOR '{' lista_declaraciones_struct '}' \n"); }
  | struct_union IDENTIFICADOR { printf (" definicion_struct_union :struct_union IDENTIFICADOR \n"); }
;



//struct_union ::= 'struct' | 'union'
struct_union: STRUCT { printf (" struct_union -> STRUCT \n"); } 
  | UNION { printf (" struct_union -> UNION \n"); }
;

//declaracion_struct ::= tipo_basico_modificado ( nombre )+ ';'| definicion_struct_union ( nombre )+ ';'
declaracion_struct : tipo_basico_modificado lista_nombre ';'  { printf (" declaracion_struct -> tipo_basico_modificado lista_nombre \n"); }
  | definicion_struct_union lista_nombre ';'{ printf (" declaracion_struct -> definicion_struct_union lista_nombre  \n"); }
;

//nombre ::= dato [ '=' elementos ]?
nombre: dato { printf (" nombre -> dato  \n");}
  | dato '=' elementos { printf (" nombre -> dato '=' elementos  \n");}
;

lista_expresiones: '[' ']' { printf (" lista_expresiones ->'[' ']'  \n");}
  | '[' expresion ']' { printf (" lista_expresiones -> '[' expresion ']'  \n");}
  | lista_expresiones '[' ']' { printf (" lista_expresiones -> lista_expresiones '[' ']'  \n");}
  | lista_expresiones '[' expresion ']' { printf (" lista_expresiones -> lista_expresiones '[' expresion ']'   \n");}
; 

//dato ::= [ '*' ]* IDENTIFICADOR [ '[' [ expresion ]? ']' ]*
dato: IDENTIFICADOR { printf ("dato ->IDENTIFICADOR    \n"); }
  | IDENTIFICADOR lista_expresiones { printf ("dato -> IDENTIFICADOR lista_expresiones  \n"); }
;
  | lista_asteriscos IDENTIFICADOR  { printf ("dato ->lista_asteriscos IDENTIFICADOR   \n"); }
  | lista_asteriscos IDENTIFICADOR lista_expresiones  { printf ("dato ->lista_asteriscos IDENTIFICADOR lista_expresiones  \n"); }
;

lista_elementos: elementos { printf ("lista_elementos ->elementos  \n"); }
  |lista_elementos ',' elementos { printf ("lista_elementos ->lista_elementos ',' elementos  \n"); }
;

//elementos ::= expresion | '{' ( elementos )+ '}'
elementos: expresion { printf ("elementos->expresion \n"); }
  | '{' lista_elementos '}' { printf ("elementos: '{' lista_elementos '}' \n"); }
;

//definicion_enum ::= 'enum' IDENTIFICADOR  [':' tipo_basico_modificado ]? cuerpo_enum
definicion_enum: ENUM IDENTIFICADOR cuerpo_enum { printf ("definicion_enum->ENUM IDENTIFICADOR cuerpo_enum  \n"); }
  | ENUM IDENTIFICADOR ':' tipo_basico_modificado cuerpo_enum { printf ("definicion_enum->ENUM IDENTIFICADOR ':' tipo_basico_modificado cuerpo_enum  \n"); }
;

lista_declaraciones_miembro_enum: declaracion_miembro_enum { printf ("definicion_enum -> ENUM IDENTIFICADOR ':' tipo_basico_modificado cuerpo_enum  \n"); }
  | lista_declaraciones_miembro_enum ',' declaracion_miembro_enum
;

//cuerpo_enum ::= '{' ( declaracion_miembro_enum )+ '}'
cuerpo_enum: '{' lista_declaraciones_miembro_enum '}' { printf (" cuerpo_enum->'{' lista_declaraciones_miembro_enum '}' \n"); }
;

//declaracion_miembro_enum ::= IDENTIFICADOR [ '=' expresion ]?
declaracion_miembro_enum: IDENTIFICADOR  { printf (" declaracion_miembro_enum->IDENTIFICADOR \n"); }
  | IDENTIFICADOR '=' expresion    { printf (" declaracion_miembro_enum->IDENTIFICADOR '=' expresion  \n"); }
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
instruccion : bloque_instrucciones { printf (" instruccion -> bloque_instrucciones  \n"); }
  | instruccion_expresion { printf (" instruccion -> instruccion_expresion  \n"); }
  | instruccion_bifurcacion  { printf (" instruccion -> instruccion_bifurcacion  \n"); }
  | instruccion_bucle { printf (" instruccion -> instruccion_bucle  \n"); }
  | instruccion_salto { printf (" instruccion -> instruccion_salto  \n"); }
  | instruccion_destino_salto { printf (" instruccion -> instruccion_destino_salto  \n"); }
  | instruccion_retorno { printf (" instruccion ->  instruccion_retorno  \n"); }
  | ';'
;

lista_declaraciones: declaracion { printf (" lista_declaraciones ->  declaracion  \n"); }
  | lista_declaraciones declaracion   { printf (" lista_declaraciones ->  lista_declaraciones declaracion  \n"); }
;
lista_instrucciones: instruccion { printf (" lista_instrucciones ->  instruccion  \n"); }
  | lista_instrucciones instruccion { printf (" lista_instrucciones ->  lista_instrucciones instruccion  \n"); }
;
//bloque_instrucciones ::= '{'[ declaracion ]* [ instruccion ]* '}'
bloque_instrucciones : '{' '}'  { printf (" bloque_instrucciones ->  '{' '}'   \n"); }
  | '{' lista_declaraciones '}'  { printf (" bloque_instrucciones ->  '{' lista_declaraciones '}'   \n"); }
  | '{' lista_instrucciones '}'  { printf (" bloque_instrucciones ->  '{' lista_instrucciones '}'    \n"); }
  | '{' lista_declaraciones lista_instrucciones '}' { printf (" bloque_instrucciones ->  '{' lista_declaraciones lista_instrucciones '}'     \n"); }
;

//instruccion_expresion ::= expresion_funcional ';'| asignacion ';' 
instruccion_expresion : expresion_funcional ';'  { printf ("  instruccion_expresion -> expresion_funcional ';'      \n"); }
| asignacion ';' { printf (" instruccion_expresion  ->  '{' instruccion_expresion->asignacion ';'      \n"); }
;

//asignacion ::= expresion_indexada operador_asignacion expresion
asignacion : expresion_indexada operador_asignacion expresion { printf (" asignacion : expresion_indexada operador_asignacion expresion  ';' '}'     \n"); }
;

//operador_asignacion ::= '='| '*='| '/='| '%='| '+='| '-='| '<<='| '>>='| '&='| '^='| '|='
operador_asignacion : '=' { printf (" operador_asignacion ->  '='  \n"); }
  | MULT_ASIG  { printf (" operador_asignacion ->  MULT_ASIG  \n"); }
  | DIV_ASIG   { printf (" operador_asignacion ->  DIV_ASIG   \n"); }
  | MOD_ASIG   { printf (" operador_asignacion ->  MOD_ASIG   \n"); }
  | SUMA_ASIG  { printf (" operador_asignacion ->  SUMA_ASIG  \n"); }
  | RESTA_ASIG { printf (" operador_asignacion ->  RESTA_ASIG \n"); }
  | DESPI_ASIG { printf (" operador_asignacion ->  DESPI_ASIG \n"); }
  | DESPD_ASIG { printf (" operador_asignacion ->  DESPD_ASIG \n"); }
  | AND_ASIG   { printf (" operador_asignacion ->  AND_ASIG   \n"); }
  | XOR_ASIG   { printf (" operador_asignacion ->  XOR_ASIG   \n"); }
  | OR_ASIG    { printf (" operador_asignacion ->  OR_ASIG    \n"); }
;


lista_instruccion_caso: instruccion_caso { printf (" lista_instruccion_caso: instruccion_caso    \n"); }
  | lista_instruccion_caso instruccion_caso { printf (" lista_instruccion_caso:  lista_instruccion_caso instruccion_caso   \n"); }
;
 

//instruccion_bifurcacion ::= 'if''('expresion ')'instruccion [ 'else'instruccion ]?| SWITCH '('expresion ')''{'[ instruccion_caso ]+ '}'
opciones: instruccion { printf (" opciones: instruccion   \n"); }
 | instruccion ELSE instruccion { printf (" opciones: instruccion ELSE instruccion   \n"); } 
 ;

instruccion_bifurcacion :IF '(' expresion ')' opciones { printf ("instruccion_bifurcacion : IF '(' expresion ')' opciones   \n"); } ;
  | SWITCH '('expresion ')' '{' lista_instruccion_caso '}' { printf ("instruccion_bifurcacion :  SWITCH '('expresion ')' '{' lista_instruccion_caso '}'  \n"); } ;
;




//instruccion_caso ::= 'case'expresion ':'instruccion| 'default'':'instruccion

instruccion_caso : CASE expresion ':' instruccion { printf ("instruccion_caso : CASE expresion ':' instruccion  \n"); } 
  | DEFAULT ':' instruccion  { printf ("instruccion_caso : DEFAULT ':' instruccion   \n"); } 
;  

lista_definicion_asignacion: definicion_asignacion { printf ("lista_definicion_asignacion: definicion_asignacion  \n"); } 
  | lista_definicion_asignacion','definicion_asignacion { printf ("lista_definicion_asignacion: lista_definicion_asignacion','definicion_asignacion  \n"); } 
;

/*
instruccion_bucle ::= 'while''('expresion ')'instruccion 
  | 'do'instruccion 'while''( expresion ')'';'  
  | 'for''('( definicion_asignacion )* ';'expresion ';'expresion ')'instruccion
  | 'for''('[ declaracion_tipo [ '*']* ]? IDENTIFICADOR ';'expresion ')'instruccioN
*/

instruccion_bucle : WHILE '('expresion ')' instruccion { printf ("instruccion_bucle : WHILE '('expresion ')' instruccion  \n"); } 
  | DO instruccion WHILE '(' expresion ')' ';' { printf ("instruccion_bucle : DO instruccion WHILE '(' expresion ')' ';'  \n"); } 
  | FOR '('lista_definicion_asignacion ';' expresion ';'expresion ')'instruccion { printf ("FOR '('lista_definicion_asignacion ';' expresion ';'expresion ')'instruccion   \n"); } 
  | FOR '(' ';' expresion ';'expresion ')' instruccion { printf ("instruccion_bucle : FOR '(' ';' expresion ';'expresion ')' instruccion    \n"); } 
  | FOR '('IDENTIFICADOR ';'expresion ')' instruccion { printf ("instruccion_bucle : FOR '('IDENTIFICADOR ';'expresion ')' instruccion    \n"); } 
  | FOR '('declaracion_tipo lista_asteriscos IDENTIFICADOR ';'expresion ')'instruccion //lista_asteriscos declarada arriba { printf ("instruccion_bucle : FOR '('declaracion_tipo lista_asteriscos IDENTIFICADOR ';'expresion ')'instruccion   \n"); } 
;

/*
definicion_asignacion ::= asignacion
  | declaracion_tipo [ '*']* expresion_indexada '='expresion 
*/
definicion_asignacion : asignacion { printf ("definicion_asignacion : asignacion  \n"); } 
  | declaracion_tipo expresion_indexada '='expresion  { printf ("definicion_asignacion : declaracion_tipo expresion_indexada '='expresion  \n"); } 
  | declaracion_tipo lista_asteriscos expresion_indexada '='expresion { printf ("definicion_asignacion : declaracion_tipo lista_asteriscos expresion_indexada '='expresion  \n"); } 
;

/*
instruccion_salto ::= 'goto'IDENTIFICADOR ';'| 'continue'';'| 'break'';'
*/
instruccion_salto : GOTO IDENTIFICADOR ';' { printf (" instruccion_salto ->  GOTO IDENTIFICADOR ';' \n"); }
  | CONTINUE ';'  { printf (" instruccion_salto ->  CONTINUE ';' \n"); }
  | BREAK ';'  { printf (" instruccion_salto ->  BREAK ';' \n"); }
;

/*
instruccion_destino_salto ::= IDENTIFICADOR ':'instruccion ';'
*/
instruccion_destino_salto : IDENTIFICADOR ':' instruccion ';'  { printf (" instruccion_destino_salto ->  IDENTIFICADOR ':' instruccion ';'  \n"); }
;

/*
instruccion_retorno ::= 'return'[ expresion ]? ';'
*/
instruccion_retorno :  RETURN ';' { printf (" instruccion_retorno ->  RETURN ';' \n"); }
  | RETURN expresion ';'  { printf (" instruccion_retorno ->  RETURN expresion ';' \n"); }
;

/***************/
/* EXPRESIONES */
/***************/

expresion_constante : ENTERO  { printf (" expresion_constante ->  ENTERO \n"); }
|REAL       { printf (" expresion_constante ->  REAL \n"); }
|STRING     { printf (" expresion_constante ->  STRING \n"); }
|CARACTER   { printf (" expresion_constante ->  CARACTER \n"); }
;

expresion_parentesis :'(' expresion ')' { printf (" expresion_parentesis ->  '(' expresion ')' \n"); }
;

lista_expresion: expresion
|lista_expresion ','  expresion 
;

//expresion_funcional : IDENTIFICADOR '(' ( expresion )* ')'
expresion_funcional : IDENTIFICADOR  '(' ')' { printf (" expresion_funcional ->  IDENTIFICADOR '(' ')' \n"); }
  |IDENTIFICADOR '(' lista_expresion ')'  { printf (" expresion_constante ->  IDENTIFICADOR '(' lista_expresion ')' \n"); }
;

expresion_indexada : IDENTIFICADOR  { printf (" expresion_indexada ->  IDENTIFICADOR \n"); }
| expresion_indexada '[' expresion ']' { printf (" expresion_indexada -> expresion_indexada '[' expresion ']' \n"); }
| expresion_indexada '.' IDENTIFICADOR { printf (" expresion_indexada -> expresion_indexada '.' IDENTIFICADOR \n"); }
| expresion_indexada PTR_ACCESO IDENTIFICADOR { printf (" expresion_indexada ->  expresion_indexada PTR_ACCESO IDENTIFICADOR \n"); }
;

expresion_postfija : expresion_constante  { printf (" expresion_postfija ->  expresion_constante \n"); }
| expresion_parentesis { printf (" expresion_postfija ->  expresion_parentesis \n"); }
| expresion_funcional { printf (" expresion_postfija ->  expresion_funcional \n"); }
| expresion_indexada { printf (" expresion_postfija ->  expresion_indexada \n"); }
| expresion_postfija INC { printf (" expresion_postfija ->  expresion_postfija INC \n"); }
| expresion_postfija DEC { printf (" expresion_postfija ->  expresion_postfija DEC \n"); }
;

expresion_prefija : expresion_postfija  { printf (" expresion_prefija ->  expresion_postfija \n"); }
| SIZEOF expresion_prefija  { printf (" expresion_prefija ->   SIZEOF expresion_prefija \n"); }
| SIZEOF '(' nombre_tipo ')'  { printf (" expresion_prefija ->  SIZEOF '(' nombre_tipo ')' \n"); }
| operador_unario expresion_cast  { printf (" expresion_prefija ->  operador_unario expresion_cast \n"); }
;

operador_unario :INC  { printf (" operador_unario ->  INC \n"); }
|DEC  { printf (" operador_unario ->  DEC \n"); }
|'&'  { printf (" operador_unario ->  '&'  \n"); }
|'*'  { printf (" operador_unario ->  '*' \n"); }
|'+'  { printf (" operador_unario ->  '+'  \n"); }
|'-'  { printf (" operador_unario ->  '-' \n"); }
|'~'  { printf (" operador_unario ->  '~' \n"); }
|'!'  { printf (" operador_unario ->  '!' \n"); }
;

expresion_cast : expresion_prefija   { printf (" expresion_cast ->  expresion_prefija \n"); }
| '(' nombre_tipo ')' expresion_prefija  { printf (" expresion_cast ->  '(' nombre_tipo ')' expresion_prefija \n"); }
;

nombre_tipo: tipo_basico_modificado  { printf (" nombre_tipo ->  tipo_basico_modificado \n"); }
| tipo_basico_modificado lista_asteriscos  { printf (" nombre_tipo ->  tipo_basico_modificado lista_asteriscos \n"); }
;

expresion_or_logico: expresion_or_logico OR expresion_and_logico { printf (" expresion_or_logico ->  expresion_or_logico OR expresion_and_logico \n"); }
  | expresion_and_logico { printf (" expresion_or_logico ->  expresion_and_logico \n"); }
;

expresion_and_logico: expresion_and_logico AND expresion_igual_distinto { printf (" expresion_and_logico ->   expresion_and_logico AND expresion_igual_distinto \n"); }
  | expresion_igual_distinto  { printf (" expresion_and_logico ->   expresion_igual_distinto \n"); }
;

expresion_igual_distinto: expresion_igual_distinto EQ expresion_mayor_menor { printf (" expresion_and_logico ->   expresion_igual_distinto EQ expresion_mayor_menor \n"); }
  | expresion_igual_distinto NEQ expresion_mayor_menor { printf (" expresion_and_logico ->  expresion_igual_distinto NEQ expresion_mayor_menor  \n"); }
  | expresion_mayor_menor { printf (" expresion_and_logico ->  expresion_mayor_menor \n"); }
;

expresion_mayor_menor: expresion_mayor_menor '<' expresion_or { printf (" expresion_mayor_menor ->  expresion_mayor_menor '<' expresion_or  \n"); }
  | expresion_mayor_menor '>' expresion_or  { printf (" expresion_and_logico -> expresion_mayor_menor '>' expresion_or    \n"); }
  | expresion_mayor_menor LE expresion_or    { printf (" expresion_mayor_menor LE expresion_or     \n"); }
  | expresion_mayor_menor GE expresion_or  { printf (" expresion_mayor_menor GE expresion_or   \n"); }
  |expresion_or  { printf (" expresion_mayor_menor -> expresion_or    \n"); }
;

expresion_or: expresion_or '|' expresion_xor  { printf (" expresion_or -> expresion_or '|' expresion_xor     \n"); }
  | expresion_xor { printf (" expresion_or ->  expresion_xor \n"); }
;

expresion_xor: expresion_xor '^' expresion_and { printf (" expresion_xor ->  expresion_xor '^' expresion_and \n"); }
   | expresion_and { printf (" expresion_xor ->  expresion_and \n"); }
;

expresion_and: expresion_and '&' expresion_desplazar { printf (" expresion_and -> expresion_and '&' expresion_desplazar     \n"); }
   | expresion_desplazar { printf (" expresion_and -> expresion_desplazar    \n"); }
;

expresion_desplazar: expresion_desplazar DESPD expresion_suma_resta { printf (" expresion_desplazar -> expresion_desplazar DESPD expresion_suma_resta     \n"); }
  | expresion_desplazar DESPI expresion_suma_resta { printf (" expresion_desplazar -> expresion_desplazar DESPI expresion_suma_resta      \n"); }
  | expresion_suma_resta { printf (" expresion_desplazar -> expresion_suma_resta\n"); }
;

expresion_suma_resta: expresion_suma_resta '+' expresion_multiplicacion { printf (" expresion_suma_resta -> expresion_suma_resta '+' expresion_multiplicacion\n"); }
  | expresion_suma_resta '-' expresion_multiplicacion
  | expresion_multiplicacion
;


expresion_multiplicacion: expresion_multiplicacion '*' expresion_potencia { printf (" expresion_multiplicacion -> expresion_multiplicacion '*' expresion_potencia\n"); }
  | expresion_multiplicacion '/' expresion_potencia { printf (" expresion_multiplicacion -> expresion_multiplicacion '/' expresion_potencia\n"); }
  | expresion_multiplicacion '%' expresion_potencia { printf (" expresion_multiplicacion ->  expresion_multiplicacion '%' expresion_potencia \n"); }
  | expresion_potencia { printf (" expresion_multiplicacion -> expresion_potencia \n"); }
;

expresion_potencia: expresion_cast  { printf (" expresion_potencia -> expresion_cast \n"); }
  | expresion_cast POTENCIA expresion_potencia { printf (" expresion_potencia -> expresion_cast POTENCIA expresion_potencia \n"); }
 ;



expresion_logica: expresion_or_logico { printf (" expresion_logica -> expresion_or_logico \n"); }
;

//expresion ::= expresion_logica [ '?' expresion ':' expresion ]?
expresion: expresion_logica { printf (" expresion -> expresion_logica \n"); }
  | expresion_logica '?' expresion ':' expresion { printf (" expresion -> expresion_logica '?' expresion  \n"); }
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
