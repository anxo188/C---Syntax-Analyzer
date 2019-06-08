#include <math.h>
#include <stdio.h>

unsigned short int prueba;

#define PI 3.141592 // Definicion de una constante

int circunferencia {
   /* Variables */
   float area, radio;

   printf("\nRadio de la /*circunferencia*/\
            : ");
   scanf("%f", &radio); /* Entrada de dato */

   /* Calculo del area */
   area = PI * pow(radio, 2);

   /* El resultado del área se saca por la "consola":
      se trata de un número real */
   printf("\nArea de la \"circunferencia\": %f", area); printf("\n");

   return 0;
   }

unsigned short int valores[] = { 88, 56, 100, 2, 25 };
typedef unsigned short int entero;

struct nombre_tag {
   [entero] *campo1;
   float campo2;
   };

typedef struct nombre_tag struct_alias;

enum color : int {
   rojo = 1,
   verde = 2,
   azul = 3
   };

struct cuenta {
   int numero_cuenta;
   char *nombre;
   char *apellido;
   float saldo;
   };

int cmpfunc {
   void * a,b #;
   return ( *(int*)a - *(int*)b );
   }

int sort_list {
   int n #;

   printf("Before sorting the list is: \n");
   for( n = 0 ; n < 5; n++ ) {
      printf("%d ", values[n]);
      }

   qsort(values, 5, sizeof(int), cmpfunc);

   printf("\nAfter sorting the list is: \n");
   for( n = 0 ; n < 5; n++ ) {
      if (n > 1)
        printf("%d ", values[n]);
      else
        printf("*********%d ", values[n]**2);
      }
  
   for (short int * elemento; lista) {
     printf("%d\n", *elemento);
     }
   return(0);
   }
