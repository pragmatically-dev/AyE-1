#include <stdio.h>
#include <stdbool.h>

//PROYECTO 1 - Programacion imperativa

void mostrarEstadoGeneral(int sub_indice,char c1,int v1,char c2,int v2,char c3,int v3,char c4,int v4, int n_params);
//Ejecricio 3
//INICIO: prototipo de funciones punto 3.1 Y 3.2
int NUMERO_DE_EXPRESIONES = 8;
char *hline = "---------------------------------------------------------------\n";
static char EXPRESIONES[8][40] = {
        "x + y + 1",
        "z * z + y * 45 - 15 * x",
        "y - 2 == (x * 3 + 1) % 5 (bool)",
        "y / 2 * x",
        "y < x * z (bool)",
        "x % 4 == 0 (bool)",
        "x + y == 0 && y - x == (-1) * z (bool)",
        "not b && w (bool)",
};

void mostrarExpresionEvaluada(int opcion, int x, int y, int z, int b, int w);

char *mapOpcionAExpresion(int opcion);

int mapOpcionAInt(int opcion, int x, int y, int z, int b, int w);

void resolverEjercicio3_1();

void resolverEjercicio3_2();

//FIN: prototipo de funciones punto 3.1 y 3.2
//--------------------------------------------------------------------------------------------

//INICIO: prototipo de funciones punto 3.3
void resolverEjercicio3_3();
//FIN: prototipo de funciones punto 3.3

void resolverEjercicio4_a();
void resolverEjercicio4_b();

void menu() {

    int opcion;

    do {
        printf("\n\nMenu de Ejercicios:\n");
        printf("1.--------------- Resolver Ejercicio 3.1 ---------------\n");
        printf("2.--------------- Resolver Ejercicio 3.2 ---------------\n");
        printf("3.--------------- Resolver Ejercicio 3.3 ---------------\n");
        printf("4.--------------- Resolver Ejercicio 4.a ---------------\n");
        printf("5.--------------- Resolver Ejercicio 4.b ---------------\n");
        printf("0.--------------- Salir\n");
        printf("Elija una opcion: ");
        scanf("%d", &opcion);

        switch (opcion) {
            case 1:
                resolverEjercicio3_1();
                break;
            case 2:
                resolverEjercicio3_2();
                break;

            case 3:
                resolverEjercicio3_3();
                break;
            case 4:
                resolverEjercicio4_a();
                break;
            case 5:
                resolverEjercicio4_b();
                break;
            case 0:
                printf("Saliendo del programa.\n");
                break;
            default:
                printf("Opcion no valida. Por favor, elija una opcion valida.\n");
                break;
        }
    } while (opcion != 0);
}

void separador(){
    printf("%s",hline);
}

int main() {
    menu();
    return 0;
}

void pedirEntrada(char a, int *x) {
    printf("Ingrese el valor de %c:", a);
    scanf("%d", x);
}

void mostrarEstadoGeneral(int sub_indice,char c1,int v1,char c2,int v2,char c3,int v3,char c4,int v4, int n_params){
    switch (n_params) {
        case 1:
            printf("[|Sigma %d|: ( %c --> %d ) ]", sub_indice, c1, v1);
            break;
        case 2:
            printf("[|Sigma %d|: ( %c --> %d , %c --> %d ) ]", sub_indice, c1, v1, c2, v2);
            break;

        case 3:
            printf("[|Sigma %d|: ( %c --> %d , %c --> %d , %c --> %d) ]", sub_indice, c1, v1, c2, v2,c3,v3);
            break;
        case 4:
            printf("[|Sigma %d|: ( %c --> %d , %c --> %d , %c --> %d , %c -->%d) ]", sub_indice, c1, v1, c2, v2,c3,v3,c4,v4);
            break;
        default:
            printf("Error al mostrar estado:\nLa cantidad de parametros pedida no ha sido implementada");
            break;
    }
}
void mostrarEstado_1(int indice,char c1,int v1){
    mostrarEstadoGeneral(indice,c1,v1,'-',-1,'-',-1,'-',-1,1);
}
void mostrarEstado_2(int indice,char c1,int v1,char c2,int v2){
    mostrarEstadoGeneral(indice,c1,v1,c2,v2,'-',-1,'-',-1,2);
}
void mostrarEstado_3(int indice,char c1,int v1,char c2,int v2,char c3,int v3){
    mostrarEstadoGeneral(indice,c1,v1,c2,v2,c3,v3,'-',-1,3);
}
void mostrarEstado_4(int indice,char c1,int v1,char c2,int v2,char c3,int v3,char c4,int v4){
    mostrarEstadoGeneral(indice,c1,v1,c2,v2,c3,v3,c4,v4,4);
}

void saltoDeLinea() {
    printf("\n");
}

void resolverEjercicio4_a_e_f() {
    int x, y;
    pedirEntrada('x', &x);
    pedirEntrada('y', &y);
    mostrarEstado_2(0, 'x', x, 'y', y );
    if (x >= y) {
        printf("\n[CONDICIONAL: x>=y ]\n");
        mostrarEstado_2(1, 'x', x, 'y', y );
        saltoDeLinea();
        x = 0;
        printf("\nx := 0;\n");
        mostrarEstado_2(2, 'x', x, 'y', y );
    } else if (x < y) {
        printf("\n\n[CONDICIONAL: x<=y ]\n");
        mostrarEstado_2(1, 'x', x, 'y', y );
        x = 2;
        printf("\n x := 2;\n");
        mostrarEstado_2(2, 'x', x, 'y', y);
    }
    saltoDeLinea();
    separador();
    mostrarEstado_2(3, 'x', x, 'y', y);

}

void resolverEjercicio4_a(){
    printf("\n[EJERCICIO 4.a.e:]\n");
    resolverEjercicio4_a_e_f();
    saltoDeLinea();
    saltoDeLinea();
    printf("\n[EJERCICIO 4.a.f:]\n");
    resolverEjercicio4_a_e_f();
    /*
     Ingrese el valor de x:3
Ingrese el valor de y:1
[|Sigma 0|: ( x --> 3 , y --> 1 ) ]
[CONDICIONAL: x>=y ]
[|Sigma 1|: ( x --> 3 , y --> 1 ) ]

x := 0;
[|Sigma 2|: ( x --> 0 , y --> 1 ) ]
---------------------------------------------------------------
[|Sigma 3|: ( x --> 0 , y --> 1 ) ]

---------------------------------------------------------------
Ingrese el valor de x:-100
Ingrese el valor de y:1
[|Sigma 0|: ( x --> -100 , y --> 1 ) ]

[CONDICIONAL: x<=y ]
[|Sigma 1|: ( x --> -100 , y --> 1 ) ]
 x := 2;
[|Sigma 2|: ( x --> 2 , y --> 1 ) ]
---------------------------------------------------------------
[|Sigma 3|: ( x --> 2 , y --> 1 ) ]
     */
}

void resolverEjercicio4_b(){
    int x,y,z,m;
    pedirEntrada('x',&x);
    pedirEntrada('y',&y);
    pedirEntrada('z',&z);
    pedirEntrada('m',&m);
    mostrarEstado_4(0,'x',x,'y',y,'z',z,'m',m);

    saltoDeLinea();
    if ( x < y){
        printf("\n\n\t\t\t[CONDICIONAL: x<y ]\n");
        m=x;
    }else {
        printf("\n\n\t\t\t[CONDICIONAL: x>=y ]\n");
        m=y;
    }
    saltoDeLinea();
    mostrarEstado_4(1,'x',x,'y',y,'z',z,'m',m);
    saltoDeLinea();

    if(m<z){
        printf("\n\n\t\t\t[SKIP (m < z)]\n");
    }else if (m >= z){
        m=z;
    }
    mostrarEstado_4(1,'x',x,'y',y,'z',z,'m',m);

    saltoDeLinea();
    separador();
    saltoDeLinea();
    printf("El programa hace lo siguiente:\n"
           "\n"
           "Declara cuatro variables enteras: x, y, z, m.\n"
           "Llama a la funcion pedirEntrada para solicitar al usuario que ingrese los valores de x, y, z, m por teclado.\n"
           "Llama a la funcion mostrarEstado para mostrar los valores actuales de x, y, z, m en la pantalla.\n"
           "Evalua si x es menor que y. Si es asi, asigna el valor de x a m. Si no, asigna el valor de y a m.\n"
           "Vuelve a llamar a la funcion mostrarEstado para mostrar los nuevos valores de x, y, z, m en la pantalla.\n"
           "Evalua si m es menor que z. Si es asi, no hace nada. Si no, asigna el valor de z a m.\n"
           "Llama por ultima vez a la funcion mostrarEstado para mostrar los valores finales de x, y, z, m en la pantalla.\n"
           "Llama a las funciones saltoDeLinea y separador para dar formato al resultado\n");
}


void ejercicio3_3_a(int index) {
    printf("\n1.a ejecucion %d:\n", index);
    separador();
    int x;
    pedirEntrada('x', &x);
    mostrarEstado_1(0, 'x', x );
    x = 5;
    printf("\nX := %d\n", x);
    mostrarEstado_1(1, 'x', x );
    saltoDeLinea();
    saltoDeLinea();
}

void ejercicio3_3_b(int index) {
    printf("\n1.b ejecucion %d:\n", index);
    separador();
    int x, y;
    pedirEntrada('x', &x);
    pedirEntrada('y', &y);
    mostrarEstado_2(0, 'x', x, 'y', y);
    x = x + y;
    printf("\nx := x + y\n");
    mostrarEstado_2(1, 'x', x, 'y', y);
    y = y + y;
    printf("\ny := y + y\n");
    mostrarEstado_2(2, 'x', x, 'y', y);
    saltoDeLinea();
    saltoDeLinea();
}

void ejercicio3_3_c(int index) {
    printf("\n1.c ejecucion %d:\n", index);
    separador();
    int x, y;
    pedirEntrada('x', &x);
    pedirEntrada('y', &y);
    mostrarEstado_2(0, 'x', x, 'y', y);

    y = y + y;
    printf("\ny := y + y\n");
    mostrarEstado_2(1, 'x', x, 'y', y);

    x = x + y;
    printf("\nx := x + y\n");
    mostrarEstado_2(2, 'x', x, 'y', y);
    saltoDeLinea();
    saltoDeLinea();
}

void resolverEjercicio3_3() {
    for (int i = 0; i < 3; i++) {
        ejercicio3_3_a(i + 1);
    }
    saltoDeLinea();
    saltoDeLinea();
    for (int i = 0; i < 3; i++) {
        ejercicio3_3_b(i + 1);
    }
    saltoDeLinea();
    saltoDeLinea();
    for (int i = 0; i < 3; i++) {
        ejercicio3_3_c(i + 1);
    }

}


void mostrarTabla(int x, int y, int z, int b, int w) {
    int from, to;
    if (b != -1 || w != -1) {
        from = 5;
        to = 8;
        printf("ESTADO: |  X -> %d  |   Y -> %d |   Z ->%d  |  B ->%d |  w -> %d | \n", x, y, z, b, w);
    }
    if (b == -1 || w == -1) {
        from = 0;
        to = 5;
        printf("\tESTADO: |  X -> %d  |   Y -> %d |   Z ->%d  |\n", x, y, z);
    }
    separador();
    for (int i = from; i < to; ++i) {
        mostrarExpresionEvaluada(i, x, y, z, b, w);
    }
}

void resolverEjercicio3_2() {
    /*
     Ingrese el estado inicial de X,Y,Z,B W :4 -4 8 1 1

     ESTADO: |  X -> 4  |   Y -> -4 |   Z ->8  |  B ->1 |  w -> 1 |
    ---------------------------------------------------------------
    |EXP-6:         x % 4 == 0 (bool) ---> 1
    ---------------------------------------------------------------
    |EXP-7:         x + y == 0 && y - x == (-1) * z (bool) ---> 1
    ---------------------------------------------------------------
    |EXP-8:         not b && w (bool) ---> 0
    ---------------------------------------------------------------
     * */

    int x, y, z, b, w;

    int continuar = 1;
    printf("EJERCICIO 3.2 : \n\n");

    while (continuar == 1) {
        printf("Ingrese el estado inicial de X,Y,Z,B W (ej: 4 5 6 7 8): ");
        scanf("%d %d %d %d %d", &x, &y, &z, &b, &w);
        mostrarTabla(x, y, z, b, w);

        printf("Desea continuar la ejecucion? (ingrese 1 para continuar o 0 para interrumpir la ejecucion)-> ");
        scanf("%d", &continuar);
        saltoDeLinea();
        saltoDeLinea();
        saltoDeLinea();

    }

}

void resolverEjercicio3_1() {
    printf("EJERCICIO 3.1' : \n\n");
    int x, y, z;
    int continuar = 1;
    while (continuar == 1) {
        printf("Ingrese el estado inicial de X,Y,Z (ej: 4 5 6): ");
        scanf("%d %d %d", &x, &y, &z);
        mostrarTabla(x, y, z, -1, -1);

        printf("Desea continuar la ejecucion? (ingrese 1 para continuar o 0 para interrumpir la ejecucion)-> ");
        scanf("%d", &continuar);
        saltoDeLinea();
        saltoDeLinea();
        saltoDeLinea();

    }

}

void mostrarExpresionEvaluada(int opcion, int x, int y, int z, int b, int w) {
    printf("|EXP-%d:  \t%s ---> %d \n", opcion + 1, mapOpcionAExpresion(opcion), mapOpcionAInt(opcion, x, y, z, b, w));
    separador();
}

int mapOpcionAInt(int opcion, int x, int y, int z, int b, int w) {
    int opciones[8] = {
            (x + y + 1),
            ((z * z) + (y * 45) + (-(15 * x))),
            ((y - 2) == ((x * 3 + 1) % 5)),
            (y / (2 * x)),
            (y < (x * z)),
            ((x % 4) == 0),
            (((x + y) == 0) && ((y - x) == (-1) * z)),
            (!(b && w))
    };
    if (!(opcion >= 0 && opcion <= 7)) {
        return -1;
    }
    return opciones[opcion];
}

char *mapOpcionAExpresion(int opcion) {
    if (!(opcion >= 0 && opcion < NUMERO_DE_EXPRESIONES)) {
        return "No se encontro la opcion";
    }
    return EXPRESIONES[opcion];
}
