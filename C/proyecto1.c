#include<stdio.h>

//PROYECTO 1 - Programacion imperativa

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


int main() {
    resolverEjercicio3_1();
    resolverEjercicio3_2();
    return 0;
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
    printf(hline);
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

    while (continuar==1) {
        printf("Ingrese el estado inicial de X,Y,Z,B W : ");
        scanf("%d %d %d %d %d", &x, &y, &z, &b, &w);
        mostrarTabla(x, y, z, b, w);

        printf("Desea continuar la ejecucion? (ingrese 1 para continuar o 0 para interrumpir la ejecucion)-> ");
        scanf("%d", &continuar);
        printf("\n\n\n");

    }

}

void resolverEjercicio3_1() {
    printf("EJERCICIO 3.1' : \n\n");
    int x, y, z;
    int continuar = 1;
    while (continuar==1) {
        printf("Ingrese el estado inicial de X,Y,Z : ");
        scanf("%d %d %d", &x, &y, &z);
        mostrarTabla(x, y, z, -1, -1);

        printf("Desea continuar la ejecucion? (ingrese 1 para continuar o 0 para interrumpir la ejecucion)-> ");
        scanf("%d", &continuar);
        printf("\n\n\n");

    }

}

void mostrarExpresionEvaluada(int opcion, int x, int y, int z, int b, int w) {
    printf("|EXP-%d:  \t%s ---> %d \n", opcion + 1, mapOpcionAExpresion(opcion), mapOpcionAInt(opcion, x, y, z, b, w));
    printf(hline);
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
