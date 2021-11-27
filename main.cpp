#include <malloc.h>
#include <time.h>

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#define DECIMAL 10
#define FIVE 5
#define NINE 9
#define NUMBER 1000000000
#define EPS 1E-9

using namespace std;

double determine(double const *m, int n)
{
    double *matrix = (double *)malloc(n * n * sizeof(double));
    for (int i = 0; i < n * n; i++) {
        matrix[i] = m[i];
    }
    double det = 1;
    for (int i = 0; i < n; i++) {
        int k = i;
        for (int j = i + 1; j < n; j++) {
            if (abs(matrix[j * n + i]) > abs(matrix[k * n + i])) {
                k = j;
            }
        }
        if (abs(matrix[k * n + i]) < EPS) {
            det = 0;
            break;
        }
        for (int p = 0; p < n; p++) {
            double c = matrix[i * n + p];
            matrix[i * n + p] = matrix[k * n + p];
            matrix[k * n + p] = c;
        }
        if (i != k) {
            det = -det;
        }
        det *= matrix[i * n + i];
        for (int j = i + 1; j < n; j++) {
            matrix[i * n + j] /= matrix[i * n + i];
        }
        for (int j = 0; j < n; j++) {
            if (j != i && abs(matrix[j * n + i]) > EPS) {
                for (int k = i + 1; k < n; k++) {
                    matrix[j * n + k] -= matrix[i * n + k] * matrix[j * n + i];
                }
            }
        }
    }
    free(matrix);
    return det;
}

double function(int n, int k, int i, int j)
{
    switch (k) {
    case 1:
        return (n - max(i, j));
    case 2:
        return max(i, j) + 1;
    case 3:
        return (max(i, j) - min(i, j));
    case 4: {
        double i1 = i;
        double j1 = j;
        return (1 / (i1 + j1 + 1));
    }
    }
    return 0;
}

void output(int m, int n, int k, double *matrix)
{
    for (int i = 0; i < min(n, m); i++) {
        for (int j = 0; j < min (k, m); j++) {
            printf(" %10.3e", matrix[i * n + j]);
        }
        cout << "\n";
    }
}

void input(int argc, int n, int m, int k, char *argv[], double *matrix)
{
    if (n <= 0 || m <= 0) {
        cout << "Неверный формат аргументов\n";
        exit(-1);
    }
    if (m > n) {
        cout << "Неверный формат аргументов\n";
        exit(-1);
    }
    if ((k == 0) && (argc == 4)) {
        cout << "Не указан файл\n";
        exit(-1);
    }
    if (matrix == NULL) {
        cout << "Не возможно выделить память\n";
        exit(-2);
    }
    switch (argc) {
    case 4: {
        if (k < 0 || k > 4) {
            cout << "Не существует такого к\n";
            free(matrix);
            exit(-1);
        }
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                matrix[i * n + j] = function(n, k, i, j);
            }
        }
        break;
    }
    case FIVE: {
        if (k == 0) {
            ifstream fin(argv[4]);

            if (!fin.is_open()) {
                cout << "Файл не существует!\n";
                free(matrix);
                exit(-3);
            } else if (fin.peek() == EOF) {
                cout << "Файл пустой\n";
                free(matrix);
                exit(-3);
            }
            /* flag = 0 нет числа в записи
  flag = 1 до запятой
  flag = 2 запятая есть*/

            for (int i = 0; i < n * n; ++i) {
                matrix[i] = 0;
            }

            string c;
            int flag = 0;
            int point = 0;
            int i = 0;
            while (!fin.eof()) {
                getline(fin, c);
                for (long unsigned int p = 0; p < c.size(); p++) {
                    if (c[p] >= '0' && c[p] <= '9') {
                        if (flag == 2) {
                            matrix[i] = matrix[i] * DECIMAL + c[p] - '0';
                            point++;
                        } else {
                            matrix[i] = matrix[i] * DECIMAL + c[p] - '0';
                            flag = 1;
                        }
                    } else if (c[p] == ',' || c[p] == '.') {
                        if (flag == 1) {
                            flag = 2;
                        } else {
                            cout << "Неверный формат данных файла! 1\n";
                            free(matrix);
                            exit(-1);
                        }
                    } else if (c[p] == ' ') {
                        if (flag != 0) {
                            matrix[i] /= pow(DECIMAL, point);
                            point = 0;
                            flag = 0;
                            i++;
                            if (i == n * n) {
                                break;
                            }
                        }
                    } else {
                        cout << "Cимвол вместо числа\n";
                        free(matrix);
                        exit(-3);
                    }
                }
                if (i == n * n) {
                    break;
                }
                if (flag != 0) {
                    matrix[i] /= pow(DECIMAL, point);
                    point = 0;
                    flag = 0;
                    i++;
                    if (i == n * n) {
                        break;
                    }
                }
            }
            fin.close();
            if (i != n * n) {
                cout << "Недостаточно элементов матрицы!\n";
                free(matrix);
                exit(-1);
            }
        } else {
            if ((k < 0) || (k > 4)) {
                cout << "Не существует такого к\n";
                free(matrix);
                exit(-1);
            }
            for (int i = 0; i < n; ++i) {
                for (int j = 0; j < n; j++) {
                    matrix[i * n + j] = function(n, k, i, j);
                }
            }
        }
        break;
    }
    }
    // return matrix;
}

void inverse_matrix(int n, const double *m, double *inverse, double *matrix)
{
    for (int i = 0; i < n * n; i++) {
        matrix[i] = m[i];
    }
    for (int i = 0; i < n * n; i++) {
        for (int i = 0; i < n; i++) { //единичная
            for (int j = 0; j < i; j++) {
                inverse[i * n + j] = 0;
            }
            inverse[i * n + i] = 1;
            for (int j = i + 1; j < n; j++) {
                inverse[i * n + j] = 0;
            }
        }
    }
    double s = 0;
    double a;
    double x[n];
    double norma_x;
    double vec;
    for (int k = 0; k < n; k++) { //приведение к треугольному виду
        for (int j = k + 1; j < n; j++) {
            s += matrix[j * n + k] * matrix[j * n + k];
        }
        a = sqrt(matrix[k * n + k] * matrix[k * n + k] + s);
        x[k] = matrix[k * n + k] - a;
        for (int j = k + 1; j < n; j++) {
            x[j] = matrix[j * n + k];
        }
        norma_x = sqrt(x[k] * x[k] + s);
        for (int j = k; j < n; j++) {
            x[j] = x[j] / norma_x;
        }
        for (int j = k; j < n; j++) { //переписываем столбцы
            vec = 0;
            for (int i = k; i < n; i++) {
                vec += x[i] * matrix[i * n + j];
            }
            vec *= 2;
            for (int i = k; i < n; i++) {
                matrix[i * n + j] = matrix[i * n + j] - x[i] * vec;
            }
        }
        for (int j = 0; j < n; j++) { //то же самое для обратной матрицы
            vec = 0;
            for (int i = k; i < n; i++) {
                vec += x[i] * inverse[i * n + j];
            }
            vec *= 2;
            for (int i = k; i < n; i++) {
                inverse[i * n + j] = inverse[i * n + j] - x[i] * vec;
            }
        }
    }
    for (int i = n - 1; i >= 0; i--) { 
        for (int j = 0; j < i; j++) { //обнуляем в строке над ним
            double h = matrix[j * n + i] / matrix[i * n + i];
            matrix[j * n + i] = 0;
            for (int l = 0; l < n; l++) { //для обратной сделаем то же самое
                inverse[j * n + l] -= h * inverse[i * n + l];
            }
        }
    }
}

void sls (int n, const double *m, const double *b, double *result, double *inverse, double *matrix)
{
    inverse_matrix(n, m, inverse, matrix);
    for (int i = 0; i < n; i++) {
        result[i] = 0;
        for (int j = 0; j < n; j++) {
            result[i] += inverse[i * n + j] * b[j];
        }
    }
}

void residual(int n, double const *matrix, double const *b, double const *result)
{
    double *m = (double *)malloc(n * sizeof(double));
    for (int i = 0; i < n; i++) {
        m[i] = 0;
        for (int j = 0; j < n; j++) {
            m[i] += matrix[i * n + j] * result[j];
        }
    }
    for (int i = 0; i < n; i++) {
        m[i] += b[i];
    }
    double norma_m = 0;
    for (int i = 0; i < n; i++) {
        norma_m += m[i] * m[i];
    }
    norma_m = sqrt(norma_m);
    double norma_b = 0;
    for (int i = 0; i < n; i++) {
        norma_b += b[i] * b[i];
    }
    norma_b = sqrt(norma_b);
    norma_m /= norma_b;
    free(m);
    cout << "Residual:";
    printf(" %10.3e\n", norma_m);

}

int main(int argc, char *argv[])
{
    char *endptr;
    if ((argc < 4) || (argc > FIVE)) {
        cout << "Неверное количество аргументов!\n";
        return (-1);
    }
    for (int k = 1; k < 4; k++) {
        if (!(argv[k][0] == '-' || (argv[k][0] >= '0' && argv[k][0] <= '9'))) {
            cout << "Неверный формат аргументов*\n";
            return -1;
        }
        for (long unsigned int i = 1; i < strlen(argv[k]); i++) {
            if (argv[k][i] < '0' || argv[k][i] > '9') {
                cout << "Неверный формат аргументов*\n";
                return -1;
            }
        }
    }
    int n = (int)strtol(argv[1], &endptr, DECIMAL);
    int m = (int)strtol(argv[2], &endptr, DECIMAL);
    int k = (int)strtol(argv[3], &endptr, DECIMAL);
    double *matrix = (double *)malloc(n * n * sizeof(double));
    double *b = (double *)malloc(n * sizeof(double));
    input(argc, n, m, k, argv, matrix);

    if (abs(determine(matrix, n)) < EPS) {
        cout << "Вырожденная матрица\n";
        free(matrix);
        return -4;
    }
    for (int i = 0; i < n; i++) {
        b[i] = 0;
        for (int k = 0; k < n; k += 2) {
            b[i] += matrix[i * n + k];
        }
    }
    output(m, n, n, matrix);
    for (int i = 0; i < m; i++) {
        printf(" %10.3e", b[i]);
    }
    cout << "\n";

    double *inverse = (double *)malloc(n * n * sizeof(double));
    double *matrix_new = (double *)malloc(n * n * sizeof(double));
    double *result = (double *)malloc(n * sizeof(double));

    struct timespec start_time;
    struct timespec end_time;
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC, &start_time);

    //алгоритм нахождения обратной
    sls(n, matrix, b, result, inverse, matrix_new);

    clock_gettime(CLOCK_MONOTONIC, &end_time);
    time.tv_sec = end_time.tv_sec - start_time.tv_sec;
    time.tv_nsec = end_time.tv_nsec - start_time.tv_nsec;
    if (time.tv_nsec < 0) {
        time.tv_sec--;
        time.tv_nsec += NUMBER;
    }

    cout << "Solution:";
    output(m, 1, n, result);

    cout << "Time:" << fixed << setprecision(NINE)
         << time.tv_sec + (double)time.tv_nsec / NUMBER << endl;

    residual(n, matrix, b, result);
    free(inverse);
    free(matrix);
    free(matrix_new);
    free(result);
    return 0;
}
