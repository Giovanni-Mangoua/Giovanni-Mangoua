#include <stdio.h>
#include <stdlib.h>

#define N 2
#define M 2

void lireMatrice(int** mat)
{
    for(int i = 0;i < N;i++)
    {
       printf("[");
       for(int j =0;j < M;j++)
       {
           printf (" %d ",mat[i][j]);
           printf("]\n");
     }
}

int** somme (int mat1[][M],int mat2[][M])
{
   int** p = malloc(N * sizeof(int));
   if (p == NULL)
        exit(EXIT_FAILURE);
    for(int j =0;j < M;j++)
    {
       p[j] = malloc (M * sizeof (int));
       if (p[i] == NULL)
           exit(EXIT_FAILURE);
    }
    for(int i = 0;i < N;i++)
    {
       for(int j =0;j < M;j++)
           p[i][j] = mat1[i][j] + mat2[i][j];
     }
  return p;
}

int main()
{
    int A = {{1,2},{3,4}};
    int B = {{1,2},{3,4}};
    int** S = somme (A,B);

    lireMatrice (S);
 
   return 0;
}

