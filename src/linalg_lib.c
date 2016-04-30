#include "suitesparse/umfpack.h"
#include "stdlib.h"

void umfpack_dsolve(int i[], int j[], double A[], double b[], double x[], int Nij, int Nx) {
	void *Symbolic, *Numeric;
	int* Ap;
	int* Ai;
	double* Ax;
	int status;

	Ap = (int*) malloc((Nx+1)*sizeof(int));
	Ai = (int*) malloc((Nij)*sizeof(int));
	Ax = (double*) malloc((Nij)*sizeof(double));

	/* convert matrix from triplet form to compressed-column form */
	status = umfpack_di_triplet_to_col(Nx,Nx,Nij,i,j,A,Ap,Ai,Ax,NULL);
	/* symbolic analysis */
	status = umfpack_di_symbolic(Nx,Nx,Ap,Ai,Ax,&Symbolic,NULL,NULL);
	/* LU factorization */
	umfpack_di_numeric(Ap,Ai,Ax,Symbolic,&Numeric,NULL,NULL);
	umfpack_di_free_symbolic(&Symbolic);
	/* solve system */
	umfpack_di_solve(UMFPACK_A,Ap,Ai,Ax,x,b,Numeric,NULL,NULL);
	umfpack_di_free_numeric(&Numeric);
	
	free(Ap);
	free(Ai);
	free(Ax);
	}

void umfpack_zsolve(int i[], int j[],
                    double Ar[], double Ai[],
                    double br[], double bi[],
                    double xr[], double xi[],
                    int Nij, int Nx) {
	void *Symbolic, *Numeric;
	int* Ap;
	int* Aj;
	double* Ax;
	double* Ay;
	int status;

	Ap = (int*) malloc((Nx+1)*sizeof(int));
	Aj = (int*) malloc((Nij)*sizeof(int));
	Ax = (double*) malloc((Nij)*sizeof(double));
	Ay = (double*) malloc((Nij)*sizeof(double));

	/* convert matrix from triplet form to compressed-column form */
	status = umfpack_zi_triplet_to_col(Nx,Nx,Nij,i,j,Ar,Ai,Ap,Aj,Ax,Ay,NULL);
	/* symbolic analysis */
	status = umfpack_zi_symbolic(Nx,Nx,Ap,Aj,Ax,Ay,&Symbolic,NULL,NULL);
	/* LU factorization */
	umfpack_zi_numeric(Ap,Aj,Ax,Ay,Symbolic,&Numeric,NULL,NULL);
	umfpack_di_free_symbolic(&Symbolic);
	/* solve system */
	umfpack_zi_solve(UMFPACK_A,Ap,Aj,Ax,Ay,xr,xi,br,bi,Numeric,NULL,NULL);
	umfpack_zi_free_numeric(&Numeric);
	
	free(Ap);
	free(Aj);
	free(Ax);
	free(Ay);
	}
