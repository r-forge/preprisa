#include<stdlib.h>

void taballoc (double ***tab, int l1, int c1)
{
/*
/ memory allocation for two dimensional table
*/
	int i, j;
	
	if ( (*tab = (double **) calloc(l1+1, sizeof(double *))) != NULL) {
		for (i=0;i<=l1;i++) {
			if ( (*(*tab+i)=(double *) calloc(c1+1, sizeof(double))) == NULL ) {
				for (j=0;j<i;j++) {
					free(*(*tab+j));
				}
				/* err_message("Error: insufficient memory (taballoc)"); */
			}
		}
	} /* else err_message("Error: insufficient memory (taballoc)"); */
	**(*tab) = l1;
	**(*tab+1) = c1;
}

void freetab (double **tab)
{
/*
/ free memory for two dimensional table
*/
	int 	i, n;

	n = *(*(tab));
	for (i=0;i<=n;i++) {
		free((char *) *(tab+i) );
	}
	free((char *) tab);
}

void repClass(int *ioptsm1,
				int *ngr1,
				int *nbech1,
				int *nlig1,
				int *nclatot1,
				int *maxnlim1,
				int *pb,
				int *ncla,
				int *tablim1,
				double *hp,
				int *nech,
				double *tabh1)
{
	int		i, j, k, ilig, numpic, nclasspic, numclasspic, iech;
	int		ioptsm, ngr, nbech, nlig, nclatot, maxnlim;
	double	xinf, xsup, maxpic, x;
	double	**tablim;
	double	**tabh;
/*
/ recover arguments
*/
	ioptsm = *ioptsm1;
	ngr = *ngr1;
	nbech = *nbech1;
	nlig = *nlig1;
	nclatot = *nclatot1;
	maxnlim = *maxnlim1;
/*
/ declare and allocate memory for two dimensional tables
*/
	taballoc(&tablim, maxnlim+1, ngr);
	taballoc(&tabh, nbech, nclatot);
/*
/ recover the table of class limits
*/
	k=0;
	for (i=1; i<=maxnlim+1; i++) {
		for (j=1; j<=ngr; j++) {
			tablim[i][j] = (double) tablim1[k];
			k=k+1;
		}
	}

	if (ioptsm == 1) {
/*
/ compute the sum of peaks within each class
*/
		numpic = 0;
		for (ilig=1; ilig<=nlig; ilig++)  {
			nclasspic = 0;
			numpic = numpic + 1;
			x = (double) pb[numpic-1];
			for (i=1; i<=ngr; i++)  {
				for (j=2; j<=ncla[i-1]+1; j++)  {
					xinf = (double) tablim[j-1][i];
					xsup = (double) tablim[j][i];
					if ((xinf <= x ) && (x < xsup)) {
						numclasspic = nclasspic + j - 1;
						tabh[(int) nech[ilig-1] ][numclasspic] = tabh[(int) nech[ilig-1] ][numclasspic] + hp[ilig-1];
						goto suite;
					}
				}
				nclasspic = nclasspic + ncla[i-1];
			}
suite:;
		}
	} else {
/*
/ compute the max of peaks within each class
*/
		for (iech=1; iech<=nbech; iech++)  {
			numpic = 0;
			maxpic = 0;
			for (ilig=1; ilig<=nlig; ilig++)  {
				numpic = numpic + 1;
				if (nech[ilig-1] == iech) {
					x = (double) pb[numpic-1];
					nclasspic = 0;
					for (i=1; i<=ngr; i++)  {
						for (j=2; j<=ncla[i-1]+1; j++)  {
							numclasspic = nclasspic + j - 1;
							xinf = (double) tablim[j-1][i];
							xsup = (double) tablim[j][i];
							if ((xinf <= x ) && (x < xsup)) {
								if (tabh[iech][numclasspic] < hp[ilig-1]) {
									tabh[iech][numclasspic] = hp[ilig-1];
								}
							}
						}
						nclasspic = nclasspic + ncla[i-1];
					}
				}
			}
		}
	}
/*
/ set up output table
*/
	k=0;
	for (i=1; i<=nbech; i++) {
		for (j=1; j<=nclatot; j++) {
			tabh1[k] = tabh[i][j];
			k=k+1;
		}
	}
/*
/ free declared memory
*/
	freetab (tablim);
	freetab (tabh);
}

