/* Copyright R. Gentleman, 2005, all rights reserved */

#include <R.h>
#include <Rinternals.h>

/* input a double vector and return the ordered values */
SEXP simpleSort(SEXP inVec)
{
    SEXP ans, tmp;
    int i;

    if( !isReal(inVec) )
       error("expected a double vector");

    /* allocate the answer */
    PROTECT(ans = allocVector(INTSXP, length(inVec)));

    for(i=0; i<length(inVec); i++)
        INTEGER(ans)[i] = i+1; /* R uses one-based indexing */

    /* we need a copy since the sorting is destructive 
       and inVec is not a copy */
    tmp = Rf_duplicate(inVec);
    
    rsort_with_index(REAL(tmp), INTEGER(ans), length(inVec));
    UNPROTECT(1);
    return(ans);
}

/* using the random number generator - slightly more complicated
   since we will call into R to figure out which RNG is in use
   then we generate "num" random Normal random variables and
   return them in a vector. Before returning we attach an attribute
   which states the RNG that was used.

   The "type" variable is not used. It is intended to allow 
   some simple exercises to be carried out by pupils, without the
   need to redefine the interface.

*/

SEXP simpleRand(SEXP num, SEXP type)
{
    SEXP ans, Rc, tmp;
    int i, x;

    /* isNumeric is true for LGLSXP - might not want that */
    if( !isNumeric(num) || length(num) > 1 )
        error("incorrect input");

    x = asInteger(num);
    PROTECT(ans = allocVector(REALSXP, x)); 

    /* allocate random normals, need to get and put state */
    GetRNGstate();
    for(i=0; i<x; i++)
	REAL(ans)[i] = norm_rand();
    PutRNGstate;

    /*create the language structure needed to call R */
    PROTECT(Rc = lang1(install("RNGkind")));
    PROTECT(tmp = eval(Rc, R_GlobalEnv));

    setAttrib(ans, install("RNG"), tmp);
    UNPROTECT(3);
    return(ans);
}

void simplePVect(double* ivec, int* len)
{
  int i;

  for(i=0; i < *len; i++)
    printf("Element %d is %f \n", i, ivec[i]);

}

SEXP evalExpr(SEXP expr, SEXP rho)
{
  return(eval(expr, rho));
}
