/* R. Gentleman, copyright 2005, all rights reserved */

/* Registration of C routines */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


SEXP simpleSort(SEXP);
SEXP simpleRand(SEXP, SEXP);
void simplePVect(double*, int*);
SEXP evalExpr(SEXP, SEXP);

#if _MSC_VER >= 1000
__declspec(dllexport)
#endif

    static const R_CallMethodDef R_CallDef[] = {
        {"simpleSort", (DL_FUNC)&simpleSort, 1},
        {"simpleRand", (DL_FUNC)&simpleRand, 2},
        {"evalExpr", (DL_FUNC)&evalExpr, 2},
        {NULL, NULL, 0},
    };

    static const R_CMethodDef R_CDef[] = {
        {"simplePVect", (DL_FUNC)&simplePVect, 2},
        {NULL, NULL, 0},
    };
  
/* name must be R_init_NAMEOFPACKAGEHERE and then it gets called
   automagically */
void R_init_RBioinf(DllInfo *info)
{
  R_registerRoutines(info,R_CDef,R_CallDef,NULL,NULL);
}


