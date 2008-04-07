#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
          
void R_init_xxx(DllInfo *info)
          {
            printf("howdy\n");
          }
          
void R_unload_xxx(DllInfo *info)
          {
            printf("dudy\n");
          }

