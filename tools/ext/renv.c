
#include <stdlib.h> // for NULL

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Helpers ----

// check for a call of the form 'package::foo(a, b, c)';
// if we match, then create a new call of the form 'foo(a, b, c)'
static SEXP renv_call_expect_package(SEXP node, SEXP package) {
  
  if (TYPEOF(node) != LANGSXP)
    return node;
  
  SEXP call = CAR(node);
  if (TYPEOF(call) != LANGSXP)
    return node;
  
  SEXP symbol = CAR(call);
  if (TYPEOF(symbol) != SYMSXP)
    return node;
  
  const char* value = CHAR(PRINTNAME(symbol));
  if (strcmp(value, "::") != 0 && strcmp(value, ":::") != 0)
    return node;
  
  SEXP name = CADR(call);
  if (TYPEOF(name) != SYMSXP)
    return node;
  
  int matches = strcmp(CHAR(PRINTNAME(name)), CHAR(STRING_ELT(package, 0))) == 0;
  if (!matches)
    return node;
  
  return Rf_lcons(CADDR(call), CDR(node));

}


// Methods ----

SEXP renv_ffi_recurse(SEXP object,
                      SEXP symbol,
                      SEXP expr,
                      SEXP envir)
{
  switch (TYPEOF(object)) {
    
  case LISTSXP:
  case LANGSXP:
    
    Rf_defineVar(symbol, object, envir);
    SEXP result = Rf_eval(expr, envir);
    if (TYPEOF(result) == LANGSXP) {
      object = result;
    }
    
    PROTECT(object);
    while (object != R_NilValue) {
      renv_ffi_recurse(CAR(object), symbol, expr, envir);
      object = CDR(object);
    }
    UNPROTECT(1);
    
    break;
    
  case VECSXP:
  case EXPRSXP:
    
    for (int i = 0, n = Rf_xlength(object); i < n; i++) {
      renv_ffi_recurse(VECTOR_ELT(object, i), symbol, expr, envir);
    }
    
    break;
    
  }
  
  return object;
  
}

SEXP renv_ffi_call_expect(SEXP node, SEXP package, SEXP methods) {
  
  node = renv_call_expect_package(node, package);
  PROTECT(node);

  SEXP symbol = CAR(node);
  if (TYPEOF(symbol) != SYMSXP) {
    UNPROTECT(1);
    return R_NilValue;
  }
  
  const char* symname = CHAR(PRINTNAME(symbol));
  for (int i = 0, n = Rf_xlength(methods); i < n; i++) {
    const char* method = CHAR(STRING_ELT(methods, i));
    if (strcmp(method, symname) == 0) {
      UNPROTECT(1);
      return node;
    }
  }
  
  UNPROTECT(1);
  return R_NilValue;
  
}


// Init ----

static const R_CallMethodDef callEntries[] = {
  { "renv_ffi_recurse",     (DL_FUNC) &renv_ffi_recurse, 4 },
  { "renv_ffi_call_expect", (DL_FUNC) &renv_ffi_call_expect, 3},
  { NULL, NULL, 0 }
};

void R_init_renv(DllInfo* dllInfo)
{
  R_registerRoutines(dllInfo, NULL, callEntries, NULL, NULL);
  R_useDynamicSymbols(dllInfo, FALSE);
}
