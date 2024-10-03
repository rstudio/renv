
#include <stdlib.h> // for NULL

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


static SEXP renv_call_expect(
    SEXP node,
    SEXP package,
    SEXP methods) {
  
  if (TYPEOF(node) == LANGSXP) {
    SEXP call = CAR(node);
    if (TYPEOF(call) == LANGSXP) {
      SEXP symbol = CAR(call);
      if (symbol == R_DoubleColonSymbol || symbol == R_TripleColonSymbol) {
        SEXP name = CADR(call);
        if (TYPEOF(name) == SYMSXP) {
          int matches = strcmp(CHAR(PRINTNAME(name)), CHAR(STRING_ELT(package, 0))) == 0;
          if (matches) {
            node = Rf_lcons(CADDR(call), CDR(node));
          }
        }
      }
    }
  }
  
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


static SEXP renv_dependencies_recurse(
    SEXP object,
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
      renv_dependencies_recurse(CAR(object), symbol, expr, envir);
      object = CDR(object);
    }
    UNPROTECT(1);
    
    break;
    
  case VECSXP:
  case EXPRSXP:
    
    for (int i = 0, n = Rf_xlength(object); i < n; i++) {
      renv_dependencies_recurse(VECTOR_ELT(object, i), symbol, expr, envir);
    }
    
    break;
    
  }
  
  return object;
  
}

#define GET_INTSXP(X)  INTEGER(X)[0]
#define GET_REALSXP(X) REAL(X)[0]
#define GET_LGLSXP(X)  LOGICAL(X)[0]
#define GET_STRSXP(X)  STRING_ELT(X, 0)
#define GET_VECSXP(X)  X

#define SET_INTSXP(X, I, V)   INTEGER(X)[I] = V
#define SET_REALSXP(X, I, V)  REAL(X)[I] = V
#define SET_LGLSXP(X, I, V)   LOGICAL(X)[I] = V
#define SET_STRSXP(X, I, V)   SET_STRING_ELT(X, I, V)
#define SET_VECSXP(X, I, V)   SET_VECTOR_ELT(X, I, V)

#define COERCE_INTSXP(X)     Rf_coerceVector(X, INTSXP)
#define COERCE_REALSXP(X)    Rf_coerceVector(X, REALSXP)
#define COERCE_LGLSXP(X)     Rf_coerceVector(X, LGLSXP)
#define COERCE_STRSXP(X)     Rf_coerceVector(X, STRSXP)
#define COERCE_VECSXP(X)     X

#define ENUMERATE_DISPATCH_VECSXP(__TYPE__)                    \
  ENUMERATE_DISPATCH_VECSXP_IMPL(                              \
    __TYPE__,                                                  \
    GET_ ## __TYPE__,                                          \
    SET_ ## __TYPE__,                                          \
    COERCE_ ## __TYPE__)
    
#define ENUMERATE_DISPATCH_VECSXP_IMPL(__TYPE__, __GET__, __SET__, __COERCE__) do { \
                                                                   \
  R_xlen_t n = Rf_xlength(x);                                      \
  SEXP output = PROTECT(Rf_allocVector(__TYPE__, n));              \
  Rf_setAttrib(output, R_NamesSymbol, names);                      \
                                                                   \
  if (names == R_NilValue) {                                       \
    Rf_defineVar(keysym, R_NilValue, envir);                       \
    for (R_xlen_t i = 0; i < n; i++) {                             \
      SEXP val = VECTOR_ELT(x, i);                                 \
      Rf_defineVar(valsym, val, envir);                            \
      SEXP call = PROTECT(Rf_lang4(fsym, keysym, valsym, R_DotsSymbol)); \
      SEXP result = PROTECT(Rf_eval(call, envir));                 \
      SEXP coerced = PROTECT(__COERCE__(result));                  \
      __SET__(output, i, __GET__(coerced));                        \
      UNPROTECT(3);                                                \
    }                                                              \
  } else {                                                         \
    for (R_xlen_t i = 0; i < n; i++) {                             \
      SEXP key = PROTECT(Rf_allocVector(STRSXP, 1));               \
      SET_STRING_ELT(key, 0, STRING_ELT(names, i));                \
      Rf_defineVar(keysym, key, envir);                            \
      SEXP val = VECTOR_ELT(x, i);                                 \
      Rf_defineVar(valsym, val, envir);                            \
      SEXP call = PROTECT(Rf_lang4(fsym, keysym, valsym, dsym));   \
      SEXP result = PROTECT(Rf_eval(call, envir));                 \
      SEXP coerced = PROTECT(__COERCE__(result));                  \
      __SET__(output, i, __GET__(coerced));                        \
      UNPROTECT(4);                                                \
    }                                                              \
  }                                                                \
                                                                   \
  UNPROTECT(2);                                                    \
  return output;                                                   \
                                                                   \
} while (0)

#define ENUMERATE_DISPATCH_ENVSXP(__TYPE__)                    \
  ENUMERATE_DISPATCH_ENVSXP_IMPL(                              \
    __TYPE__,                                                  \
    GET_ ## __TYPE__,                                          \
    SET_ ## __TYPE__,                                          \
    COERCE_ ## __TYPE__)

#define ENUMERATE_DISPATCH_ENVSXP_IMPL(__TYPE__, __GET__, __SET__, __COERCE__) do { \
                                                                 \
  R_xlen_t n = Rf_xlength(names);                                \
  SEXP output = PROTECT(Rf_allocVector(__TYPE__, n));            \
                                                                 \
  for (R_xlen_t i = 0; i < n; i++) {                             \
    SEXP key = PROTECT(Rf_allocVector(STRSXP, 1));               \
    SET_STRING_ELT(key, 0, STRING_ELT(names, i));                \
    Rf_defineVar(keysym, key, envir);                            \
    SEXP val = Rf_findVarInFrame(x, keysym);                     \
    Rf_defineVar(valsym, val, envir);                            \
    SEXP call = PROTECT(Rf_lang4(fsym, keysym, valsym, dsym));   \
    SEXP result = PROTECT(Rf_eval(call, envir));                 \
    SEXP coerced = PROTECT(__COERCE__(result));                  \
    __SET__(output, i, __GET__(coerced));                        \
    UNPROTECT(4);                                                \
  }                                                              \
                                                                 \
  UNPROTECT(2);                                                  \
  return output;                                                 \
                                                                 \
} while (0)
  
static SEXP enumerate(
    SEXP x,
    SEXP type,
    SEXP envir) {
  
  SEXP dsym = R_DotsSymbol;
  SEXP fsym = Rf_install("f");
  SEXP keysym = Rf_install("key");
  SEXP valsym = Rf_install("val");

  switch (TYPEOF(x)) {
  
  case VECSXP: {
    
    SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
    switch (TYPEOF(type)) {
    case INTSXP:  ENUMERATE_DISPATCH_VECSXP(INTSXP);
    case REALSXP: ENUMERATE_DISPATCH_VECSXP(REALSXP);
    case LGLSXP:  ENUMERATE_DISPATCH_VECSXP(LGLSXP);
    case STRSXP:  ENUMERATE_DISPATCH_VECSXP(STRSXP);
    case NILSXP:  ENUMERATE_DISPATCH_VECSXP(VECSXP);
    }
    
  }
    
  case ENVSXP: {
    
    SEXP names = PROTECT(R_lsInternal(x, TRUE));
    switch (TYPEOF(type)) {
    case INTSXP:  ENUMERATE_DISPATCH_ENVSXP(INTSXP);
    case REALSXP: ENUMERATE_DISPATCH_ENVSXP(REALSXP);
    case LGLSXP:  ENUMERATE_DISPATCH_ENVSXP(LGLSXP);
    case STRSXP:  ENUMERATE_DISPATCH_ENVSXP(STRSXP);
    case NILSXP:  ENUMERATE_DISPATCH_ENVSXP(VECSXP);
    }
    
  }
    
  default: {
    return R_NilValue;
  }
    
  }
}

// Init ----

static const R_CallMethodDef callEntries[] = {
  { "renv_ffi__renv_dependencies_recurse", (DL_FUNC) &renv_dependencies_recurse, 4 },
  { "renv_ffi__renv_call_expect",          (DL_FUNC) &renv_call_expect, 3},
  { "renv_ffi__enumerate" ,                (DL_FUNC) &enumerate, 3 },
  { NULL, NULL, 0 }
};

void R_init_renv(DllInfo* dllInfo)
{
  R_registerRoutines(dllInfo, NULL, callEntries, NULL, NULL);
  R_useDynamicSymbols(dllInfo, FALSE);
}
