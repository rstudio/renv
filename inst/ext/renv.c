
#include <stdlib.h> // for NULL

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#define DBLSXP REALSXP

#define DBL_PTR REAL
#define INT_PTR INTEGER
#define LGL_PTR LOGICAL

enum {
  _NILSXP = NILSXP,
  _INTSXP = INTSXP,
  _DBLSXP = DBLSXP,
  _LGLSXP = LGLSXP,
  _STRSXP = STRSXP,
  _VECSXP = VECSXP,
  _ENVSXP = ENVSXP,
};

// Initialized in R_init_renv
static SEXP s_callbacksym;
static SEXP s_objectsym;

static SEXP renv_call_expect(SEXP node,
                             SEXP package,
                             SEXP methods)
{

  if (TYPEOF(node) == LANGSXP)
  {
    SEXP call = CAR(node);
    if (TYPEOF(call) == LANGSXP)
    {
      SEXP symbol = CAR(call);
      if (symbol == R_DoubleColonSymbol || symbol == R_TripleColonSymbol)
      {
        SEXP name = CADR(call);
        if (TYPEOF(name) == SYMSXP)
        {
          int matches = strcmp(CHAR(PRINTNAME(name)), CHAR(STRING_ELT(package, 0))) == 0;
          if (matches)
          {
            node = Rf_lcons(CADDR(call), CDR(node));
          }
        }
      }
    }
  }

  PROTECT(node);
  SEXP symbol = CAR(node);
  if (TYPEOF(symbol) != SYMSXP)
  {
    UNPROTECT(1);
    return R_NilValue;
  }

  const char* symname = CHAR(PRINTNAME(symbol));
  for (int i = 0, n = Rf_xlength(methods); i < n; i++)
  {
    const char* method = CHAR(STRING_ELT(methods, i));
    if (strcmp(method, symname) == 0)
    {
      UNPROTECT(1);
      return node;
    }
  }

  UNPROTECT(1);
  return R_NilValue;
}

static SEXP renv_dependencies_recurse(SEXP object,
                                      SEXP symbol,
                                      SEXP expr,
                                      SEXP envir)
{
  switch (TYPEOF(object))
  {

  case LISTSXP:
  case LANGSXP:

    Rf_defineVar(symbol, object, envir);
    SEXP result = Rf_eval(expr, envir);
    if (TYPEOF(result) == LANGSXP)
    {
      object = result;
    }

    PROTECT(object);
    while (object != R_NilValue)
    {
      renv_dependencies_recurse(CAR(object), symbol, expr, envir);
      object = CDR(object);
    }
    UNPROTECT(1);

    break;

  case VECSXP:
  case EXPRSXP:

    for (int i = 0, n = Rf_xlength(object); i < n; i++)
    {
      renv_dependencies_recurse(VECTOR_ELT(object, i), symbol, expr, envir);
    }

    break;
  }

  return object;
}

#define EMPTY

#define GET_NAMES_INTSXP(__X__) Rf_getAttrib(__X__, R_NamesSymbol)
#define GET_NAMES_DBLSXP(__X__) Rf_getAttrib(__X__, R_NamesSymbol)
#define GET_NAMES_LGLSXP(__X__) Rf_getAttrib(__X__, R_NamesSymbol)
#define GET_NAMES_STRSXP(__X__) Rf_getAttrib(__X__, R_NamesSymbol)
#define GET_NAMES_VECSXP(__X__) Rf_getAttrib(__X__, R_NamesSymbol)
#define GET_NAMES_ENVSXP(__X__) R_lsInternal(__X__, FALSE)

#define GET_INTSXP(__X__, __I__) Rf_ScalarInteger(INTEGER(__X__)[__I__])
#define GET_DBLSXP(__X__, __I__) Rf_ScalarReal(REAL(__X__)[__I__])
#define GET_LGLSXP(__X__, __I__) Rf_ScalarLogical(LOGICAL(__X__)[__I__])
#define GET_STRSXP(__X__, __I__) Rf_ScalarString(STRING_ELT(__X__, __I__))
#define GET_VECSXP(__X__, __I__) VECTOR_ELT(__X__, __I__)

#define SET_INTSXP(__X__, __I__, __V__) INT_PTR(__X__)[__I__] = __V__
#define SET_DBLSXP(__X__, __I__, __V__) DBL_PTR(__X__)[__I__] = __V__
#define SET_LGLSXP(__X__, __I__, __V__) LGL_PTR(__X__)[__I__] = __V__
#define SET_STRSXP(__X__, __I__, __V__) SET_STRING_ELT(__X__, __I__, __V__)
#define SET_VECSXP(__X__, __I__, __V__) SET_VECTOR_ELT(__X__, __I__, __V__)

#define EXTRACT_INTSXP(__X__) INT_PTR(__X__)[0]
#define EXTRACT_DBLSXP(__X__) DBL_PTR(__X__)[0]
#define EXTRACT_LGLSXP(__X__) LGL_PTR(__X__)[0]
#define EXTRACT_STRSXP(__X__) STRING_ELT(__X__, 0)
#define EXTRACT_VECSXP(__X__) __X__

#define COERCE_INTSXP(__X__) Rf_coerceVector(__X__, INTSXP)
#define COERCE_DBLSXP(__X__) Rf_coerceVector(__X__, DBLSXP)
#define COERCE_LGLSXP(__X__) Rf_coerceVector(__X__, LGLSXP)
#define COERCE_STRSXP(__X__) Rf_coerceVector(__X__, STRSXP)
#define COERCE_VECSXP(__X__) __X__

#define ENUMERATE_CASE_IMPL_INTSXP ENUMERATE_CASE_DISPATCH
#define ENUMERATE_CASE_IMPL_DBLSXP ENUMERATE_CASE_DISPATCH
#define ENUMERATE_CASE_IMPL_LGLSXP ENUMERATE_CASE_DISPATCH
#define ENUMERATE_CASE_IMPL_STRSXP ENUMERATE_CASE_DISPATCH
#define ENUMERATE_CASE_IMPL_VECSXP ENUMERATE_CASE_DISPATCH
#define ENUMERATE_CASE_IMPL_ENVSXP ENUMERATE_CASE_DISPATCH_ENVSXP

#define ENUMERATE_CASE(__TYPE__) ENUMERATE_CASE_IMPL(__TYPE__, GET_NAMES##__TYPE__, ENUMERATE_CASE_IMPL##__TYPE__)

#define ENUMERATE_CASE_IMPL(__TYPE__, __GET_NAMES__, __DISPATCH__)                                                     \
  do                                                                                                                   \
  {                                                                                                                    \
    SEXP result = R_NilValue;                                                                                          \
    SEXP names = PROTECT(__GET_NAMES__(x));                                                                            \
                                                                                                                       \
    switch (TYPEOF(type))                                                                                              \
    {                                                                                                                  \
    case _INTSXP: __DISPATCH__(result, __TYPE__, _INTSXP); break;                                                      \
    case _DBLSXP: __DISPATCH__(result, __TYPE__, _DBLSXP); break;                                                      \
    case _LGLSXP: __DISPATCH__(result, __TYPE__, _LGLSXP); break;                                                      \
    case _STRSXP: __DISPATCH__(result, __TYPE__, _STRSXP); break;                                                      \
    case _VECSXP: __DISPATCH__(result, __TYPE__, _VECSXP); break;                                                      \
    case _NILSXP: __DISPATCH__(result, __TYPE__, _VECSXP); break;                                                      \
    }                                                                                                                  \
                                                                                                                       \
    UNPROTECT(1);                                                                                                      \
    return result;                                                                                                     \
  } while (0)

#define ENUMERATE_CASE_DISPATCH(__RESULT__, __INPUT_TYPE__, __OUTPUT_TYPE__)                                           \
  ENUMERATE_CASE_DISPATCH_IMPL(__RESULT__,                                                                             \
                               __OUTPUT_TYPE__,                                                                        \
                               GET##__INPUT_TYPE__,                                                                    \
                               SET##__OUTPUT_TYPE__,                                                                   \
                               EXTRACT##__OUTPUT_TYPE__,                                                               \
                               COERCE##__OUTPUT_TYPE__)

#define ENUMERATE_CASE_DISPATCH_IMPL(__RESULT__, __TYPE__, __GET__, __SET__, __EXTRACT__, __COERCE__)                  \
  do                                                                                                                   \
  {                                                                                                                    \
    R_xlen_t n = Rf_xlength(x);                                                                                        \
    SEXP output = PROTECT(Rf_allocVector(__TYPE__, n));                                                                \
    Rf_setAttrib(output, R_NamesSymbol, names);                                                                        \
                                                                                                                       \
    if (names == R_NilValue)                                                                                           \
    {                                                                                                                  \
      Rf_defineVar(keysym, R_NilValue, envir);                                                                         \
      for (R_xlen_t i = 0; i < n; i++)                                                                                 \
      {                                                                                                                \
        SEXP val = __GET__(x, i);                                                                                      \
        Rf_defineVar(valsym, val, envir);                                                                              \
        SEXP call = PROTECT(Rf_lang4(fsym, keysym, valsym, dsym));                                                     \
        SEXP result = PROTECT(R_forceAndCall(call, 2, envir));                                                         \
        SEXP coerced = PROTECT(__COERCE__(result));                                                                    \
        __SET__(output, i, __EXTRACT__(coerced));                                                                      \
        UNPROTECT(3);                                                                                                  \
      }                                                                                                                \
    }                                                                                                                  \
    else                                                                                                               \
    {                                                                                                                  \
      for (R_xlen_t i = 0; i < n; i++)                                                                                 \
      {                                                                                                                \
        SEXP key = PROTECT(Rf_allocVector(STRSXP, 1));                                                                 \
        SET_STRING_ELT(key, 0, STRING_ELT(names, i));                                                                  \
        Rf_defineVar(keysym, key, envir);                                                                              \
        SEXP val = __GET__(x, i);                                                                                      \
        Rf_defineVar(valsym, val, envir);                                                                              \
        SEXP call = PROTECT(Rf_lang4(fsym, keysym, valsym, dsym));                                                     \
        SEXP result = PROTECT(R_forceAndCall(call, 2, envir));                                                         \
        SEXP coerced = PROTECT(__COERCE__(result));                                                                    \
        __SET__(output, i, __EXTRACT__(coerced));                                                                      \
        UNPROTECT(4);                                                                                                  \
      }                                                                                                                \
    }                                                                                                                  \
                                                                                                                       \
    UNPROTECT(1);                                                                                                      \
    __RESULT__ = output;                                                                                               \
                                                                                                                       \
  } while (0)

#define ENUMERATE_CASE_DISPATCH_ENVSXP(__RESULT__, __INPUT_TYPE__, __OUTPUT_TYPE__)                                    \
  ENUMERATE_CASE_DISPATCH_ENVSXP_IMPL(__RESULT__,                                                                      \
                                      __OUTPUT_TYPE__,                                                                 \
                                      GET##__INPUT_TYPE__,                                                             \
                                      SET##__OUTPUT_TYPE__,                                                            \
                                      EXTRACT##__OUTPUT_TYPE__,                                                        \
                                      COERCE##__OUTPUT_TYPE__)

#define ENUMERATE_CASE_DISPATCH_ENVSXP_IMPL(__RESULT__, __TYPE__, __GET__, __SET__, __EXTRACT__, __COERCE__)           \
  do                                                                                                                   \
  {                                                                                                                    \
    R_xlen_t n = Rf_xlength(names);                                                                                    \
    SEXP output = PROTECT(Rf_allocVector(__TYPE__, n));                                                                \
    Rf_setAttrib(output, R_NamesSymbol, names);                                                                        \
                                                                                                                       \
    for (R_xlen_t i = 0; i < n; i++)                                                                                   \
    {                                                                                                                  \
      SEXP name = STRING_ELT(names, i);                                                                                \
      SEXP key = PROTECT(Rf_allocVector(STRSXP, 1));                                                                   \
      SET_STRING_ELT(key, 0, name);                                                                                    \
      Rf_defineVar(keysym, key, envir);                                                                                \
      SEXP val = Rf_findVarInFrame(x, Rf_installChar(name));                                                           \
      Rf_defineVar(valsym, val, envir);                                                                                \
      SEXP call = PROTECT(Rf_lang4(fsym, keysym, valsym, dsym));                                                       \
      SEXP result = PROTECT(R_forceAndCall(call, 2, envir));                                                           \
      SEXP coerced = PROTECT(__COERCE__(result));                                                                      \
      __SET__(output, i, __EXTRACT__(coerced));                                                                        \
      UNPROTECT(4);                                                                                                    \
    }                                                                                                                  \
                                                                                                                       \
    UNPROTECT(1);                                                                                                      \
    __RESULT__ = output;                                                                                               \
                                                                                                                       \
  } while (0)

static SEXP enumerate(SEXP x,
                      SEXP type,
                      SEXP envir)
{

  SEXP dsym = R_DotsSymbol;
  SEXP fsym = Rf_install("f");
  SEXP keysym = Rf_install("key");
  SEXP valsym = Rf_install("val");

  switch (TYPEOF(x))
  {
  case _NILSXP: return R_NilValue;
  case _INTSXP: ENUMERATE_CASE(_INTSXP);
  case _DBLSXP: ENUMERATE_CASE(_DBLSXP);
  case _LGLSXP: ENUMERATE_CASE(_LGLSXP);
  case _STRSXP: ENUMERATE_CASE(_STRSXP);
  case _VECSXP: ENUMERATE_CASE(_VECSXP);
  case _ENVSXP: ENUMERATE_CASE(_ENVSXP);
  }

  Rf_error("unsupported object types '%s' and '%s'", Rf_type2char(TYPEOF(x)), Rf_type2char(TYPEOF(type)));
  return R_NilValue;
}

static SEXP recurse(SEXP object,
                    SEXP callback,
                    SEXP envir)
{
  SEXP symbol, expr, frame = R_NilValue;
  SEXP dots = Rf_findVarInFrame(envir, R_DotsSymbol);
  if (TYPEOF(callback) == CLOSXP && dots == R_MissingArg)
  {
    symbol = TAG(FORMALS(callback));
    expr = BODY(callback);
    SEXP call = PROTECT(Rf_lang3(Rf_install("new.env"), Rf_ScalarLogical(0), CLOENV(callback)));
    frame = PROTECT(Rf_eval(call, R_BaseNamespace));
    UNPROTECT(1);
  }

  const int size = 16384;
  SEXP queue[size];
  queue[0] = object;

  int index = 0;
  int slot = 1;

  while (index != slot)
  {
    object = queue[index++];
    index = index % size;

    if (object != R_MissingArg)
    {
      if (frame == R_NilValue)
      {
        Rf_defineVar(s_objectsym, object, envir);
        SEXP call = Rf_lang3(s_callbacksym, s_objectsym, R_DotsSymbol);
        R_forceAndCall(call, 1, envir);
      }
      else
      {
        Rf_defineVar(symbol, object, frame);
        Rf_eval(expr, frame);
      }
    }

    switch (TYPEOF(object))
    {
    case VECSXP:
    case EXPRSXP:
    {
      for (R_xlen_t i = 0, n = Rf_xlength(object); i < n; i++)
      {
        queue[slot++] = VECTOR_ELT(object, i);
        slot = slot % size;
      }
      break;
    }

    case LISTSXP:
    case LANGSXP:
    {
      while (object != R_NilValue)
      {
        queue[slot++] = CAR(object);
        slot = slot % size;
        object = CDR(object);
      }
      break;
    }
    }
  }

  UNPROTECT(frame != R_NilValue ? 1 : 0);
  return R_NilValue;
}

// Init ----

static const R_CallMethodDef callEntries[] = {
    {"renv_ffi__renv_call_expect",          (DL_FUNC) &renv_call_expect,          3},
    {"renv_ffi__renv_dependencies_recurse", (DL_FUNC) &renv_dependencies_recurse, 4},
    {"renv_ffi__enumerate",                 (DL_FUNC) &enumerate,                 3},
    {"renv_ffi__recurse",                   (DL_FUNC) &recurse,                   3},
    {NULL,                                  NULL,                                 0}
};

void R_init_renv(DllInfo* dllInfo)
{
  s_callbacksym = Rf_install("callback");
  s_objectsym = Rf_install("object");

  R_registerRoutines(dllInfo, NULL, callEntries, NULL, NULL);
  R_useDynamicSymbols(dllInfo, FALSE);
}
