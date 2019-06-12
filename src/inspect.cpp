#include <Rcpp.h>
using namespace Rcpp;
#include <Rversion.h>

struct Expand {
  bool alrep;
  bool charsxp;
  bool env;
  bool call;
  bool bytecode;
};

class GrowableList {
  Rcpp::List data_;
  Rcpp::CharacterVector names_;
  R_xlen_t n_;

public:
  GrowableList(R_xlen_t size = 10) : data_(size), names_(size), n_(0) {
  }

  void push_back(const char* string, SEXP x) {
    if (Rf_xlength(data_) == n_) {
      data_ = Rf_xlengthgets(data_, n_ * 2);
      names_ = Rf_xlengthgets(names_, n_ * 2);
    }
    SET_STRING_ELT(names_, n_, Rf_mkChar(string));
    SET_VECTOR_ELT(data_, n_, x);
    n_++;
  }

  Rcpp::List vector() {
    if (Rf_xlength(data_) != n_) {
      data_ = Rf_xlengthgets(data_, n_);
      names_ = Rf_xlengthgets(names_, n_);
    }
    Rf_setAttrib(data_, R_NamesSymbol, names_);

    return data_;
  }
};

SEXP obj_children_(SEXP x, std::map<SEXP, int>& seen, Expand expand);
bool is_namespace(Environment env);

bool is_altrep(SEXP x) {
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
  return ALTREP(x);
#else
  return false;
#endif
}

SEXP obj_inspect_(SEXP x,
                 std::map<SEXP, int>& seen,
                 Expand& expand) {

  int id;
  SEXP children;
  bool has_seen;
  if (seen.count(x)) {
    has_seen = true;
    id = seen[x];
    children = PROTECT(Rf_allocVector(VECSXP, 0));
  } else {
    has_seen = false;
    id = seen.size() + 1;
    seen[x] = id;
    children = PROTECT(obj_children_(x, seen, expand));
  }

  // don't store object directly to avoid increasing refcount
  Rf_setAttrib(children, Rf_install("addr"), PROTECT(Rf_mkString(tfm::format("%p", x).c_str())));
  Rf_setAttrib(children, Rf_install("has_seen"), PROTECT(Rf_ScalarLogical(has_seen)));
  Rf_setAttrib(children, Rf_install("id"), PROTECT(Rf_ScalarInteger(id)));
  Rf_setAttrib(children, Rf_install("type"), PROTECT(Rf_ScalarInteger(TYPEOF(x))));
  Rf_setAttrib(children, Rf_install("length"), PROTECT(Rf_ScalarReal(Rf_length(x))));
  Rf_setAttrib(children, Rf_install("altrep"), PROTECT(Rf_ScalarLogical(is_altrep(x))));
  Rf_setAttrib(children, Rf_install("named"), PROTECT(Rf_ScalarInteger(NAMED(x))));
  Rf_setAttrib(children, Rf_install("object"), PROTECT(Rf_ScalarInteger(OBJECT(x))));
  UNPROTECT(8);

  if (Rf_isVector(x)) {
    if (TRUELENGTH(x) > 0) {
      Rf_setAttrib(children, Rf_install("truelength"), PROTECT(Rf_ScalarReal(TRUELENGTH(x))));
      UNPROTECT(1);
    }
  }

  const char* value = NULL;
  if (TYPEOF(x) == SYMSXP && PRINTNAME(x) != R_NilValue) {
    value = CHAR(PRINTNAME(x));
  } else if (TYPEOF(x) == ENVSXP) {
    if (x == R_GlobalEnv) {
      value = "global";
    } else if (x == R_EmptyEnv) {
      value = "empty";
    } else if (x == R_BaseEnv) {
      value = "base";
    } else {
      if (R_PackageEnvName(x) != R_NilValue)
        value = CHAR(STRING_ELT(R_PackageEnvName(x), 0));
    }
  }
  if (value != NULL) {
    Rf_setAttrib(children, Rf_install("value"), PROTECT(Rf_mkString(value)));
    UNPROTECT(1);
  }

  Rf_setAttrib(children, Rf_install("class"), PROTECT(Rf_mkString("lobstr_inspector")));
  UNPROTECT(1);

  UNPROTECT(1);
  return children;
}

inline void recurse(
                    GrowableList* children,
                    std::map<SEXP, int>& seen,
                    const char* name,
                    SEXP child,
                    Expand& expand) {

  SEXP descendents = PROTECT(obj_inspect_(child, seen, expand));
  children->push_back(name, descendents);
  UNPROTECT(1);
}

SEXP obj_children_(
                  SEXP x,
                  std::map<SEXP, int>& seen,
                  Expand expand) {

  GrowableList children;
  bool skip = false;

  // Handle ALTREP objects
  if (expand.alrep && is_altrep(x)) {
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
    SEXP klass = ALTREP_CLASS(x);
    SEXP classname = CAR(ATTRIB(klass));

    recurse(&children, seen, "_class", klass, expand);
    if (classname == Rf_install("deferred_string")) {
      // Deferred string ALTREP uses an pairlist, but stores data in the CDR
      SEXP data1 = R_altrep_data1(x);
      recurse(&children, seen, "_data1_car", CAR(data1), expand);
      recurse(&children, seen, "_data1_cdr", CDR(data1), expand);
    } else {
      recurse(&children, seen, "_data1", R_altrep_data1(x), expand);
    }
    recurse(&children, seen, "_data2", R_altrep_data2(x), expand);
#endif
  } else {
    switch (TYPEOF(x)) {
    // Non-recursive types
    case NILSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case RAWSXP:
    case CHARSXP:
    case SYMSXP:
      break;

    // Strings
    case STRSXP:
      if (expand.charsxp) {
        for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
          recurse(&children, seen, "", STRING_ELT(x, i), expand);
        }
      }
      break;

    // Recursive vectors
    case VECSXP:
    case EXPRSXP:
    case WEAKREFSXP: {
      SEXP names = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
      if (TYPEOF(names) == STRSXP) {
        for (R_xlen_t i = 0; i < XLENGTH(x); ++i) {
          recurse(&children, seen, CHAR(STRING_ELT(names, i)), VECTOR_ELT(x, i), expand);
        }
      } else {
        for (R_xlen_t i = 0; i < XLENGTH(x); ++i) {
          recurse(&children, seen, "", VECTOR_ELT(x, i), expand);
        }
      }
      UNPROTECT(1);
      break;
    }

    // Linked lists
    case LANGSXP:
      if (!expand.call) {
        skip = true;
        break;
      }
    case DOTSXP:
    case LISTSXP:
      if (x == R_MissingArg) // Needed for DOTSXP
        break;

      for(SEXP cons = x; cons != R_NilValue; cons = CDR(cons)) {
        SEXP tag = TAG(cons);
        if (TYPEOF(tag) == NILSXP) {
          recurse(&children, seen, "", CAR(cons), expand);
        } else if (TYPEOF(tag) == SYMSXP) {
          recurse(&children, seen, CHAR(PRINTNAME(tag)), CAR(cons), expand);
        } else {
          // TODO: add index? needs to be a list?
          recurse(&children, seen, "_tag", tag, expand);
          recurse(&children, seen, "_car", CAR(cons), expand);
        }
      }
      break;

    case BCODESXP:
      if (!expand.bytecode) {
        skip = true;
        break;
      }
      recurse(&children, seen, "_tag", TAG(x), expand);
      recurse(&children, seen, "_car", CAR(x), expand);
      recurse(&children, seen, "_cdr", CDR(x), expand);
      break;

    // Environments
    case ENVSXP:
      if (x == R_BaseEnv || x == R_GlobalEnv || x == R_EmptyEnv || is_namespace(x))
        break;

      if (expand.env) {
        recurse(&children, seen, "_frame", FRAME(x), expand);
        recurse(&children, seen, "_hashtab", HASHTAB(x), expand);
      } else {
        SEXP names = PROTECT(R_lsInternal(x, TRUE));
        for (R_xlen_t i = 0; i < XLENGTH(names); ++i) {
          const char* name = CHAR(STRING_ELT(names, i));
          SEXP obj = Rf_findVarInFrame(x, Rf_install(name));
          recurse(&children, seen, name, obj, expand);
        }
        UNPROTECT(1);
      }

      recurse(&children, seen, "_enclos", ENCLOS(x), expand);
      break;

    // Functions
    case CLOSXP:
      recurse(&children, seen, "_formals", FORMALS(x), expand);
      recurse(&children, seen, "_body", BODY(x), expand);
      recurse(&children, seen, "_env", CLOENV(x), expand);
      break;

    case PROMSXP:
      recurse(&children, seen, "_value", PRVALUE(x), expand);
      recurse(&children, seen, "_code", PRCODE(x), expand);
      recurse(&children, seen, "_env", PRENV(x), expand);
      break;

    case EXTPTRSXP:
      recurse(&children, seen, "_prot", EXTPTR_PROT(x), expand);
      recurse(&children, seen, "_tag", EXTPTR_TAG(x), expand);
      break;

    case S4SXP:
      recurse(&children, seen, "_tag", TAG(x), expand);
      break;

    default:
      stop("Don't know how to handle type %s", Rf_type2char(TYPEOF(x)));
    }
  }

  // CHARSXPs have fake attriibutes
  if (TYPEOF(x) != CHARSXP && !Rf_isNull(ATTRIB(x))) {
    recurse(&children, seen, "_attrib", ATTRIB(x), expand);
  }

  SEXP out = PROTECT(children.vector());
  if (skip) {
    Rf_setAttrib(out, Rf_install("skip"), PROTECT(Rf_ScalarLogical(skip)));
    UNPROTECT(1);
  }
  UNPROTECT(1);

  return out;
}


// [[Rcpp::export]]
Rcpp::List obj_inspect_(SEXP x,
                        bool expand_char = false,
                        bool expand_altrep = false,
                        bool expand_env = false,
                        bool expand_call = false,
                        bool expand_bytecode = false) {
  std::map<SEXP, int> seen;
  Expand expand = {expand_altrep, expand_char, expand_env, expand_call};

  return obj_inspect_(x, seen, expand);
}
