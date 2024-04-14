// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// sfc_point_xy
List sfc_point_xy(const List points);
RcppExport SEXP _arcgisutils_sfc_point_xy(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_point_xy(points));
    return rcpp_result_gen;
END_RCPP
}
// sfc_point_xyz
List sfc_point_xyz(const List points);
RcppExport SEXP _arcgisutils_sfc_point_xyz(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_point_xyz(points));
    return rcpp_result_gen;
END_RCPP
}
// sfc_point_xyzm
List sfc_point_xyzm(const List points);
RcppExport SEXP _arcgisutils_sfc_point_xyzm(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_point_xyzm(points));
    return rcpp_result_gen;
END_RCPP
}
// sfc_point_impl
List sfc_point_impl(const List points);
RcppExport SEXP _arcgisutils_sfc_point_impl(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_point_impl(points));
    return rcpp_result_gen;
END_RCPP
}
// sfc_multipoint_impl
List sfc_multipoint_impl(const List mpoints);
RcppExport SEXP _arcgisutils_sfc_multipoint_impl(SEXP mpointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mpoints(mpointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_multipoint_impl(mpoints));
    return rcpp_result_gen;
END_RCPP
}
// sfc_linestring_impl
List sfc_linestring_impl(const List mpoints);
RcppExport SEXP _arcgisutils_sfc_linestring_impl(SEXP mpointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mpoints(mpointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_linestring_impl(mpoints));
    return rcpp_result_gen;
END_RCPP
}
// sfc_multilinestring_inner_impl
List sfc_multilinestring_inner_impl(List mpoints);
RcppExport SEXP _arcgisutils_sfc_multilinestring_inner_impl(SEXP mpointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mpoints(mpointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_multilinestring_inner_impl(mpoints));
    return rcpp_result_gen;
END_RCPP
}
// sfc_multilinestring_impl
List sfc_multilinestring_impl(const List mlines);
RcppExport SEXP _arcgisutils_sfc_multilinestring_impl(SEXP mlinesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mlines(mlinesSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_multilinestring_impl(mlines));
    return rcpp_result_gen;
END_RCPP
}
// sfg_polygon_impl
List sfg_polygon_impl(List mply);
RcppExport SEXP _arcgisutils_sfg_polygon_impl(SEXP mplySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mply(mplySEXP);
    rcpp_result_gen = Rcpp::wrap(sfg_polygon_impl(mply));
    return rcpp_result_gen;
END_RCPP
}
// sfc_polygon_impl
List sfc_polygon_impl(const List mply);
RcppExport SEXP _arcgisutils_sfc_polygon_impl(SEXP mplySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mply(mplySEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_polygon_impl(mply));
    return rcpp_result_gen;
END_RCPP
}
// sfg_multipolygon_inner_impl
List sfg_multipolygon_inner_impl(const List mply);
RcppExport SEXP _arcgisutils_sfg_multipolygon_inner_impl(SEXP mplySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mply(mplySEXP);
    rcpp_result_gen = Rcpp::wrap(sfg_multipolygon_inner_impl(mply));
    return rcpp_result_gen;
END_RCPP
}
// sfg_multipolygon_impl
List sfg_multipolygon_impl(List mply);
RcppExport SEXP _arcgisutils_sfg_multipolygon_impl(SEXP mplySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mply(mplySEXP);
    rcpp_result_gen = Rcpp::wrap(sfg_multipolygon_impl(mply));
    return rcpp_result_gen;
END_RCPP
}
// transpose_impl
SEXP transpose_impl(SEXP x, SEXP names_template);
RcppExport SEXP _arcgisutils_transpose_impl(SEXP xSEXP, SEXP names_templateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type names_template(names_templateSEXP);
    rcpp_result_gen = Rcpp::wrap(transpose_impl(x, names_template));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_arcgisutils_sfc_point_xy", (DL_FUNC) &_arcgisutils_sfc_point_xy, 1},
    {"_arcgisutils_sfc_point_xyz", (DL_FUNC) &_arcgisutils_sfc_point_xyz, 1},
    {"_arcgisutils_sfc_point_xyzm", (DL_FUNC) &_arcgisutils_sfc_point_xyzm, 1},
    {"_arcgisutils_sfc_point_impl", (DL_FUNC) &_arcgisutils_sfc_point_impl, 1},
    {"_arcgisutils_sfc_multipoint_impl", (DL_FUNC) &_arcgisutils_sfc_multipoint_impl, 1},
    {"_arcgisutils_sfc_linestring_impl", (DL_FUNC) &_arcgisutils_sfc_linestring_impl, 1},
    {"_arcgisutils_sfc_multilinestring_inner_impl", (DL_FUNC) &_arcgisutils_sfc_multilinestring_inner_impl, 1},
    {"_arcgisutils_sfc_multilinestring_impl", (DL_FUNC) &_arcgisutils_sfc_multilinestring_impl, 1},
    {"_arcgisutils_sfg_polygon_impl", (DL_FUNC) &_arcgisutils_sfg_polygon_impl, 1},
    {"_arcgisutils_sfc_polygon_impl", (DL_FUNC) &_arcgisutils_sfc_polygon_impl, 1},
    {"_arcgisutils_sfg_multipolygon_inner_impl", (DL_FUNC) &_arcgisutils_sfg_multipolygon_inner_impl, 1},
    {"_arcgisutils_sfg_multipolygon_impl", (DL_FUNC) &_arcgisutils_sfg_multipolygon_impl, 1},
    {"_arcgisutils_transpose_impl", (DL_FUNC) &_arcgisutils_transpose_impl, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_arcgisutils(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
