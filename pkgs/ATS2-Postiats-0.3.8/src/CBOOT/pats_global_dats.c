/*
**
** The C code is generated by ATS/Anairiats
** The compilation time is: 2017-11-12:  9h:12m
**
*/

/* include some .h files */
#ifndef _ATS_HEADER_NONE
#include "ats_config.h"
#include "ats_basics.h"
#include "ats_types.h"
#include "ats_exception.h"
#include "ats_memory.h"
#endif /* _ATS_HEADER_NONE */

/* include some .cats files */
#ifndef _ATS_PRELUDE_NONE
#include "prelude/CATS/basics.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/sizetype.cats"
#include "prelude/CATS/integer_ptr.cats"
#include "prelude/CATS/integer_fixed.cats"
#include "prelude/CATS/pointer.cats"
#include "prelude/CATS/bool.cats"
#include "prelude/CATS/char.cats"
#include "prelude/CATS/byte.cats"
#include "prelude/CATS/float.cats"
#include "prelude/CATS/string.cats"
#include "prelude/CATS/reference.cats"
#include "prelude/CATS/lazy.cats"
#include "prelude/CATS/lazy_vt.cats"
#include "prelude/CATS/printf.cats"
#include "prelude/CATS/list.cats"
#include "prelude/CATS/option.cats"
#include "prelude/CATS/array.cats"
#include "prelude/CATS/matrix.cats"
#endif /* _ATS_PRELUDE_NONE */
/* prologues from statically loaded files */
/* external codes at top */
/* type definitions */
typedef
struct {
ats_ptr_type atslab_3 ;
ats_ptr_type atslab_4 ;
} anairiats_rec_0 ;

typedef struct {
ats_ptr_type atslab_0 ;
ats_ptr_type atslab_1 ;
} anairiats_sum_1 ;

/* external typedefs */
/* external dynamic constructor declarations */
ATSextern_val(ats_sum_type, ATS_2d0_2e2_2e12_2prelude_2basics_sta_2esats__list_cons_0) ;
ATSextern_val(ats_sum_type, ATS_2d0_2e2_2e12_2prelude_2basics_sta_2esats__list_nil_1) ;

/* external dynamic constant declarations */
ATSextern_fun(ats_void_type, atspre_vbox_make_view_ptr) (ats_ptr_type) ;
ATSextern_fun(ats_bool_type, atspre_lte_int_int) (ats_int_type, ats_int_type) ;
ATSextern_fun(ats_int_type, atspre_isucc) (ats_int_type) ;
ATSextern_fun(ats_int_type, atspre_iadd) (ats_int_type, ats_int_type) ;
ATSextern_fun(ats_int_type, atspre_isub) (ats_int_type, ats_int_type) ;
ATSextern_fun(ats_int_type, atspre_idiv) (ats_int_type, ats_int_type) ;
ATSextern_fun(ats_bool_type, atspre_ilt) (ats_int_type, ats_int_type) ;
ATSextern_fun(ats_bool_type, atspre_igt) (ats_int_type, ats_int_type) ;
ATSextern_fun(ats_bool_type, atspre_igte) (ats_int_type, ats_int_type) ;
ATSextern_fun(ats_size_type, atspre_size1_of_int1) (ats_int_type) ;
ATSextern_fun(ats_size_type, atspre_add_size1_int1) (ats_size_type, ats_int_type) ;
ATSextern_fun(ats_size_type, atspre_sub_size1_int1) (ats_size_type, ats_int_type) ;
ATSextern_fun(ats_size_type, atspre_mul2_size1_size1) (ats_size_type, ats_size_type) ;
ATSextern_fun(ats_bool_type, atspre_lt_size1_size1) (ats_size_type, ats_size_type) ;
ATSextern_fun(ats_bool_type, atspre_gt_size1_int1) (ats_size_type, ats_int_type) ;
ATSextern_fun(ats_bool_type, atspre_neq_size1_size1) (ats_size_type, ats_size_type) ;
ATSextern_fun(ats_ptr_type, atspre_ptr_alloc_tsz) (ats_size_type) ;
ATSextern_fun(ats_void_type, atspre_ptr_zero_tsz) (ats_ref_type, ats_size_type) ;
ATSextern_fun(ats_ptr_type, atspre_ref_make_elt_tsz) (ats_ref_type, ats_size_type) ;
ATSextern_val(ats_ptr_type, atspre_stropt_none) ;
ATSextern_fun(ats_ptr_type, ListSubscriptException_make) () ;
ATSextern_fun(ats_ptr_type, atspre_array_ptr_alloc_tsz) (ats_size_type, ats_size_type) ;
ATSextern_fun(ats_void_type, atspre_array_ptr_free) (ats_ptr_type) ;
ATSextern_fun(ats_void_type, atspre_array_ptr_initialize_funenv_tsz) (ats_ref_type, ats_size_type, ats_ptr_type, ats_size_type, ats_ptr_type) ;
ATSextern_fun(ats_void_type, atspre_array_ptr_initialize_cloenv_tsz) (ats_ref_type, ats_size_type, ats_ref_type, ats_size_type, ats_ptr_type) ;
ATSextern_fun(ats_ptr_type, atspre_array_ptr_split_tsz) (ats_ptr_type, ats_size_type, ats_size_type) ;
ATSextern_fun(ats_ptr_type, atspre_array_ptr_takeout_tsz) (ats_ptr_type, ats_size_type, ats_size_type) ;
ATSextern_fun(anairiats_rec_0, atspre_array_ptr_takeout2_tsz) (ats_ptr_type, ats_size_type, ats_size_type, ats_size_type) ;
ATSextern_fun(ats_void_type, atspre_array_ptr_foreach_funenv_tsz) (ats_ref_type, ats_ptr_type, ats_size_type, ats_size_type, ats_ptr_type) ;
ATSextern_fun(ats_void_type, atspre_array_ptr_iforeach_funenv_tsz) (ats_ref_type, ats_ptr_type, ats_size_type, ats_size_type, ats_ptr_type) ;
ATSextern_fun(ats_ptr_type, atspre_array2_ptr_takeout_tsz) (ats_ptr_type, ats_size_type, ats_size_type, ats_size_type) ;
ATSextern_fun(ats_void_type, atslib_qsort) (ats_ref_type, ats_size_type, ats_size_type, ats_ptr_type) ;
ATSextern_fun(ats_void_type, _2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set) (ats_ptr_type) ;

/* external dynamic terminating constant declarations */
#ifdef _ATS_PROOFCHECK
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2SATS_2list_2esats__list_length_is_nonnegative_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2SATS_2list_vt_2esats__list_vt_length_is_nonnegative_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2SATS_2array_2esats__array_v_takeout2_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2array_2edats____copy_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2array_2edats____free_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2array_2edats____assert_prfck () ;
extern
ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2array_2edats____assert_prfck () ;
#endif /* _ATS_PROOFCHECK */

/* assuming abstract types */
/* sum constructor declarations */
/* exn constructor declarations */
/* global dynamic (non-functional) constant declarations */
/* internal function declarations */
static
ats_ptr_type ref_01088_ats_ptr_type (ats_ptr_type arg0) ;
static
ats_ptr_type ref_01088_ats_int_type (ats_int_type arg0) ;

/* partial value template declarations */
/* static temporary variable declarations */
ATSstatic (ats_ptr_type, statmp0) ;
ATSstatic (ats_ptr_type, statmp7) ;
ATSstatic (ats_ptr_type, statmp10) ;
ATSstatic (ats_ptr_type, statmp16) ;
ATSstatic (ats_ptr_type, statmp19) ;
ATSstatic (ats_ptr_type, statmp23) ;
ATSstatic (ats_ptr_type, statmp26) ;
ATSstatic (ats_ptr_type, statmp30) ;
ATSstatic (ats_ptr_type, statmp31) ;
ATSstatic (ats_ptr_type, statmp36) ;
ATSstatic (ats_ptr_type, statmp39) ;
ATSstatic (ats_ptr_type, statmp42) ;
ATSstatic (ats_ptr_type, statmp45) ;

/* external value variable declarations */

/* function implementations */

/*
// /home/hwxi/Research/ATS-Anairiats/prelude/DATS/reference.dats: 1828(line=57, offs=18) -- 1902(line=59, offs=4)
*/
ATSstaticdec()
ats_ptr_type
ref_01088_ats_ptr_type (ats_ptr_type arg0) {
/* local vardec */
ATSlocal (ats_ptr_type, tmp1) ;
ATSlocal (ats_ptr_type, tmp2) ;

__ats_lab_ref_01088_ats_ptr_type:
/* ats_ptr_type tmp2 ; */
tmp2 = arg0 ;
tmp1 = atspre_ref_make_elt_tsz ((&tmp2), sizeof(ats_ptr_type)) ;
return (tmp1) ;
} /* end of [ref_01088_ats_ptr_type] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 1781(line=63, offs=17) -- 1799(line=63, offs=35)
*/
ATSglobaldec()
ats_ptr_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_get () {
/* local vardec */
ATSlocal (ats_ptr_type, tmp3) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_get:
tmp3 = ats_ptrget_mac(ats_ptr_type, statmp0) ;
return (tmp3) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 1827(line=65, offs=18) -- 1855(line=65, offs=46)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set (ats_ptr_type arg0) {
/* local vardec */
// ATSlocal_void (tmp4) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set:
ats_ptrget_mac(ats_ptr_type, statmp0) = arg0 ;
return /* (tmp4) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 1914(line=71, offs=3) -- 1952(line=71, offs=41)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set_name (ats_ptr_type arg0) {
/* local vardec */
// ATSlocal_void (tmp5) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set_name:
/* tmp5 = */ _2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set (ats_castfn_mac(ats_ptr_type, arg0)) ;
return /* (tmp5) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set_name] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 2018(line=75, offs=3) -- 2059(line=75, offs=44)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set_none () {
/* local vardec */
// ATSlocal_void (tmp6) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set_none:
/* tmp6 = */ _2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set (atspre_stropt_none) ;
return /* (tmp6) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_PACKNAME_set_none] */

/*
// /home/hwxi/Research/ATS-Anairiats/prelude/DATS/reference.dats: 1828(line=57, offs=18) -- 1902(line=59, offs=4)
*/
ATSstaticdec()
ats_ptr_type
ref_01088_ats_int_type (ats_int_type arg0) {
/* local vardec */
ATSlocal (ats_ptr_type, tmp8) ;
ATSlocal (ats_int_type, tmp9) ;

__ats_lab_ref_01088_ats_int_type:
/* ats_int_type tmp9 ; */
tmp9 = arg0 ;
tmp8 = atspre_ref_make_elt_tsz ((&tmp9), sizeof(ats_int_type)) ;
return (tmp8) ;
} /* end of [ref_01088_ats_int_type] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 2249(line=88, offs=17) -- 2267(line=88, offs=35)
*/
ATSglobaldec()
ats_int_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_get () {
/* local vardec */
ATSlocal (ats_int_type, tmp11) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_get:
tmp11 = ats_ptrget_mac(ats_int_type, statmp7) ;
return (tmp11) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 2294(line=90, offs=17) -- 2324(line=90, offs=47)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_set (ats_int_type arg0) {
/* local vardec */
// ATSlocal_void (tmp12) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_set:
ats_ptrget_mac(ats_int_type, statmp7) = arg0 ;
return /* (tmp12) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_set] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 2359(line=93, offs=22) -- 2455(line=96, offs=4)
*/
ATSglobaldec()
ats_ptr_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_get_decl () {
/* local vardec */
ATSlocal (ats_ptr_type, tmp13) ;
ATSlocal (ats_ptr_type, tmp14) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_get_decl:
tmp14 = ats_ptrget_mac(ats_ptr_type, statmp10) ;
ats_ptrget_mac(ats_ptr_type, statmp10) = atspre_null_ptr ;
tmp13 = tmp14 ;
return (tmp13) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_get_decl] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 2524(line=99, offs=22) -- 2557(line=99, offs=55)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_set_decl (ats_ptr_type arg0) {
/* local vardec */
// ATSlocal_void (tmp15) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_set_decl:
ats_ptrget_mac(ats_ptr_type, statmp10) = arg0 ;
return /* (tmp15) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_ATSRELOC_set_decl] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 2967(line=127, offs=20) -- 2988(line=127, offs=41)
*/
ATSglobaldec()
ats_int_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADFLAG_get () {
/* local vardec */
ATSlocal (ats_int_type, tmp17) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADFLAG_get:
tmp17 = ats_ptrget_mac(ats_int_type, statmp16) ;
return (tmp17) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADFLAG_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 3018(line=129, offs=20) -- 3051(line=129, offs=53)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADFLAG_set (ats_int_type arg0) {
/* local vardec */
// ATSlocal_void (tmp18) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADFLAG_set:
ats_ptrget_mac(ats_int_type, statmp16) = arg0 ;
return /* (tmp18) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADFLAG_set] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 3212(line=145, offs=3) -- 3233(line=145, offs=24)
*/
ATSglobaldec()
ats_ptr_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADNAME_get () {
/* local vardec */
ATSlocal (ats_ptr_type, tmp20) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADNAME_get:
tmp20 = ats_ptrget_mac(ats_ptr_type, statmp19) ;
return (tmp20) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADNAME_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 3271(line=148, offs=3) -- 3307(line=148, offs=39)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADNAME_set_none () {
/* local vardec */
// ATSlocal_void (tmp21) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADNAME_set_none:
ats_ptrget_mac(ats_ptr_type, statmp19) = atspre_stropt_none ;
return /* (tmp21) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADNAME_set_none] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 3353(line=151, offs=3) -- 3398(line=151, offs=48)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADNAME_set_name (ats_ptr_type arg0) {
/* local vardec */
// ATSlocal_void (tmp22) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADNAME_set_name:
ats_ptrget_mac(ats_ptr_type, statmp19) = ats_castfn_mac(ats_ptr_type, arg0) ;
return /* (tmp22) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DYNLOADNAME_set_name] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 3540(line=164, offs=20) -- 3561(line=164, offs=41)
*/
ATSglobaldec()
ats_int_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_MAINATSFLAG_get () {
/* local vardec */
ATSlocal (ats_int_type, tmp24) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_MAINATSFLAG_get:
tmp24 = ats_ptrget_mac(ats_int_type, statmp23) ;
return (tmp24) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_MAINATSFLAG_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 3591(line=166, offs=20) -- 3624(line=166, offs=53)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_MAINATSFLAG_set (ats_int_type arg0) {
/* local vardec */
// ATSlocal_void (tmp25) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_MAINATSFLAG_set:
ats_ptrget_mac(ats_int_type, statmp23) = arg0 ;
return /* (tmp25) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_MAINATSFLAG_set] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 3791(line=182, offs=3) -- 3814(line=182, offs=26)
*/
ATSglobaldec()
ats_ptr_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_STATIC_PREFIX_get () {
/* local vardec */
ATSlocal (ats_ptr_type, tmp27) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_STATIC_PREFIX_get:
tmp27 = ats_ptrget_mac(ats_ptr_type, statmp26) ;
return (tmp27) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_STATIC_PREFIX_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 3854(line=185, offs=3) -- 3892(line=185, offs=41)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_STATIC_PREFIX_set_none () {
/* local vardec */
// ATSlocal_void (tmp28) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_STATIC_PREFIX_set_none:
ats_ptrget_mac(ats_ptr_type, statmp26) = atspre_stropt_none ;
return /* (tmp28) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_STATIC_PREFIX_set_none] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 3932(line=188, offs=3) -- 3973(line=188, offs=44)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_STATIC_PREFIX_set_name (ats_ptr_type arg0) {
/* local vardec */
// ATSlocal_void (tmp29) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_STATIC_PREFIX_set_name:
ats_ptrget_mac(ats_ptr_type, statmp26) = ats_castfn_mac(ats_ptr_type, arg0) ;
return /* (tmp29) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_STATIC_PREFIX_set_name] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 4178(line=207, offs=3) -- 4199(line=207, offs=24)
*/
ATSglobaldec()
ats_ptr_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_IATS_dirlst_get () {
/* local vardec */
ATSlocal (ats_ptr_type, tmp32) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_IATS_dirlst_get:
tmp32 = ats_ptrget_mac(ats_ptr_type, statmp30) ;
return (tmp32) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_IATS_dirlst_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 4237(line=211, offs=3) -- 4328(line=215, offs=4)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_IATS_dirlst_ppush (ats_ptr_type arg0) {
/* local vardec */
// ATSlocal_void (tmp33) ;
ATSlocal (ats_ptr_type, tmp34) ;
ATSlocal (ats_ptr_type, tmp35) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_IATS_dirlst_ppush:
tmp34 = ats_ptrget_mac(ats_ptr_type, statmp30) ;
tmp35 = ATS_MALLOC(sizeof(anairiats_sum_1)) ;
ats_selptrset_mac(anairiats_sum_1, tmp35, atslab_0, arg0) ;
ats_selptrset_mac(anairiats_sum_1, tmp35, atslab_1, tmp34) ;
ats_ptrget_mac(ats_ptr_type, statmp30) = tmp35 ;
return /* (tmp33) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_IATS_dirlst_ppush] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 4969(line=258, offs=25) -- 4983(line=258, offs=39)
*/
ATSglobaldec()
ats_int_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgflag_get () {
/* local vardec */
ATSlocal (ats_int_type, tmp37) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgflag_get:
tmp37 = ats_ptrget_mac(ats_int_type, statmp36) ;
return (tmp37) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgflag_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 5018(line=260, offs=25) -- 5044(line=260, offs=51)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgflag_set (ats_int_type arg0) {
/* local vardec */
// ATSlocal_void (tmp38) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgflag_set:
ats_ptrget_mac(ats_int_type, statmp36) = arg0 ;
return /* (tmp38) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgflag_set] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 5187(line=273, offs=25) -- 5201(line=273, offs=39)
*/
ATSglobaldec()
ats_int_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgline_get () {
/* local vardec */
ATSlocal (ats_int_type, tmp40) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgline_get:
tmp40 = ats_ptrget_mac(ats_int_type, statmp39) ;
return (tmp40) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgline_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 5236(line=275, offs=25) -- 5262(line=275, offs=51)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgline_set (ats_int_type arg0) {
/* local vardec */
// ATSlocal_void (tmp41) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgline_set:
ats_ptrget_mac(ats_int_type, statmp39) = arg0 ;
return /* (tmp41) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_DEBUGATS_dbgline_set] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 5391(line=288, offs=26) -- 5406(line=288, offs=41)
*/
ATSglobaldec()
ats_int_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPATS_tlcalopt_get () {
/* local vardec */
ATSlocal (ats_int_type, tmp43) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPATS_tlcalopt_get:
tmp43 = ats_ptrget_mac(ats_int_type, statmp42) ;
return (tmp43) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPATS_tlcalopt_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 5442(line=290, offs=26) -- 5469(line=290, offs=53)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPATS_tlcalopt_set (ats_int_type arg0) {
/* local vardec */
// ATSlocal_void (tmp44) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPATS_tlcalopt_set:
ats_ptrget_mac(ats_int_type, statmp42) = arg0 ;
return /* (tmp44) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPATS_tlcalopt_set] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 5648(line=304, offs=32) -- 5669(line=304, offs=53)
*/
ATSglobaldec()
ats_int_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPENV_maxtmprecdepth_get () {
/* local vardec */
ATSlocal (ats_int_type, tmp46) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPENV_maxtmprecdepth_get:
tmp46 = ats_ptrget_mac(ats_int_type, statmp45) ;
return (tmp46) ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPENV_maxtmprecdepth_get] */

/*
// /home/hwxi/Research/ATS-Postiats/src/pats_global.dats: 5711(line=306, offs=32) -- 5742(line=306, offs=63)
*/
ATSglobaldec()
ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPENV_maxtmprecdepth_set (ats_int_type arg0) {
/* local vardec */
// ATSlocal_void (tmp47) ;

__ats_lab__2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPENV_maxtmprecdepth_set:
ats_ptrget_mac(ats_int_type, statmp45) = arg0 ;
return /* (tmp47) */ ;
} /* end of [_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__the_CCOMPENV_maxtmprecdepth_set] */

/* static load function */

// extern ats_void_type _2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_atspre_2edats__staload (void) ;
extern ats_void_type _2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__staload (void) ;
// extern ats_void_type ATS_2d0_2e2_2e12_2prelude_2SATS_2unsafe_2esats__staload (void) ;
extern ats_void_type ATS_2d0_2e2_2e12_2prelude_2DATS_2unsafe_2edats__staload (void) ;

ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2edats__staload () {
static int _2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2edats__staload_flag = 0 ;
if (_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2edats__staload_flag) return ;
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2edats__staload_flag = 1 ;

// _2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_atspre_2edats__staload () ;
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2esats__staload () ;
// ATS_2d0_2e2_2e12_2prelude_2SATS_2unsafe_2esats__staload () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2unsafe_2edats__staload () ;

return ;
} /* staload function */

/* dynamic load function */

// dynload flag declaration
extern ats_int_type _2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2edats__dynload_flag ;

ats_void_type
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2edats__dynload () {
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2edats__dynload_flag = 1 ;
_2home_2hwxi_2Research_2ATS_2dPostiats_2src_2pats_global_2edats__staload () ;

#ifdef _ATS_PROOFCHECK
ATS_2d0_2e2_2e12_2prelude_2SATS_2list_2esats__list_length_is_nonnegative_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2SATS_2list_vt_2esats__list_vt_length_is_nonnegative_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2SATS_2array_2esats__array_v_takeout2_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2list_vt_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2array_2edats____copy_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2array_2edats____free_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2array_2edats____assert_prfck () ;
ATS_2d0_2e2_2e12_2prelude_2DATS_2array_2edats____assert_prfck () ;
#endif /* _ATS_PROOFCHECK */

/* marking static variables for GC */
ATS_GC_MARKROOT(&statmp0, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp7, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp10, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp16, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp19, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp23, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp26, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp30, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp31, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp36, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp39, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp42, sizeof(ats_ptr_type)) ;
ATS_GC_MARKROOT(&statmp45, sizeof(ats_ptr_type)) ;

/* marking external values for GC */

/* code for dynamic loading */
statmp0 = ref_01088_ats_ptr_type (atspre_stropt_none) ;
statmp7 = ref_01088_ats_int_type (0) ;
statmp10 = ref_01088_ats_ptr_type (atspre_null_ptr) ;
statmp16 = ref_01088_ats_int_type (0) ;
statmp19 = ref_01088_ats_ptr_type (atspre_stropt_none) ;
statmp23 = ref_01088_ats_int_type (0) ;
statmp26 = ref_01088_ats_ptr_type (atspre_stropt_none) ;
statmp31 = (ats_sum_ptr_type)0 ;
statmp30 = ref_01088_ats_ptr_type (statmp31) ;
statmp36 = ref_01088_ats_int_type (0) ;
statmp39 = ref_01088_ats_int_type (0) ;
statmp42 = ref_01088_ats_int_type (1) ;
statmp45 = ref_01088_ats_int_type (100) ;
return ;
} /* end of [dynload function] */

/* external codes at mid */
/* external codes at bot */

/* ****** ****** */

/* end of [pats_global_dats.c] */
