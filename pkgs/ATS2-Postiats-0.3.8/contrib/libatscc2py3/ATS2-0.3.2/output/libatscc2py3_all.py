
######
#
# Time of Generation:
# Mon Oct 23 15:54:44 EDT 2017
#
######

######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
#beg of [basics_cats.py]
######

######
import sys
######

############################################
#
def ATSCKiseqz(x): return (x == 0)
def ATSCKisneqz(x): return (x != 0)
#
def ATSCKptrisnull(xs): return (xs == None)
def ATSCKptriscons(xs): return (xs != None)
#
def ATSCKpat_int(tmp, given): return (tmp == given)
def ATSCKpat_bool(tmp, given): return (tmp == given)
def ATSCKpat_char(tmp, given): return (tmp == given)
def ATSCKpat_float(tmp, given): return (tmp == given)
#
def ATSCKpat_con0 (con, tag): return (con == tag)
def ATSCKpat_con1 (con, tag): return (con[0] == tag)
#
############################################
#
def ats2pypre_list_nil(): return None
def ats2pypre_list_cons(x, xs): return (x, xs)
#
############################################
#
def ATSINScaseof_fail(em):
  print("ATSINScaseof_fail:", em, file=sys.__stderr__); sys.exit(1)
  return
#
def ATSINSdeadcode_fail():
  print("ATSINSdeadcode_fail(", ")", file=sys.__stderr__); sys.exit(1)
  return
#
############################################

def ATSPMVempty(): return

############################################

def ATSPMVlazyval_eval(lazyval):
  flag = lazyval[0]
  if (flag==0):
    lazyval[0] = 1
    mythunk = lazyval[1]
    lazyval[1] = mythunk[0](mythunk)
  else:
    lazyval[0] = flag + 1
  #endif
  return lazyval[1]
#end-of-[ATSPMVlazyval_eval]

############################################
#
def ATSPMVllazyval_eval(llazyval):
  return llazyval[0](llazyval, True)
def atspre_lazy_vt_free(llazyval):
  return llazyval[0](llazyval, False)
#
############################################

def ats2pypre_tostring(x): return str(x)
def ats2pypre_toString(x): return str(x)

############################################

def ats2pypre_lazy2cloref(lazyval): return lazyval[1]

############################################
#
def ats2pypre_exit(ecode):
  sys.exit(ecode); return
#
def ats2pypre_exit_errmsg(ecode, errmsg):
  print(errmsg, file=sys.__stderr__); sys.exit(1); return
#
############################################
#
def ats2pypre_assert_bool0(tfv):
  if not(tfv): sys.exit(1)
  return
def ats2pypre_assert_bool1(tfv):
  if not(tfv): sys.exit(1)
  return
#
def ats2pypre_assert_errmsg_bool0(tfv, errmsg):
  if not(tfv):
    print(errmsg, file=sys.__stderr__); sys.exit(1)
  return
def ats2pypre_assert_errmsg_bool1(tfv, errmsg):
  if not(tfv):
    print(errmsg, file=sys.__stderr__); sys.exit(1)
  return
#
############################################
#
def ats2pypre_cloref0_app(cf): return cf[0](cf)
def ats2pypre_cloref1_app(cf, x): return cf[0](cf, x)
def ats2pypre_cloref2_app(cf, x1, x2): return cf[0](cf, x1, x2)
def ats2pypre_cloref3_app(cf, x1, x2, x3): return cf[0](cf, x1, x2, x3)
#
############################################
#
def ats2pypre_cloref2fun0(cf):
  return lambda: ats2pypre_cloref0_app(cf)
def ats2pypre_cloref2fun1(cf):
  return lambda x: ats2pypre_cloref1_app(cf, x)
def ats2pypre_cloref2fun2(cf):
  return lambda x1, x2: ats2pypre_cloref2_app(cf, x1, x2)
def ats2pypre_cloref2fun3(cf):
  return lambda x1, x2, x3: ats2pypre_cloref3_app(cf, x1, x2, x3)
#
############################################

###### end of [basics_cats.py] ######
######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
#beg of [integer_cats.py]
######

############################################
#
def ats2pypre_abs_int0(x): return abs(x)
#
############################################
#
def ats2pypre_neg_int0(x): return ( -x )
def ats2pypre_neg_int1(x): return ( -x )
#
############################################
#
def ats2pypre_succ_int0(x): return (x + 1)
def ats2pypre_succ_int1(x): return (x + 1)
#
def ats2pypre_pred_int0(x): return (x - 1)
def ats2pypre_pred_int1(x): return (x - 1)
#
############################################
#
def ats2pypre_half_int0(x): return (x // 2)
def ats2pypre_half_int1(x): return (x // 2)
#
############################################
#
def ats2pypre_add_int0_int0(x, y): return (x + y)
def ats2pypre_add_int1_int1(x, y): return (x + y)
#
def ats2pypre_sub_int0_int0(x, y): return (x - y)
def ats2pypre_sub_int1_int1(x, y): return (x - y)
#
def ats2pypre_mul_int0_int0(x, y): return (x * y)
def ats2pypre_mul_int1_int1(x, y): return (x * y)
#
def ats2pypre_div_int0_int0(x, y): return (x // y)
def ats2pypre_div_int1_int1(x, y): return (x // y)
#
def ats2pypre_mod_int0_int0(x, y): return (x % y)
def ats2pypre_mod_int1_int1(x, y): return (x % y)
def ats2pypre_nmod_int1_int1(x, y): return (x % y)
#
############################################
#
def ats2pypre_lt_int0_int0(x, y): return (x < y)
def ats2pypre_lt_int1_int1(x, y): return (x < y)
#
def ats2pypre_lte_int0_int0(x, y): return (x <= y)
def ats2pypre_lte_int1_int1(x, y): return (x <= y)
#
def ats2pypre_gt_int0_int0(x, y): return (x > y)
def ats2pypre_gt_int1_int1(x, y): return (x > y)
#
def ats2pypre_gte_int0_int0(x, y): return (x >= y)
def ats2pypre_gte_int1_int1(x, y): return (x >= y)
#
def ats2pypre_eq_int0_int0(x, y): return (x == y)
def ats2pypre_eq_int1_int1(x, y): return (x == y)
#
def ats2pypre_neq_int0_int0(x, y): return (x != y)
def ats2pypre_neq_int1_int1(x, y): return (x != y)
#
############################################
#
def ats2pypre_compare_int0_int0(x, y):
  return -1 if (x < y) else (1 if (x > y) else 0)
#
############################################
#
def ats2pypre_max_int0_int0(x, y): return (max(x, y))
def ats2pypre_max_int1_int1(x, y): return (max(x, y))
#
def ats2pypre_min_int0_int0(x, y): return (min(x, y))
def ats2pypre_min_int1_int1(x, y): return (min(x, y))
#
############################################
#
# HX-2016-06
# The code is in print_cats.py:
#
# def ats2pypre_print_int(i):
#   return ats2pypre_fprint_int(sys.__stdout__, i)
# def ats2pypre_prerr_int(i):
#   return ats2pypre_fprint_int(sys.__stderr__, i)
# def ats2pypre_fprint_int(out, i): return ats2pypre_fprint_obj(out, i)
#
############################################

###### end of [integer_cats.py] ######
######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
#beg of [bool_cats.py]
######

############################################
#
def ats2pypre_neg_bool0(x): return(not(x))
def ats2pypre_neg_bool1(x): return(not(x))
#
############################################

def ats2pypre_add_bool0_bool0(x, y): return(x or y)
def ats2pypre_add_bool0_bool1(x, y): return(x or y)
def ats2pypre_add_bool1_bool0(x, y): return(x or y)
def ats2pypre_add_bool1_bool1(x, y): return(x or y)

############################################

def ats2pypre_mul_bool0_bool0(x, y): return(x and y)
def ats2pypre_mul_bool0_bool1(x, y): return(x and y)
def ats2pypre_mul_bool1_bool0(x, y): return(x and y)
def ats2pypre_mul_bool1_bool1(x, y): return(x and y)

############################################
#
def ats2pypre_eq_bool0_bool0(x, y): return(x == y)
def ats2pypre_eq_bool1_bool1(x, y): return(x == y)
#
def ats2pypre_neq_bool0_bool0(x, y): return(x != y)
def ats2pypre_neq_bool1_bool1(x, y): return(x != y)
#
############################################

def ats2pypre_bool2int0(x): return(1 if x else 0)
def ats2pypre_bool2int1(x): return(1 if x else 0) 

############################################

def ats2pypre_int2bool0(x): return(True if x != 0 else False)
def ats2pypre_int2bool1(x): return(True if x != 0 else False)

############################################

###### end of [bool_cats.py] ######
######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
#beg of [char_cats.py]
######

############################################

###### end of [char_cats.py] ######
######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
#beg of [float_cats.py]
######

############################################
#
def ats2pypre_double2int(x): return int(x)
def ats2pypre_int_of_double(x): return int(x)
#
def ats2pypre_int2double(x): return float(x)
def ats2pypre_double_of_int(x): return float(x)
#
############################################
#
def ats2pypre_abs_double(x): return abs(x)
def ats2pypre_neg_double(x): return ( -x )
#
def ats2pypre_succ_double(x): return (x + 1)
def ats2pypre_pred_double(x): return (x + 1)
#
############################################
#
def ats2pypre_add_int_double(x, y): return (x + y)
def ats2pypre_sub_int_double(x, y): return (x - y)
def ats2pypre_mul_int_double(x, y): return (x * y)
def ats2pypre_div_int_double(x, y): return (x / y)
#
def ats2pypre_add_double_int(x, y): return (x + y)
def ats2pypre_sub_double_int(x, y): return (x - y)
def ats2pypre_mul_double_int(x, y): return (x * y)
def ats2pypre_div_double_int(x, y): return (x / y)
#
def ats2pypre_add_double_double(x, y): return (x + y)
def ats2pypre_sub_double_double(x, y): return (x - y)
def ats2pypre_mul_double_double(x, y): return (x * y)
def ats2pypre_div_double_double(x, y): return (x / y)
#
############################################
#
def ats2pypre_lt_int_double(x, y): return (x < y)
def ats2pypre_lte_int_double(x, y): return (x <= y)
def ats2pypre_gt_int_double(x, y): return (x > y)
def ats2pypre_gte_int_double(x, y): return (x >= y)
#
def ats2pypre_lt_double_int(x, y): return (x < y)
def ats2pypre_lte_double_int(x, y): return (x <= y)
def ats2pypre_gt_double_int(x, y): return (x > y)
def ats2pypre_gte_double_int(x, y): return (x >= y)
#
############################################
#
def ats2pypre_lt_double_double(x, y): return (x < y)
def ats2pypre_lte_double_double(x, y): return (x <= y)
def ats2pypre_gt_double_double(x, y): return (x > y)
def ats2pypre_gte_double_double(x, y): return (x >= y)
#
def ats2pypre_eq_double_double(x, y): return (x == y)
def ats2pypre_neq_double_double(x, y): return (x != y)
#
def ats2pypre_compare_double_double(x, y): return cmp(x, y)
#
############################################

###### end of [float_cats.py] ######
######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
#beg of [string_cats.py]
######

def ats2pypre_strchr_chr(x): return chr(x)
def ats2pypre_strchr_ord(x): return ord(x)

############################################

def ats2pypre_strlen(x): return (x.__len__())

############################################

def ats2pypre_string_length(x): return len(x)

############################################

def ats2pypre_string_get_at(x, i): return(x[i])

############################################

def ats2pypre_string_substring_beg_end(x, i, j): return(x[i:j])
def ats2pypre_string_substring_beg_len(x, i, n): return(x[i:i+n])

############################################
#
def ats2pypre_lt_string_string(x, y): return (x < y)
def ats2pypre_lte_string_string(x, y): return (x <= y)
#
def ats2pypre_gt_string_string(x, y): return (x > y)
def ats2pypre_gte_string_string(x, y): return (x >= y)
#
def ats2pypre_eq_string_string(x, y): return (x == y)
def ats2pypre_neq_string_string(x, y): return (x != y)
#
############################################
#
def ats2pypre_compare_string_string(x, y):
  return -1 if (x < y) else (1 if (x > y) else 0)
#
############################################

def ats2pypre_string_isalnum(x): return (x.isalnum())
def ats2pypre_string_isalpha(x): return (x.isalpha())
def ats2pypre_string_isdecimal(x): return (x.isdecimal())

############################################

def ats2pypre_string_lower(x): return (x.lower())
def ats2pypre_string_upper(x): return (x.upper())

############################################

def ats2pypre_string_append_2(x1, x2): return (x1+x2)
def ats2pypre_string_append_3(x1, x2, x3): return "".join((x1, x2, x3))
def ats2pypre_string_append_4(x1, x2, x3, x4): return "".join((x1, x2, x3, x4))

############################################

###### end of [string_cats.py] ######
######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
#beg of [print_cats.py]
######

############################################
#
def ats2pypre_print_int(i):
  return ats2pypre_fprint_int(sys.__stdout__, i)
def ats2pypre_prerr_int(i):
  return ats2pypre_fprint_int(sys.__stderr__, i)
#
def ats2pypre_fprint_int(out, i): return ats2pypre_fprint_obj(out, i)
#
############################################
#
def ats2pypre_print_bool(b):
  return ats2pypre_fprint_bool(sys.__stdout__, b)
def ats2pypre_prerr_bool(b):
  return ats2pypre_fprint_bool(sys.__stderr__, b)
#
def ats2pypre_fprint_bool(out, b): return ats2pypre_fprint_obj(out, b)
#
############################################
#
def ats2pypre_print_char(c):
  return ats2pypre_fprint_char(sys.__stdout__, c)
def ats2pypre_prerr_char(c):
  return ats2pypre_fprint_char(sys.__stderr__, c)
#
def ats2pypre_fprint_char(out, c): return ats2pypre_fprint_obj(out, c)
#
############################################
#
def ats2pypre_print_double(i):
  return ats2pypre_fprint_double(sys.__stdout__, i)
def ats2pypre_prerr_double(i):
  return ats2pypre_fprint_double(sys.__stderr__, i)
#
def ats2pypre_fprint_double(out, i): return ats2pypre_fprint_obj(out, i)
#
############################################
#
def ats2pypre_print_string(x):
  return ats2pypre_fprint_string(sys.__stdout__, x)
def ats2pypre_prerr_string(x):
  return ats2pypre_fprint_string(sys.__stderr__, x)
#
def ats2pypre_fprint_string(out, x): return ats2pypre_fprint_obj(out, x)
#
############################################
#
def ats2pypre_print_obj(x):
  out = sys.__stdout__
  ats2pypre_fprint_obj(out, x); return
def ats2pypre_println_obj(x):
  out = sys.__stdout__
  ats2pypre_fprintln_obj(out, x); return
#
def ats2pypre_prerr_obj(x):
  out = sys.__stderr__
  ats2pypre_fprint_obj(out, x); return
def ats2pypre_prerrln_obj(x):
  out = sys.__stderr__
  ats2pypre_fprintln_obj(out, x); return
#
def ats2pypre_fprint_obj(out, x):
  print(x, file=out, end=''); return
def ats2pypre_fprintln_obj(out, x):
  print(x, file=out, end='\n'); return
#
############################################
#
def ats2pypre_print_newline():
  out = sys.__stdout__
  ats2pypre_fprint_newline(out); return
def ats2pypre_prerr_newline():
  out = sys.__stderr__
  ats2pypre_fprint_newline(out); return
#
def ats2pypre_fprint_newline(out):
  print(file=out, end='\n'); sys.stdout.flush(); return
#
############################################

###### end of [print_cats.py] ######
######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
#beg of [filebas_cats.py]
######

############################################
#
ats2pypre_stdin = sys.__stdin__
ats2pypre_stdout = sys.__stdout__
ats2pypre_stderr = sys.__stderr__
#
############################################
#
def \
ats2pypre_fileref_open_exn(path, fm):
  return open(path, fm)
def \
ats2pypre_fileref_open_opt(path, fm):
  try:
    filr = open(path, fm)
    return ats2pypre_option_some(filr)
  except IOError:
    return ats2pypre_option_none()
#
def \
ats2pypre_fileref_close(filr): return filr.close()
#
def \
ats2pypre_fileref_get_file_string(filr): return filr.read(-1)
#
############################################

###### end of [filebas_cats.py] ######
######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
# beg of [PYlist_cats.py]
######

######
import functools
######

############################################

def ats2pypre_PYlist_nil(): return []
def ats2pypre_PYlist_sing(x): return [x]
def ats2pypre_PYlist_pair(x1, x2): return [x1, x2]

############################################

def ats2pypre_PYlist_cons(x0, xs): return xs.insert(0, x0)

############################################

def ats2pypre_PYlist_make_elt(n, x):
  res = []
  while (n > 0): n = n - 1; res.append(x)
  return res

############################################

def ats2pypre_PYlist_is_nil(xs): return not(xs)
def ats2pypre_PYlist_is_cons(xs): return True if xs else False
def ats2pypre_PYlist_isnot_nil(xs): return True if xs else False

############################################

def ats2pypre_PYlist_length(xs): return xs.__len__()

############################################

def ats2pypre_PYlist_get_at(xs, ind): return xs[ind]
def ats2pypre_PYlist_set_at(xs, ind, x): xs[ind] = x; return

############################################

def ats2pypre_PYlist_copy(xs):
  res = []
  for x in iter(xs): res.append(x)
  return res

############################################

def ats2pypre_PYlist_append(xs, x): xs.append(x); return
def ats2pypre_PYlist_extend(xs1, xs2): xs1.extend(xs2); return

############################################

def ats2pypre_PYlist_pop_0(xs): return xs.pop()
def ats2pypre_PYlist_pop_1(xs, i): return xs.pop(i)

############################################

def ats2pypre_PYlist_insert(xs, i, x): xs.insert(i, x); return

############################################

def ats2pypre_PYlist_map(xs, f): return list(map(f, xs))
def ats2pypre_PYlist_filter(xs, f): return list(filter(f, xs))

############################################

def ats2pypre_PYlist_string_join(xs): return ''.join(xs)

############################################

def \
ats2pypre_PYlist_reduce(xs, ini, f):
  res = ini
  for x in iter(xs): res = f(res, x)
  return res

############################################

def ats2pypre_PYlist2list_rev(xs):
  res = ats2pypre_list_nil()
  for x in iter(xs): res = ats2pypre_list_cons(x, res)
  return res

############################################
#
def ats2pypre_PYlist_sort_2(xs, cmp):
  xs.sort(key=functools.cmp_to_key(ats2pypre_cloref2fun2(cmp))); return
#
############################################

###### end of [PYlist_cats.py] ######
######
#
# HX-2014-08:
# for Python code translated from ATS
#
######

######
# beg of [reference_cats.py]
######

############################################

def ats2pypre_ref(x): return [x]
def ats2pypre_ref_make_elt(x): return [x]

############################################
#
def ats2pypre_ref_get_elt(ref): return ref[0]
def ats2pypre_ref_set_elt(ref, x0): ref[0] = x0; return
#
def ats2pypre_ref_exch_elt(ref, x0): x1 = ref[0]; ref[0] = x0; return x1
#
############################################

###### end of [reference_cats.py] ######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def ats2pypre_string_fset_at(arg0, arg1, arg2):
  tmpret0 = None
  tmp1 = None
  tmp2 = None
  tmp3 = None
  tmp4 = None
  tmp5 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_string_fset_at
  tmp1 = ats2pypre_string_length(arg0)
  tmp2 = ats2pypre_string_substring_beg_end(arg0, 0, arg1)
  tmp4 = ats2pypre_add_int1_int1(arg1, 1)
  tmp3 = ats2pypre_string_substring_beg_end(arg0, tmp4, tmp1)
  tmp5 = ats2pypre_string_append_3(tmp2, arg2, tmp3)
  tmpret0 = tmp5
  return tmpret0

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######
######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######
######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def _ats2pypre_list_patsfun_40__closurerize(env0):
  def _ats2pypre_list_patsfun_40__cfun(cenv, arg0): return _ats2pypre_list_patsfun_40(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_40__cfun, env0)

def _ats2pypre_list_patsfun_44__closurerize(env0):
  def _ats2pypre_list_patsfun_44__cfun(cenv, arg0): return _ats2pypre_list_patsfun_44(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_44__cfun, env0)

def _ats2pypre_list_patsfun_47__closurerize(env0):
  def _ats2pypre_list_patsfun_47__cfun(cenv, arg0): return _ats2pypre_list_patsfun_47(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_47__cfun, env0)

def _ats2pypre_list_patsfun_51__closurerize(env0):
  def _ats2pypre_list_patsfun_51__cfun(cenv, arg0): return _ats2pypre_list_patsfun_51(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_51__cfun, env0)

def _ats2pypre_list_patsfun_55__closurerize(env0):
  def _ats2pypre_list_patsfun_55__cfun(cenv, arg0): return _ats2pypre_list_patsfun_55(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_55__cfun, env0)

def _ats2pypre_list_patsfun_59__closurerize(env0):
  def _ats2pypre_list_patsfun_59__cfun(cenv, arg0): return _ats2pypre_list_patsfun_59(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_59__cfun, env0)

def _ats2pypre_list_patsfun_62__closurerize(env0):
  def _ats2pypre_list_patsfun_62__cfun(cenv, arg0): return _ats2pypre_list_patsfun_62(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_62__cfun, env0)

def _ats2pypre_list_patsfun_66__closurerize(env0):
  def _ats2pypre_list_patsfun_66__cfun(cenv, arg0): return _ats2pypre_list_patsfun_66(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_66__cfun, env0)

def _ats2pypre_list_patsfun_68__closurerize():
  def _ats2pypre_list_patsfun_68__cfun(cenv, arg0, arg1): return _ats2pypre_list_patsfun_68(arg0, arg1)
  return (_ats2pypre_list_patsfun_68__cfun, )

def _ats2pypre_list_patsfun_72__closurerize(env0):
  def _ats2pypre_list_patsfun_72__cfun(cenv, arg0): return _ats2pypre_list_patsfun_72(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_72__cfun, env0)

def _ats2pypre_list_patsfun_76__closurerize(env0):
  def _ats2pypre_list_patsfun_76__cfun(cenv, arg0): return _ats2pypre_list_patsfun_76(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_76__cfun, env0)

def _ats2pypre_list_patsfun_81__closurerize(env0, env1):
  def _ats2pypre_list_patsfun_81__cfun(cenv, arg0): return _ats2pypre_list_patsfun_81(cenv[1], cenv[2], arg0)
  return (_ats2pypre_list_patsfun_81__cfun, env0, env1)

def _ats2pypre_list_patsfun_85__closurerize(env0, env1):
  def _ats2pypre_list_patsfun_85__cfun(cenv, arg0): return _ats2pypre_list_patsfun_85(cenv[1], cenv[2], arg0)
  return (_ats2pypre_list_patsfun_85__cfun, env0, env1)

def _ats2pypre_list_patsfun_89__closurerize(env0, env1):
  def _ats2pypre_list_patsfun_89__cfun(cenv, arg0): return _ats2pypre_list_patsfun_89(cenv[1], cenv[2], arg0)
  return (_ats2pypre_list_patsfun_89__cfun, env0, env1)

def _ats2pypre_list_patsfun_93__closurerize(env0, env1):
  def _ats2pypre_list_patsfun_93__cfun(cenv, arg0): return _ats2pypre_list_patsfun_93(cenv[1], cenv[2], arg0)
  return (_ats2pypre_list_patsfun_93__cfun, env0, env1)

def _ats2pypre_list_patsfun_101__closurerize(env0):
  def _ats2pypre_list_patsfun_101__cfun(cenv, arg0): return _ats2pypre_list_patsfun_101(cenv[1], arg0)
  return (_ats2pypre_list_patsfun_101__cfun, env0)

def _ats2pypre_list_patsfun_104__closurerize(env0, env1):
  def _ats2pypre_list_patsfun_104__cfun(cenv, arg0): return _ats2pypre_list_patsfun_104(cenv[1], cenv[2], arg0)
  return (_ats2pypre_list_patsfun_104__cfun, env0, env1)

def _ats2pypre_list_patsfun_107__closurerize(env0, env1):
  def _ats2pypre_list_patsfun_107__cfun(cenv, arg0): return _ats2pypre_list_patsfun_107(cenv[1], cenv[2], arg0)
  return (_ats2pypre_list_patsfun_107__cfun, env0, env1)

def _ats2pypre_list_patsfun_109__closurerize(env0, env1):
  def _ats2pypre_list_patsfun_109__cfun(cenv, arg0): return _ats2pypre_list_patsfun_109(cenv[1], cenv[2], arg0)
  return (_ats2pypre_list_patsfun_109__cfun, env0, env1)

def ats2pypre_list_make_elt(arg0, arg1):
  tmpret2 = None
  tmp7 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_make_elt
  tmp7 = None
  tmpret2 = _ats2pypre_list_loop_3(arg1, arg0, tmp7)
  return tmpret2


def _ats2pypre_list_loop_3(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret3 = None
  tmp4 = None
  tmp5 = None
  tmp6 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_3
    tmp4 = ats2pypre_gt_int1_int1(arg0, 0)
    if (tmp4):
      tmp5 = ats2pypre_sub_int1_int1(arg0, 1)
      tmp6 = (env0, arg1)
      #ATStailcalseq_beg
      apy0 = tmp5
      apy1 = tmp6
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab__ats2pypre_list_loop_3
      #ATStailcalseq_end
    else:
      tmpret3 = arg1
    #endif
    if (funlab_py == 0): break
  return tmpret3


def ats2pypre_list_make_intrange_2(arg0, arg1):
  tmpret8 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_make_intrange_2
  tmpret8 = ats2pypre_list_make_intrange_3(arg0, arg1, 1)
  return tmpret8


def ats2pypre_list_make_intrange_3(arg0, arg1, arg2):
  tmpret9 = None
  tmp20 = None
  tmp21 = None
  tmp22 = None
  tmp23 = None
  tmp24 = None
  tmp25 = None
  tmp26 = None
  tmp27 = None
  tmp28 = None
  tmp29 = None
  tmp30 = None
  tmp31 = None
  tmp32 = None
  tmp33 = None
  tmp34 = None
  tmp35 = None
  tmp36 = None
  tmp37 = None
  tmp38 = None
  tmp39 = None
  tmp40 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab6():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret9, tmp20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26, tmp27, tmp28, tmp29, tmp30, tmp31, tmp32, tmp33, tmp34, tmp35, tmp36, tmp37, tmp38, tmp39, tmp40
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp20 = ats2pypre_gt_int0_int0(arg2, 0)
    if not(ATSCKpat_bool(tmp20, True)): tmplab_py = 2 ; return#__atstmplab7
    tmp21 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp21):
      tmp25 = ats2pypre_sub_int0_int0(arg1, arg0)
      tmp24 = ats2pypre_add_int0_int0(tmp25, arg2)
      tmp23 = ats2pypre_sub_int0_int0(tmp24, 1)
      tmp22 = ats2pypre_div_int0_int0(tmp23, arg2)
      tmp28 = ats2pypre_sub_int0_int0(tmp22, 1)
      tmp27 = ats2pypre_mul_int0_int0(tmp28, arg2)
      tmp26 = ats2pypre_add_int0_int0(arg0, tmp27)
      tmp29 = None
      tmpret9 = _ats2pypre_list_loop1_6(tmp22, tmp26, arg2, tmp29)
    else:
      tmpret9 = None
    #endif
    return
  def __atstmplab7():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret9, tmp20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26, tmp27, tmp28, tmp29, tmp30, tmp31, tmp32, tmp33, tmp34, tmp35, tmp36, tmp37, tmp38, tmp39, tmp40
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp30 = ats2pypre_lt_int0_int0(arg2, 0)
    if not(ATSCKpat_bool(tmp30, True)): tmplab_py = 3 ; return#__atstmplab8
    tmp31 = ats2pypre_gt_int0_int0(arg0, arg1)
    if (tmp31):
      tmp32 = ats2pypre_neg_int0(arg2)
      tmp36 = ats2pypre_sub_int0_int0(arg0, arg1)
      tmp35 = ats2pypre_add_int0_int0(tmp36, tmp32)
      tmp34 = ats2pypre_sub_int0_int0(tmp35, 1)
      tmp33 = ats2pypre_div_int0_int0(tmp34, tmp32)
      tmp39 = ats2pypre_sub_int0_int0(tmp33, 1)
      tmp38 = ats2pypre_mul_int0_int0(tmp39, tmp32)
      tmp37 = ats2pypre_sub_int0_int0(arg0, tmp38)
      tmp40 = None
      tmpret9 = _ats2pypre_list_loop2_7(tmp33, tmp37, tmp32, tmp40)
    else:
      tmpret9 = None
    #endif
    return
  def __atstmplab8():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret9, tmp20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26, tmp27, tmp28, tmp29, tmp30, tmp31, tmp32, tmp33, tmp34, tmp35, tmp36, tmp37, tmp38, tmp39, tmp40
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret9 = None
    return
  mbranch_1 = { 1: __atstmplab6, 2: __atstmplab7, 3: __atstmplab8 }
  #__patsflab_list_make_intrange_3
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret9


def _ats2pypre_list_loop1_6(arg0, arg1, arg2, arg3):
  apy0 = None
  apy1 = None
  apy2 = None
  apy3 = None
  tmpret10 = None
  tmp11 = None
  tmp12 = None
  tmp13 = None
  tmp14 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop1_6
    tmp11 = ats2pypre_gt_int0_int0(arg0, 0)
    if (tmp11):
      tmp12 = ats2pypre_sub_int0_int0(arg0, 1)
      tmp13 = ats2pypre_sub_int0_int0(arg1, arg2)
      tmp14 = (arg1, arg3)
      #ATStailcalseq_beg
      apy0 = tmp12
      apy1 = tmp13
      apy2 = arg2
      apy3 = tmp14
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      arg3 = apy3
      funlab_py = 1 #__patsflab__ats2pypre_list_loop1_6
      #ATStailcalseq_end
    else:
      tmpret10 = arg3
    #endif
    if (funlab_py == 0): break
  return tmpret10


def _ats2pypre_list_loop2_7(arg0, arg1, arg2, arg3):
  apy0 = None
  apy1 = None
  apy2 = None
  apy3 = None
  tmpret15 = None
  tmp16 = None
  tmp17 = None
  tmp18 = None
  tmp19 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop2_7
    tmp16 = ats2pypre_gt_int0_int0(arg0, 0)
    if (tmp16):
      tmp17 = ats2pypre_sub_int0_int0(arg0, 1)
      tmp18 = ats2pypre_add_int0_int0(arg1, arg2)
      tmp19 = (arg1, arg3)
      #ATStailcalseq_beg
      apy0 = tmp17
      apy1 = tmp18
      apy2 = arg2
      apy3 = tmp19
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      arg3 = apy3
      funlab_py = 1 #__patsflab__ats2pypre_list_loop2_7
      #ATStailcalseq_end
    else:
      tmpret15 = arg3
    #endif
    if (funlab_py == 0): break
  return tmpret15


def ats2pypre_list_length(arg0):
  tmpret52 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_length
  tmpret52 = _ats2pypre_list_loop_14(arg0, 0)
  return tmpret52


def _ats2pypre_list_loop_14(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret53 = None
  tmp55 = None
  tmp56 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab13():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret53, tmp55, tmp56
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab16
    __atstmplab14()
    return
  def __atstmplab14():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret53, tmp55, tmp56
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret53 = arg1
    return
  def __atstmplab15():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret53, tmp55, tmp56
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab16()
    return
  def __atstmplab16():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret53, tmp55, tmp56
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp55 = arg0[1]
    tmp56 = ats2pypre_add_int1_int1(arg1, 1)
    #ATStailcalseq_beg
    apy0 = tmp55
    apy1 = tmp56
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_list_loop_14
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab13, 2: __atstmplab14, 3: __atstmplab15, 4: __atstmplab16 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_14
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret53


def _057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2py3_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_length_gte(arg0, arg1):
  tmpret57 = None
  tmp58 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_length_gte
  tmp58 = _057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2py3_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_length_compare(arg0, arg1)
  tmpret57 = ats2pypre_gte_int1_int1(tmp58, 0)
  return tmpret57


def _057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2py3_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_length_compare(arg0, arg1):
  tmpret59 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_length_compare
  tmpret59 = _ats2pypre_list_loop_17(arg0, arg1)
  return tmpret59


def _ats2pypre_list_loop_17(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret60 = None
  tmp61 = None
  tmp63 = None
  tmp64 = None
  tmp65 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab17():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret60, tmp61, tmp63, tmp64, tmp65
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptrisnull(arg0)): tmplab_py = 3 ; return#__atstmplab19
    __atstmplab18()
    return
  def __atstmplab18():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret60, tmp61, tmp63, tmp64, tmp65
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp63 = arg0[1]
    tmp64 = ats2pypre_sub_int1_int1(arg1, 1)
    #ATStailcalseq_beg
    apy0 = tmp63
    apy1 = tmp64
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_list_loop_17
    #ATStailcalseq_end
    return
  def __atstmplab19():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret60, tmp61, tmp63, tmp64, tmp65
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp65 = ats2pypre_eq_int1_int1(arg1, 0)
    if (tmp65):
      tmpret60 = 0
    else:
      tmpret60 = ats2pypre_neg_int1(1)
    #endif
    return
  mbranch_1 = { 1: __atstmplab17, 2: __atstmplab18, 3: __atstmplab19 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_17
    tmp61 = ats2pypre_lt_int1_int1(arg1, 0)
    if (tmp61):
      tmpret60 = 1
    else:
      #ATScaseofseq_beg
      tmplab_py = 1
      while(1):
        mbranch_1.get(tmplab_py)()
        if (tmplab_py == 0): break
      #ATScaseofseq_end
    #endif
    if (funlab_py == 0): break
  return tmpret60


def ats2pypre_list_head(arg0):
  tmpret66 = None
  tmp67 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_head
  tmp67 = arg0[0]
  tmpret66 = tmp67
  return tmpret66


def ats2pypre_list_tail(arg0):
  tmpret68 = None
  tmp69 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_tail
  tmp69 = arg0[1]
  tmpret68 = tmp69
  return tmpret68


def ats2pypre_list_last(arg0):
  apy0 = None
  tmpret70 = None
  tmp71 = None
  tmp72 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab20():
    nonlocal arg0
    nonlocal apy0, tmpret70, tmp71, tmp72
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp72)): tmplab_py = 4 ; return#__atstmplab23
    __atstmplab21()
    return
  def __atstmplab21():
    nonlocal arg0
    nonlocal apy0, tmpret70, tmp71, tmp72
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret70 = tmp71
    return
  def __atstmplab22():
    nonlocal arg0
    nonlocal apy0, tmpret70, tmp71, tmp72
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab23()
    return
  def __atstmplab23():
    nonlocal arg0
    nonlocal apy0, tmpret70, tmp71, tmp72
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    #ATStailcalseq_beg
    apy0 = tmp72
    arg0 = apy0
    funlab_py = 1 #__patsflab_list_last
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab20, 2: __atstmplab21, 3: __atstmplab22, 4: __atstmplab23 }
  while(1):
    funlab_py = 0
    #__patsflab_list_last
    tmp71 = arg0[0]
    tmp72 = arg0[1]
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret70


def ats2pypre_list_get_at(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret73 = None
  tmp74 = None
  tmp75 = None
  tmp76 = None
  tmp77 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab_list_get_at
    tmp74 = ats2pypre_eq_int1_int1(arg1, 0)
    if (tmp74):
      tmp75 = arg0[0]
      tmpret73 = tmp75
    else:
      tmp76 = arg0[1]
      tmp77 = ats2pypre_sub_int1_int1(arg1, 1)
      #ATStailcalseq_beg
      apy0 = tmp76
      apy1 = tmp77
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab_list_get_at
      #ATStailcalseq_end
    #endif
    if (funlab_py == 0): break
  return tmpret73


def ats2pypre_list_snoc(arg0, arg1):
  tmpret78 = None
  tmp79 = None
  tmp80 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_snoc
  tmp80 = None
  tmp79 = (arg1, tmp80)
  tmpret78 = ats2pypre_list_append(arg0, tmp79)
  return tmpret78


def ats2pypre_list_extend(arg0, arg1):
  tmpret81 = None
  tmp82 = None
  tmp83 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_extend
  tmp83 = None
  tmp82 = (arg1, tmp83)
  tmpret81 = ats2pypre_list_append(arg0, tmp82)
  return tmpret81


def ats2pypre_list_append(arg0, arg1):
  tmpret84 = None
  tmp85 = None
  tmp86 = None
  tmp87 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab24():
    nonlocal arg0, arg1
    nonlocal tmpret84, tmp85, tmp86, tmp87
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab27
    __atstmplab25()
    return
  def __atstmplab25():
    nonlocal arg0, arg1
    nonlocal tmpret84, tmp85, tmp86, tmp87
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret84 = arg1
    return
  def __atstmplab26():
    nonlocal arg0, arg1
    nonlocal tmpret84, tmp85, tmp86, tmp87
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab27()
    return
  def __atstmplab27():
    nonlocal arg0, arg1
    nonlocal tmpret84, tmp85, tmp86, tmp87
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp85 = arg0[0]
    tmp86 = arg0[1]
    tmp87 = ats2pypre_list_append(tmp86, arg1)
    tmpret84 = (tmp85, tmp87)
    return
  mbranch_1 = { 1: __atstmplab24, 2: __atstmplab25, 3: __atstmplab26, 4: __atstmplab27 }
  #__patsflab_list_append
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret84


def ats2pypre_mul_int_list(arg0, arg1):
  tmpret88 = None
  tmp93 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_mul_int_list
  tmp93 = None
  tmpret88 = _ats2pypre_list_loop_26(arg1, arg0, tmp93)
  return tmpret88


def _ats2pypre_list_loop_26(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret89 = None
  tmp90 = None
  tmp91 = None
  tmp92 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_26
    tmp90 = ats2pypre_gt_int1_int1(arg0, 0)
    if (tmp90):
      tmp91 = ats2pypre_sub_int1_int1(arg0, 1)
      tmp92 = ats2pypre_list_append(env0, arg1)
      #ATStailcalseq_beg
      apy0 = tmp91
      apy1 = tmp92
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab__ats2pypre_list_loop_26
      #ATStailcalseq_end
    else:
      tmpret89 = arg1
    #endif
    if (funlab_py == 0): break
  return tmpret89


def ats2pypre_list_reverse(arg0):
  tmpret94 = None
  tmp95 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_reverse
  tmp95 = None
  tmpret94 = ats2pypre_list_reverse_append(arg0, tmp95)
  return tmpret94


def ats2pypre_list_reverse_append(arg0, arg1):
  tmpret96 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_reverse_append
  tmpret96 = _ats2pypre_list_loop_29(arg0, arg1)
  return tmpret96


def _ats2pypre_list_loop_29(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret97 = None
  tmp98 = None
  tmp99 = None
  tmp100 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab28():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret97, tmp98, tmp99, tmp100
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab31
    __atstmplab29()
    return
  def __atstmplab29():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret97, tmp98, tmp99, tmp100
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret97 = arg1
    return
  def __atstmplab30():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret97, tmp98, tmp99, tmp100
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab31()
    return
  def __atstmplab31():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret97, tmp98, tmp99, tmp100
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp98 = arg0[0]
    tmp99 = arg0[1]
    tmp100 = (tmp98, arg1)
    #ATStailcalseq_beg
    apy0 = tmp99
    apy1 = tmp100
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_list_loop_29
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab28, 2: __atstmplab29, 3: __atstmplab30, 4: __atstmplab31 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_29
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret97


def ats2pypre_list_concat(arg0):
  tmpret101 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_concat
  tmpret101 = _ats2pypre_list_auxlst_31(arg0)
  return tmpret101


def _ats2pypre_list_auxlst_31(arg0):
  tmpret102 = None
  tmp103 = None
  tmp104 = None
  tmp105 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab32():
    nonlocal arg0
    nonlocal tmpret102, tmp103, tmp104, tmp105
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab35
    __atstmplab33()
    return
  def __atstmplab33():
    nonlocal arg0
    nonlocal tmpret102, tmp103, tmp104, tmp105
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret102 = None
    return
  def __atstmplab34():
    nonlocal arg0
    nonlocal tmpret102, tmp103, tmp104, tmp105
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab35()
    return
  def __atstmplab35():
    nonlocal arg0
    nonlocal tmpret102, tmp103, tmp104, tmp105
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp103 = arg0[0]
    tmp104 = arg0[1]
    tmp105 = _ats2pypre_list_auxlst_31(tmp104)
    tmpret102 = ats2pypre_list_append(tmp103, tmp105)
    return
  mbranch_1 = { 1: __atstmplab32, 2: __atstmplab33, 3: __atstmplab34, 4: __atstmplab35 }
  #__patsflab__ats2pypre_list_auxlst_31
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret102


def ats2pypre_list_take(arg0, arg1):
  tmpret106 = None
  tmp107 = None
  tmp108 = None
  tmp109 = None
  tmp110 = None
  tmp111 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_take
  tmp107 = ats2pypre_gt_int1_int1(arg1, 0)
  if (tmp107):
    tmp108 = arg0[0]
    tmp109 = arg0[1]
    tmp111 = ats2pypre_sub_int1_int1(arg1, 1)
    tmp110 = ats2pypre_list_take(tmp109, tmp111)
    tmpret106 = (tmp108, tmp110)
  else:
    tmpret106 = None
  #endif
  return tmpret106


def ats2pypre_list_drop(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret112 = None
  tmp113 = None
  tmp114 = None
  tmp115 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab_list_drop
    tmp113 = ats2pypre_gt_int1_int1(arg1, 0)
    if (tmp113):
      tmp114 = arg0[1]
      tmp115 = ats2pypre_sub_int1_int1(arg1, 1)
      #ATStailcalseq_beg
      apy0 = tmp114
      apy1 = tmp115
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab_list_drop
      #ATStailcalseq_end
    else:
      tmpret112 = arg0
    #endif
    if (funlab_py == 0): break
  return tmpret112


def ats2pypre_list_split_at(arg0, arg1):
  tmpret116 = None
  tmp117 = None
  tmp118 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_split_at
  tmp117 = ats2pypre_list_take(arg0, arg1)
  tmp118 = ats2pypre_list_drop(arg0, arg1)
  tmpret116 = (tmp117, tmp118)
  return tmpret116


def ats2pypre_list_insert_at(arg0, arg1, arg2):
  tmpret119 = None
  tmp120 = None
  tmp121 = None
  tmp122 = None
  tmp123 = None
  tmp124 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_insert_at
  tmp120 = ats2pypre_gt_int1_int1(arg1, 0)
  if (tmp120):
    tmp121 = arg0[0]
    tmp122 = arg0[1]
    tmp124 = ats2pypre_sub_int1_int1(arg1, 1)
    tmp123 = ats2pypre_list_insert_at(tmp122, tmp124, arg2)
    tmpret119 = (tmp121, tmp123)
  else:
    tmpret119 = (arg2, arg0)
  #endif
  return tmpret119


def ats2pypre_list_remove_at(arg0, arg1):
  tmpret125 = None
  tmp126 = None
  tmp127 = None
  tmp128 = None
  tmp129 = None
  tmp130 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_remove_at
  tmp126 = arg0[0]
  tmp127 = arg0[1]
  tmp128 = ats2pypre_gt_int1_int1(arg1, 0)
  if (tmp128):
    tmp130 = ats2pypre_sub_int1_int1(arg1, 1)
    tmp129 = ats2pypre_list_remove_at(tmp127, tmp130)
    tmpret125 = (tmp126, tmp129)
  else:
    tmpret125 = tmp127
  #endif
  return tmpret125


def ats2pypre_list_takeout_at(arg0, arg1):
  tmpret131 = None
  tmp132 = None
  tmp133 = None
  tmp134 = None
  tmp135 = None
  tmp136 = None
  tmp137 = None
  tmp138 = None
  tmp139 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_takeout_at
  tmp132 = arg0[0]
  tmp133 = arg0[1]
  tmp134 = ats2pypre_gt_int1_int1(arg1, 0)
  if (tmp134):
    tmp136 = ats2pypre_sub_int1_int1(arg1, 1)
    tmp135 = ats2pypre_list_takeout_at(tmp133, tmp136)
    tmp137 = tmp135[0]
    tmp138 = tmp135[1]
    tmp139 = (tmp132, tmp138)
    tmpret131 = (tmp137, tmp139)
  else:
    tmpret131 = (tmp132, tmp133)
  #endif
  return tmpret131


def ats2pypre_list_exists(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret140 = None
  tmp141 = None
  tmp142 = None
  tmp143 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab36():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret140, tmp141, tmp142, tmp143
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab39
    __atstmplab37()
    return
  def __atstmplab37():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret140, tmp141, tmp142, tmp143
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret140 = False
    return
  def __atstmplab38():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret140, tmp141, tmp142, tmp143
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab39()
    return
  def __atstmplab39():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret140, tmp141, tmp142, tmp143
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp141 = arg0[0]
    tmp142 = arg0[1]
    tmp143 = arg1[0](arg1, tmp141)
    if (tmp143):
      tmpret140 = True
    else:
      #ATStailcalseq_beg
      apy0 = tmp142
      apy1 = arg1
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab_list_exists
      #ATStailcalseq_end
    #endif
    return
  mbranch_1 = { 1: __atstmplab36, 2: __atstmplab37, 3: __atstmplab38, 4: __atstmplab39 }
  while(1):
    funlab_py = 0
    #__patsflab_list_exists
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret140


def ats2pypre_list_exists_method(arg0):
  tmpret144 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_exists_method
  tmpret144 = _ats2pypre_list_patsfun_40__closurerize(arg0)
  return tmpret144


def _ats2pypre_list_patsfun_40(env0, arg0):
  tmpret145 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_40
  tmpret145 = ats2pypre_list_exists(env0, arg0)
  return tmpret145


def ats2pypre_list_iexists(arg0, arg1):
  tmpret146 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_iexists
  tmpret146 = _ats2pypre_list_loop_42(arg1, 0, arg0)
  return tmpret146


def _ats2pypre_list_loop_42(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret147 = None
  tmp148 = None
  tmp149 = None
  tmp150 = None
  tmp151 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab40():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret147, tmp148, tmp149, tmp150, tmp151
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab43
    __atstmplab41()
    return
  def __atstmplab41():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret147, tmp148, tmp149, tmp150, tmp151
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret147 = False
    return
  def __atstmplab42():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret147, tmp148, tmp149, tmp150, tmp151
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab43()
    return
  def __atstmplab43():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret147, tmp148, tmp149, tmp150, tmp151
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp148 = arg1[0]
    tmp149 = arg1[1]
    tmp150 = env0[0](env0, arg0, tmp148)
    if (tmp150):
      tmpret147 = True
    else:
      tmp151 = ats2pypre_add_int1_int1(arg0, 1)
      #ATStailcalseq_beg
      apy0 = tmp151
      apy1 = tmp149
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab__ats2pypre_list_loop_42
      #ATStailcalseq_end
    #endif
    return
  mbranch_1 = { 1: __atstmplab40, 2: __atstmplab41, 3: __atstmplab42, 4: __atstmplab43 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_42
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret147


def ats2pypre_list_iexists_method(arg0):
  tmpret152 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_iexists_method
  tmpret152 = _ats2pypre_list_patsfun_44__closurerize(arg0)
  return tmpret152


def _ats2pypre_list_patsfun_44(env0, arg0):
  tmpret153 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_44
  tmpret153 = ats2pypre_list_iexists(env0, arg0)
  return tmpret153


def ats2pypre_list_forall(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret154 = None
  tmp155 = None
  tmp156 = None
  tmp157 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab44():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret154, tmp155, tmp156, tmp157
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab47
    __atstmplab45()
    return
  def __atstmplab45():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret154, tmp155, tmp156, tmp157
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret154 = True
    return
  def __atstmplab46():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret154, tmp155, tmp156, tmp157
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab47()
    return
  def __atstmplab47():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret154, tmp155, tmp156, tmp157
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp155 = arg0[0]
    tmp156 = arg0[1]
    tmp157 = arg1[0](arg1, tmp155)
    if (tmp157):
      #ATStailcalseq_beg
      apy0 = tmp156
      apy1 = arg1
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab_list_forall
      #ATStailcalseq_end
    else:
      tmpret154 = False
    #endif
    return
  mbranch_1 = { 1: __atstmplab44, 2: __atstmplab45, 3: __atstmplab46, 4: __atstmplab47 }
  while(1):
    funlab_py = 0
    #__patsflab_list_forall
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret154


def ats2pypre_list_forall_method(arg0):
  tmpret158 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_forall_method
  tmpret158 = _ats2pypre_list_patsfun_47__closurerize(arg0)
  return tmpret158


def _ats2pypre_list_patsfun_47(env0, arg0):
  tmpret159 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_47
  tmpret159 = ats2pypre_list_forall(env0, arg0)
  return tmpret159


def ats2pypre_list_iforall(arg0, arg1):
  tmpret160 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_iforall
  tmpret160 = _ats2pypre_list_loop_49(arg1, 0, arg0)
  return tmpret160


def _ats2pypre_list_loop_49(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret161 = None
  tmp162 = None
  tmp163 = None
  tmp164 = None
  tmp165 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab48():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret161, tmp162, tmp163, tmp164, tmp165
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab51
    __atstmplab49()
    return
  def __atstmplab49():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret161, tmp162, tmp163, tmp164, tmp165
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret161 = True
    return
  def __atstmplab50():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret161, tmp162, tmp163, tmp164, tmp165
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab51()
    return
  def __atstmplab51():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret161, tmp162, tmp163, tmp164, tmp165
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp162 = arg1[0]
    tmp163 = arg1[1]
    tmp164 = env0[0](env0, arg0, tmp162)
    if (tmp164):
      tmp165 = ats2pypre_add_int1_int1(arg0, 1)
      #ATStailcalseq_beg
      apy0 = tmp165
      apy1 = tmp163
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab__ats2pypre_list_loop_49
      #ATStailcalseq_end
    else:
      tmpret161 = False
    #endif
    return
  mbranch_1 = { 1: __atstmplab48, 2: __atstmplab49, 3: __atstmplab50, 4: __atstmplab51 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_49
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret161


def ats2pypre_list_iforall_method(arg0):
  tmpret166 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_iforall_method
  tmpret166 = _ats2pypre_list_patsfun_51__closurerize(arg0)
  return tmpret166


def _ats2pypre_list_patsfun_51(env0, arg0):
  tmpret167 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_51
  tmpret167 = ats2pypre_list_iforall(env0, arg0)
  return tmpret167


def ats2pypre_list_app(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_app
  ats2pypre_list_foreach(arg0, arg1)
  return#_void


def ats2pypre_list_foreach(arg0, arg1):
  apy0 = None
  apy1 = None
  tmp170 = None
  tmp171 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab52():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmp170, tmp171
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab55
    __atstmplab53()
    return
  def __atstmplab53():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmp170, tmp171
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    None#ATSINSmove_void
    return
  def __atstmplab54():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmp170, tmp171
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab55()
    return
  def __atstmplab55():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmp170, tmp171
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp170 = arg0[0]
    tmp171 = arg0[1]
    arg1[0](arg1, tmp170)
    #ATStailcalseq_beg
    apy0 = tmp171
    apy1 = arg1
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab_list_foreach
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab52, 2: __atstmplab53, 3: __atstmplab54, 4: __atstmplab55 }
  while(1):
    funlab_py = 0
    #__patsflab_list_foreach
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return#_void


def ats2pypre_list_foreach_method(arg0):
  tmpret173 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_foreach_method
  tmpret173 = _ats2pypre_list_patsfun_55__closurerize(arg0)
  return tmpret173


def _ats2pypre_list_patsfun_55(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_55
  ats2pypre_list_foreach(env0, arg0)
  return#_void


def ats2pypre_list_iforeach(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_iforeach
  _ats2pypre_list_aux_57(arg1, 0, arg0)
  return#_void


def _ats2pypre_list_aux_57(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmp177 = None
  tmp178 = None
  tmp180 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab56():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp177, tmp178, tmp180
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab59
    __atstmplab57()
    return
  def __atstmplab57():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp177, tmp178, tmp180
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    None#ATSINSmove_void
    return
  def __atstmplab58():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp177, tmp178, tmp180
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab59()
    return
  def __atstmplab59():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp177, tmp178, tmp180
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp177 = arg1[0]
    tmp178 = arg1[1]
    env0[0](env0, arg0, tmp177)
    tmp180 = ats2pypre_add_int1_int1(arg0, 1)
    #ATStailcalseq_beg
    apy0 = tmp180
    apy1 = tmp178
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_list_aux_57
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab56, 2: __atstmplab57, 3: __atstmplab58, 4: __atstmplab59 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_aux_57
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return#_void


def ats2pypre_list_iforeach_method(arg0):
  tmpret181 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_iforeach_method
  tmpret181 = _ats2pypre_list_patsfun_59__closurerize(arg0)
  return tmpret181


def _ats2pypre_list_patsfun_59(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_59
  ats2pypre_list_iforeach(env0, arg0)
  return#_void


def ats2pypre_list_rforeach(arg0, arg1):
  tmp184 = None
  tmp185 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab60():
    nonlocal arg0, arg1
    nonlocal tmp184, tmp185
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab63
    __atstmplab61()
    return
  def __atstmplab61():
    nonlocal arg0, arg1
    nonlocal tmp184, tmp185
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    None#ATSINSmove_void
    return
  def __atstmplab62():
    nonlocal arg0, arg1
    nonlocal tmp184, tmp185
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab63()
    return
  def __atstmplab63():
    nonlocal arg0, arg1
    nonlocal tmp184, tmp185
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp184 = arg0[0]
    tmp185 = arg0[1]
    ats2pypre_list_rforeach(tmp185, arg1)
    arg1[0](arg1, tmp184)
    return
  mbranch_1 = { 1: __atstmplab60, 2: __atstmplab61, 3: __atstmplab62, 4: __atstmplab63 }
  #__patsflab_list_rforeach
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return#_void


def ats2pypre_list_rforeach_method(arg0):
  tmpret187 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_rforeach_method
  tmpret187 = _ats2pypre_list_patsfun_62__closurerize(arg0)
  return tmpret187


def _ats2pypre_list_patsfun_62(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_62
  ats2pypre_list_rforeach(env0, arg0)
  return#_void


def ats2pypre_list_filter(arg0, arg1):
  tmpret189 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_filter
  tmpret189 = _ats2pypre_list_aux_64(arg1, arg0)
  return tmpret189


def _ats2pypre_list_aux_64(env0, arg0):
  apy0 = None
  tmpret190 = None
  tmp191 = None
  tmp192 = None
  tmp193 = None
  tmp194 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab64():
    nonlocal env0, arg0
    nonlocal apy0, tmpret190, tmp191, tmp192, tmp193, tmp194
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab67
    __atstmplab65()
    return
  def __atstmplab65():
    nonlocal env0, arg0
    nonlocal apy0, tmpret190, tmp191, tmp192, tmp193, tmp194
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret190 = None
    return
  def __atstmplab66():
    nonlocal env0, arg0
    nonlocal apy0, tmpret190, tmp191, tmp192, tmp193, tmp194
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab67()
    return
  def __atstmplab67():
    nonlocal env0, arg0
    nonlocal apy0, tmpret190, tmp191, tmp192, tmp193, tmp194
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp191 = arg0[0]
    tmp192 = arg0[1]
    tmp193 = env0[0](env0, tmp191)
    if (tmp193):
      tmp194 = _ats2pypre_list_aux_64(env0, tmp192)
      tmpret190 = (tmp191, tmp194)
    else:
      #ATStailcalseq_beg
      apy0 = tmp192
      arg0 = apy0
      funlab_py = 1 #__patsflab__ats2pypre_list_aux_64
      #ATStailcalseq_end
    #endif
    return
  mbranch_1 = { 1: __atstmplab64, 2: __atstmplab65, 3: __atstmplab66, 4: __atstmplab67 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_aux_64
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret190


def ats2pypre_list_filter_method(arg0):
  tmpret195 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_filter_method
  tmpret195 = _ats2pypre_list_patsfun_66__closurerize(arg0)
  return tmpret195


def _ats2pypre_list_patsfun_66(env0, arg0):
  tmpret196 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_66
  tmpret196 = ats2pypre_list_filter(env0, arg0)
  return tmpret196


def _057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2py3_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_labelize(arg0):
  tmpret197 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_labelize
  tmpret197 = ats2pypre_list_imap(arg0, _ats2pypre_list_patsfun_68__closurerize())
  return tmpret197


def _ats2pypre_list_patsfun_68(arg0, arg1):
  tmpret198 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_68
  tmpret198 = (arg0, arg1)
  return tmpret198


def ats2pypre_list_map(arg0, arg1):
  tmpret199 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_map
  tmpret199 = _ats2pypre_list_aux_70(arg1, arg0)
  return tmpret199


def _ats2pypre_list_aux_70(env0, arg0):
  tmpret200 = None
  tmp201 = None
  tmp202 = None
  tmp203 = None
  tmp204 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab68():
    nonlocal env0, arg0
    nonlocal tmpret200, tmp201, tmp202, tmp203, tmp204
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab71
    __atstmplab69()
    return
  def __atstmplab69():
    nonlocal env0, arg0
    nonlocal tmpret200, tmp201, tmp202, tmp203, tmp204
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret200 = None
    return
  def __atstmplab70():
    nonlocal env0, arg0
    nonlocal tmpret200, tmp201, tmp202, tmp203, tmp204
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab71()
    return
  def __atstmplab71():
    nonlocal env0, arg0
    nonlocal tmpret200, tmp201, tmp202, tmp203, tmp204
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp201 = arg0[0]
    tmp202 = arg0[1]
    tmp203 = env0[0](env0, tmp201)
    tmp204 = _ats2pypre_list_aux_70(env0, tmp202)
    tmpret200 = (tmp203, tmp204)
    return
  mbranch_1 = { 1: __atstmplab68, 2: __atstmplab69, 3: __atstmplab70, 4: __atstmplab71 }
  #__patsflab__ats2pypre_list_aux_70
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret200


def ats2pypre_list_map_method(arg0, arg1):
  tmpret205 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_map_method
  tmpret205 = _ats2pypre_list_patsfun_72__closurerize(arg0)
  return tmpret205


def _ats2pypre_list_patsfun_72(env0, arg0):
  tmpret206 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_72
  tmpret206 = ats2pypre_list_map(env0, arg0)
  return tmpret206


def ats2pypre_list_imap(arg0, arg1):
  tmpret207 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_imap
  tmpret207 = _ats2pypre_list_aux_74(arg1, 0, arg0)
  return tmpret207


def _ats2pypre_list_aux_74(env0, arg0, arg1):
  tmpret208 = None
  tmp209 = None
  tmp210 = None
  tmp211 = None
  tmp212 = None
  tmp213 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab72():
    nonlocal env0, arg0, arg1
    nonlocal tmpret208, tmp209, tmp210, tmp211, tmp212, tmp213
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab75
    __atstmplab73()
    return
  def __atstmplab73():
    nonlocal env0, arg0, arg1
    nonlocal tmpret208, tmp209, tmp210, tmp211, tmp212, tmp213
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret208 = None
    return
  def __atstmplab74():
    nonlocal env0, arg0, arg1
    nonlocal tmpret208, tmp209, tmp210, tmp211, tmp212, tmp213
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab75()
    return
  def __atstmplab75():
    nonlocal env0, arg0, arg1
    nonlocal tmpret208, tmp209, tmp210, tmp211, tmp212, tmp213
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp209 = arg1[0]
    tmp210 = arg1[1]
    tmp211 = env0[0](env0, arg0, tmp209)
    tmp213 = ats2pypre_add_int1_int1(arg0, 1)
    tmp212 = _ats2pypre_list_aux_74(env0, tmp213, tmp210)
    tmpret208 = (tmp211, tmp212)
    return
  mbranch_1 = { 1: __atstmplab72, 2: __atstmplab73, 3: __atstmplab74, 4: __atstmplab75 }
  #__patsflab__ats2pypre_list_aux_74
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret208


def ats2pypre_list_imap_method(arg0, arg1):
  tmpret214 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_imap_method
  tmpret214 = _ats2pypre_list_patsfun_76__closurerize(arg0)
  return tmpret214


def _ats2pypre_list_patsfun_76(env0, arg0):
  tmpret215 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_76
  tmpret215 = ats2pypre_list_imap(env0, arg0)
  return tmpret215


def ats2pypre_list_map2(arg0, arg1, arg2):
  tmpret216 = None
  tmp217 = None
  tmp218 = None
  tmp219 = None
  tmp220 = None
  tmp221 = None
  tmp222 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  mbranch_2 = None
  def __atstmplab76():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret216, tmp217, tmp218, tmp219, tmp220, tmp221, tmp222
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab79
    __atstmplab77()
    return
  def __atstmplab77():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret216, tmp217, tmp218, tmp219, tmp220, tmp221, tmp222
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret216 = None
    return
  def __atstmplab78():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret216, tmp217, tmp218, tmp219, tmp220, tmp221, tmp222
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab79()
    return
  def __atstmplab79():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret216, tmp217, tmp218, tmp219, tmp220, tmp221, tmp222
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp217 = arg0[0]
    tmp218 = arg0[1]
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_2.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    return
  def __atstmplab80():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret216, tmp217, tmp218, tmp219, tmp220, tmp221, tmp222
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab83
    __atstmplab81()
    return
  def __atstmplab81():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret216, tmp217, tmp218, tmp219, tmp220, tmp221, tmp222
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret216 = None
    return
  def __atstmplab82():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret216, tmp217, tmp218, tmp219, tmp220, tmp221, tmp222
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab83()
    return
  def __atstmplab83():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret216, tmp217, tmp218, tmp219, tmp220, tmp221, tmp222
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp219 = arg1[0]
    tmp220 = arg1[1]
    tmp221 = arg2[0](arg2, tmp217, tmp219)
    tmp222 = ats2pypre_list_map2(tmp218, tmp220, arg2)
    tmpret216 = (tmp221, tmp222)
    return
  mbranch_1 = { 1: __atstmplab76, 2: __atstmplab77, 3: __atstmplab78, 4: __atstmplab79 }
  mbranch_2 = { 1: __atstmplab80, 2: __atstmplab81, 3: __atstmplab82, 4: __atstmplab83 }
  #__patsflab_list_map2
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret216


def ats2pypre_list_foldleft(arg0, arg1, arg2):
  tmpret223 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_foldleft
  tmpret223 = _ats2pypre_list_loop_79(arg2, arg1, arg0)
  return tmpret223


def _ats2pypre_list_loop_79(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret224 = None
  tmp225 = None
  tmp226 = None
  tmp227 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab84():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret224, tmp225, tmp226, tmp227
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab87
    __atstmplab85()
    return
  def __atstmplab85():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret224, tmp225, tmp226, tmp227
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret224 = arg0
    return
  def __atstmplab86():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret224, tmp225, tmp226, tmp227
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab87()
    return
  def __atstmplab87():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret224, tmp225, tmp226, tmp227
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp225 = arg1[0]
    tmp226 = arg1[1]
    tmp227 = env0[0](env0, arg0, tmp225)
    #ATStailcalseq_beg
    apy0 = tmp227
    apy1 = tmp226
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_list_loop_79
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab84, 2: __atstmplab85, 3: __atstmplab86, 4: __atstmplab87 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_79
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret224


def ats2pypre_list_foldleft_method(arg0, arg1):
  tmpret228 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_foldleft_method
  tmpret228 = _ats2pypre_list_patsfun_81__closurerize(arg0, arg1)
  return tmpret228


def _ats2pypre_list_patsfun_81(env0, env1, arg0):
  tmpret229 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_81
  tmpret229 = ats2pypre_list_foldleft(env0, env1, arg0)
  return tmpret229


def ats2pypre_list_ifoldleft(arg0, arg1, arg2):
  tmpret230 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_ifoldleft
  tmpret230 = _ats2pypre_list_loop_83(arg2, 0, arg1, arg0)
  return tmpret230


def _ats2pypre_list_loop_83(env0, arg0, arg1, arg2):
  apy0 = None
  apy1 = None
  apy2 = None
  tmpret231 = None
  tmp232 = None
  tmp233 = None
  tmp234 = None
  tmp235 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab88():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret231, tmp232, tmp233, tmp234, tmp235
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg2)): tmplab_py = 4 ; return#__atstmplab91
    __atstmplab89()
    return
  def __atstmplab89():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret231, tmp232, tmp233, tmp234, tmp235
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret231 = arg1
    return
  def __atstmplab90():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret231, tmp232, tmp233, tmp234, tmp235
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab91()
    return
  def __atstmplab91():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret231, tmp232, tmp233, tmp234, tmp235
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp232 = arg2[0]
    tmp233 = arg2[1]
    tmp234 = ats2pypre_add_int1_int1(arg0, 1)
    tmp235 = env0[0](env0, arg0, arg1, tmp232)
    #ATStailcalseq_beg
    apy0 = tmp234
    apy1 = tmp235
    apy2 = tmp233
    arg0 = apy0
    arg1 = apy1
    arg2 = apy2
    funlab_py = 1 #__patsflab__ats2pypre_list_loop_83
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab88, 2: __atstmplab89, 3: __atstmplab90, 4: __atstmplab91 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_83
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret231


def ats2pypre_list_ifoldleft_method(arg0, arg1):
  tmpret236 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_ifoldleft_method
  tmpret236 = _ats2pypre_list_patsfun_85__closurerize(arg0, arg1)
  return tmpret236


def _ats2pypre_list_patsfun_85(env0, env1, arg0):
  tmpret237 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_85
  tmpret237 = ats2pypre_list_ifoldleft(env0, env1, arg0)
  return tmpret237


def ats2pypre_list_foldright(arg0, arg1, arg2):
  tmpret238 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_foldright
  tmpret238 = _ats2pypre_list_aux_87(arg1, arg0, arg2)
  return tmpret238


def _ats2pypre_list_aux_87(env0, arg0, arg1):
  tmpret239 = None
  tmp240 = None
  tmp241 = None
  tmp242 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab92():
    nonlocal env0, arg0, arg1
    nonlocal tmpret239, tmp240, tmp241, tmp242
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab95
    __atstmplab93()
    return
  def __atstmplab93():
    nonlocal env0, arg0, arg1
    nonlocal tmpret239, tmp240, tmp241, tmp242
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret239 = arg1
    return
  def __atstmplab94():
    nonlocal env0, arg0, arg1
    nonlocal tmpret239, tmp240, tmp241, tmp242
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab95()
    return
  def __atstmplab95():
    nonlocal env0, arg0, arg1
    nonlocal tmpret239, tmp240, tmp241, tmp242
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp240 = arg0[0]
    tmp241 = arg0[1]
    tmp242 = _ats2pypre_list_aux_87(env0, tmp241, arg1)
    tmpret239 = env0[0](env0, tmp240, tmp242)
    return
  mbranch_1 = { 1: __atstmplab92, 2: __atstmplab93, 3: __atstmplab94, 4: __atstmplab95 }
  #__patsflab__ats2pypre_list_aux_87
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret239


def ats2pypre_list_foldright_method(arg0, arg1):
  tmpret243 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_foldright_method
  tmpret243 = _ats2pypre_list_patsfun_89__closurerize(arg0, arg1)
  return tmpret243


def _ats2pypre_list_patsfun_89(env0, env1, arg0):
  tmpret244 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_89
  tmpret244 = ats2pypre_list_foldright(env0, arg0, env1)
  return tmpret244


def ats2pypre_list_ifoldright(arg0, arg1, arg2):
  tmpret245 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_ifoldright
  tmpret245 = _ats2pypre_list_aux_91(arg1, 0, arg0, arg2)
  return tmpret245


def _ats2pypre_list_aux_91(env0, arg0, arg1, arg2):
  tmpret246 = None
  tmp247 = None
  tmp248 = None
  tmp249 = None
  tmp250 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab96():
    nonlocal env0, arg0, arg1, arg2
    nonlocal tmpret246, tmp247, tmp248, tmp249, tmp250
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab99
    __atstmplab97()
    return
  def __atstmplab97():
    nonlocal env0, arg0, arg1, arg2
    nonlocal tmpret246, tmp247, tmp248, tmp249, tmp250
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret246 = arg2
    return
  def __atstmplab98():
    nonlocal env0, arg0, arg1, arg2
    nonlocal tmpret246, tmp247, tmp248, tmp249, tmp250
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab99()
    return
  def __atstmplab99():
    nonlocal env0, arg0, arg1, arg2
    nonlocal tmpret246, tmp247, tmp248, tmp249, tmp250
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp247 = arg1[0]
    tmp248 = arg1[1]
    tmp250 = ats2pypre_add_int1_int1(arg0, 1)
    tmp249 = _ats2pypre_list_aux_91(env0, tmp250, tmp248, arg2)
    tmpret246 = env0[0](env0, arg0, tmp247, tmp249)
    return
  mbranch_1 = { 1: __atstmplab96, 2: __atstmplab97, 3: __atstmplab98, 4: __atstmplab99 }
  #__patsflab__ats2pypre_list_aux_91
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret246


def ats2pypre_list_ifoldright_method(arg0, arg1):
  tmpret251 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_ifoldright_method
  tmpret251 = _ats2pypre_list_patsfun_93__closurerize(arg0, arg1)
  return tmpret251


def _ats2pypre_list_patsfun_93(env0, env1, arg0):
  tmpret252 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_patsfun_93
  tmpret252 = ats2pypre_list_ifoldright(env0, arg0, env1)
  return tmpret252


def ats2pypre_list_mergesort(arg0, arg1):
  tmpret255 = None
  tmp274 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_mergesort
  tmp274 = ats2pypre_list_length(arg0)
  tmpret255 = _ats2pypre_list_msort_97(arg1, arg0, tmp274)
  return tmpret255


def _ats2pypre_list_msort_97(env0, arg0, arg1):
  tmpret256 = None
  tmp257 = None
  tmp258 = None
  tmp259 = None
  tmp260 = None
  tmp261 = None
  tmp262 = None
  tmp263 = None
  tmp264 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_msort_97
  tmp257 = ats2pypre_lt_int1_int1(arg1, 2)
  if (tmp257):
    tmpret256 = arg0
  else:
    tmp258 = ats2pypre_half_int1(arg1)
    tmp259 = ats2pypre_list_split_at(arg0, tmp258)
    tmp260 = tmp259[0]
    tmp261 = tmp259[1]
    tmp262 = _ats2pypre_list_msort_97(env0, tmp260, tmp258)
    tmp264 = ats2pypre_sub_int1_int1(arg1, tmp258)
    tmp263 = _ats2pypre_list_msort_97(env0, tmp261, tmp264)
    tmpret256 = _ats2pypre_list_merge_98(env0, tmp262, tmp263)
  #endif
  return tmpret256


def _ats2pypre_list_merge_98(env0, arg0, arg1):
  tmpret265 = None
  tmp266 = None
  tmp267 = None
  tmp268 = None
  tmp269 = None
  tmp270 = None
  tmp271 = None
  tmp272 = None
  tmp273 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  mbranch_2 = None
  def __atstmplab100():
    nonlocal env0, arg0, arg1
    nonlocal tmpret265, tmp266, tmp267, tmp268, tmp269, tmp270, tmp271, tmp272, tmp273
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab103
    __atstmplab101()
    return
  def __atstmplab101():
    nonlocal env0, arg0, arg1
    nonlocal tmpret265, tmp266, tmp267, tmp268, tmp269, tmp270, tmp271, tmp272, tmp273
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret265 = arg1
    return
  def __atstmplab102():
    nonlocal env0, arg0, arg1
    nonlocal tmpret265, tmp266, tmp267, tmp268, tmp269, tmp270, tmp271, tmp272, tmp273
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab103()
    return
  def __atstmplab103():
    nonlocal env0, arg0, arg1
    nonlocal tmpret265, tmp266, tmp267, tmp268, tmp269, tmp270, tmp271, tmp272, tmp273
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp266 = arg0[0]
    tmp267 = arg0[1]
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_2.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    return
  def __atstmplab104():
    nonlocal env0, arg0, arg1
    nonlocal tmpret265, tmp266, tmp267, tmp268, tmp269, tmp270, tmp271, tmp272, tmp273
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab107
    __atstmplab105()
    return
  def __atstmplab105():
    nonlocal env0, arg0, arg1
    nonlocal tmpret265, tmp266, tmp267, tmp268, tmp269, tmp270, tmp271, tmp272, tmp273
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret265 = arg0
    return
  def __atstmplab106():
    nonlocal env0, arg0, arg1
    nonlocal tmpret265, tmp266, tmp267, tmp268, tmp269, tmp270, tmp271, tmp272, tmp273
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab107()
    return
  def __atstmplab107():
    nonlocal env0, arg0, arg1
    nonlocal tmpret265, tmp266, tmp267, tmp268, tmp269, tmp270, tmp271, tmp272, tmp273
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp268 = arg1[0]
    tmp269 = arg1[1]
    tmp270 = env0[0](env0, tmp266, tmp268)
    tmp271 = ats2pypre_lte_int0_int0(tmp270, 0)
    if (tmp271):
      tmp272 = _ats2pypre_list_merge_98(env0, tmp267, arg1)
      tmpret265 = (tmp266, tmp272)
    else:
      tmp273 = _ats2pypre_list_merge_98(env0, arg0, tmp269)
      tmpret265 = (tmp268, tmp273)
    #endif
    return
  mbranch_1 = { 1: __atstmplab100, 2: __atstmplab101, 3: __atstmplab102, 4: __atstmplab103 }
  mbranch_2 = { 1: __atstmplab104, 2: __atstmplab105, 3: __atstmplab106, 4: __atstmplab107 }
  #__patsflab__ats2pypre_list_merge_98
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret265


def ats2pypre_streamize_list_elt(arg0):
  tmpret275 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_streamize_list_elt
  tmpret275 = _ats2pypre_list_auxmain_100(arg0)
  return tmpret275


def _ats2pypre_list_auxmain_100(arg0):
  tmpret276 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_auxmain_100
  tmpret276 = _ats2pypre_list_patsfun_101__closurerize(arg0)
  return tmpret276


def _ats2pypre_list_patsfun_101(env0, arg0):
  tmpret277 = None
  tmp278 = None
  tmp279 = None
  tmp280 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab108():
    nonlocal env0, arg0
    nonlocal tmpret277, tmp278, tmp279, tmp280
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(env0)): tmplab_py = 4 ; return#__atstmplab111
    __atstmplab109()
    return
  def __atstmplab109():
    nonlocal env0, arg0
    nonlocal tmpret277, tmp278, tmp279, tmp280
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret277 = None
    return
  def __atstmplab110():
    nonlocal env0, arg0
    nonlocal tmpret277, tmp278, tmp279, tmp280
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab111()
    return
  def __atstmplab111():
    nonlocal env0, arg0
    nonlocal tmpret277, tmp278, tmp279, tmp280
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp278 = env0[0]
    tmp279 = env0[1]
    tmp280 = _ats2pypre_list_auxmain_100(tmp279)
    tmpret277 = (tmp278, tmp280)
    return
  mbranch_1 = { 1: __atstmplab108, 2: __atstmplab109, 3: __atstmplab110, 4: __atstmplab111 }
  #__patsflab__ats2pypre_list_patsfun_101
  if (arg0):
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
  #endif
  return tmpret277


def ats2pypre_streamize_list_zip(arg0, arg1):
  tmpret281 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_streamize_list_zip
  tmpret281 = _ats2pypre_list_auxmain_103(arg0, arg1)
  return tmpret281


def _ats2pypre_list_auxmain_103(arg0, arg1):
  tmpret282 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_auxmain_103
  tmpret282 = _ats2pypre_list_patsfun_104__closurerize(arg0, arg1)
  return tmpret282


def _ats2pypre_list_patsfun_104(env0, env1, arg0):
  tmpret283 = None
  tmp284 = None
  tmp285 = None
  tmp286 = None
  tmp287 = None
  tmp288 = None
  tmp289 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  mbranch_2 = None
  def __atstmplab112():
    nonlocal env0, env1, arg0
    nonlocal tmpret283, tmp284, tmp285, tmp286, tmp287, tmp288, tmp289
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(env0)): tmplab_py = 4 ; return#__atstmplab115
    __atstmplab113()
    return
  def __atstmplab113():
    nonlocal env0, env1, arg0
    nonlocal tmpret283, tmp284, tmp285, tmp286, tmp287, tmp288, tmp289
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret283 = None
    return
  def __atstmplab114():
    nonlocal env0, env1, arg0
    nonlocal tmpret283, tmp284, tmp285, tmp286, tmp287, tmp288, tmp289
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab115()
    return
  def __atstmplab115():
    nonlocal env0, env1, arg0
    nonlocal tmpret283, tmp284, tmp285, tmp286, tmp287, tmp288, tmp289
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp284 = env0[0]
    tmp285 = env0[1]
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_2.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    return
  def __atstmplab116():
    nonlocal env0, env1, arg0
    nonlocal tmpret283, tmp284, tmp285, tmp286, tmp287, tmp288, tmp289
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(env1)): tmplab_py = 4 ; return#__atstmplab119
    __atstmplab117()
    return
  def __atstmplab117():
    nonlocal env0, env1, arg0
    nonlocal tmpret283, tmp284, tmp285, tmp286, tmp287, tmp288, tmp289
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret283 = None
    return
  def __atstmplab118():
    nonlocal env0, env1, arg0
    nonlocal tmpret283, tmp284, tmp285, tmp286, tmp287, tmp288, tmp289
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab119()
    return
  def __atstmplab119():
    nonlocal env0, env1, arg0
    nonlocal tmpret283, tmp284, tmp285, tmp286, tmp287, tmp288, tmp289
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp286 = env1[0]
    tmp287 = env1[1]
    tmp288 = (tmp284, tmp286)
    tmp289 = _ats2pypre_list_auxmain_103(tmp285, tmp287)
    tmpret283 = (tmp288, tmp289)
    return
  mbranch_1 = { 1: __atstmplab112, 2: __atstmplab113, 3: __atstmplab114, 4: __atstmplab115 }
  mbranch_2 = { 1: __atstmplab116, 2: __atstmplab117, 3: __atstmplab118, 4: __atstmplab119 }
  #__patsflab__ats2pypre_list_patsfun_104
  if (arg0):
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
  #endif
  return tmpret283


def ats2pypre_streamize_list_cross(arg0, arg1):
  tmpret290 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_streamize_list_cross
  tmpret290 = _ats2pypre_list_auxmain_108(arg0, arg1)
  return tmpret290


def _ats2pypre_list_auxone_106(arg0, arg1):
  tmpret291 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_auxone_106
  tmpret291 = _ats2pypre_list_patsfun_107__closurerize(arg0, arg1)
  return tmpret291


def _ats2pypre_list_patsfun_107(env0, env1, arg0):
  tmpret292 = None
  tmp293 = None
  tmp294 = None
  tmp295 = None
  tmp296 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab120():
    nonlocal env0, env1, arg0
    nonlocal tmpret292, tmp293, tmp294, tmp295, tmp296
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(env1)): tmplab_py = 4 ; return#__atstmplab123
    __atstmplab121()
    return
  def __atstmplab121():
    nonlocal env0, env1, arg0
    nonlocal tmpret292, tmp293, tmp294, tmp295, tmp296
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret292 = None
    return
  def __atstmplab122():
    nonlocal env0, env1, arg0
    nonlocal tmpret292, tmp293, tmp294, tmp295, tmp296
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab123()
    return
  def __atstmplab123():
    nonlocal env0, env1, arg0
    nonlocal tmpret292, tmp293, tmp294, tmp295, tmp296
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp293 = env1[0]
    tmp294 = env1[1]
    tmp295 = (env0, tmp293)
    tmp296 = _ats2pypre_list_auxone_106(env0, tmp294)
    tmpret292 = (tmp295, tmp296)
    return
  mbranch_1 = { 1: __atstmplab120, 2: __atstmplab121, 3: __atstmplab122, 4: __atstmplab123 }
  #__patsflab__ats2pypre_list_patsfun_107
  if (arg0):
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
  #endif
  return tmpret292


def _ats2pypre_list_auxmain_108(arg0, arg1):
  tmpret297 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_list_auxmain_108
  tmpret297 = _ats2pypre_list_patsfun_109__closurerize(arg0, arg1)
  return tmpret297


def _ats2pypre_list_patsfun_109(env0, env1, arg0):
  tmpret298 = None
  tmp299 = None
  tmp300 = None
  tmp301 = None
  tmp302 = None
  tmp303 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab124():
    nonlocal env0, env1, arg0
    nonlocal tmpret298, tmp299, tmp300, tmp301, tmp302, tmp303
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(env0)): tmplab_py = 4 ; return#__atstmplab127
    __atstmplab125()
    return
  def __atstmplab125():
    nonlocal env0, env1, arg0
    nonlocal tmpret298, tmp299, tmp300, tmp301, tmp302, tmp303
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret298 = None
    return
  def __atstmplab126():
    nonlocal env0, env1, arg0
    nonlocal tmpret298, tmp299, tmp300, tmp301, tmp302, tmp303
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab127()
    return
  def __atstmplab127():
    nonlocal env0, env1, arg0
    nonlocal tmpret298, tmp299, tmp300, tmp301, tmp302, tmp303
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp299 = env0[0]
    tmp300 = env0[1]
    tmp302 = _ats2pypre_list_auxone_106(tmp299, env1)
    tmp303 = _ats2pypre_list_auxmain_108(tmp300, env1)
    tmp301 = ats2pypre_stream_vt_append(tmp302, tmp303)
    tmpret298 = ATSPMVllazyval_eval(tmp301)
    return
  mbranch_1 = { 1: __atstmplab124, 2: __atstmplab125, 3: __atstmplab126, 4: __atstmplab127 }
  #__patsflab__ats2pypre_list_patsfun_109
  if (arg0):
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
  #endif
  return tmpret298


def ats2pypre_PYlist_oflist(arg0):
  tmpret306 = None
  tmp311 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_PYlist_oflist
  tmp311 = ats2pypre_PYlist_nil()
  tmpret306 = _ats2pypre_list_aux_113(arg0, tmp311)
  return tmpret306


def _ats2pypre_list_aux_113(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret307 = None
  tmp308 = None
  tmp309 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab128():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret307, tmp308, tmp309
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab131
    __atstmplab129()
    return
  def __atstmplab129():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret307, tmp308, tmp309
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret307 = arg1
    return
  def __atstmplab130():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret307, tmp308, tmp309
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab131()
    return
  def __atstmplab131():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret307, tmp308, tmp309
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp308 = arg0[0]
    tmp309 = arg0[1]
    ats2pypre_PYlist_append(arg1, tmp308)
    #ATStailcalseq_beg
    apy0 = tmp309
    apy1 = arg1
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_list_aux_113
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab128, 2: __atstmplab129, 3: __atstmplab130, 4: __atstmplab131 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_aux_113
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret307


def ats2pypre_PYlist_oflist_rev(arg0):
  tmpret312 = None
  tmp317 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_PYlist_oflist_rev
  tmp317 = ats2pypre_PYlist_nil()
  tmpret312 = _ats2pypre_list_aux_115(arg0, tmp317)
  return tmpret312


def _ats2pypre_list_aux_115(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret313 = None
  tmp314 = None
  tmp315 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab132():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret313, tmp314, tmp315
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab135
    __atstmplab133()
    return
  def __atstmplab133():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret313, tmp314, tmp315
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret313 = arg1
    return
  def __atstmplab134():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret313, tmp314, tmp315
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab135()
    return
  def __atstmplab135():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret313, tmp314, tmp315
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp314 = arg0[0]
    tmp315 = arg0[1]
    ats2pypre_PYlist_cons(tmp314, arg1)
    #ATStailcalseq_beg
    apy0 = tmp315
    apy1 = arg1
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_list_aux_115
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab132, 2: __atstmplab133, 3: __atstmplab134, 4: __atstmplab135 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_aux_115
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret313


def ats2pypre_list_sort_2(arg0, arg1):
  tmpret318 = None
  tmp319 = None
  tmp321 = None
  tmp327 = None
  tmp328 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_sort_2
  tmp319 = ats2pypre_PYlist_oflist(arg0)
  ats2pypre_PYlist_sort_2(tmp319, arg1)
  tmp321 = ats2pypre_PYlist_length(tmp319)
  tmp328 = None
  tmp327 = _ats2pypre_list_loop_117(tmp319, tmp321, 0, tmp328)
  tmpret318 = tmp327
  return tmpret318


def _ats2pypre_list_loop_117(env0, env1, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret322 = None
  tmp323 = None
  tmp324 = None
  tmp325 = None
  tmp326 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_117
    tmp323 = ats2pypre_lt_int0_int0(arg0, env1)
    if (tmp323):
      tmp324 = ats2pypre_add_int0_int0(arg0, 1)
      tmp326 = ats2pypre_PYlist_pop_0(env0)
      tmp325 = (tmp326, arg1)
      #ATStailcalseq_beg
      apy0 = tmp324
      apy1 = tmp325
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab__ats2pypre_list_loop_117
      #ATStailcalseq_end
    else:
      tmpret322 = arg1
    #endif
    if (funlab_py == 0): break
  return tmpret322

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def ats2pypre_list_vt_length(arg0):
  tmpret2 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_vt_length
  tmpret2 = _ats2pypre_list_loop_3(arg0, 0)
  return tmpret2


def _ats2pypre_list_loop_3(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret3 = None
  tmp5 = None
  tmp6 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab8():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret3, tmp5, tmp6
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab11
    __atstmplab9()
    return
  def __atstmplab9():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret3, tmp5, tmp6
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret3 = arg1
    return
  def __atstmplab10():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret3, tmp5, tmp6
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab11()
    return
  def __atstmplab11():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret3, tmp5, tmp6
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp5 = arg0[1]
    tmp6 = ats2pypre_add_int1_int1(arg1, 1)
    #ATStailcalseq_beg
    apy0 = tmp5
    apy1 = tmp6
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_list_loop_3
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab8, 2: __atstmplab9, 3: __atstmplab10, 4: __atstmplab11 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_3
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret3


def ats2pypre_list_vt_snoc(arg0, arg1):
  tmpret7 = None
  tmp8 = None
  tmp9 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_vt_snoc
  tmp9 = None
  tmp8 = (arg1, tmp9)
  tmpret7 = ats2pypre_list_vt_append(arg0, tmp8)
  return tmpret7


def ats2pypre_list_vt_extend(arg0, arg1):
  tmpret10 = None
  tmp11 = None
  tmp12 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_vt_extend
  tmp12 = None
  tmp11 = (arg1, tmp12)
  tmpret10 = ats2pypre_list_vt_append(arg0, tmp11)
  return tmpret10


def ats2pypre_list_vt_append(arg0, arg1):
  tmpret13 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_vt_append
  tmpret13 = _ats2pypre_list_aux_7(arg0, arg1)
  return tmpret13


def _ats2pypre_list_aux_7(arg0, arg1):
  tmpret14 = None
  tmp15 = None
  tmp16 = None
  tmp17 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab12():
    nonlocal arg0, arg1
    nonlocal tmpret14, tmp15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab15
    __atstmplab13()
    return
  def __atstmplab13():
    nonlocal arg0, arg1
    nonlocal tmpret14, tmp15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret14 = arg1
    return
  def __atstmplab14():
    nonlocal arg0, arg1
    nonlocal tmpret14, tmp15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab15()
    return
  def __atstmplab15():
    nonlocal arg0, arg1
    nonlocal tmpret14, tmp15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp15 = arg0[0]
    tmp16 = arg0[1]
    #ATSINSfreecon(arg0);
    tmp17 = _ats2pypre_list_aux_7(tmp16, arg1)
    tmpret14 = (tmp15, tmp17)
    return
  mbranch_1 = { 1: __atstmplab12, 2: __atstmplab13, 3: __atstmplab14, 4: __atstmplab15 }
  #__patsflab__ats2pypre_list_aux_7
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret14


def ats2pypre_list_vt_reverse(arg0):
  tmpret18 = None
  tmp19 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_vt_reverse
  tmp19 = None
  tmpret18 = ats2pypre_list_vt_reverse_append(arg0, tmp19)
  return tmpret18


def ats2pypre_list_vt_reverse_append(arg0, arg1):
  tmpret20 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list_vt_reverse_append
  tmpret20 = _ats2pypre_list_loop_10(arg0, arg1)
  return tmpret20


def _ats2pypre_list_loop_10(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret21 = None
  tmp22 = None
  tmp23 = None
  tmp24 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab16():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp23, tmp24
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab19
    __atstmplab17()
    return
  def __atstmplab17():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp23, tmp24
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret21 = arg1
    return
  def __atstmplab18():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp23, tmp24
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab19()
    return
  def __atstmplab19():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp23, tmp24
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp22 = arg0[0]
    tmp23 = arg0[1]
    #ATSINSfreecon(arg0);
    tmp24 = (tmp22, arg1)
    #ATStailcalseq_beg
    apy0 = tmp23
    apy1 = tmp24
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_list_loop_10
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab16, 2: __atstmplab17, 3: __atstmplab18, 4: __atstmplab19 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_list_loop_10
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret21

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def ats2pypre_option_some(arg0):
  tmpret0 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_option_some
  tmpret0 = (arg0, )
  return tmpret0


def ats2pypre_option_none():
  tmpret1 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_option_none
  tmpret1 = None
  return tmpret1


def ats2pypre_option_unsome(arg0):
  tmpret2 = None
  tmp3 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_option_unsome
  tmp3 = arg0[0]
  tmpret2 = tmp3
  return tmpret2


def ats2pypre_option_is_some(arg0):
  tmpret4 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab0():
    nonlocal arg0
    nonlocal tmpret4
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptrisnull(arg0)): tmplab_py = 4 ; return#__atstmplab3
    __atstmplab1()
    return
  def __atstmplab1():
    nonlocal arg0
    nonlocal tmpret4
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret4 = True
    return
  def __atstmplab2():
    nonlocal arg0
    nonlocal tmpret4
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab3()
    return
  def __atstmplab3():
    nonlocal arg0
    nonlocal tmpret4
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret4 = False
    return
  mbranch_1 = { 1: __atstmplab0, 2: __atstmplab1, 3: __atstmplab2, 4: __atstmplab3 }
  #__patsflab_option_is_some
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret4


def ats2pypre_option_is_none(arg0):
  tmpret5 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab4():
    nonlocal arg0
    nonlocal tmpret5
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab7
    __atstmplab5()
    return
  def __atstmplab5():
    nonlocal arg0
    nonlocal tmpret5
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret5 = True
    return
  def __atstmplab6():
    nonlocal arg0
    nonlocal tmpret5
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab7()
    return
  def __atstmplab7():
    nonlocal arg0
    nonlocal tmpret5
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret5 = False
    return
  mbranch_1 = { 1: __atstmplab4, 2: __atstmplab5, 3: __atstmplab6, 4: __atstmplab7 }
  #__patsflab_option_is_none
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret5

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def _ats2pypre_stream_patsfun_6__closurerize(env0):
  def _ats2pypre_stream_patsfun_6__cfun(cenv): return _ats2pypre_stream_patsfun_6(cenv[1])
  return (_ats2pypre_stream_patsfun_6__cfun, env0)

def _ats2pypre_stream_patsfun_16__closurerize(env0, env1):
  def _ats2pypre_stream_patsfun_16__cfun(cenv, arg0): return _ats2pypre_stream_patsfun_16(cenv[1], cenv[2], arg0)
  return (_ats2pypre_stream_patsfun_16__cfun, env0, env1)

def _ats2pypre_stream_patsfun_22__closurerize(env0, env1):
  def _ats2pypre_stream_patsfun_22__cfun(cenv): return _ats2pypre_stream_patsfun_22(cenv[1], cenv[2])
  return (_ats2pypre_stream_patsfun_22__cfun, env0, env1)

def _ats2pypre_stream_patsfun_24__closurerize(env0):
  def _ats2pypre_stream_patsfun_24__cfun(cenv): return _ats2pypre_stream_patsfun_24(cenv[1])
  return (_ats2pypre_stream_patsfun_24__cfun, env0)

def _ats2pypre_stream_patsfun_26__closurerize(env0, env1):
  def _ats2pypre_stream_patsfun_26__cfun(cenv): return _ats2pypre_stream_patsfun_26(cenv[1], cenv[2])
  return (_ats2pypre_stream_patsfun_26__cfun, env0, env1)

def _ats2pypre_stream_patsfun_28__closurerize(env0):
  def _ats2pypre_stream_patsfun_28__cfun(cenv, arg0): return _ats2pypre_stream_patsfun_28(cenv[1], arg0)
  return (_ats2pypre_stream_patsfun_28__cfun, env0)

def _ats2pypre_stream_patsfun_30__closurerize(env0, env1):
  def _ats2pypre_stream_patsfun_30__cfun(cenv): return _ats2pypre_stream_patsfun_30(cenv[1], cenv[2])
  return (_ats2pypre_stream_patsfun_30__cfun, env0, env1)

def _ats2pypre_stream_patsfun_32__closurerize(env0):
  def _ats2pypre_stream_patsfun_32__cfun(cenv, arg0): return _ats2pypre_stream_patsfun_32(cenv[1], arg0)
  return (_ats2pypre_stream_patsfun_32__cfun, env0)

def _ats2pypre_stream_patsfun_35__closurerize(env0):
  def _ats2pypre_stream_patsfun_35__cfun(cenv, arg0): return _ats2pypre_stream_patsfun_35(cenv[1], arg0)
  return (_ats2pypre_stream_patsfun_35__cfun, env0)

def _ats2pypre_stream_patsfun_38__closurerize(env0):
  def _ats2pypre_stream_patsfun_38__cfun(cenv, arg0): return _ats2pypre_stream_patsfun_38(cenv[1], arg0)
  return (_ats2pypre_stream_patsfun_38__cfun, env0)

def _ats2pypre_stream_patsfun_41__closurerize(env0):
  def _ats2pypre_stream_patsfun_41__cfun(cenv, arg0): return _ats2pypre_stream_patsfun_41(cenv[1], arg0)
  return (_ats2pypre_stream_patsfun_41__cfun, env0)

def _ats2pypre_stream_patsfun_45__closurerize(env0):
  def _ats2pypre_stream_patsfun_45__cfun(cenv, arg0): return _ats2pypre_stream_patsfun_45(cenv[1], arg0)
  return (_ats2pypre_stream_patsfun_45__cfun, env0)

def _ats2pypre_stream_patsfun_48__closurerize(env0, env1):
  def _ats2pypre_stream_patsfun_48__cfun(cenv): return _ats2pypre_stream_patsfun_48(cenv[1], cenv[2])
  return (_ats2pypre_stream_patsfun_48__cfun, env0, env1)

def _ats2pypre_stream_patsfun_51__closurerize(env0, env1, env2, env3):
  def _ats2pypre_stream_patsfun_51__cfun(cenv): return _ats2pypre_stream_patsfun_51(cenv[1], cenv[2], cenv[3], cenv[4])
  return (_ats2pypre_stream_patsfun_51__cfun, env0, env1, env2, env3)

def _ats2pypre_stream_patsfun_52__closurerize(env0, env1):
  def _ats2pypre_stream_patsfun_52__cfun(cenv): return _ats2pypre_stream_patsfun_52(cenv[1], cenv[2])
  return (_ats2pypre_stream_patsfun_52__cfun, env0, env1)

def _ats2pypre_stream_patsfun_55__closurerize(env0):
  def _ats2pypre_stream_patsfun_55__cfun(cenv): return _ats2pypre_stream_patsfun_55(cenv[1])
  return (_ats2pypre_stream_patsfun_55__cfun, env0)

def _ats2pypre_stream_patsfun_57__closurerize(env0):
  def _ats2pypre_stream_patsfun_57__cfun(cenv): return _ats2pypre_stream_patsfun_57(cenv[1])
  return (_ats2pypre_stream_patsfun_57__cfun, env0)

def _ats2pypre_stream_patsfun_59__closurerize(env0, env1):
  def _ats2pypre_stream_patsfun_59__cfun(cenv): return _ats2pypre_stream_patsfun_59(cenv[1], cenv[2])
  return (_ats2pypre_stream_patsfun_59__cfun, env0, env1)

def _ats2pypre_stream_patsfun_64__closurerize(env0):
  def _ats2pypre_stream_patsfun_64__cfun(cenv, arg0, arg1): return _ats2pypre_stream_patsfun_64(cenv[1], arg0, arg1)
  return (_ats2pypre_stream_patsfun_64__cfun, env0)

def _ats2pypre_stream_patsfun_66__closurerize(env0):
  def _ats2pypre_stream_patsfun_66__cfun(cenv, arg0, arg1): return _ats2pypre_stream_patsfun_66(cenv[1], arg0, arg1)
  return (_ats2pypre_stream_patsfun_66__cfun, env0)

def _ats2pypre_stream_patsfun_69__closurerize(env0, env1):
  def _ats2pypre_stream_patsfun_69__cfun(cenv): return _ats2pypre_stream_patsfun_69(cenv[1], cenv[2])
  return (_ats2pypre_stream_patsfun_69__cfun, env0, env1)

def _ats2pypre_stream_patsfun_71__closurerize(env0, env1):
  def _ats2pypre_stream_patsfun_71__cfun(cenv): return _ats2pypre_stream_patsfun_71(cenv[1], cenv[2])
  return (_ats2pypre_stream_patsfun_71__cfun, env0, env1)

def ats2pypre_stream_make_list(arg0):
  tmpret7 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_make_list
  tmpret7 = [0, _ats2pypre_stream_patsfun_6__closurerize(arg0)]
  return tmpret7


def _ats2pypre_stream_patsfun_6(env0):
  tmpret8 = None
  tmp9 = None
  tmp10 = None
  tmp11 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab0():
    nonlocal env0
    nonlocal tmpret8, tmp9, tmp10, tmp11
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(env0)): tmplab_py = 4 ; return#__atstmplab3
    __atstmplab1()
    return
  def __atstmplab1():
    nonlocal env0
    nonlocal tmpret8, tmp9, tmp10, tmp11
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret8 = None
    return
  def __atstmplab2():
    nonlocal env0
    nonlocal tmpret8, tmp9, tmp10, tmp11
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab3()
    return
  def __atstmplab3():
    nonlocal env0
    nonlocal tmpret8, tmp9, tmp10, tmp11
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp9 = env0[0]
    tmp10 = env0[1]
    tmp11 = ats2pypre_stream_make_list(tmp10)
    tmpret8 = (tmp9, tmp11)
    return
  mbranch_1 = { 1: __atstmplab0, 2: __atstmplab1, 3: __atstmplab2, 4: __atstmplab3 }
  #__patsflab__ats2pypre_stream_patsfun_6
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret8


def ats2pypre_stream_make_list0(arg0):
  tmpret12 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_make_list0
  tmpret12 = ats2pypre_stream_make_list(arg0)
  return tmpret12


def ats2pypre_stream_nth_opt(arg0, arg1):
  tmpret13 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_nth_opt
  tmpret13 = _ats2pypre_stream_loop_9(arg0, arg1)
  return tmpret13


def _ats2pypre_stream_loop_9(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret14 = None
  tmp15 = None
  tmp16 = None
  tmp17 = None
  tmp18 = None
  tmp19 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab4():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret14, tmp15, tmp16, tmp17, tmp18, tmp19
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp15)): tmplab_py = 4 ; return#__atstmplab7
    __atstmplab5()
    return
  def __atstmplab5():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret14, tmp15, tmp16, tmp17, tmp18, tmp19
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret14 = None
    return
  def __atstmplab6():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret14, tmp15, tmp16, tmp17, tmp18, tmp19
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab7()
    return
  def __atstmplab7():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret14, tmp15, tmp16, tmp17, tmp18, tmp19
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp16 = tmp15[0]
    tmp17 = tmp15[1]
    tmp18 = ats2pypre_gt_int1_int1(arg1, 0)
    if (tmp18):
      tmp19 = ats2pypre_pred_int1(arg1)
      #ATStailcalseq_beg
      apy0 = tmp17
      apy1 = tmp19
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab__ats2pypre_stream_loop_9
      #ATStailcalseq_end
    else:
      tmpret14 = (tmp16, )
    #endif
    return
  mbranch_1 = { 1: __atstmplab4, 2: __atstmplab5, 3: __atstmplab6, 4: __atstmplab7 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_loop_9
    tmp15 = ATSPMVlazyval_eval(arg0); 
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret14


def ats2pypre_stream_length(arg0):
  tmpret20 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_length
  tmpret20 = _ats2pypre_stream_loop_11(arg0, 0)
  return tmpret20


def _ats2pypre_stream_loop_11(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret21 = None
  tmp22 = None
  tmp24 = None
  tmp25 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab8():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp24, tmp25
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp22)): tmplab_py = 4 ; return#__atstmplab11
    __atstmplab9()
    return
  def __atstmplab9():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp24, tmp25
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret21 = arg1
    return
  def __atstmplab10():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp24, tmp25
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab11()
    return
  def __atstmplab11():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp24, tmp25
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp24 = tmp22[1]
    tmp25 = ats2pypre_add_int1_int1(arg1, 1)
    #ATStailcalseq_beg
    apy0 = tmp24
    apy1 = tmp25
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_stream_loop_11
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab8, 2: __atstmplab9, 3: __atstmplab10, 4: __atstmplab11 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_loop_11
    tmp22 = ATSPMVlazyval_eval(arg0); 
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret21


def ats2pypre_stream2list(arg0):
  tmpret26 = None
  tmp27 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream2list
  tmp27 = ats2pypre_stream2list_rev(arg0)
  tmpret26 = ats2pypre_list_reverse(tmp27)
  return tmpret26


def ats2pypre_stream2list_rev(arg0):
  tmpret28 = None
  tmp34 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream2list_rev
  tmp34 = None
  tmpret28 = _ats2pypre_stream_loop_14(arg0, tmp34)
  return tmpret28


def _ats2pypre_stream_loop_14(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret29 = None
  tmp30 = None
  tmp31 = None
  tmp32 = None
  tmp33 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab12():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret29, tmp30, tmp31, tmp32, tmp33
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp30)): tmplab_py = 4 ; return#__atstmplab15
    __atstmplab13()
    return
  def __atstmplab13():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret29, tmp30, tmp31, tmp32, tmp33
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret29 = arg1
    return
  def __atstmplab14():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret29, tmp30, tmp31, tmp32, tmp33
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab15()
    return
  def __atstmplab15():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret29, tmp30, tmp31, tmp32, tmp33
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp31 = tmp30[0]
    tmp32 = tmp30[1]
    tmp33 = (tmp31, arg1)
    #ATStailcalseq_beg
    apy0 = tmp32
    apy1 = tmp33
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_stream_loop_14
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab12, 2: __atstmplab13, 3: __atstmplab14, 4: __atstmplab15 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_loop_14
    tmp30 = ATSPMVlazyval_eval(arg0); 
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret29


def ats2pypre_stream_takeLte(arg0, arg1):
  tmpret35 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_takeLte
  tmpret35 = _ats2pypre_stream_patsfun_16__closurerize(arg0, arg1)
  return tmpret35


def _ats2pypre_stream_patsfun_16(env0, env1, arg0):
  tmpret36 = None
  tmp37 = None
  tmp38 = None
  tmp39 = None
  tmp40 = None
  tmp41 = None
  tmp42 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab16():
    nonlocal env0, env1, arg0
    nonlocal tmpret36, tmp37, tmp38, tmp39, tmp40, tmp41, tmp42
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp38)): tmplab_py = 4 ; return#__atstmplab19
    __atstmplab17()
    return
  def __atstmplab17():
    nonlocal env0, env1, arg0
    nonlocal tmpret36, tmp37, tmp38, tmp39, tmp40, tmp41, tmp42
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret36 = None
    return
  def __atstmplab18():
    nonlocal env0, env1, arg0
    nonlocal tmpret36, tmp37, tmp38, tmp39, tmp40, tmp41, tmp42
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab19()
    return
  def __atstmplab19():
    nonlocal env0, env1, arg0
    nonlocal tmpret36, tmp37, tmp38, tmp39, tmp40, tmp41, tmp42
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp39 = tmp38[0]
    tmp40 = tmp38[1]
    tmp42 = ats2pypre_sub_int1_int1(env1, 1)
    tmp41 = ats2pypre_stream_takeLte(tmp40, tmp42)
    tmpret36 = (tmp39, tmp41)
    return
  mbranch_1 = { 1: __atstmplab16, 2: __atstmplab17, 3: __atstmplab18, 4: __atstmplab19 }
  #__patsflab__ats2pypre_stream_patsfun_16
  if (arg0):
    tmp37 = ats2pypre_gt_int1_int1(env1, 0)
    if (tmp37):
      tmp38 = ATSPMVlazyval_eval(env0); 
      #ATScaseofseq_beg
      tmplab_py = 1
      while(1):
        mbranch_1.get(tmplab_py)()
        if (tmplab_py == 0): break
      #ATScaseofseq_end
    else:
      tmpret36 = None
    #endif
  #endif
  return tmpret36


def ats2pypre_stream_take_opt(arg0, arg1):
  tmpret43 = None
  tmp52 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_take_opt
  tmp52 = None
  tmpret43 = _ats2pypre_stream_auxmain_18(arg1, arg0, 0, tmp52)
  return tmpret43


def _ats2pypre_stream_auxmain_18(env0, arg0, arg1, arg2):
  apy0 = None
  apy1 = None
  apy2 = None
  tmpret44 = None
  tmp45 = None
  tmp46 = None
  tmp47 = None
  tmp48 = None
  tmp49 = None
  tmp50 = None
  tmp51 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab20():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret44, tmp45, tmp46, tmp47, tmp48, tmp49, tmp50, tmp51
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp46)): tmplab_py = 4 ; return#__atstmplab23
    __atstmplab21()
    return
  def __atstmplab21():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret44, tmp45, tmp46, tmp47, tmp48, tmp49, tmp50, tmp51
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret44 = None
    return
  def __atstmplab22():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret44, tmp45, tmp46, tmp47, tmp48, tmp49, tmp50, tmp51
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab23()
    return
  def __atstmplab23():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret44, tmp45, tmp46, tmp47, tmp48, tmp49, tmp50, tmp51
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp47 = tmp46[0]
    tmp48 = tmp46[1]
    tmp49 = ats2pypre_add_int1_int1(arg1, 1)
    tmp50 = (tmp47, arg2)
    #ATStailcalseq_beg
    apy0 = tmp48
    apy1 = tmp49
    apy2 = tmp50
    arg0 = apy0
    arg1 = apy1
    arg2 = apy2
    funlab_py = 1 #__patsflab__ats2pypre_stream_auxmain_18
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab20, 2: __atstmplab21, 3: __atstmplab22, 4: __atstmplab23 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_auxmain_18
    tmp45 = ats2pypre_lt_int1_int1(arg1, env0)
    if (tmp45):
      tmp46 = ATSPMVlazyval_eval(arg0); 
      #ATScaseofseq_beg
      tmplab_py = 1
      while(1):
        mbranch_1.get(tmplab_py)()
        if (tmplab_py == 0): break
      #ATScaseofseq_end
    else:
      tmp51 = ats2pypre_list_reverse(arg2)
      tmpret44 = (tmp51, )
    #endif
    if (funlab_py == 0): break
  return tmpret44


def ats2pypre_stream_drop_opt(arg0, arg1):
  tmpret53 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_drop_opt
  tmpret53 = _ats2pypre_stream_auxmain_20(arg1, arg0, 0)
  return tmpret53


def _ats2pypre_stream_auxmain_20(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret54 = None
  tmp55 = None
  tmp56 = None
  tmp58 = None
  tmp59 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab24():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret54, tmp55, tmp56, tmp58, tmp59
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp56)): tmplab_py = 4 ; return#__atstmplab27
    __atstmplab25()
    return
  def __atstmplab25():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret54, tmp55, tmp56, tmp58, tmp59
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret54 = None
    return
  def __atstmplab26():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret54, tmp55, tmp56, tmp58, tmp59
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab27()
    return
  def __atstmplab27():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret54, tmp55, tmp56, tmp58, tmp59
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp58 = tmp56[1]
    tmp59 = ats2pypre_add_int1_int1(arg1, 1)
    #ATStailcalseq_beg
    apy0 = tmp58
    apy1 = tmp59
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_stream_auxmain_20
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab24, 2: __atstmplab25, 3: __atstmplab26, 4: __atstmplab27 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_auxmain_20
    tmp55 = ats2pypre_lt_int1_int1(arg1, env0)
    if (tmp55):
      tmp56 = ATSPMVlazyval_eval(arg0); 
      #ATScaseofseq_beg
      tmplab_py = 1
      while(1):
        mbranch_1.get(tmplab_py)()
        if (tmplab_py == 0): break
      #ATScaseofseq_end
    else:
      tmpret54 = (arg0, )
    #endif
    if (funlab_py == 0): break
  return tmpret54


def ats2pypre_stream_append(arg0, arg1):
  tmpret60 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_append
  tmpret60 = [0, _ats2pypre_stream_patsfun_22__closurerize(arg0, arg1)]
  return tmpret60


def _ats2pypre_stream_patsfun_22(env0, env1):
  tmpret61 = None
  tmp62 = None
  tmp63 = None
  tmp64 = None
  tmp65 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab28():
    nonlocal env0, env1
    nonlocal tmpret61, tmp62, tmp63, tmp64, tmp65
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp62)): tmplab_py = 4 ; return#__atstmplab31
    __atstmplab29()
    return
  def __atstmplab29():
    nonlocal env0, env1
    nonlocal tmpret61, tmp62, tmp63, tmp64, tmp65
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret61 = ATSPMVlazyval_eval(env1); 
    return
  def __atstmplab30():
    nonlocal env0, env1
    nonlocal tmpret61, tmp62, tmp63, tmp64, tmp65
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab31()
    return
  def __atstmplab31():
    nonlocal env0, env1
    nonlocal tmpret61, tmp62, tmp63, tmp64, tmp65
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp63 = tmp62[0]
    tmp64 = tmp62[1]
    tmp65 = ats2pypre_stream_append(tmp64, env1)
    tmpret61 = (tmp63, tmp65)
    return
  mbranch_1 = { 1: __atstmplab28, 2: __atstmplab29, 3: __atstmplab30, 4: __atstmplab31 }
  #__patsflab__ats2pypre_stream_patsfun_22
  tmp62 = ATSPMVlazyval_eval(env0); 
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret61


def ats2pypre_stream_concat(arg0):
  tmpret66 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_concat
  tmpret66 = [0, _ats2pypre_stream_patsfun_24__closurerize(arg0)]
  return tmpret66


def _ats2pypre_stream_patsfun_24(env0):
  tmpret67 = None
  tmp68 = None
  tmp69 = None
  tmp70 = None
  tmp71 = None
  tmp72 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab32():
    nonlocal env0
    nonlocal tmpret67, tmp68, tmp69, tmp70, tmp71, tmp72
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp68)): tmplab_py = 4 ; return#__atstmplab35
    __atstmplab33()
    return
  def __atstmplab33():
    nonlocal env0
    nonlocal tmpret67, tmp68, tmp69, tmp70, tmp71, tmp72
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret67 = None
    return
  def __atstmplab34():
    nonlocal env0
    nonlocal tmpret67, tmp68, tmp69, tmp70, tmp71, tmp72
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab35()
    return
  def __atstmplab35():
    nonlocal env0
    nonlocal tmpret67, tmp68, tmp69, tmp70, tmp71, tmp72
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp69 = tmp68[0]
    tmp70 = tmp68[1]
    tmp72 = ats2pypre_stream_concat(tmp70)
    tmp71 = ats2pypre_stream_append(tmp69, tmp72)
    tmpret67 = ATSPMVlazyval_eval(tmp71); 
    return
  mbranch_1 = { 1: __atstmplab32, 2: __atstmplab33, 3: __atstmplab34, 4: __atstmplab35 }
  #__patsflab__ats2pypre_stream_patsfun_24
  tmp68 = ATSPMVlazyval_eval(env0); 
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret67


def ats2pypre_stream_map_cloref(arg0, arg1):
  tmpret73 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_map_cloref
  tmpret73 = [0, _ats2pypre_stream_patsfun_26__closurerize(arg0, arg1)]
  return tmpret73


def _ats2pypre_stream_patsfun_26(env0, env1):
  tmpret74 = None
  tmp75 = None
  tmp76 = None
  tmp77 = None
  tmp78 = None
  tmp79 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab36():
    nonlocal env0, env1
    nonlocal tmpret74, tmp75, tmp76, tmp77, tmp78, tmp79
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp75)): tmplab_py = 4 ; return#__atstmplab39
    __atstmplab37()
    return
  def __atstmplab37():
    nonlocal env0, env1
    nonlocal tmpret74, tmp75, tmp76, tmp77, tmp78, tmp79
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret74 = None
    return
  def __atstmplab38():
    nonlocal env0, env1
    nonlocal tmpret74, tmp75, tmp76, tmp77, tmp78, tmp79
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab39()
    return
  def __atstmplab39():
    nonlocal env0, env1
    nonlocal tmpret74, tmp75, tmp76, tmp77, tmp78, tmp79
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp76 = tmp75[0]
    tmp77 = tmp75[1]
    tmp78 = env1[0](env1, tmp76)
    tmp79 = ats2pypre_stream_map_cloref(tmp77, env1)
    tmpret74 = (tmp78, tmp79)
    return
  mbranch_1 = { 1: __atstmplab36, 2: __atstmplab37, 3: __atstmplab38, 4: __atstmplab39 }
  #__patsflab__ats2pypre_stream_patsfun_26
  tmp75 = ATSPMVlazyval_eval(env0); 
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret74


def ats2pypre_stream_map_method(arg0, arg1):
  tmpret80 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_map_method
  tmpret80 = _ats2pypre_stream_patsfun_28__closurerize(arg0)
  return tmpret80


def _ats2pypre_stream_patsfun_28(env0, arg0):
  tmpret81 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_28
  tmpret81 = ats2pypre_stream_map_cloref(env0, arg0)
  return tmpret81


def ats2pypre_stream_filter_cloref(arg0, arg1):
  tmpret82 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_filter_cloref
  tmpret82 = [0, _ats2pypre_stream_patsfun_30__closurerize(arg0, arg1)]
  return tmpret82


def _ats2pypre_stream_patsfun_30(env0, env1):
  tmpret83 = None
  tmp84 = None
  tmp85 = None
  tmp86 = None
  tmp87 = None
  tmp88 = None
  tmp89 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab40():
    nonlocal env0, env1
    nonlocal tmpret83, tmp84, tmp85, tmp86, tmp87, tmp88, tmp89
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp84)): tmplab_py = 4 ; return#__atstmplab43
    __atstmplab41()
    return
  def __atstmplab41():
    nonlocal env0, env1
    nonlocal tmpret83, tmp84, tmp85, tmp86, tmp87, tmp88, tmp89
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret83 = None
    return
  def __atstmplab42():
    nonlocal env0, env1
    nonlocal tmpret83, tmp84, tmp85, tmp86, tmp87, tmp88, tmp89
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab43()
    return
  def __atstmplab43():
    nonlocal env0, env1
    nonlocal tmpret83, tmp84, tmp85, tmp86, tmp87, tmp88, tmp89
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp85 = tmp84[0]
    tmp86 = tmp84[1]
    tmp87 = env1[0](env1, tmp85)
    if (tmp87):
      tmp88 = ats2pypre_stream_filter_cloref(tmp86, env1)
      tmpret83 = (tmp85, tmp88)
    else:
      tmp89 = ats2pypre_stream_filter_cloref(tmp86, env1)
      tmpret83 = ATSPMVlazyval_eval(tmp89); 
    #endif
    return
  mbranch_1 = { 1: __atstmplab40, 2: __atstmplab41, 3: __atstmplab42, 4: __atstmplab43 }
  #__patsflab__ats2pypre_stream_patsfun_30
  tmp84 = ATSPMVlazyval_eval(env0); 
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret83


def ats2pypre_stream_filter_method(arg0):
  tmpret90 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_filter_method
  tmpret90 = _ats2pypre_stream_patsfun_32__closurerize(arg0)
  return tmpret90


def _ats2pypre_stream_patsfun_32(env0, arg0):
  tmpret91 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_32
  tmpret91 = ats2pypre_stream_filter_cloref(env0, arg0)
  return tmpret91


def ats2pypre_stream_forall_cloref(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret92 = None
  tmp93 = None
  tmp94 = None
  tmp95 = None
  tmp96 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab44():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret92, tmp93, tmp94, tmp95, tmp96
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp93)): tmplab_py = 4 ; return#__atstmplab47
    __atstmplab45()
    return
  def __atstmplab45():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret92, tmp93, tmp94, tmp95, tmp96
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret92 = True
    return
  def __atstmplab46():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret92, tmp93, tmp94, tmp95, tmp96
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab47()
    return
  def __atstmplab47():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret92, tmp93, tmp94, tmp95, tmp96
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp94 = tmp93[0]
    tmp95 = tmp93[1]
    tmp96 = arg1[0](arg1, tmp94)
    if (tmp96):
      #ATStailcalseq_beg
      apy0 = tmp95
      apy1 = arg1
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab_stream_forall_cloref
      #ATStailcalseq_end
    else:
      tmpret92 = False
    #endif
    return
  mbranch_1 = { 1: __atstmplab44, 2: __atstmplab45, 3: __atstmplab46, 4: __atstmplab47 }
  while(1):
    funlab_py = 0
    #__patsflab_stream_forall_cloref
    tmp93 = ATSPMVlazyval_eval(arg0); 
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret92


def ats2pypre_stream_forall_method(arg0):
  tmpret97 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_forall_method
  tmpret97 = _ats2pypre_stream_patsfun_35__closurerize(arg0)
  return tmpret97


def _ats2pypre_stream_patsfun_35(env0, arg0):
  tmpret98 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_35
  tmpret98 = ats2pypre_stream_forall_cloref(env0, arg0)
  return tmpret98


def ats2pypre_stream_exists_cloref(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret99 = None
  tmp100 = None
  tmp101 = None
  tmp102 = None
  tmp103 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab48():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret99, tmp100, tmp101, tmp102, tmp103
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp100)): tmplab_py = 4 ; return#__atstmplab51
    __atstmplab49()
    return
  def __atstmplab49():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret99, tmp100, tmp101, tmp102, tmp103
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret99 = False
    return
  def __atstmplab50():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret99, tmp100, tmp101, tmp102, tmp103
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab51()
    return
  def __atstmplab51():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret99, tmp100, tmp101, tmp102, tmp103
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp101 = tmp100[0]
    tmp102 = tmp100[1]
    tmp103 = arg1[0](arg1, tmp101)
    if (tmp103):
      tmpret99 = True
    else:
      #ATStailcalseq_beg
      apy0 = tmp102
      apy1 = arg1
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab_stream_exists_cloref
      #ATStailcalseq_end
    #endif
    return
  mbranch_1 = { 1: __atstmplab48, 2: __atstmplab49, 3: __atstmplab50, 4: __atstmplab51 }
  while(1):
    funlab_py = 0
    #__patsflab_stream_exists_cloref
    tmp100 = ATSPMVlazyval_eval(arg0); 
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret99


def ats2pypre_stream_exists_method(arg0):
  tmpret104 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_exists_method
  tmpret104 = _ats2pypre_stream_patsfun_38__closurerize(arg0)
  return tmpret104


def _ats2pypre_stream_patsfun_38(env0, arg0):
  tmpret105 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_38
  tmpret105 = ats2pypre_stream_exists_cloref(env0, arg0)
  return tmpret105


def ats2pypre_stream_foreach_cloref(arg0, arg1):
  apy0 = None
  apy1 = None
  tmp107 = None
  tmp108 = None
  tmp109 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab52():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmp107, tmp108, tmp109
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp107)): tmplab_py = 4 ; return#__atstmplab55
    __atstmplab53()
    return
  def __atstmplab53():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmp107, tmp108, tmp109
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    None#ATSINSmove_void
    return
  def __atstmplab54():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmp107, tmp108, tmp109
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab55()
    return
  def __atstmplab55():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmp107, tmp108, tmp109
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp108 = tmp107[0]
    tmp109 = tmp107[1]
    arg1[0](arg1, tmp108)
    #ATStailcalseq_beg
    apy0 = tmp109
    apy1 = arg1
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab_stream_foreach_cloref
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab52, 2: __atstmplab53, 3: __atstmplab54, 4: __atstmplab55 }
  while(1):
    funlab_py = 0
    #__patsflab_stream_foreach_cloref
    tmp107 = ATSPMVlazyval_eval(arg0); 
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return#_void


def ats2pypre_stream_foreach_method(arg0):
  tmpret111 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_foreach_method
  tmpret111 = _ats2pypre_stream_patsfun_41__closurerize(arg0)
  return tmpret111


def _ats2pypre_stream_patsfun_41(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_41
  ats2pypre_stream_foreach_cloref(env0, arg0)
  return#_void


def ats2pypre_stream_iforeach_cloref(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_iforeach_cloref
  _ats2pypre_stream_loop_43(arg1, 0, arg0)
  return#_void


def _ats2pypre_stream_loop_43(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmp115 = None
  tmp116 = None
  tmp117 = None
  tmp119 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab56():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp115, tmp116, tmp117, tmp119
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp115)): tmplab_py = 4 ; return#__atstmplab59
    __atstmplab57()
    return
  def __atstmplab57():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp115, tmp116, tmp117, tmp119
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    None#ATSINSmove_void
    return
  def __atstmplab58():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp115, tmp116, tmp117, tmp119
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab59()
    return
  def __atstmplab59():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp115, tmp116, tmp117, tmp119
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp116 = tmp115[0]
    tmp117 = tmp115[1]
    env0[0](env0, arg0, tmp116)
    tmp119 = ats2pypre_add_int1_int1(arg0, 1)
    #ATStailcalseq_beg
    apy0 = tmp119
    apy1 = tmp117
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_stream_loop_43
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab56, 2: __atstmplab57, 3: __atstmplab58, 4: __atstmplab59 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_loop_43
    tmp115 = ATSPMVlazyval_eval(arg1); 
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return#_void


def ats2pypre_stream_iforeach_method(arg0):
  tmpret120 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_iforeach_method
  tmpret120 = _ats2pypre_stream_patsfun_45__closurerize(arg0)
  return tmpret120


def _ats2pypre_stream_patsfun_45(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_45
  ats2pypre_stream_iforeach_cloref(env0, arg0)
  return#_void


def ats2pypre_stream_tabulate_cloref(arg0):
  tmpret122 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_tabulate_cloref
  tmpret122 = _ats2pypre_stream_auxmain_47(arg0, 0)
  return tmpret122


def _ats2pypre_stream_auxmain_47(env0, arg0):
  tmpret123 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_auxmain_47
  tmpret123 = [0, _ats2pypre_stream_patsfun_48__closurerize(env0, arg0)]
  return tmpret123


def _ats2pypre_stream_patsfun_48(env0, env1):
  tmpret124 = None
  tmp125 = None
  tmp126 = None
  tmp127 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_48
  tmp125 = env0[0](env0, env1)
  tmp127 = ats2pypre_add_int1_int1(env1, 1)
  tmp126 = _ats2pypre_stream_auxmain_47(env0, tmp127)
  tmpret124 = (tmp125, tmp126)
  return tmpret124


def ats2pypre_cross_stream_list(arg0, arg1):
  tmpret128 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_cross_stream_list
  tmpret128 = [0, _ats2pypre_stream_patsfun_52__closurerize(arg0, arg1)]
  return tmpret128


def _ats2pypre_stream_auxmain_50(arg0, arg1, arg2, arg3):
  tmpret129 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_auxmain_50
  tmpret129 = [0, _ats2pypre_stream_patsfun_51__closurerize(arg0, arg1, arg2, arg3)]
  return tmpret129


def _ats2pypre_stream_patsfun_51(env0, env1, env2, env3):
  tmpret130 = None
  tmp131 = None
  tmp132 = None
  tmp133 = None
  tmp134 = None
  tmp135 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab60():
    nonlocal env0, env1, env2, env3
    nonlocal tmpret130, tmp131, tmp132, tmp133, tmp134, tmp135
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(env3)): tmplab_py = 4 ; return#__atstmplab63
    __atstmplab61()
    return
  def __atstmplab61():
    nonlocal env0, env1, env2, env3
    nonlocal tmpret130, tmp131, tmp132, tmp133, tmp134, tmp135
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp133 = ats2pypre_cross_stream_list(env1, env2)
    tmpret130 = ATSPMVlazyval_eval(tmp133); 
    return
  def __atstmplab62():
    nonlocal env0, env1, env2, env3
    nonlocal tmpret130, tmp131, tmp132, tmp133, tmp134, tmp135
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab63()
    return
  def __atstmplab63():
    nonlocal env0, env1, env2, env3
    nonlocal tmpret130, tmp131, tmp132, tmp133, tmp134, tmp135
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp131 = env3[0]
    tmp132 = env3[1]
    tmp134 = (env0, tmp131)
    tmp135 = _ats2pypre_stream_auxmain_50(env0, env1, env2, tmp132)
    tmpret130 = (tmp134, tmp135)
    return
  mbranch_1 = { 1: __atstmplab60, 2: __atstmplab61, 3: __atstmplab62, 4: __atstmplab63 }
  #__patsflab__ats2pypre_stream_patsfun_51
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret130


def _ats2pypre_stream_patsfun_52(env0, env1):
  tmpret136 = None
  tmp137 = None
  tmp138 = None
  tmp139 = None
  tmp140 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab64():
    nonlocal env0, env1
    nonlocal tmpret136, tmp137, tmp138, tmp139, tmp140
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp137)): tmplab_py = 4 ; return#__atstmplab67
    __atstmplab65()
    return
  def __atstmplab65():
    nonlocal env0, env1
    nonlocal tmpret136, tmp137, tmp138, tmp139, tmp140
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret136 = None
    return
  def __atstmplab66():
    nonlocal env0, env1
    nonlocal tmpret136, tmp137, tmp138, tmp139, tmp140
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptrisnull(tmp137)): ATSINScaseof_fail("/home/hwxi/Research/ATS-Postiats/contrib/libatscc/ATS2-0.3.2/DATS/stream.dats: 7014(line=463, offs=1) -- 7106(line=465, offs=50)");
    __atstmplab67()
    return
  def __atstmplab67():
    nonlocal env0, env1
    nonlocal tmpret136, tmp137, tmp138, tmp139, tmp140
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp138 = tmp137[0]
    tmp139 = tmp137[1]
    tmp140 = _ats2pypre_stream_auxmain_50(tmp138, tmp139, env1, env1)
    tmpret136 = ATSPMVlazyval_eval(tmp140); 
    return
  mbranch_1 = { 1: __atstmplab64, 2: __atstmplab65, 3: __atstmplab66, 4: __atstmplab67 }
  #__patsflab__ats2pypre_stream_patsfun_52
  tmp137 = ATSPMVlazyval_eval(env0); 
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret136


def ats2pypre_cross_stream_list0(arg0, arg1):
  tmpret141 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_cross_stream_list0
  tmpret141 = ats2pypre_cross_stream_list(arg0, arg1)
  return tmpret141


def ats2pypre_stream2cloref_exn(arg0):
  tmpret142 = None
  tmp143 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream2cloref_exn
  tmp143 = ats2pypre_ref(arg0)
  tmpret142 = _ats2pypre_stream_patsfun_55__closurerize(tmp143)
  return tmpret142


def _ats2pypre_stream_patsfun_55(env0):
  tmpret144 = None
  tmp145 = None
  tmp146 = None
  tmp147 = None
  tmp148 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_55
  tmp145 = ats2pypre_ref_get_elt(env0)
  tmp146 = ATSPMVlazyval_eval(tmp145); 
  if(ATSCKptrisnull(tmp146)): ATSINScaseof_fail("/home/hwxi/Research/ATS-Postiats/contrib/libatscc/ATS2-0.3.2/DATS/stream.dats: 7407(line=491, offs=5) -- 7431(line=491, offs=29)");
  tmp147 = tmp146[0]
  tmp148 = tmp146[1]
  ats2pypre_ref_set_elt(env0, tmp148)
  tmpret144 = tmp147
  return tmpret144


def ats2pypre_stream2cloref_opt(arg0):
  tmpret150 = None
  tmp151 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream2cloref_opt
  tmp151 = ats2pypre_ref(arg0)
  tmpret150 = _ats2pypre_stream_patsfun_57__closurerize(tmp151)
  return tmpret150


def _ats2pypre_stream_patsfun_57(env0):
  tmpret152 = None
  tmp153 = None
  tmp154 = None
  tmp155 = None
  tmp156 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab68():
    nonlocal env0
    nonlocal tmpret152, tmp153, tmp154, tmp155, tmp156
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp154)): tmplab_py = 4 ; return#__atstmplab71
    __atstmplab69()
    return
  def __atstmplab69():
    nonlocal env0
    nonlocal tmpret152, tmp153, tmp154, tmp155, tmp156
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret152 = None
    return
  def __atstmplab70():
    nonlocal env0
    nonlocal tmpret152, tmp153, tmp154, tmp155, tmp156
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab71()
    return
  def __atstmplab71():
    nonlocal env0
    nonlocal tmpret152, tmp153, tmp154, tmp155, tmp156
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp155 = tmp154[0]
    tmp156 = tmp154[1]
    ats2pypre_ref_set_elt(env0, tmp156)
    tmpret152 = (tmp155, )
    return
  mbranch_1 = { 1: __atstmplab68, 2: __atstmplab69, 3: __atstmplab70, 4: __atstmplab71 }
  #__patsflab__ats2pypre_stream_patsfun_57
  tmp153 = ats2pypre_ref_get_elt(env0)
  tmp154 = ATSPMVlazyval_eval(tmp153); 
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret152


def ats2pypre_stream2cloref_last(arg0, arg1):
  tmpret158 = None
  tmp159 = None
  tmp160 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream2cloref_last
  tmp159 = ats2pypre_ref(arg0)
  tmp160 = ats2pypre_ref(arg1)
  tmpret158 = _ats2pypre_stream_patsfun_59__closurerize(tmp159, tmp160)
  return tmpret158


def _ats2pypre_stream_patsfun_59(env0, env1):
  tmpret161 = None
  tmp162 = None
  tmp163 = None
  tmp164 = None
  tmp165 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab72():
    nonlocal env0, env1
    nonlocal tmpret161, tmp162, tmp163, tmp164, tmp165
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp163)): tmplab_py = 4 ; return#__atstmplab75
    __atstmplab73()
    return
  def __atstmplab73():
    nonlocal env0, env1
    nonlocal tmpret161, tmp162, tmp163, tmp164, tmp165
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret161 = ats2pypre_ref_get_elt(env1)
    return
  def __atstmplab74():
    nonlocal env0, env1
    nonlocal tmpret161, tmp162, tmp163, tmp164, tmp165
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab75()
    return
  def __atstmplab75():
    nonlocal env0, env1
    nonlocal tmpret161, tmp162, tmp163, tmp164, tmp165
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp164 = tmp163[0]
    tmp165 = tmp163[1]
    ats2pypre_ref_set_elt(env0, tmp165)
    ats2pypre_ref_set_elt(env1, tmp164)
    tmpret161 = tmp164
    return
  mbranch_1 = { 1: __atstmplab72, 2: __atstmplab73, 3: __atstmplab74, 4: __atstmplab75 }
  #__patsflab__ats2pypre_stream_patsfun_59
  tmp162 = ats2pypre_ref_get_elt(env0)
  tmp163 = ATSPMVlazyval_eval(tmp162); 
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret161


def ats2pypre_stream_take_while_cloref(arg0, arg1):
  tmpret168 = None
  tmp169 = None
  tmp170 = None
  tmp171 = None
  tmp172 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_take_while_cloref
  tmp169 = ats2pypre_stream_rtake_while_cloref(arg0, arg1)
  tmp170 = tmp169[0]
  tmp171 = tmp169[1]
  tmp172 = ats2pypre_list_reverse(tmp171)
  tmpret168 = (tmp170, tmp172)
  return tmpret168


def ats2pypre_stream_rtake_while_cloref(arg0, arg1):
  tmpret173 = None
  tmp181 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_rtake_while_cloref
  tmp181 = None
  tmpret173 = _ats2pypre_stream_loop_62(arg1, arg0, 0, tmp181)
  return tmpret173


def _ats2pypre_stream_loop_62(env0, arg0, arg1, arg2):
  apy0 = None
  apy1 = None
  apy2 = None
  tmpret174 = None
  tmp175 = None
  tmp176 = None
  tmp177 = None
  tmp178 = None
  tmp179 = None
  tmp180 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab76():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret174, tmp175, tmp176, tmp177, tmp178, tmp179, tmp180
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp175)): tmplab_py = 4 ; return#__atstmplab79
    __atstmplab77()
    return
  def __atstmplab77():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret174, tmp175, tmp176, tmp177, tmp178, tmp179, tmp180
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret174 = (arg0, arg2)
    return
  def __atstmplab78():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret174, tmp175, tmp176, tmp177, tmp178, tmp179, tmp180
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab79()
    return
  def __atstmplab79():
    nonlocal env0, arg0, arg1, arg2
    nonlocal apy0, apy1, apy2, tmpret174, tmp175, tmp176, tmp177, tmp178, tmp179, tmp180
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp176 = tmp175[0]
    tmp177 = tmp175[1]
    tmp178 = env0[0](env0, arg1, tmp176)
    if (tmp178):
      tmp179 = ats2pypre_add_int1_int1(arg1, 1)
      tmp180 = (tmp176, arg2)
      #ATStailcalseq_beg
      apy0 = tmp177
      apy1 = tmp179
      apy2 = tmp180
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      funlab_py = 1 #__patsflab__ats2pypre_stream_loop_62
      #ATStailcalseq_end
    else:
      tmpret174 = (arg0, arg2)
    #endif
    return
  mbranch_1 = { 1: __atstmplab76, 2: __atstmplab77, 3: __atstmplab78, 4: __atstmplab79 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_loop_62
    tmp175 = ATSPMVlazyval_eval(arg0); 
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret174


def ats2pypre_stream_take_until_cloref(arg0, arg1):
  tmpret182 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_take_until_cloref
  tmpret182 = ats2pypre_stream_take_while_cloref(arg0, _ats2pypre_stream_patsfun_64__closurerize(arg1))
  return tmpret182


def _ats2pypre_stream_patsfun_64(env0, arg0, arg1):
  tmpret183 = None
  tmp184 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_64
  tmp184 = env0[0](env0, arg0, arg1)
  tmpret183 = atspre_neg_bool0(tmp184)
  return tmpret183


def ats2pypre_stream_rtake_until_cloref(arg0, arg1):
  tmpret185 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_rtake_until_cloref
  tmpret185 = ats2pypre_stream_rtake_while_cloref(arg0, _ats2pypre_stream_patsfun_66__closurerize(arg1))
  return tmpret185


def _ats2pypre_stream_patsfun_66(env0, arg0, arg1):
  tmpret186 = None
  tmp187 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_patsfun_66
  tmp187 = env0[0](env0, arg0, arg1)
  tmpret186 = atspre_neg_bool0(tmp187)
  return tmpret186


def ats2pypre_stream_list_xprod2(arg0, arg1):
  tmpret188 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_list_xprod2
  tmpret188 = _ats2pypre_stream_auxlst_70(arg0, arg1)
  return tmpret188


def _ats2pypre_stream_aux_68(arg0, arg1):
  tmpret189 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_aux_68
  tmpret189 = [0, _ats2pypre_stream_patsfun_69__closurerize(arg0, arg1)]
  return tmpret189


def _ats2pypre_stream_patsfun_69(env0, env1):
  tmpret190 = None
  tmp191 = None
  tmp192 = None
  tmp193 = None
  tmp194 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab80():
    nonlocal env0, env1
    nonlocal tmpret190, tmp191, tmp192, tmp193, tmp194
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(env1)): tmplab_py = 4 ; return#__atstmplab83
    __atstmplab81()
    return
  def __atstmplab81():
    nonlocal env0, env1
    nonlocal tmpret190, tmp191, tmp192, tmp193, tmp194
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret190 = None
    return
  def __atstmplab82():
    nonlocal env0, env1
    nonlocal tmpret190, tmp191, tmp192, tmp193, tmp194
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab83()
    return
  def __atstmplab83():
    nonlocal env0, env1
    nonlocal tmpret190, tmp191, tmp192, tmp193, tmp194
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp191 = env1[0]
    tmp192 = env1[1]
    tmp193 = (env0, tmp191)
    tmp194 = _ats2pypre_stream_aux_68(env0, tmp192)
    tmpret190 = (tmp193, tmp194)
    return
  mbranch_1 = { 1: __atstmplab80, 2: __atstmplab81, 3: __atstmplab82, 4: __atstmplab83 }
  #__patsflab__ats2pypre_stream_patsfun_69
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret190


def _ats2pypre_stream_auxlst_70(arg0, arg1):
  tmpret195 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_auxlst_70
  tmpret195 = [0, _ats2pypre_stream_patsfun_71__closurerize(arg0, arg1)]
  return tmpret195


def _ats2pypre_stream_patsfun_71(env0, env1):
  tmpret196 = None
  tmp197 = None
  tmp198 = None
  tmp199 = None
  tmp200 = None
  tmp201 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab84():
    nonlocal env0, env1
    nonlocal tmpret196, tmp197, tmp198, tmp199, tmp200, tmp201
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(env0)): tmplab_py = 4 ; return#__atstmplab87
    __atstmplab85()
    return
  def __atstmplab85():
    nonlocal env0, env1
    nonlocal tmpret196, tmp197, tmp198, tmp199, tmp200, tmp201
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret196 = None
    return
  def __atstmplab86():
    nonlocal env0, env1
    nonlocal tmpret196, tmp197, tmp198, tmp199, tmp200, tmp201
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab87()
    return
  def __atstmplab87():
    nonlocal env0, env1
    nonlocal tmpret196, tmp197, tmp198, tmp199, tmp200, tmp201
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp197 = env0[0]
    tmp198 = env0[1]
    tmp200 = _ats2pypre_stream_aux_68(tmp197, env1)
    tmp201 = _ats2pypre_stream_auxlst_70(tmp198, env1)
    tmp199 = ats2pypre_stream_append(tmp200, tmp201)
    tmpret196 = ATSPMVlazyval_eval(tmp199); 
    return
  mbranch_1 = { 1: __atstmplab84, 2: __atstmplab85, 3: __atstmplab86, 4: __atstmplab87 }
  #__patsflab__ats2pypre_stream_patsfun_71
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret196

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def _ats2pypre_stream_vt_patsfun_10__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_10__cfun(cenv): return _ats2pypre_stream_vt_patsfun_10(cenv[1])
  return (_ats2pypre_stream_vt_patsfun_10__cfun, env0)

def _ats2pypre_stream_vt_patsfun_13__closurerize(env0, env1):
  def _ats2pypre_stream_vt_patsfun_13__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_13(cenv[1], cenv[2], arg0)
  return (_ats2pypre_stream_vt_patsfun_13__cfun, env0, env1)

def _ats2pypre_stream_vt_patsfun_21__closurerize(env0, env1):
  def _ats2pypre_stream_vt_patsfun_21__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_21(cenv[1], cenv[2], arg0)
  return (_ats2pypre_stream_vt_patsfun_21__cfun, env0, env1)

def _ats2pypre_stream_vt_patsfun_24__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_24__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_24(cenv[1], arg0)
  return (_ats2pypre_stream_vt_patsfun_24__cfun, env0)

def _ats2pypre_stream_vt_patsfun_27__closurerize(env0, env1):
  def _ats2pypre_stream_vt_patsfun_27__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_27(cenv[1], cenv[2], arg0)
  return (_ats2pypre_stream_vt_patsfun_27__cfun, env0, env1)

def _ats2pypre_stream_vt_patsfun_29__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_29__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_29(cenv[1], arg0)
  return (_ats2pypre_stream_vt_patsfun_29__cfun, env0)

def _ats2pypre_stream_vt_patsfun_32__closurerize(env0, env1):
  def _ats2pypre_stream_vt_patsfun_32__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_32(cenv[1], cenv[2], arg0)
  return (_ats2pypre_stream_vt_patsfun_32__cfun, env0, env1)

def _ats2pypre_stream_vt_patsfun_34__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_34__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_34(cenv[1], arg0)
  return (_ats2pypre_stream_vt_patsfun_34__cfun, env0)

def _ats2pypre_stream_vt_patsfun_37__closurerize(env0, env1):
  def _ats2pypre_stream_vt_patsfun_37__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_37(cenv[1], cenv[2], arg0)
  return (_ats2pypre_stream_vt_patsfun_37__cfun, env0, env1)

def _ats2pypre_stream_vt_patsfun_39__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_39__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_39(cenv[1], arg0)
  return (_ats2pypre_stream_vt_patsfun_39__cfun, env0)

def _ats2pypre_stream_vt_patsfun_43__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_43__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_43(cenv[1], arg0)
  return (_ats2pypre_stream_vt_patsfun_43__cfun, env0)

def _ats2pypre_stream_vt_patsfun_47__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_47__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_47(cenv[1], arg0)
  return (_ats2pypre_stream_vt_patsfun_47__cfun, env0)

def _ats2pypre_stream_vt_patsfun_51__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_51__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_51(cenv[1], arg0)
  return (_ats2pypre_stream_vt_patsfun_51__cfun, env0)

def _ats2pypre_stream_vt_patsfun_55__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_55__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_55(cenv[1], arg0)
  return (_ats2pypre_stream_vt_patsfun_55__cfun, env0)

def _ats2pypre_stream_vt_patsfun_59__closurerize(env0):
  def _ats2pypre_stream_vt_patsfun_59__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_59(cenv[1], arg0)
  return (_ats2pypre_stream_vt_patsfun_59__cfun, env0)

def _ats2pypre_stream_vt_patsfun_62__closurerize(env0, env1):
  def _ats2pypre_stream_vt_patsfun_62__cfun(cenv, arg0): return _ats2pypre_stream_vt_patsfun_62(cenv[1], cenv[2], arg0)
  return (_ats2pypre_stream_vt_patsfun_62__cfun, env0, env1)

def ats2pypre_stream_vt_free(arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_free
  atspre_lazy_vt_free(arg0)
  return#_void


def ats2pypre_stream_vt2t(arg0):
  tmpret11 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt2t
  tmpret11 = _ats2pypre_stream_vt_aux_9(arg0)
  return tmpret11


def _ats2pypre_stream_vt_aux_9(arg0):
  tmpret12 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_aux_9
  tmpret12 = [0, _ats2pypre_stream_vt_patsfun_10__closurerize(arg0)]
  return tmpret12


def _ats2pypre_stream_vt_patsfun_10(env0):
  tmpret13 = None
  tmp14 = None
  tmp15 = None
  tmp16 = None
  tmp17 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab0():
    nonlocal env0
    nonlocal tmpret13, tmp14, tmp15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp14)): tmplab_py = 4 ; return#__atstmplab3
    __atstmplab1()
    return
  def __atstmplab1():
    nonlocal env0
    nonlocal tmpret13, tmp14, tmp15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret13 = None
    return
  def __atstmplab2():
    nonlocal env0
    nonlocal tmpret13, tmp14, tmp15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab3()
    return
  def __atstmplab3():
    nonlocal env0
    nonlocal tmpret13, tmp14, tmp15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp15 = tmp14[0]
    tmp16 = tmp14[1]
    #ATSINSfreecon(tmp14);
    tmp17 = _ats2pypre_stream_vt_aux_9(tmp16)
    tmpret13 = (tmp15, tmp17)
    return
  mbranch_1 = { 1: __atstmplab0, 2: __atstmplab1, 3: __atstmplab2, 4: __atstmplab3 }
  #__patsflab__ats2pypre_stream_vt_patsfun_10
  tmp14 = ATSPMVllazyval_eval(env0)
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret13


def ats2pypre_stream_vt_takeLte(arg0, arg1):
  tmpret18 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_takeLte
  tmpret18 = _ats2pypre_stream_vt_auxmain_12(arg0, arg1)
  return tmpret18


def _ats2pypre_stream_vt_auxmain_12(arg0, arg1):
  tmpret19 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_auxmain_12
  tmpret19 = _ats2pypre_stream_vt_patsfun_13__closurerize(arg0, arg1)
  return tmpret19


def _ats2pypre_stream_vt_patsfun_13(env0, env1, arg0):
  tmpret20 = None
  tmp21 = None
  tmp22 = None
  tmp23 = None
  tmp24 = None
  tmp25 = None
  tmp26 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab4():
    nonlocal env0, env1, arg0
    nonlocal tmpret20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp22)): tmplab_py = 4 ; return#__atstmplab7
    __atstmplab5()
    return
  def __atstmplab5():
    nonlocal env0, env1, arg0
    nonlocal tmpret20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret20 = None
    return
  def __atstmplab6():
    nonlocal env0, env1, arg0
    nonlocal tmpret20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab7()
    return
  def __atstmplab7():
    nonlocal env0, env1, arg0
    nonlocal tmpret20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp23 = tmp22[0]
    tmp24 = tmp22[1]
    #ATSINSfreecon(tmp22);
    tmp26 = ats2pypre_sub_int1_int1(env1, 1)
    tmp25 = _ats2pypre_stream_vt_auxmain_12(tmp24, tmp26)
    tmpret20 = (tmp23, tmp25)
    return
  mbranch_1 = { 1: __atstmplab4, 2: __atstmplab5, 3: __atstmplab6, 4: __atstmplab7 }
  #__patsflab__ats2pypre_stream_vt_patsfun_13
  if (arg0):
    tmp21 = ats2pypre_gt_int1_int1(env1, 0)
    if (tmp21):
      tmp22 = ATSPMVllazyval_eval(env0)
      #ATScaseofseq_beg
      tmplab_py = 1
      while(1):
        mbranch_1.get(tmplab_py)()
        if (tmplab_py == 0): break
      #ATScaseofseq_end
    else:
      atspre_lazy_vt_free(env0)
      tmpret20 = None
    #endif
  else:
    atspre_lazy_vt_free(env0)
  #endif
  return tmpret20


def ats2pypre_stream_vt_length(arg0):
  tmpret29 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_length
  tmpret29 = _ats2pypre_stream_vt_loop_15(arg0, 0)
  return tmpret29


def _ats2pypre_stream_vt_loop_15(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret30 = None
  tmp31 = None
  tmp33 = None
  tmp34 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab8():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret30, tmp31, tmp33, tmp34
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp31)): tmplab_py = 4 ; return#__atstmplab11
    __atstmplab9()
    return
  def __atstmplab9():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret30, tmp31, tmp33, tmp34
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret30 = arg1
    return
  def __atstmplab10():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret30, tmp31, tmp33, tmp34
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab11()
    return
  def __atstmplab11():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret30, tmp31, tmp33, tmp34
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp33 = tmp31[1]
    #ATSINSfreecon(tmp31);
    tmp34 = ats2pypre_add_int1_int1(arg1, 1)
    #ATStailcalseq_beg
    apy0 = tmp33
    apy1 = tmp34
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_stream_vt_loop_15
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab8, 2: __atstmplab9, 3: __atstmplab10, 4: __atstmplab11 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_vt_loop_15
    tmp31 = ATSPMVllazyval_eval(arg0)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret30


def ats2pypre_stream2list_vt(arg0):
  tmpret35 = None
  tmp36 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream2list_vt
  tmp36 = ats2pypre_stream2list_vt_rev(arg0)
  tmpret35 = ats2pypre_list_vt_reverse(tmp36)
  return tmpret35


def ats2pypre_stream2list_vt_rev(arg0):
  tmpret37 = None
  tmp43 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream2list_vt_rev
  tmp43 = None
  tmpret37 = _ats2pypre_stream_vt_loop_18(arg0, tmp43)
  return tmpret37


def _ats2pypre_stream_vt_loop_18(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret38 = None
  tmp39 = None
  tmp40 = None
  tmp41 = None
  tmp42 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab12():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret38, tmp39, tmp40, tmp41, tmp42
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp39)): tmplab_py = 4 ; return#__atstmplab15
    __atstmplab13()
    return
  def __atstmplab13():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret38, tmp39, tmp40, tmp41, tmp42
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret38 = arg1
    return
  def __atstmplab14():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret38, tmp39, tmp40, tmp41, tmp42
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab15()
    return
  def __atstmplab15():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret38, tmp39, tmp40, tmp41, tmp42
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp40 = tmp39[0]
    tmp41 = tmp39[1]
    #ATSINSfreecon(tmp39);
    tmp42 = (tmp40, arg1)
    #ATStailcalseq_beg
    apy0 = tmp41
    apy1 = tmp42
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_stream_vt_loop_18
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab12, 2: __atstmplab13, 3: __atstmplab14, 4: __atstmplab15 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_vt_loop_18
    tmp39 = ATSPMVllazyval_eval(arg0)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret38


def ats2pypre_stream_vt_append(arg0, arg1):
  tmpret44 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_append
  tmpret44 = _ats2pypre_stream_vt_auxmain_20(arg0, arg1)
  return tmpret44


def _ats2pypre_stream_vt_auxmain_20(arg0, arg1):
  tmpret45 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_auxmain_20
  tmpret45 = _ats2pypre_stream_vt_patsfun_21__closurerize(arg0, arg1)
  return tmpret45


def _ats2pypre_stream_vt_patsfun_21(env0, env1, arg0):
  tmpret46 = None
  tmp47 = None
  tmp48 = None
  tmp49 = None
  tmp50 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab16():
    nonlocal env0, env1, arg0
    nonlocal tmpret46, tmp47, tmp48, tmp49, tmp50
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp47)): tmplab_py = 4 ; return#__atstmplab19
    __atstmplab17()
    return
  def __atstmplab17():
    nonlocal env0, env1, arg0
    nonlocal tmpret46, tmp47, tmp48, tmp49, tmp50
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret46 = ATSPMVllazyval_eval(env1)
    return
  def __atstmplab18():
    nonlocal env0, env1, arg0
    nonlocal tmpret46, tmp47, tmp48, tmp49, tmp50
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab19()
    return
  def __atstmplab19():
    nonlocal env0, env1, arg0
    nonlocal tmpret46, tmp47, tmp48, tmp49, tmp50
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp48 = tmp47[0]
    tmp49 = tmp47[1]
    #ATSINSfreecon(tmp47);
    tmp50 = _ats2pypre_stream_vt_auxmain_20(tmp49, env1)
    tmpret46 = (tmp48, tmp50)
    return
  mbranch_1 = { 1: __atstmplab16, 2: __atstmplab17, 3: __atstmplab18, 4: __atstmplab19 }
  #__patsflab__ats2pypre_stream_vt_patsfun_21
  if (arg0):
    tmp47 = ATSPMVllazyval_eval(env0)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
  else:
    atspre_lazy_vt_free(env0)
    atspre_lazy_vt_free(env1)
  #endif
  return tmpret46


def ats2pypre_stream_vt_concat(arg0):
  tmpret53 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_concat
  tmpret53 = _ats2pypre_stream_vt_auxmain_23(arg0)
  return tmpret53


def _ats2pypre_stream_vt_auxmain_23(arg0):
  tmpret54 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_auxmain_23
  tmpret54 = _ats2pypre_stream_vt_patsfun_24__closurerize(arg0)
  return tmpret54


def _ats2pypre_stream_vt_patsfun_24(env0, arg0):
  tmpret55 = None
  tmp56 = None
  tmp57 = None
  tmp58 = None
  tmp59 = None
  tmp60 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab20():
    nonlocal env0, arg0
    nonlocal tmpret55, tmp56, tmp57, tmp58, tmp59, tmp60
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp56)): tmplab_py = 4 ; return#__atstmplab23
    __atstmplab21()
    return
  def __atstmplab21():
    nonlocal env0, arg0
    nonlocal tmpret55, tmp56, tmp57, tmp58, tmp59, tmp60
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret55 = None
    return
  def __atstmplab22():
    nonlocal env0, arg0
    nonlocal tmpret55, tmp56, tmp57, tmp58, tmp59, tmp60
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab23()
    return
  def __atstmplab23():
    nonlocal env0, arg0
    nonlocal tmpret55, tmp56, tmp57, tmp58, tmp59, tmp60
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp57 = tmp56[0]
    tmp58 = tmp56[1]
    #ATSINSfreecon(tmp56);
    tmp60 = _ats2pypre_stream_vt_auxmain_23(tmp58)
    tmp59 = ats2pypre_stream_vt_append(tmp57, tmp60)
    tmpret55 = ATSPMVllazyval_eval(tmp59)
    return
  mbranch_1 = { 1: __atstmplab20, 2: __atstmplab21, 3: __atstmplab22, 4: __atstmplab23 }
  #__patsflab__ats2pypre_stream_vt_patsfun_24
  if (arg0):
    tmp56 = ATSPMVllazyval_eval(env0)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
  else:
    atspre_lazy_vt_free(env0)
  #endif
  return tmpret55


def ats2pypre_stream_vt_map_cloref(arg0, arg1):
  tmpret62 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_map_cloref
  tmpret62 = _ats2pypre_stream_vt_auxmain_26(arg1, arg0)
  return tmpret62


def _ats2pypre_stream_vt_auxmain_26(env0, arg0):
  tmpret63 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_auxmain_26
  tmpret63 = _ats2pypre_stream_vt_patsfun_27__closurerize(env0, arg0)
  return tmpret63


def _ats2pypre_stream_vt_patsfun_27(env0, env1, arg0):
  tmpret64 = None
  tmp65 = None
  tmp66 = None
  tmp67 = None
  tmp68 = None
  tmp69 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab24():
    nonlocal env0, env1, arg0
    nonlocal tmpret64, tmp65, tmp66, tmp67, tmp68, tmp69
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp65)): tmplab_py = 4 ; return#__atstmplab27
    __atstmplab25()
    return
  def __atstmplab25():
    nonlocal env0, env1, arg0
    nonlocal tmpret64, tmp65, tmp66, tmp67, tmp68, tmp69
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret64 = None
    return
  def __atstmplab26():
    nonlocal env0, env1, arg0
    nonlocal tmpret64, tmp65, tmp66, tmp67, tmp68, tmp69
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab27()
    return
  def __atstmplab27():
    nonlocal env0, env1, arg0
    nonlocal tmpret64, tmp65, tmp66, tmp67, tmp68, tmp69
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp66 = tmp65[0]
    tmp67 = tmp65[1]
    #ATSINSfreecon(tmp65);
    tmp68 = env0[0](env0, tmp66)
    tmp69 = _ats2pypre_stream_vt_auxmain_26(env0, tmp67)
    tmpret64 = (tmp68, tmp69)
    return
  mbranch_1 = { 1: __atstmplab24, 2: __atstmplab25, 3: __atstmplab26, 4: __atstmplab27 }
  #__patsflab__ats2pypre_stream_vt_patsfun_27
  if (arg0):
    tmp65 = ATSPMVllazyval_eval(env1)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
  else:
    atspre_lazy_vt_free(env1)
  #endif
  return tmpret64


def ats2pypre_stream_vt_map_method(arg0, arg1):
  tmpret71 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_map_method
  tmpret71 = _ats2pypre_stream_vt_patsfun_29__closurerize(arg0)
  return tmpret71


def _ats2pypre_stream_vt_patsfun_29(env0, arg0):
  tmpret72 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_patsfun_29
  tmpret72 = ats2pypre_stream_vt_map_cloref(env0, arg0)
  return tmpret72


def ats2pypre_stream_vt_mapopt_cloref(arg0, arg1):
  tmpret73 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_mapopt_cloref
  tmpret73 = _ats2pypre_stream_vt_auxmain_31(arg1, arg0)
  return tmpret73


def _ats2pypre_stream_vt_auxmain_31(env0, arg0):
  tmpret74 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_auxmain_31
  tmpret74 = _ats2pypre_stream_vt_patsfun_32__closurerize(env0, arg0)
  return tmpret74


def _ats2pypre_stream_vt_patsfun_32(env0, env1, arg0):
  tmpret75 = None
  tmp76 = None
  tmp77 = None
  tmp78 = None
  tmp79 = None
  tmp80 = None
  tmp81 = None
  tmp82 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  mbranch_2 = None
  def __atstmplab28():
    nonlocal env0, env1, arg0
    nonlocal tmpret75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(tmp76)): tmplab_py = 4 ; return#__atstmplab31
    __atstmplab29()
    return
  def __atstmplab29():
    nonlocal env0, env1, arg0
    nonlocal tmpret75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret75 = None
    return
  def __atstmplab30():
    nonlocal env0, env1, arg0
    nonlocal tmpret75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab31()
    return
  def __atstmplab31():
    nonlocal env0, env1, arg0
    nonlocal tmpret75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp77 = tmp76[0]
    tmp78 = tmp76[1]
    #ATSINSfreecon(tmp76);
    tmp79 = env0[0](env0, tmp77)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_2.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    return
  def __atstmplab32():
    nonlocal env0, env1, arg0
    nonlocal tmpret75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(tmp79)): tmplab_py = 4 ; return#__atstmplab35
    __atstmplab33()
    return
  def __atstmplab33():
    nonlocal env0, env1, arg0
    nonlocal tmpret75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp81 = _ats2pypre_stream_vt_auxmain_31(env0, tmp78)
    tmpret75 = ATSPMVllazyval_eval(tmp81)
    return
  def __atstmplab34():
    nonlocal env0, env1, arg0
    nonlocal tmpret75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab35()
    return
  def __atstmplab35():
    nonlocal env0, env1, arg0
    nonlocal tmpret75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp80 = tmp79[0]
    #ATSINSfreecon(tmp79);
    tmp82 = _ats2pypre_stream_vt_auxmain_31(env0, tmp78)
    tmpret75 = (tmp80, tmp82)
    return
  mbranch_1 = { 1: __atstmplab28, 2: __atstmplab29, 3: __atstmplab30, 4: __atstmplab31 }
  mbranch_2 = { 1: __atstmplab32, 2: __atstmplab33, 3: __atstmplab34, 4: __atstmplab35 }
  #__patsflab__ats2pypre_stream_vt_patsfun_32
  if (arg0):
    tmp76 = ATSPMVllazyval_eval(env1)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
  else:
    atspre_lazy_vt_free(env1)
  #endif
  return tmpret75


def ats2pypre_stream_vt_mapopt_method(arg0, arg1):
  tmpret84 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_mapopt_method
  tmpret84 = _ats2pypre_stream_vt_patsfun_34__closurerize(arg0)
  return tmpret84


def _ats2pypre_stream_vt_patsfun_34(env0, arg0):
  tmpret85 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_patsfun_34
  tmpret85 = ats2pypre_stream_vt_mapopt_cloref(env0, arg0)
  return tmpret85


def ats2pypre_stream_vt_filter_cloref(arg0, arg1):
  tmpret86 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_filter_cloref
  tmpret86 = _ats2pypre_stream_vt_auxmain_36(arg1, arg0)
  return tmpret86


def _ats2pypre_stream_vt_auxmain_36(env0, arg0):
  tmpret87 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_auxmain_36
  tmpret87 = _ats2pypre_stream_vt_patsfun_37__closurerize(env0, arg0)
  return tmpret87


def _ats2pypre_stream_vt_patsfun_37(env0, env1, arg0):
  tmpret88 = None
  tmp89 = None
  tmp90 = None
  tmp91 = None
  tmp92 = None
  tmp93 = None
  tmp94 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab36():
    nonlocal env0, env1, arg0
    nonlocal tmpret88, tmp89, tmp90, tmp91, tmp92, tmp93, tmp94
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp89)): tmplab_py = 4 ; return#__atstmplab39
    __atstmplab37()
    return
  def __atstmplab37():
    nonlocal env0, env1, arg0
    nonlocal tmpret88, tmp89, tmp90, tmp91, tmp92, tmp93, tmp94
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret88 = None
    return
  def __atstmplab38():
    nonlocal env0, env1, arg0
    nonlocal tmpret88, tmp89, tmp90, tmp91, tmp92, tmp93, tmp94
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab39()
    return
  def __atstmplab39():
    nonlocal env0, env1, arg0
    nonlocal tmpret88, tmp89, tmp90, tmp91, tmp92, tmp93, tmp94
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp90 = tmp89[0]
    tmp91 = tmp89[1]
    #ATSINSfreecon(tmp89);
    tmp92 = env0[0](env0, tmp90)
    if (tmp92):
      tmp93 = _ats2pypre_stream_vt_auxmain_36(env0, tmp91)
      tmpret88 = (tmp90, tmp93)
    else:
      tmp94 = _ats2pypre_stream_vt_auxmain_36(env0, tmp91)
      tmpret88 = ATSPMVllazyval_eval(tmp94)
    #endif
    return
  mbranch_1 = { 1: __atstmplab36, 2: __atstmplab37, 3: __atstmplab38, 4: __atstmplab39 }
  #__patsflab__ats2pypre_stream_vt_patsfun_37
  if (arg0):
    tmp89 = ATSPMVllazyval_eval(env1)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
  else:
    atspre_lazy_vt_free(env1)
  #endif
  return tmpret88


def ats2pypre_stream_vt_filter_method(arg0):
  tmpret96 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_filter_method
  tmpret96 = _ats2pypre_stream_vt_patsfun_39__closurerize(arg0)
  return tmpret96


def _ats2pypre_stream_vt_patsfun_39(env0, arg0):
  tmpret97 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_patsfun_39
  tmpret97 = ats2pypre_stream_vt_filter_cloref(env0, arg0)
  return tmpret97


def ats2pypre_stream_vt_exists_cloref(arg0, arg1):
  tmpret98 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_exists_cloref
  tmpret98 = _ats2pypre_stream_vt_loop_41(arg1, arg0)
  return tmpret98


def _ats2pypre_stream_vt_loop_41(env0, arg0):
  apy0 = None
  tmpret99 = None
  tmp100 = None
  tmp101 = None
  tmp102 = None
  tmp103 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab40():
    nonlocal env0, arg0
    nonlocal apy0, tmpret99, tmp100, tmp101, tmp102, tmp103
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp100)): tmplab_py = 4 ; return#__atstmplab43
    __atstmplab41()
    return
  def __atstmplab41():
    nonlocal env0, arg0
    nonlocal apy0, tmpret99, tmp100, tmp101, tmp102, tmp103
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret99 = False
    return
  def __atstmplab42():
    nonlocal env0, arg0
    nonlocal apy0, tmpret99, tmp100, tmp101, tmp102, tmp103
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab43()
    return
  def __atstmplab43():
    nonlocal env0, arg0
    nonlocal apy0, tmpret99, tmp100, tmp101, tmp102, tmp103
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp101 = tmp100[0]
    tmp102 = tmp100[1]
    #ATSINSfreecon(tmp100);
    tmp103 = env0[0](env0, tmp101)
    if (tmp103):
      atspre_lazy_vt_free(tmp102)
      tmpret99 = True
    else:
      #ATStailcalseq_beg
      apy0 = tmp102
      arg0 = apy0
      funlab_py = 1 #__patsflab__ats2pypre_stream_vt_loop_41
      #ATStailcalseq_end
    #endif
    return
  mbranch_1 = { 1: __atstmplab40, 2: __atstmplab41, 3: __atstmplab42, 4: __atstmplab43 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_vt_loop_41
    tmp100 = ATSPMVllazyval_eval(arg0)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret99


def ats2pypre_stream_vt_exists_method(arg0):
  tmpret105 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_exists_method
  tmpret105 = _ats2pypre_stream_vt_patsfun_43__closurerize(arg0)
  return tmpret105


def _ats2pypre_stream_vt_patsfun_43(env0, arg0):
  tmpret106 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_patsfun_43
  tmpret106 = ats2pypre_stream_vt_exists_cloref(env0, arg0)
  return tmpret106


def ats2pypre_stream_vt_forall_cloref(arg0, arg1):
  tmpret107 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_forall_cloref
  tmpret107 = _ats2pypre_stream_vt_loop_45(arg1, arg0)
  return tmpret107


def _ats2pypre_stream_vt_loop_45(env0, arg0):
  apy0 = None
  tmpret108 = None
  tmp109 = None
  tmp110 = None
  tmp111 = None
  tmp112 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab44():
    nonlocal env0, arg0
    nonlocal apy0, tmpret108, tmp109, tmp110, tmp111, tmp112
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp109)): tmplab_py = 4 ; return#__atstmplab47
    __atstmplab45()
    return
  def __atstmplab45():
    nonlocal env0, arg0
    nonlocal apy0, tmpret108, tmp109, tmp110, tmp111, tmp112
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret108 = True
    return
  def __atstmplab46():
    nonlocal env0, arg0
    nonlocal apy0, tmpret108, tmp109, tmp110, tmp111, tmp112
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab47()
    return
  def __atstmplab47():
    nonlocal env0, arg0
    nonlocal apy0, tmpret108, tmp109, tmp110, tmp111, tmp112
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp110 = tmp109[0]
    tmp111 = tmp109[1]
    #ATSINSfreecon(tmp109);
    tmp112 = env0[0](env0, tmp110)
    if (tmp112):
      #ATStailcalseq_beg
      apy0 = tmp111
      arg0 = apy0
      funlab_py = 1 #__patsflab__ats2pypre_stream_vt_loop_45
      #ATStailcalseq_end
    else:
      atspre_lazy_vt_free(tmp111)
      tmpret108 = False
    #endif
    return
  mbranch_1 = { 1: __atstmplab44, 2: __atstmplab45, 3: __atstmplab46, 4: __atstmplab47 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_vt_loop_45
    tmp109 = ATSPMVllazyval_eval(arg0)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret108


def ats2pypre_stream_vt_forall_method(arg0):
  tmpret114 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_forall_method
  tmpret114 = _ats2pypre_stream_vt_patsfun_47__closurerize(arg0)
  return tmpret114


def _ats2pypre_stream_vt_patsfun_47(env0, arg0):
  tmpret115 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_patsfun_47
  tmpret115 = ats2pypre_stream_vt_forall_cloref(env0, arg0)
  return tmpret115


def ats2pypre_stream_vt_foreach_cloref(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_foreach_cloref
  _ats2pypre_stream_vt_loop_49(arg1, arg0)
  return#_void


def _ats2pypre_stream_vt_loop_49(env0, arg0):
  apy0 = None
  tmp118 = None
  tmp119 = None
  tmp120 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab48():
    nonlocal env0, arg0
    nonlocal apy0, tmp118, tmp119, tmp120
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp118)): tmplab_py = 4 ; return#__atstmplab51
    __atstmplab49()
    return
  def __atstmplab49():
    nonlocal env0, arg0
    nonlocal apy0, tmp118, tmp119, tmp120
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    None#ATSINSmove_void
    return
  def __atstmplab50():
    nonlocal env0, arg0
    nonlocal apy0, tmp118, tmp119, tmp120
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab51()
    return
  def __atstmplab51():
    nonlocal env0, arg0
    nonlocal apy0, tmp118, tmp119, tmp120
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp119 = tmp118[0]
    tmp120 = tmp118[1]
    #ATSINSfreecon(tmp118);
    env0[0](env0, tmp119)
    #ATStailcalseq_beg
    apy0 = tmp120
    arg0 = apy0
    funlab_py = 1 #__patsflab__ats2pypre_stream_vt_loop_49
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab48, 2: __atstmplab49, 3: __atstmplab50, 4: __atstmplab51 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_vt_loop_49
    tmp118 = ATSPMVllazyval_eval(arg0)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return#_void


def ats2pypre_stream_vt_foreach_method(arg0):
  tmpret122 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_foreach_method
  tmpret122 = _ats2pypre_stream_vt_patsfun_51__closurerize(arg0)
  return tmpret122


def _ats2pypre_stream_vt_patsfun_51(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_patsfun_51
  ats2pypre_stream_vt_foreach_cloref(env0, arg0)
  return#_void


def ats2pypre_stream_vt_iforeach_cloref(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_iforeach_cloref
  _ats2pypre_stream_vt_loop_53(arg1, 0, arg0)
  return#_void


def _ats2pypre_stream_vt_loop_53(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmp126 = None
  tmp127 = None
  tmp128 = None
  tmp130 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab52():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp126, tmp127, tmp128, tmp130
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp126)): tmplab_py = 4 ; return#__atstmplab55
    __atstmplab53()
    return
  def __atstmplab53():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp126, tmp127, tmp128, tmp130
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    None#ATSINSmove_void
    return
  def __atstmplab54():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp126, tmp127, tmp128, tmp130
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab55()
    return
  def __atstmplab55():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmp126, tmp127, tmp128, tmp130
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp127 = tmp126[0]
    tmp128 = tmp126[1]
    #ATSINSfreecon(tmp126);
    env0[0](env0, arg0, tmp127)
    tmp130 = ats2pypre_add_int1_int1(arg0, 1)
    #ATStailcalseq_beg
    apy0 = tmp130
    apy1 = tmp128
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_stream_vt_loop_53
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab52, 2: __atstmplab53, 3: __atstmplab54, 4: __atstmplab55 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_stream_vt_loop_53
    tmp126 = ATSPMVllazyval_eval(arg1)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return#_void


def ats2pypre_stream_vt_iforeach_method(arg0):
  tmpret131 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_iforeach_method
  tmpret131 = _ats2pypre_stream_vt_patsfun_55__closurerize(arg0)
  return tmpret131


def _ats2pypre_stream_vt_patsfun_55(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_patsfun_55
  ats2pypre_stream_vt_iforeach_cloref(env0, arg0)
  return#_void


def ats2pypre_stream_vt_rforeach_cloref(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_rforeach_cloref
  _ats2pypre_stream_vt_auxmain_57(arg1, arg0)
  return#_void


def _ats2pypre_stream_vt_auxmain_57(env0, arg0):
  tmp135 = None
  tmp136 = None
  tmp137 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab56():
    nonlocal env0, arg0
    nonlocal tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp135)): tmplab_py = 4 ; return#__atstmplab59
    __atstmplab57()
    return
  def __atstmplab57():
    nonlocal env0, arg0
    nonlocal tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    None#ATSINSmove_void
    return
  def __atstmplab58():
    nonlocal env0, arg0
    nonlocal tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab59()
    return
  def __atstmplab59():
    nonlocal env0, arg0
    nonlocal tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp136 = tmp135[0]
    tmp137 = tmp135[1]
    #ATSINSfreecon(tmp135);
    _ats2pypre_stream_vt_auxmain_57(env0, tmp137)
    env0[0](env0, tmp136)
    return
  mbranch_1 = { 1: __atstmplab56, 2: __atstmplab57, 3: __atstmplab58, 4: __atstmplab59 }
  #__patsflab__ats2pypre_stream_vt_auxmain_57
  tmp135 = ATSPMVllazyval_eval(arg0)
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return#_void


def ats2pypre_stream_vt_rforeach_method(arg0):
  tmpret139 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_rforeach_method
  tmpret139 = _ats2pypre_stream_vt_patsfun_59__closurerize(arg0)
  return tmpret139


def _ats2pypre_stream_vt_patsfun_59(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_patsfun_59
  ats2pypre_stream_vt_rforeach_cloref(env0, arg0)
  return#_void


def ats2pypre_stream_vt_tabulate_cloref(arg0):
  tmpret141 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_stream_vt_tabulate_cloref
  tmpret141 = _ats2pypre_stream_vt_auxmain_61(arg0, 0)
  return tmpret141


def _ats2pypre_stream_vt_auxmain_61(env0, arg0):
  tmpret142 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_auxmain_61
  tmpret142 = _ats2pypre_stream_vt_patsfun_62__closurerize(env0, arg0)
  return tmpret142


def _ats2pypre_stream_vt_patsfun_62(env0, env1, arg0):
  tmpret143 = None
  tmp144 = None
  tmp145 = None
  tmp146 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_stream_vt_patsfun_62
  if (arg0):
    tmp144 = env0[0](env0, env1)
    tmp146 = ats2pypre_add_int1_int1(env1, 1)
    tmp145 = _ats2pypre_stream_vt_auxmain_61(env0, tmp146)
    tmpret143 = (tmp144, tmp145)
  #endif
  return tmpret143

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def _ats2pypre_intrange_patsfun_4__closurerize(env0):
  def _ats2pypre_intrange_patsfun_4__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_4(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_4__cfun, env0)

def _ats2pypre_intrange_patsfun_8__closurerize(env0):
  def _ats2pypre_intrange_patsfun_8__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_8(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_8__cfun, env0)

def _ats2pypre_intrange_patsfun_10__closurerize(env0):
  def _ats2pypre_intrange_patsfun_10__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_10(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_10__cfun, env0)

def _ats2pypre_intrange_patsfun_14__closurerize(env0):
  def _ats2pypre_intrange_patsfun_14__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_14(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_14__cfun, env0)

def _ats2pypre_intrange_patsfun_17__closurerize(env0):
  def _ats2pypre_intrange_patsfun_17__cfun(cenv, arg0, arg1): return _ats2pypre_intrange_patsfun_17(cenv[1], arg0, arg1)
  return (_ats2pypre_intrange_patsfun_17__cfun, env0)

def _ats2pypre_intrange_patsfun_20__closurerize(env0):
  def _ats2pypre_intrange_patsfun_20__cfun(cenv, arg0, arg1): return _ats2pypre_intrange_patsfun_20(cenv[1], arg0, arg1)
  return (_ats2pypre_intrange_patsfun_20__cfun, env0)

def _ats2pypre_intrange_patsfun_24__closurerize(env0):
  def _ats2pypre_intrange_patsfun_24__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_24(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_24__cfun, env0)

def _ats2pypre_intrange_patsfun_27__closurerize(env0):
  def _ats2pypre_intrange_patsfun_27__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_27(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_27__cfun, env0)

def _ats2pypre_intrange_patsfun_30__closurerize(env0, env1, env2):
  def _ats2pypre_intrange_patsfun_30__cfun(cenv): return _ats2pypre_intrange_patsfun_30(cenv[1], cenv[2], cenv[3])
  return (_ats2pypre_intrange_patsfun_30__cfun, env0, env1, env2)

def _ats2pypre_intrange_patsfun_32__closurerize(env0):
  def _ats2pypre_intrange_patsfun_32__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_32(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_32__cfun, env0)

def _ats2pypre_intrange_patsfun_35__closurerize(env0, env1, env2):
  def _ats2pypre_intrange_patsfun_35__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_35(cenv[1], cenv[2], cenv[3], arg0)
  return (_ats2pypre_intrange_patsfun_35__cfun, env0, env1, env2)

def _ats2pypre_intrange_patsfun_37__closurerize(env0):
  def _ats2pypre_intrange_patsfun_37__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_37(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_37__cfun, env0)

def _ats2pypre_intrange_patsfun_44__closurerize(env0):
  def _ats2pypre_intrange_patsfun_44__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_44(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_44__cfun, env0)

def _ats2pypre_intrange_patsfun_48__closurerize(env0):
  def _ats2pypre_intrange_patsfun_48__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_48(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_48__cfun, env0)

def _ats2pypre_intrange_patsfun_52__closurerize(env0):
  def _ats2pypre_intrange_patsfun_52__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_52(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_52__cfun, env0)

def _ats2pypre_intrange_patsfun_56__closurerize(env0):
  def _ats2pypre_intrange_patsfun_56__cfun(cenv, arg0): return _ats2pypre_intrange_patsfun_56(cenv[1], arg0)
  return (_ats2pypre_intrange_patsfun_56__cfun, env0)

def _ats2pypre_intrange_patsfun_60__closurerize(env0, env1):
  def _ats2pypre_intrange_patsfun_60__cfun(cenv, arg0, arg1): return _ats2pypre_intrange_patsfun_60(cenv[1], cenv[2], arg0, arg1)
  return (_ats2pypre_intrange_patsfun_60__cfun, env0, env1)

def _ats2pypre_intrange_patsfun_64__closurerize(env0, env1):
  def _ats2pypre_intrange_patsfun_64__cfun(cenv, arg0, arg1): return _ats2pypre_intrange_patsfun_64(cenv[1], cenv[2], arg0, arg1)
  return (_ats2pypre_intrange_patsfun_64__cfun, env0, env1)

def ats2pypre_int_repeat_lazy(arg0, arg1):
  tmp1 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_repeat_lazy
  tmp1 = ats2pypre_lazy2cloref(arg1)
  ats2pypre_int_repeat_cloref(arg0, tmp1)
  return#_void


def ats2pypre_int_repeat_cloref(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_repeat_cloref
  _ats2pypre_intrange_loop_2(arg0, arg1)
  return#_void


def _ats2pypre_intrange_loop_2(arg0, arg1):
  apy0 = None
  apy1 = None
  tmp4 = None
  tmp6 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_intrange_loop_2
    tmp4 = ats2pypre_gt_int0_int0(arg0, 0)
    if (tmp4):
      arg1[0](arg1)
      tmp6 = ats2pypre_sub_int0_int0(arg0, 1)
      #ATStailcalseq_beg
      apy0 = tmp6
      apy1 = arg1
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab__ats2pypre_intrange_loop_2
      #ATStailcalseq_end
    else:
      None#ATSINSmove_void
    #endif
    if (funlab_py == 0): break
  return#_void


def ats2pypre_int_repeat_method(arg0):
  tmpret7 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_repeat_method
  tmpret7 = _ats2pypre_intrange_patsfun_4__closurerize(arg0)
  return tmpret7


def _ats2pypre_intrange_patsfun_4(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_4
  ats2pypre_int_repeat_cloref(env0, arg0)
  return#_void


def ats2pypre_int_exists_cloref(arg0, arg1):
  tmpret9 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_exists_cloref
  tmpret9 = ats2pypre_intrange_exists_cloref(0, arg0, arg1)
  return tmpret9


def ats2pypre_int_forall_cloref(arg0, arg1):
  tmpret10 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_forall_cloref
  tmpret10 = ats2pypre_intrange_forall_cloref(0, arg0, arg1)
  return tmpret10


def ats2pypre_int_exists_method(arg0):
  tmpret11 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_exists_method
  tmpret11 = _ats2pypre_intrange_patsfun_8__closurerize(arg0)
  return tmpret11


def _ats2pypre_intrange_patsfun_8(env0, arg0):
  tmpret12 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_8
  tmpret12 = ats2pypre_int_exists_cloref(env0, arg0)
  return tmpret12


def ats2pypre_int_forall_method(arg0):
  tmpret13 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_forall_method
  tmpret13 = _ats2pypre_intrange_patsfun_10__closurerize(arg0)
  return tmpret13


def _ats2pypre_intrange_patsfun_10(env0, arg0):
  tmpret14 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_10
  tmpret14 = ats2pypre_int_forall_cloref(env0, arg0)
  return tmpret14


def ats2pypre_int_foreach_cloref(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_foreach_cloref
  ats2pypre_intrange_foreach_cloref(0, arg0, arg1)
  return#_void


def ats2pypre_int_rforeach_cloref(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_rforeach_cloref
  ats2pypre_intrange_rforeach_cloref(0, arg0, arg1)
  return#_void


def ats2pypre_int_foreach_method(arg0):
  tmpret17 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_foreach_method
  tmpret17 = _ats2pypre_intrange_patsfun_14__closurerize(arg0)
  return tmpret17


def _ats2pypre_intrange_patsfun_14(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_14
  ats2pypre_int_foreach_cloref(env0, arg0)
  return#_void


def ats2pypre_int_foldleft_cloref(arg0, arg1, arg2):
  tmpret19 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_foldleft_cloref
  tmpret19 = ats2pypre_intrange_foldleft_cloref(0, arg0, arg1, arg2)
  return tmpret19


def ats2pypre_int_foldleft_method(arg0, arg1):
  tmpret20 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_foldleft_method
  tmpret20 = _ats2pypre_intrange_patsfun_17__closurerize(arg0)
  return tmpret20


def _ats2pypre_intrange_patsfun_17(env0, arg0, arg1):
  tmpret21 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_17
  tmpret21 = ats2pypre_int_foldleft_cloref(env0, arg0, arg1)
  return tmpret21


def ats2pypre_int_foldright_cloref(arg0, arg1, arg2):
  tmpret22 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_foldright_cloref
  tmpret22 = ats2pypre_intrange_foldright_cloref(0, arg0, arg1, arg2)
  return tmpret22


def ats2pypre_int_foldright_method(arg0, arg1):
  tmpret23 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_foldright_method
  tmpret23 = _ats2pypre_intrange_patsfun_20__closurerize(arg0)
  return tmpret23


def _ats2pypre_intrange_patsfun_20(env0, arg0, arg1):
  tmpret24 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_20
  tmpret24 = ats2pypre_int_foldright_cloref(env0, arg0, arg1)
  return tmpret24


def ats2pypre_int_list_map_cloref(arg0, arg1):
  tmpret25 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_list_map_cloref
  tmpret25 = _ats2pypre_intrange_aux_22(arg0, arg1, 0)
  return tmpret25


def _ats2pypre_intrange_aux_22(env0, env1, arg0):
  tmpret26 = None
  tmp27 = None
  tmp28 = None
  tmp29 = None
  tmp30 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_aux_22
  tmp27 = ats2pypre_lt_int1_int1(arg0, env0)
  if (tmp27):
    tmp28 = env1[0](env1, arg0)
    tmp30 = ats2pypre_add_int1_int1(arg0, 1)
    tmp29 = _ats2pypre_intrange_aux_22(env0, env1, tmp30)
    tmpret26 = (tmp28, tmp29)
  else:
    tmpret26 = None
  #endif
  return tmpret26


def ats2pypre_int_list_map_method(arg0, arg1):
  tmpret31 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_list_map_method
  tmpret31 = _ats2pypre_intrange_patsfun_24__closurerize(arg0)
  return tmpret31


def _ats2pypre_intrange_patsfun_24(env0, arg0):
  tmpret32 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_24
  tmpret32 = ats2pypre_int_list_map_cloref(env0, arg0)
  return tmpret32


def ats2pypre_int_list0_map_cloref(arg0, arg1):
  tmpret33 = None
  tmp34 = None
  tmp35 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_list0_map_cloref
  tmp34 = ats2pypre_gte_int1_int1(arg0, 0)
  if (tmp34):
    tmp35 = ats2pypre_int_list_map_cloref(arg0, arg1)
    tmpret33 = tmp35
  else:
    tmpret33 = None
  #endif
  return tmpret33


def ats2pypre_int_list0_map_method(arg0, arg1):
  tmpret36 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_list0_map_method
  tmpret36 = _ats2pypre_intrange_patsfun_27__closurerize(arg0)
  return tmpret36


def _ats2pypre_intrange_patsfun_27(env0, arg0):
  tmpret37 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_27
  tmpret37 = ats2pypre_int_list0_map_cloref(env0, arg0)
  return tmpret37


def ats2pypre_int_stream_map_cloref(arg0, arg1):
  tmpret38 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_stream_map_cloref
  tmpret38 = _ats2pypre_intrange_aux_29(arg0, arg1, 0)
  return tmpret38


def _ats2pypre_intrange_aux_29(env0, env1, arg0):
  tmpret39 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_aux_29
  tmpret39 = [0, _ats2pypre_intrange_patsfun_30__closurerize(env0, env1, arg0)]
  return tmpret39


def _ats2pypre_intrange_patsfun_30(env0, env1, env2):
  tmpret40 = None
  tmp41 = None
  tmp42 = None
  tmp43 = None
  tmp44 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_30
  tmp41 = ats2pypre_lt_int1_int1(env2, env0)
  if (tmp41):
    tmp42 = env1[0](env1, env2)
    tmp44 = ats2pypre_add_int1_int1(env2, 1)
    tmp43 = _ats2pypre_intrange_aux_29(env0, env1, tmp44)
    tmpret40 = (tmp42, tmp43)
  else:
    tmpret40 = None
  #endif
  return tmpret40


def ats2pypre_int_stream_map_method(arg0, arg1):
  tmpret45 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_stream_map_method
  tmpret45 = _ats2pypre_intrange_patsfun_32__closurerize(arg0)
  return tmpret45


def _ats2pypre_intrange_patsfun_32(env0, arg0):
  tmpret46 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_32
  tmpret46 = ats2pypre_int_stream_map_cloref(env0, arg0)
  return tmpret46


def ats2pypre_int_stream_vt_map_cloref(arg0, arg1):
  tmpret47 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_stream_vt_map_cloref
  tmpret47 = _ats2pypre_intrange_aux_34(arg0, arg1, 0)
  return tmpret47


def _ats2pypre_intrange_aux_34(env0, env1, arg0):
  tmpret48 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_aux_34
  tmpret48 = _ats2pypre_intrange_patsfun_35__closurerize(env0, env1, arg0)
  return tmpret48


def _ats2pypre_intrange_patsfun_35(env0, env1, env2, arg0):
  tmpret49 = None
  tmp50 = None
  tmp51 = None
  tmp52 = None
  tmp53 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_35
  if (arg0):
    tmp50 = ats2pypre_lt_int1_int1(env2, env0)
    if (tmp50):
      tmp51 = env1[0](env1, env2)
      tmp53 = ats2pypre_add_int1_int1(env2, 1)
      tmp52 = _ats2pypre_intrange_aux_34(env0, env1, tmp53)
      tmpret49 = (tmp51, tmp52)
    else:
      tmpret49 = None
    #endif
  #endif
  return tmpret49


def ats2pypre_int_stream_vt_map_method(arg0, arg1):
  tmpret54 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int_stream_vt_map_method
  tmpret54 = _ats2pypre_intrange_patsfun_37__closurerize(arg0)
  return tmpret54


def _ats2pypre_intrange_patsfun_37(env0, arg0):
  tmpret55 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_37
  tmpret55 = ats2pypre_int_stream_vt_map_cloref(env0, arg0)
  return tmpret55


def ats2pypre_int2_exists_cloref(arg0, arg1, arg2):
  tmpret56 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int2_exists_cloref
  tmpret56 = ats2pypre_intrange2_exists_cloref(0, arg0, 0, arg1, arg2)
  return tmpret56


def ats2pypre_int2_forall_cloref(arg0, arg1, arg2):
  tmpret57 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_int2_forall_cloref
  tmpret57 = ats2pypre_intrange2_forall_cloref(0, arg0, 0, arg1, arg2)
  return tmpret57


def ats2pypre_int2_foreach_cloref(arg0, arg1, arg2):
  funlab_py = None
  tmplab_py = None
  #__patsflab_int2_foreach_cloref
  ats2pypre_intrange2_foreach_cloref(0, arg0, 0, arg1, arg2)
  return#_void


def ats2pypre_intrange_exists_cloref(arg0, arg1, arg2):
  tmpret59 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_exists_cloref
  tmpret59 = _ats2pypre_intrange_loop_42(arg0, arg1, arg2)
  return tmpret59


def _ats2pypre_intrange_loop_42(arg0, arg1, arg2):
  apy0 = None
  apy1 = None
  apy2 = None
  tmpret60 = None
  tmp61 = None
  tmp62 = None
  tmp63 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_intrange_loop_42
    tmp61 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp61):
      tmp62 = arg2[0](arg2, arg0)
      if (tmp62):
        tmpret60 = True
      else:
        tmp63 = ats2pypre_add_int0_int0(arg0, 1)
        #ATStailcalseq_beg
        apy0 = tmp63
        apy1 = arg1
        apy2 = arg2
        arg0 = apy0
        arg1 = apy1
        arg2 = apy2
        funlab_py = 1 #__patsflab__ats2pypre_intrange_loop_42
        #ATStailcalseq_end
      #endif
    else:
      tmpret60 = False
    #endif
    if (funlab_py == 0): break
  return tmpret60


def ats2pypre_intrange_exists_method(arg0):
  tmpret64 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_exists_method
  tmpret64 = _ats2pypre_intrange_patsfun_44__closurerize(arg0)
  return tmpret64


def _ats2pypre_intrange_patsfun_44(env0, arg0):
  tmpret65 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_44
  tmpret65 = ats2pypre_intrange_exists_cloref(env0[0], env0[1], arg0)
  return tmpret65


def ats2pypre_intrange_forall_cloref(arg0, arg1, arg2):
  tmpret66 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_forall_cloref
  tmpret66 = _ats2pypre_intrange_loop_46(arg0, arg1, arg2)
  return tmpret66


def _ats2pypre_intrange_loop_46(arg0, arg1, arg2):
  apy0 = None
  apy1 = None
  apy2 = None
  tmpret67 = None
  tmp68 = None
  tmp69 = None
  tmp70 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_intrange_loop_46
    tmp68 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp68):
      tmp69 = arg2[0](arg2, arg0)
      if (tmp69):
        tmp70 = ats2pypre_add_int0_int0(arg0, 1)
        #ATStailcalseq_beg
        apy0 = tmp70
        apy1 = arg1
        apy2 = arg2
        arg0 = apy0
        arg1 = apy1
        arg2 = apy2
        funlab_py = 1 #__patsflab__ats2pypre_intrange_loop_46
        #ATStailcalseq_end
      else:
        tmpret67 = False
      #endif
    else:
      tmpret67 = True
    #endif
    if (funlab_py == 0): break
  return tmpret67


def ats2pypre_intrange_forall_method(arg0):
  tmpret71 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_forall_method
  tmpret71 = _ats2pypre_intrange_patsfun_48__closurerize(arg0)
  return tmpret71


def _ats2pypre_intrange_patsfun_48(env0, arg0):
  tmpret72 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_48
  tmpret72 = ats2pypre_intrange_forall_cloref(env0[0], env0[1], arg0)
  return tmpret72


def ats2pypre_intrange_foreach_cloref(arg0, arg1, arg2):
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_foreach_cloref
  _ats2pypre_intrange_loop_50(arg0, arg1, arg2)
  return#_void


def _ats2pypre_intrange_loop_50(arg0, arg1, arg2):
  apy0 = None
  apy1 = None
  apy2 = None
  tmp75 = None
  tmp77 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_intrange_loop_50
    tmp75 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp75):
      arg2[0](arg2, arg0)
      tmp77 = ats2pypre_add_int0_int0(arg0, 1)
      #ATStailcalseq_beg
      apy0 = tmp77
      apy1 = arg1
      apy2 = arg2
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      funlab_py = 1 #__patsflab__ats2pypre_intrange_loop_50
      #ATStailcalseq_end
    else:
      None#ATSINSmove_void
    #endif
    if (funlab_py == 0): break
  return#_void


def ats2pypre_intrange_foreach_method(arg0):
  tmpret78 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_foreach_method
  tmpret78 = _ats2pypre_intrange_patsfun_52__closurerize(arg0)
  return tmpret78


def _ats2pypre_intrange_patsfun_52(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_52
  ats2pypre_intrange_foreach_cloref(env0[0], env0[1], arg0)
  return#_void


def ats2pypre_intrange_rforeach_cloref(arg0, arg1, arg2):
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_rforeach_cloref
  _ats2pypre_intrange_loop_54(arg0, arg1, arg2)
  return#_void


def _ats2pypre_intrange_loop_54(arg0, arg1, arg2):
  apy0 = None
  apy1 = None
  apy2 = None
  tmp82 = None
  tmp84 = None
  tmp85 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_intrange_loop_54
    tmp82 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp82):
      tmp84 = ats2pypre_sub_int0_int0(arg1, 1)
      arg2[0](arg2, tmp84)
      tmp85 = ats2pypre_sub_int0_int0(arg1, 1)
      #ATStailcalseq_beg
      apy0 = arg0
      apy1 = tmp85
      apy2 = arg2
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      funlab_py = 1 #__patsflab__ats2pypre_intrange_loop_54
      #ATStailcalseq_end
    else:
      None#ATSINSmove_void
    #endif
    if (funlab_py == 0): break
  return#_void


def ats2pypre_intrange_rforeach_method(arg0):
  tmpret86 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_rforeach_method
  tmpret86 = _ats2pypre_intrange_patsfun_56__closurerize(arg0)
  return tmpret86


def _ats2pypre_intrange_patsfun_56(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_56
  ats2pypre_intrange_rforeach_cloref(env0[0], env0[1], arg0)
  return#_void


def ats2pypre_intrange_foldleft_cloref(arg0, arg1, arg2, arg3):
  tmpret88 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_foldleft_cloref
  tmpret88 = _ats2pypre_intrange_loop_58(arg3, arg0, arg1, arg2)
  return tmpret88


def _ats2pypre_intrange_loop_58(env0, arg0, arg1, arg2):
  apy0 = None
  apy1 = None
  apy2 = None
  tmpret89 = None
  tmp90 = None
  tmp91 = None
  tmp92 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_intrange_loop_58
    tmp90 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp90):
      tmp91 = ats2pypre_add_int0_int0(arg0, 1)
      tmp92 = env0[0](env0, arg2, arg0)
      #ATStailcalseq_beg
      apy0 = tmp91
      apy1 = arg1
      apy2 = tmp92
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      funlab_py = 1 #__patsflab__ats2pypre_intrange_loop_58
      #ATStailcalseq_end
    else:
      tmpret89 = arg2
    #endif
    if (funlab_py == 0): break
  return tmpret89


def ats2pypre_intrange_foldleft_method(arg0, arg1):
  tmp93 = None
  tmp94 = None
  tmpret95 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_foldleft_method
  tmp93 = arg0[0]
  tmp94 = arg0[1]
  tmpret95 = _ats2pypre_intrange_patsfun_60__closurerize(tmp93, tmp94)
  return tmpret95


def _ats2pypre_intrange_patsfun_60(env0, env1, arg0, arg1):
  tmpret96 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_60
  tmpret96 = ats2pypre_intrange_foldleft_cloref(env0, env1, arg0, arg1)
  return tmpret96


def ats2pypre_intrange_foldright_cloref(arg0, arg1, arg2, arg3):
  tmpret97 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_foldright_cloref
  tmpret97 = _ats2pypre_intrange_loop_62(arg2, arg0, arg1, arg3)
  return tmpret97


def _ats2pypre_intrange_loop_62(env0, arg0, arg1, arg2):
  apy0 = None
  apy1 = None
  apy2 = None
  tmpret98 = None
  tmp99 = None
  tmp100 = None
  tmp101 = None
  tmp102 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_intrange_loop_62
    tmp99 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp99):
      tmp100 = ats2pypre_sub_int0_int0(arg1, 1)
      tmp102 = ats2pypre_sub_int0_int0(arg1, 1)
      tmp101 = env0[0](env0, tmp102, arg2)
      #ATStailcalseq_beg
      apy0 = arg0
      apy1 = tmp100
      apy2 = tmp101
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      funlab_py = 1 #__patsflab__ats2pypre_intrange_loop_62
      #ATStailcalseq_end
    else:
      tmpret98 = arg2
    #endif
    if (funlab_py == 0): break
  return tmpret98


def ats2pypre_intrange_foldright_method(arg0, arg1):
  tmp103 = None
  tmp104 = None
  tmpret105 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange_foldright_method
  tmp103 = arg0[0]
  tmp104 = arg0[1]
  tmpret105 = _ats2pypre_intrange_patsfun_64__closurerize(tmp103, tmp104)
  return tmpret105


def _ats2pypre_intrange_patsfun_64(env0, env1, arg0, arg1):
  tmpret106 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_intrange_patsfun_64
  tmpret106 = ats2pypre_intrange_foldright_cloref(env0, env1, arg0, arg1)
  return tmpret106


def ats2pypre_intrange2_exists_cloref(arg0, arg1, arg2, arg3, arg4):
  tmpret107 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange2_exists_cloref
  tmpret107 = _ats2pypre_intrange_loop1_66(arg2, arg3, arg4, arg0, arg1, arg2, arg3, arg4)
  return tmpret107


def _ats2pypre_intrange_loop1_66(env0, env1, env2, arg0, arg1, arg2, arg3, arg4):
  apy0 = None
  apy1 = None
  apy2 = None
  apy3 = None
  apy4 = None
  tmpret108 = None
  tmp109 = None
  a2rg0 = None
  a2rg1 = None
  a2rg2 = None
  a2rg3 = None
  a2rg4 = None
  a2py0 = None
  a2py1 = None
  a2py2 = None
  a2py3 = None
  a2py4 = None
  tmpret110 = None
  tmp111 = None
  tmp112 = None
  tmp113 = None
  tmp114 = None
  funlab_py = None
  tmplab_py = None
  tmpret_py = None
  def __patsflab__ats2pypre_intrange_loop1_66():
    nonlocal env0, env1, env2, arg0, arg1, arg2, arg3, arg4
    nonlocal apy0, apy1, apy2, apy3, apy4, tmpret108, tmp109, a2rg0, a2rg1, a2rg2, a2rg3, a2rg4, a2py0, a2py1, a2py2, a2py3, a2py4, tmpret110, tmp111, tmp112, tmp113, tmp114
    nonlocal funlab_py, tmplab_py
    funlab_py = 0
    tmp109 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp109):
      #ATStailcalseq_beg
      a2py0 = arg0
      a2py1 = arg1
      a2py2 = arg2
      a2py3 = arg3
      a2py4 = env2
      a2rg0 = a2py0
      a2rg1 = a2py1
      a2rg2 = a2py2
      a2rg3 = a2py3
      a2rg4 = a2py4
      funlab_py = 2 #__patsflab__ats2pypre_intrange_loop2_67
      #ATStailcalseq_end
    else:
      tmpret108 = False
    #endif
    return tmpret108
  def __patsflab__ats2pypre_intrange_loop2_67():
    nonlocal env0, env1, env2, arg0, arg1, arg2, arg3, arg4
    nonlocal apy0, apy1, apy2, apy3, apy4, tmpret108, tmp109, a2rg0, a2rg1, a2rg2, a2rg3, a2rg4, a2py0, a2py1, a2py2, a2py3, a2py4, tmpret110, tmp111, tmp112, tmp113, tmp114
    nonlocal funlab_py, tmplab_py
    funlab_py = 0
    tmp111 = ats2pypre_lt_int0_int0(a2rg2, a2rg3)
    if (tmp111):
      tmp112 = a2rg4[0](a2rg4, a2rg0, a2rg2)
      if (tmp112):
        tmpret110 = True
      else:
        tmp113 = ats2pypre_add_int0_int0(a2rg2, 1)
        #ATStailcalseq_beg
        a2py0 = a2rg0
        a2py1 = a2rg1
        a2py2 = tmp113
        a2py3 = a2rg3
        a2py4 = a2rg4
        a2rg0 = a2py0
        a2rg1 = a2py1
        a2rg2 = a2py2
        a2rg3 = a2py3
        a2rg4 = a2py4
        funlab_py = 2 #__patsflab__ats2pypre_intrange_loop2_67
        #ATStailcalseq_end
      #endif
    else:
      tmp114 = ats2pypre_add_int0_int0(a2rg0, 1)
      #ATStailcalseq_beg
      apy0 = tmp114
      apy1 = a2rg1
      apy2 = env0
      apy3 = env1
      apy4 = a2rg4
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      arg3 = apy3
      arg4 = apy4
      funlab_py = 1 #__patsflab__ats2pypre_intrange_loop1_66
      #ATStailcalseq_end
    #endif
    return tmpret110
  mfundef = { 1: __patsflab__ats2pypre_intrange_loop1_66, 2: __patsflab__ats2pypre_intrange_loop2_67 }
  funlab_py = 1
  while(1):
    tmpret_py = mfundef.get(funlab_py)()
    if (funlab_py == 0): break
  return tmpret_py


def ats2pypre_intrange2_forall_cloref(arg0, arg1, arg2, arg3, arg4):
  tmpret115 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange2_forall_cloref
  tmpret115 = _ats2pypre_intrange_loop1_69(arg2, arg3, arg0, arg1, arg2, arg3, arg4)
  return tmpret115


def _ats2pypre_intrange_loop1_69(env0, env1, arg0, arg1, arg2, arg3, arg4):
  apy0 = None
  apy1 = None
  apy2 = None
  apy3 = None
  apy4 = None
  tmpret116 = None
  tmp117 = None
  a2rg0 = None
  a2rg1 = None
  a2rg2 = None
  a2rg3 = None
  a2rg4 = None
  a2py0 = None
  a2py1 = None
  a2py2 = None
  a2py3 = None
  a2py4 = None
  tmpret118 = None
  tmp119 = None
  tmp120 = None
  tmp121 = None
  tmp122 = None
  funlab_py = None
  tmplab_py = None
  tmpret_py = None
  def __patsflab__ats2pypre_intrange_loop1_69():
    nonlocal env0, env1, arg0, arg1, arg2, arg3, arg4
    nonlocal apy0, apy1, apy2, apy3, apy4, tmpret116, tmp117, a2rg0, a2rg1, a2rg2, a2rg3, a2rg4, a2py0, a2py1, a2py2, a2py3, a2py4, tmpret118, tmp119, tmp120, tmp121, tmp122
    nonlocal funlab_py, tmplab_py
    funlab_py = 0
    tmp117 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp117):
      #ATStailcalseq_beg
      a2py0 = arg0
      a2py1 = arg1
      a2py2 = arg2
      a2py3 = arg3
      a2py4 = arg4
      a2rg0 = a2py0
      a2rg1 = a2py1
      a2rg2 = a2py2
      a2rg3 = a2py3
      a2rg4 = a2py4
      funlab_py = 2 #__patsflab__ats2pypre_intrange_loop2_70
      #ATStailcalseq_end
    else:
      tmpret116 = True
    #endif
    return tmpret116
  def __patsflab__ats2pypre_intrange_loop2_70():
    nonlocal env0, env1, arg0, arg1, arg2, arg3, arg4
    nonlocal apy0, apy1, apy2, apy3, apy4, tmpret116, tmp117, a2rg0, a2rg1, a2rg2, a2rg3, a2rg4, a2py0, a2py1, a2py2, a2py3, a2py4, tmpret118, tmp119, tmp120, tmp121, tmp122
    nonlocal funlab_py, tmplab_py
    funlab_py = 0
    tmp119 = ats2pypre_lt_int0_int0(a2rg2, a2rg3)
    if (tmp119):
      tmp120 = a2rg4[0](a2rg4, a2rg0, a2rg2)
      if (tmp120):
        tmp121 = ats2pypre_add_int0_int0(a2rg2, 1)
        #ATStailcalseq_beg
        a2py0 = a2rg0
        a2py1 = a2rg1
        a2py2 = tmp121
        a2py3 = a2rg3
        a2py4 = a2rg4
        a2rg0 = a2py0
        a2rg1 = a2py1
        a2rg2 = a2py2
        a2rg3 = a2py3
        a2rg4 = a2py4
        funlab_py = 2 #__patsflab__ats2pypre_intrange_loop2_70
        #ATStailcalseq_end
      else:
        tmpret118 = False
      #endif
    else:
      tmp122 = ats2pypre_add_int0_int0(a2rg0, 1)
      #ATStailcalseq_beg
      apy0 = tmp122
      apy1 = a2rg1
      apy2 = env0
      apy3 = env1
      apy4 = a2rg4
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      arg3 = apy3
      arg4 = apy4
      funlab_py = 1 #__patsflab__ats2pypre_intrange_loop1_69
      #ATStailcalseq_end
    #endif
    return tmpret118
  mfundef = { 1: __patsflab__ats2pypre_intrange_loop1_69, 2: __patsflab__ats2pypre_intrange_loop2_70 }
  funlab_py = 1
  while(1):
    tmpret_py = mfundef.get(funlab_py)()
    if (funlab_py == 0): break
  return tmpret_py


def ats2pypre_intrange2_foreach_cloref(arg0, arg1, arg2, arg3, arg4):
  funlab_py = None
  tmplab_py = None
  #__patsflab_intrange2_foreach_cloref
  _ats2pypre_intrange_loop1_72(arg2, arg3, arg0, arg1, arg2, arg3, arg4)
  return#_void


def _ats2pypre_intrange_loop1_72(env0, env1, arg0, arg1, arg2, arg3, arg4):
  apy0 = None
  apy1 = None
  apy2 = None
  apy3 = None
  apy4 = None
  tmp125 = None
  a2rg0 = None
  a2rg1 = None
  a2rg2 = None
  a2rg3 = None
  a2rg4 = None
  a2py0 = None
  a2py1 = None
  a2py2 = None
  a2py3 = None
  a2py4 = None
  tmp127 = None
  tmp129 = None
  tmp130 = None
  funlab_py = None
  tmplab_py = None
  tmpret_py = None
  def __patsflab__ats2pypre_intrange_loop1_72():
    nonlocal env0, env1, arg0, arg1, arg2, arg3, arg4
    nonlocal apy0, apy1, apy2, apy3, apy4, tmp125, a2rg0, a2rg1, a2rg2, a2rg3, a2rg4, a2py0, a2py1, a2py2, a2py3, a2py4, tmp127, tmp129, tmp130
    nonlocal funlab_py, tmplab_py
    funlab_py = 0
    tmp125 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp125):
      #ATStailcalseq_beg
      a2py0 = arg0
      a2py1 = arg1
      a2py2 = arg2
      a2py3 = arg3
      a2py4 = arg4
      a2rg0 = a2py0
      a2rg1 = a2py1
      a2rg2 = a2py2
      a2rg3 = a2py3
      a2rg4 = a2py4
      funlab_py = 2 #__patsflab__ats2pypre_intrange_loop2_73
      #ATStailcalseq_end
    else:
      None#ATSINSmove_void
    #endif
    return#_void
  def __patsflab__ats2pypre_intrange_loop2_73():
    nonlocal env0, env1, arg0, arg1, arg2, arg3, arg4
    nonlocal apy0, apy1, apy2, apy3, apy4, tmp125, a2rg0, a2rg1, a2rg2, a2rg3, a2rg4, a2py0, a2py1, a2py2, a2py3, a2py4, tmp127, tmp129, tmp130
    nonlocal funlab_py, tmplab_py
    funlab_py = 0
    tmp127 = ats2pypre_lt_int0_int0(a2rg2, a2rg3)
    if (tmp127):
      a2rg4[0](a2rg4, a2rg0, a2rg2)
      tmp129 = ats2pypre_add_int0_int0(a2rg2, 1)
      #ATStailcalseq_beg
      a2py0 = a2rg0
      a2py1 = a2rg1
      a2py2 = tmp129
      a2py3 = a2rg3
      a2py4 = a2rg4
      a2rg0 = a2py0
      a2rg1 = a2py1
      a2rg2 = a2py2
      a2rg3 = a2py3
      a2rg4 = a2py4
      funlab_py = 2 #__patsflab__ats2pypre_intrange_loop2_73
      #ATStailcalseq_end
    else:
      tmp130 = ats2pypre_succ_int0(a2rg0)
      #ATStailcalseq_beg
      apy0 = tmp130
      apy1 = a2rg1
      apy2 = env0
      apy3 = env1
      apy4 = a2rg4
      arg0 = apy0
      arg1 = apy1
      arg2 = apy2
      arg3 = apy3
      arg4 = apy4
      funlab_py = 1 #__patsflab__ats2pypre_intrange_loop1_72
      #ATStailcalseq_end
    #endif
    return#_void
  mfundef = { 1: __patsflab__ats2pypre_intrange_loop1_72, 2: __patsflab__ats2pypre_intrange_loop2_73 }
  funlab_py = 1
  while(1):
    tmpret_py = mfundef.get(funlab_py)()
    if (funlab_py == 0): break
  return tmpret_py

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def _ats2pypre_arrayref_patsfun_8__closurerize(env0):
  def _ats2pypre_arrayref_patsfun_8__cfun(cenv, arg0): return _ats2pypre_arrayref_patsfun_8(cenv[1], arg0)
  return (_ats2pypre_arrayref_patsfun_8__cfun, env0)

def ats2pypre_arrayref_exists_cloref(arg0, arg1, arg2):
  tmpret0 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrayref_exists_cloref
  tmpret0 = ats2pypre_int_exists_cloref(arg1, arg2)
  return tmpret0


def ats2pypre_arrayref_forall_cloref(arg0, arg1, arg2):
  tmpret1 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrayref_forall_cloref
  tmpret1 = ats2pypre_int_forall_cloref(arg1, arg2)
  return tmpret1


def ats2pypre_arrayref_foreach_cloref(arg0, arg1, arg2):
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrayref_foreach_cloref
  ats2pypre_int_foreach_cloref(arg1, arg2)
  return#_void


def ats2pypre_arrszref_make_elt(arg0, arg1):
  tmpret3 = None
  tmp4 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrszref_make_elt
  tmp4 = ats2pypre_arrayref_make_elt(arg0, arg1)
  tmpret3 = ats2pypre_arrszref_make_arrayref(tmp4, arg0)
  return tmpret3


def ats2pypre_arrszref_exists_cloref(arg0, arg1):
  tmpret5 = None
  tmp6 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrszref_exists_cloref
  tmp6 = ats2pypre_arrszref_size(arg0)
  tmpret5 = ats2pypre_int_exists_cloref(tmp6, arg1)
  return tmpret5


def ats2pypre_arrszref_forall_cloref(arg0, arg1):
  tmpret7 = None
  tmp8 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrszref_forall_cloref
  tmp8 = ats2pypre_arrszref_size(arg0)
  tmpret7 = ats2pypre_int_forall_cloref(tmp8, arg1)
  return tmpret7


def ats2pypre_arrszref_foreach_cloref(arg0, arg1):
  tmp10 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrszref_foreach_cloref
  tmp10 = ats2pypre_arrszref_size(arg0)
  ats2pypre_int_foreach_cloref(tmp10, arg1)
  return#_void


def ats2pypre_arrszref_foreach_method(arg0):
  tmpret11 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrszref_foreach_method
  tmpret11 = _ats2pypre_arrayref_patsfun_8__closurerize(arg0)
  return tmpret11


def _ats2pypre_arrayref_patsfun_8(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_arrayref_patsfun_8
  ats2pypre_arrszref_foreach_cloref(env0, arg0)
  return#_void


def ats2pypre_arrayref_make_elt(arg0, arg1):
  tmpret13 = None
  tmp14 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrayref_make_elt
  tmp14 = ats2pypre_PYlist_make_elt(arg0, arg1)
  tmpret13 = tmp14
  return tmpret13


def ats2pypre_arrayref_get_at(arg0, arg1):
  tmpret15 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrayref_get_at
  tmpret15 = ats2pypre_PYlist_get_at(arg0, arg1)
  return tmpret15


def ats2pypre_arrayref_set_at(arg0, arg1, arg2):
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrayref_set_at
  ats2pypre_PYlist_set_at(arg0, arg1, arg2)
  return#_void


def ats2pypre_arrszref_make_arrayref(arg0, arg1):
  tmpret17 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrszref_make_arrayref
  tmpret17 = arg0
  return tmpret17


def ats2pypre_arrszref_size(arg0):
  tmpret18 = None
  tmp19 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrszref_size
  tmp19 = ats2pypre_PYlist_length(arg0)
  tmpret18 = tmp19
  return tmpret18


def ats2pypre_arrszref_get_at(arg0, arg1):
  tmpret20 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrszref_get_at
  tmpret20 = ats2pypre_PYlist_get_at(arg0, arg1)
  return tmpret20


def ats2pypre_arrszref_set_at(arg0, arg1, arg2):
  funlab_py = None
  tmplab_py = None
  #__patsflab_arrszref_set_at
  ats2pypre_PYlist_set_at(arg0, arg1, arg2)
  return#_void

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

######
#ATSextcode_beg()
######
######
def ats2pypre_matrixref_make_elt(m, n, x0):
  M = []
  i0 = 0
  mn = m * n
  while (i0 < mn): i0 = i0 + 1; M.append(x0)
  return M
######
######
#ATSextcode_end()
######

######
#ATSextcode_beg()
######
######
def ats2pypre_mtrxszref_make_matrixref(M, m, n):
  return { 'matrix' : M, 'nrow' : m, 'ncol' : n }
######
def ats2pypre_mtrxszref_get_nrow(MSZ): return MSZ['nrow']
def ats2pypre_mtrxszref_get_ncol(MSZ): return MSZ['ncol']
######
def ats2pypre_mtrxszref_get_at(MSZ, i, j):
  nrow = MSZ['nrow']
  ncol = MSZ['ncol']
  if (i < 0): raise IndexError('mtrxszref_get_at')
  if (j < 0): raise IndexError('mtrxszref_get_at')
  if (i >= nrow): raise IndexError('mtrxszref_get_at')
  if (j >= ncol): raise IndexError('mtrxszref_get_at')
  return MSZ['matrix'][i*ncol+j]
######
def ats2pypre_mtrxszref_set_at(MSZ, i, j, x0):
  nrow = MSZ['nrow']
  ncol = MSZ['ncol']
  if (i < 0): raise IndexError('mtrxszref_set_at')
  if (j < 0): raise IndexError('mtrxszref_set_at')
  if (i >= nrow): raise IndexError('mtrxszref_set_at')
  if (j >= ncol): raise IndexError('mtrxszref_set_at')
  MSZ['matrix'][i*ncol+j] = x0; return##_void
######
######
#ATSextcode_end()
######

def _ats2pypre_matrixref_patsfun_7__closurerize(env0):
  def _ats2pypre_matrixref_patsfun_7__cfun(cenv, arg0): return _ats2pypre_matrixref_patsfun_7(cenv[1], arg0)
  return (_ats2pypre_matrixref_patsfun_7__cfun, env0)

def _ats2pypre_matrixref_patsfun_9__closurerize(env0):
  def _ats2pypre_matrixref_patsfun_9__cfun(cenv, arg0): return _ats2pypre_matrixref_patsfun_9(cenv[1], arg0)
  return (_ats2pypre_matrixref_patsfun_9__cfun, env0)

def _ats2pypre_matrixref_patsfun_12__closurerize(env0):
  def _ats2pypre_matrixref_patsfun_12__cfun(cenv, arg0): return _ats2pypre_matrixref_patsfun_12(cenv[1], arg0)
  return (_ats2pypre_matrixref_patsfun_12__cfun, env0)

def ats2pypre_matrixref_exists_cloref(arg0, arg1, arg2, arg3):
  tmpret0 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_matrixref_exists_cloref
  tmpret0 = ats2pypre_int2_exists_cloref(arg1, arg2, arg3)
  return tmpret0


def ats2pypre_matrixref_forall_cloref(arg0, arg1, arg2, arg3):
  tmpret1 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_matrixref_forall_cloref
  tmpret1 = ats2pypre_int2_forall_cloref(arg1, arg2, arg3)
  return tmpret1


def ats2pypre_matrixref_foreach_cloref(arg0, arg1, arg2, arg3):
  funlab_py = None
  tmplab_py = None
  #__patsflab_matrixref_foreach_cloref
  ats2pypre_int2_foreach_cloref(arg1, arg2, arg3)
  return#_void


def ats2pypre_mtrxszref_make_elt(arg0, arg1, arg2):
  tmpret3 = None
  tmp4 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_mtrxszref_make_elt
  tmp4 = ats2pypre_matrixref_make_elt(arg0, arg1, arg2)
  tmpret3 = ats2pypre_mtrxszref_make_matrixref(tmp4, arg0, arg1)
  return tmpret3


def ats2pypre_mtrxszref_exists_cloref(arg0, arg1):
  tmpret5 = None
  tmp6 = None
  tmp7 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_mtrxszref_exists_cloref
  tmp6 = ats2pypre_mtrxszref_get_nrow(arg0)
  tmp7 = ats2pypre_mtrxszref_get_ncol(arg0)
  tmpret5 = ats2pypre_int2_exists_cloref(tmp6, tmp7, arg1)
  return tmpret5


def ats2pypre_mtrxszref_forall_cloref(arg0, arg1):
  tmpret8 = None
  tmp9 = None
  tmp10 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_mtrxszref_forall_cloref
  tmp9 = ats2pypre_mtrxszref_get_nrow(arg0)
  tmp10 = ats2pypre_mtrxszref_get_ncol(arg0)
  tmpret8 = ats2pypre_int2_forall_cloref(tmp9, tmp10, arg1)
  return tmpret8


def ats2pypre_mtrxszref_exists_method(arg0):
  tmpret11 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_mtrxszref_exists_method
  tmpret11 = _ats2pypre_matrixref_patsfun_7__closurerize(arg0)
  return tmpret11


def _ats2pypre_matrixref_patsfun_7(env0, arg0):
  tmpret12 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_matrixref_patsfun_7
  tmpret12 = ats2pypre_mtrxszref_exists_cloref(env0, arg0)
  return tmpret12


def ats2pypre_mtrxszref_forall_method(arg0):
  tmpret13 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_mtrxszref_forall_method
  tmpret13 = _ats2pypre_matrixref_patsfun_9__closurerize(arg0)
  return tmpret13


def _ats2pypre_matrixref_patsfun_9(env0, arg0):
  tmpret14 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_matrixref_patsfun_9
  tmpret14 = ats2pypre_mtrxszref_forall_cloref(env0, arg0)
  return tmpret14


def ats2pypre_mtrxszref_foreach_cloref(arg0, arg1):
  tmp16 = None
  tmp17 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_mtrxszref_foreach_cloref
  tmp16 = ats2pypre_mtrxszref_get_nrow(arg0)
  tmp17 = ats2pypre_mtrxszref_get_ncol(arg0)
  ats2pypre_int2_foreach_cloref(tmp16, tmp17, arg1)
  return#_void


def ats2pypre_mtrxszref_foreach_method(arg0):
  tmpret18 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_mtrxszref_foreach_method
  tmpret18 = _ats2pypre_matrixref_patsfun_12__closurerize(arg0)
  return tmpret18


def _ats2pypre_matrixref_patsfun_12(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_matrixref_patsfun_12
  ats2pypre_mtrxszref_foreach_cloref(env0, arg0)
  return#_void


def ats2pypre_matrixref_get_at(arg0, arg1, arg2, arg3):
  tmpret20 = None
  tmp21 = None
  tmp22 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_matrixref_get_at
  tmp22 = ats2pypre_mul_int1_int1(arg1, arg2)
  tmp21 = ats2pypre_add_int1_int1(tmp22, arg3)
  tmpret20 = ats2pypre_PYlist_get_at(arg0, tmp21)
  return tmpret20


def ats2pypre_matrixref_set_at(arg0, arg1, arg2, arg3, arg4):
  tmp24 = None
  tmp25 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_matrixref_set_at
  tmp25 = ats2pypre_mul_int1_int1(arg1, arg2)
  tmp24 = ats2pypre_add_int1_int1(tmp25, arg3)
  ats2pypre_PYlist_set_at(arg0, tmp24, arg4)
  return#_void

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def slistref_make_nil():
  tmpret0 = None
  tmp1 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_slistref_make_nil
  tmp1 = None
  tmpret0 = ats2pypre_ref(tmp1)
  return tmpret0


def slistref_length(arg0):
  tmpret2 = None
  tmp3 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_slistref_length
  tmp3 = ats2pypre_ref_get_elt(arg0)
  tmpret2 = ats2pypre_list_length(tmp3)
  return tmpret2


def slistref_push(arg0, arg1):
  tmp5 = None
  tmp6 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_slistref_push
  tmp6 = ats2pypre_ref_get_elt(arg0)
  tmp5 = (arg1, tmp6)
  ats2pypre_ref_set_elt(arg0, tmp5)
  return#_void


def slistref_pop_opt(arg0):
  tmpret7 = None
  tmp8 = None
  tmp9 = None
  tmp10 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab0():
    nonlocal arg0
    nonlocal tmpret7, tmp8, tmp9, tmp10
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(tmp8)): tmplab_py = 4 ; return#__atstmplab3
    __atstmplab1()
    return
  def __atstmplab1():
    nonlocal arg0
    nonlocal tmpret7, tmp8, tmp9, tmp10
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret7 = None
    return
  def __atstmplab2():
    nonlocal arg0
    nonlocal tmpret7, tmp8, tmp9, tmp10
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab3()
    return
  def __atstmplab3():
    nonlocal arg0
    nonlocal tmpret7, tmp8, tmp9, tmp10
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp9 = tmp8[0]
    tmp10 = tmp8[1]
    ats2pypre_ref_set_elt(arg0, tmp10)
    tmpret7 = (tmp9, )
    return
  mbranch_1 = { 1: __atstmplab0, 2: __atstmplab1, 3: __atstmplab2, 4: __atstmplab3 }
  #__patsflab_slistref_pop_opt
  tmp8 = ats2pypre_ref_get_elt(arg0)
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret7


def slistref_foldleft(arg0, arg1, arg2):
  tmpret12 = None
  tmp13 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_slistref_foldleft
  tmp13 = ats2pypre_ref_get_elt(arg0)
  tmpret12 = ats2pypre_list_foldleft(tmp13, arg1, arg2)
  return tmpret12


def slistref_foldright(arg0, arg1, arg2):
  tmpret14 = None
  tmp15 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_slistref_foldright
  tmp15 = ats2pypre_ref_get_elt(arg0)
  tmpret14 = ats2pypre_list_foldright(tmp15, arg1, arg2)
  return tmpret14

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def ats2pypre_qlistref_make_nil():
  tmpret0 = None
  tmp1 = None
  tmp2 = None
  tmp3 = None
  tmp4 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_qlistref_make_nil
  tmp2 = None
  tmp1 = ats2pypre_ref(tmp2)
  tmp4 = None
  tmp3 = ats2pypre_ref(tmp4)
  tmpret0 = (tmp1, tmp3)
  return tmpret0


def ats2pypre_qlistref_length(arg0):
  tmpret5 = None
  tmp6 = None
  tmp7 = None
  tmp8 = None
  tmp9 = None
  tmp10 = None
  tmp11 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_qlistref_length
  tmp6 = arg0[0]
  tmp7 = arg0[1]
  tmp9 = ats2pypre_ref_get_elt(tmp6)
  tmp8 = ats2pypre_list_length(tmp9)
  tmp11 = ats2pypre_ref_get_elt(tmp7)
  tmp10 = ats2pypre_list_length(tmp11)
  tmpret5 = ats2pypre_add_int1_int1(tmp8, tmp10)
  return tmpret5


def ats2pypre_qlistref_enqueue(arg0, arg1):
  tmp13 = None
  tmp14 = None
  tmp15 = None
  tmp16 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_qlistref_enqueue
  tmp13 = arg0[0]
  tmp14 = arg0[1]
  tmp16 = ats2pypre_ref_get_elt(tmp13)
  tmp15 = (arg1, tmp16)
  ats2pypre_ref_set_elt(tmp13, tmp15)
  return#_void


def ats2pypre_qlistref_dequeue_opt(arg0):
  tmpret17 = None
  tmp18 = None
  tmp19 = None
  tmp20 = None
  tmp21 = None
  tmp22 = None
  tmp23 = None
  tmp25 = None
  tmp26 = None
  tmp27 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  mbranch_2 = None
  def __atstmplab0():
    nonlocal arg0
    nonlocal tmpret17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp25, tmp26, tmp27
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(tmp20)): tmplab_py = 4 ; return#__atstmplab3
    __atstmplab1()
    return
  def __atstmplab1():
    nonlocal arg0
    nonlocal tmpret17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp25, tmp26, tmp27
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp23 = ats2pypre_ref_get_elt(tmp18)
    tmp25 = None
    ats2pypre_ref_set_elt(tmp18, tmp25)
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_2.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    return
  def __atstmplab2():
    nonlocal arg0
    nonlocal tmpret17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp25, tmp26, tmp27
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab3()
    return
  def __atstmplab3():
    nonlocal arg0
    nonlocal tmpret17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp25, tmp26, tmp27
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp21 = tmp20[0]
    tmp22 = tmp20[1]
    ats2pypre_ref_set_elt(tmp19, tmp22)
    tmpret17 = (tmp21, )
    return
  def __atstmplab4():
    nonlocal arg0
    nonlocal tmpret17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp25, tmp26, tmp27
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(tmp23)): tmplab_py = 4 ; return#__atstmplab7
    __atstmplab5()
    return
  def __atstmplab5():
    nonlocal arg0
    nonlocal tmpret17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp25, tmp26, tmp27
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret17 = None
    return
  def __atstmplab6():
    nonlocal arg0
    nonlocal tmpret17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp25, tmp26, tmp27
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab7()
    return
  def __atstmplab7():
    nonlocal arg0
    nonlocal tmpret17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp25, tmp26, tmp27
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp26 = tmp23[0]
    tmp27 = tmp23[1]
    ats2pypre_ref_set_elt(tmp19, tmp27)
    tmpret17 = (tmp26, )
    return
  mbranch_1 = { 1: __atstmplab0, 2: __atstmplab1, 3: __atstmplab2, 4: __atstmplab3 }
  mbranch_2 = { 1: __atstmplab4, 2: __atstmplab5, 3: __atstmplab6, 4: __atstmplab7 }
  #__patsflab_qlistref_dequeue_opt
  tmp18 = arg0[0]
  tmp19 = arg0[1]
  tmp20 = ats2pypre_ref_get_elt(tmp19)
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret17


def ats2pypre_qlistref_foldleft(arg0, arg1, arg2):
  tmpret30 = None
  tmp31 = None
  tmp32 = None
  tmp41 = None
  tmp42 = None
  tmp43 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_qlistref_foldleft
  tmp31 = arg0[0]
  tmp32 = arg0[1]
  tmp41 = ats2pypre_ref_get_elt(tmp31)
  tmp43 = ats2pypre_ref_get_elt(tmp32)
  tmp42 = _ats2pypre_qlistref_auxl_5(arg2, arg1, tmp43)
  tmpret30 = _ats2pypre_qlistref_auxr_6(arg2, tmp41, tmp42)
  return tmpret30


def _ats2pypre_qlistref_auxl_5(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret33 = None
  tmp34 = None
  tmp35 = None
  tmp36 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab8():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret33, tmp34, tmp35, tmp36
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab11
    __atstmplab9()
    return
  def __atstmplab9():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret33, tmp34, tmp35, tmp36
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret33 = arg0
    return
  def __atstmplab10():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret33, tmp34, tmp35, tmp36
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab11()
    return
  def __atstmplab11():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret33, tmp34, tmp35, tmp36
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp34 = arg1[0]
    tmp35 = arg1[1]
    tmp36 = env0[0](env0, arg0, tmp34)
    #ATStailcalseq_beg
    apy0 = tmp36
    apy1 = tmp35
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_qlistref_auxl_5
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab8, 2: __atstmplab9, 3: __atstmplab10, 4: __atstmplab11 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_qlistref_auxl_5
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret33


def _ats2pypre_qlistref_auxr_6(env0, arg0, arg1):
  tmpret37 = None
  tmp38 = None
  tmp39 = None
  tmp40 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab12():
    nonlocal env0, arg0, arg1
    nonlocal tmpret37, tmp38, tmp39, tmp40
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab15
    __atstmplab13()
    return
  def __atstmplab13():
    nonlocal env0, arg0, arg1
    nonlocal tmpret37, tmp38, tmp39, tmp40
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret37 = arg1
    return
  def __atstmplab14():
    nonlocal env0, arg0, arg1
    nonlocal tmpret37, tmp38, tmp39, tmp40
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab15()
    return
  def __atstmplab15():
    nonlocal env0, arg0, arg1
    nonlocal tmpret37, tmp38, tmp39, tmp40
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp38 = arg0[0]
    tmp39 = arg0[1]
    tmp40 = _ats2pypre_qlistref_auxr_6(env0, tmp39, arg1)
    tmpret37 = env0[0](env0, tmp40, tmp38)
    return
  mbranch_1 = { 1: __atstmplab12, 2: __atstmplab13, 3: __atstmplab14, 4: __atstmplab15 }
  #__patsflab__ats2pypre_qlistref_auxr_6
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret37


def ats2pypre_qlistref_foldright(arg0, arg1, arg2):
  tmpret44 = None
  tmp45 = None
  tmp46 = None
  tmp55 = None
  tmp56 = None
  tmp57 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_qlistref_foldright
  tmp45 = arg0[0]
  tmp46 = arg0[1]
  tmp55 = ats2pypre_ref_get_elt(tmp46)
  tmp57 = ats2pypre_ref_get_elt(tmp45)
  tmp56 = _ats2pypre_qlistref_auxl_8(arg1, arg2, tmp57)
  tmpret44 = _ats2pypre_qlistref_auxr_9(arg1, tmp55, tmp56)
  return tmpret44


def _ats2pypre_qlistref_auxl_8(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret47 = None
  tmp48 = None
  tmp49 = None
  tmp50 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab16():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret47, tmp48, tmp49, tmp50
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab19
    __atstmplab17()
    return
  def __atstmplab17():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret47, tmp48, tmp49, tmp50
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret47 = arg0
    return
  def __atstmplab18():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret47, tmp48, tmp49, tmp50
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab19()
    return
  def __atstmplab19():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret47, tmp48, tmp49, tmp50
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp48 = arg1[0]
    tmp49 = arg1[1]
    tmp50 = env0[0](env0, tmp48, arg0)
    #ATStailcalseq_beg
    apy0 = tmp50
    apy1 = tmp49
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_qlistref_auxl_8
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab16, 2: __atstmplab17, 3: __atstmplab18, 4: __atstmplab19 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_qlistref_auxl_8
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret47


def _ats2pypre_qlistref_auxr_9(env0, arg0, arg1):
  tmpret51 = None
  tmp52 = None
  tmp53 = None
  tmp54 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab20():
    nonlocal env0, arg0, arg1
    nonlocal tmpret51, tmp52, tmp53, tmp54
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab23
    __atstmplab21()
    return
  def __atstmplab21():
    nonlocal env0, arg0, arg1
    nonlocal tmpret51, tmp52, tmp53, tmp54
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret51 = arg1
    return
  def __atstmplab22():
    nonlocal env0, arg0, arg1
    nonlocal tmpret51, tmp52, tmp53, tmp54
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab23()
    return
  def __atstmplab23():
    nonlocal env0, arg0, arg1
    nonlocal tmpret51, tmp52, tmp53, tmp54
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp52 = arg0[0]
    tmp53 = arg0[1]
    tmp54 = _ats2pypre_qlistref_auxr_9(env0, tmp53, arg1)
    tmpret51 = env0[0](env0, tmp52, tmp54)
    return
  mbranch_1 = { 1: __atstmplab20, 2: __atstmplab21, 3: __atstmplab22, 4: __atstmplab23 }
  #__patsflab__ats2pypre_qlistref_auxr_9
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret51

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def _ats2pypre_ML_list0_patsfun_29__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_29__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_29(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_29__cfun, env0)

def _ats2pypre_ML_list0_patsfun_32__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_32__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_32(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_32__cfun, env0)

def _ats2pypre_ML_list0_patsfun_35__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_35__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_35(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_35__cfun, env0)

def _ats2pypre_ML_list0_patsfun_38__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_38__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_38(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_38__cfun, env0)

def _ats2pypre_ML_list0_patsfun_42__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_42__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_42(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_42__cfun, env0)

def _ats2pypre_ML_list0_patsfun_45__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_45__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_45(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_45__cfun, env0)

def _ats2pypre_ML_list0_patsfun_48__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_48__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_48(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_48__cfun, env0)

def _ats2pypre_ML_list0_patsfun_51__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_51__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_51(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_51__cfun, env0)

def _ats2pypre_ML_list0_patsfun_55__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_55__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_55(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_55__cfun, env0)

def _ats2pypre_ML_list0_patsfun_58__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_58__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_58(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_58__cfun, env0)

def _ats2pypre_ML_list0_patsfun_63__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_63__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_63(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_63__cfun, env0)

def _ats2pypre_ML_list0_patsfun_66__closurerize(env0):
  def _ats2pypre_ML_list0_patsfun_66__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_66(cenv[1], arg0)
  return (_ats2pypre_ML_list0_patsfun_66__cfun, env0)

def _ats2pypre_ML_list0_patsfun_72__closurerize(env0, env1):
  def _ats2pypre_ML_list0_patsfun_72__cfun(cenv, arg0): return _ats2pypre_ML_list0_patsfun_72(cenv[1], cenv[2], arg0)
  return (_ats2pypre_ML_list0_patsfun_72__cfun, env0, env1)

def ats2pypre_ML_list0_head_opt(arg0):
  tmpret7 = None
  tmp8 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab6():
    nonlocal arg0
    nonlocal tmpret7, tmp8
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab9
    __atstmplab7()
    return
  def __atstmplab7():
    nonlocal arg0
    nonlocal tmpret7, tmp8
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret7 = None
    return
  def __atstmplab8():
    nonlocal arg0
    nonlocal tmpret7, tmp8
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab9()
    return
  def __atstmplab9():
    nonlocal arg0
    nonlocal tmpret7, tmp8
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp8 = arg0[0]
    tmpret7 = (tmp8, )
    return
  mbranch_1 = { 1: __atstmplab6, 2: __atstmplab7, 3: __atstmplab8, 4: __atstmplab9 }
  #__patsflab_list0_head_opt
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret7


def ats2pypre_ML_list0_tail_opt(arg0):
  tmpret10 = None
  tmp12 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab10():
    nonlocal arg0
    nonlocal tmpret10, tmp12
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab13
    __atstmplab11()
    return
  def __atstmplab11():
    nonlocal arg0
    nonlocal tmpret10, tmp12
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret10 = None
    return
  def __atstmplab12():
    nonlocal arg0
    nonlocal tmpret10, tmp12
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab13()
    return
  def __atstmplab13():
    nonlocal arg0
    nonlocal tmpret10, tmp12
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp12 = arg0[1]
    tmpret10 = (tmp12, )
    return
  mbranch_1 = { 1: __atstmplab10, 2: __atstmplab11, 3: __atstmplab12, 4: __atstmplab13 }
  #__patsflab_list0_tail_opt
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret10


def ats2pypre_ML_list0_length(arg0):
  tmpret13 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_length
  tmpret13 = ats2pypre_list_length(arg0)
  return tmpret13


def ats2pypre_ML_list0_last_opt(arg0):
  tmpret14 = None
  tmp18 = None
  tmp19 = None
  tmp20 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab18():
    nonlocal arg0
    nonlocal tmpret14, tmp18, tmp19, tmp20
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab21
    __atstmplab19()
    return
  def __atstmplab19():
    nonlocal arg0
    nonlocal tmpret14, tmp18, tmp19, tmp20
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret14 = None
    return
  def __atstmplab20():
    nonlocal arg0
    nonlocal tmpret14, tmp18, tmp19, tmp20
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab21()
    return
  def __atstmplab21():
    nonlocal arg0
    nonlocal tmpret14, tmp18, tmp19, tmp20
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp18 = arg0[0]
    tmp19 = arg0[1]
    tmp20 = _ats2pypre_ML_list0_loop_8(tmp18, tmp19)
    tmpret14 = (tmp20, )
    return
  mbranch_1 = { 1: __atstmplab18, 2: __atstmplab19, 3: __atstmplab20, 4: __atstmplab21 }
  #__patsflab_list0_last_opt
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret14


def _ats2pypre_ML_list0_loop_8(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret15 = None
  tmp16 = None
  tmp17 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab14():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab17
    __atstmplab15()
    return
  def __atstmplab15():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret15 = arg0
    return
  def __atstmplab16():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab17()
    return
  def __atstmplab17():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret15, tmp16, tmp17
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp16 = arg1[0]
    tmp17 = arg1[1]
    #ATStailcalseq_beg
    apy0 = tmp16
    apy1 = tmp17
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_ML_list0_loop_8
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab14, 2: __atstmplab15, 3: __atstmplab16, 4: __atstmplab17 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_ML_list0_loop_8
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret15


def ats2pypre_ML_list0_get_at_opt(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret21 = None
  tmp22 = None
  tmp23 = None
  tmp24 = None
  tmp25 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab22():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp23, tmp24, tmp25
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab25
    __atstmplab23()
    return
  def __atstmplab23():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp23, tmp24, tmp25
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret21 = None
    return
  def __atstmplab24():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp23, tmp24, tmp25
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab25()
    return
  def __atstmplab25():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret21, tmp22, tmp23, tmp24, tmp25
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp22 = arg0[0]
    tmp23 = arg0[1]
    tmp24 = ats2pypre_gt_int1_int1(arg1, 0)
    if (tmp24):
      tmp25 = ats2pypre_sub_int1_int1(arg1, 1)
      #ATStailcalseq_beg
      apy0 = tmp23
      apy1 = tmp25
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab_list0_get_at_opt
      #ATStailcalseq_end
    else:
      tmpret21 = (tmp22, )
    #endif
    return
  mbranch_1 = { 1: __atstmplab22, 2: __atstmplab23, 3: __atstmplab24, 4: __atstmplab25 }
  while(1):
    funlab_py = 0
    #__patsflab_list0_get_at_opt
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret21


def ats2pypre_ML_list0_make_elt(arg0, arg1):
  tmpret26 = None
  tmp27 = None
  tmp28 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_make_elt
  tmp27 = ats2pypre_gte_int1_int1(arg0, 0)
  if (tmp27):
    tmp28 = ats2pypre_list_make_elt(arg0, arg1)
    tmpret26 = tmp28
  else:
    tmpret26 = None
  #endif
  return tmpret26


def ats2pypre_ML_list0_make_intrange_2(arg0, arg1):
  tmpret29 = None
  tmp30 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_make_intrange_2
  tmp30 = ats2pypre_list_make_intrange_2(arg0, arg1)
  tmpret29 = tmp30
  return tmpret29


def ats2pypre_ML_list0_make_intrange_3(arg0, arg1, arg2):
  tmpret31 = None
  tmp32 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_make_intrange_3
  tmp32 = ats2pypre_list_make_intrange_3(arg0, arg1, arg2)
  tmpret31 = tmp32
  return tmpret31


def ats2pypre_ML_list0_snoc(arg0, arg1):
  tmpret44 = None
  tmp45 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_snoc
  tmp45 = ats2pypre_list_snoc(arg0, arg1)
  tmpret44 = tmp45
  return tmpret44


def ats2pypre_ML_list0_extend(arg0, arg1):
  tmpret46 = None
  tmp47 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_extend
  tmp47 = ats2pypre_list_extend(arg0, arg1)
  tmpret46 = tmp47
  return tmpret46


def ats2pypre_ML_list0_append(arg0, arg1):
  tmpret48 = None
  tmp49 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_append
  tmp49 = ats2pypre_list_append(arg0, arg1)
  tmpret48 = tmp49
  return tmpret48


def ats2pypre_ML_mul_int_list0(arg0, arg1):
  tmpret50 = None
  tmp51 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_mul_int_list0
  tmp51 = ats2pypre_mul_int_list(arg0, arg1)
  tmpret50 = tmp51
  return tmpret50


def ats2pypre_ML_list0_reverse(arg0):
  tmpret52 = None
  tmp53 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_reverse
  tmp53 = ats2pypre_list_reverse(arg0)
  tmpret52 = tmp53
  return tmpret52


def ats2pypre_ML_list0_reverse_append(arg0, arg1):
  tmpret54 = None
  tmp55 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_reverse_append
  tmp55 = ats2pypre_list_reverse_append(arg0, arg1)
  tmpret54 = tmp55
  return tmpret54


def ats2pypre_ML_list0_concat(arg0):
  tmpret56 = None
  tmp57 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_concat
  tmp57 = ats2pypre_list_concat(arg0)
  tmpret56 = tmp57
  return tmpret56


def ats2pypre_ML_list0_remove_at_opt(arg0, arg1):
  tmpret58 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_remove_at_opt
  tmpret58 = _ats2pypre_ML_list0_aux_26(arg0, 0)
  return tmpret58


def _ats2pypre_ML_list0_aux_26(arg0, arg1):
  tmpret59 = None
  tmp60 = None
  tmp61 = None
  tmp62 = None
  tmp63 = None
  tmp64 = None
  tmp65 = None
  tmp66 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  mbranch_2 = None
  def __atstmplab30():
    nonlocal arg0, arg1
    nonlocal tmpret59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab33
    __atstmplab31()
    return
  def __atstmplab31():
    nonlocal arg0, arg1
    nonlocal tmpret59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret59 = None
    return
  def __atstmplab32():
    nonlocal arg0, arg1
    nonlocal tmpret59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab33()
    return
  def __atstmplab33():
    nonlocal arg0, arg1
    nonlocal tmpret59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp60 = arg0[0]
    tmp61 = arg0[1]
    tmp62 = ats2pypre_gt_int1_int1(arg1, 0)
    if (tmp62):
      tmp64 = ats2pypre_sub_int1_int1(arg1, 1)
      tmp63 = _ats2pypre_ML_list0_aux_26(tmp61, tmp64)
      #ATScaseofseq_beg
      tmplab_py = 1
      while(1):
        mbranch_2.get(tmplab_py)()
        if (tmplab_py == 0): break
      #ATScaseofseq_end
    else:
      tmpret59 = (tmp61, )
    #endif
    return
  def __atstmplab34():
    nonlocal arg0, arg1
    nonlocal tmpret59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(tmp63)): tmplab_py = 4 ; return#__atstmplab37
    __atstmplab35()
    return
  def __atstmplab35():
    nonlocal arg0, arg1
    nonlocal tmpret59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret59 = None
    return
  def __atstmplab36():
    nonlocal arg0, arg1
    nonlocal tmpret59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab37()
    return
  def __atstmplab37():
    nonlocal arg0, arg1
    nonlocal tmpret59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp65 = tmp63[0]
    #ATSINSfreecon(tmp63);
    tmp66 = (tmp60, tmp65)
    tmpret59 = (tmp66, )
    return
  mbranch_1 = { 1: __atstmplab30, 2: __atstmplab31, 3: __atstmplab32, 4: __atstmplab33 }
  mbranch_2 = { 1: __atstmplab34, 2: __atstmplab35, 3: __atstmplab36, 4: __atstmplab37 }
  #__patsflab__ats2pypre_ML_list0_aux_26
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret59


def ats2pypre_ML_list0_exists(arg0, arg1):
  tmpret67 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_exists
  tmpret67 = ats2pypre_list_exists(arg0, arg1)
  return tmpret67


def ats2pypre_ML_list0_exists_method(arg0):
  tmpret68 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_exists_method
  tmpret68 = _ats2pypre_ML_list0_patsfun_29__closurerize(arg0)
  return tmpret68


def _ats2pypre_ML_list0_patsfun_29(env0, arg0):
  tmpret69 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_29
  tmpret69 = ats2pypre_ML_list0_exists(env0, arg0)
  return tmpret69


def ats2pypre_ML_list0_iexists(arg0, arg1):
  tmpret70 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_iexists
  tmpret70 = ats2pypre_list_iexists(arg0, arg1)
  return tmpret70


def ats2pypre_ML_list0_iexists_method(arg0):
  tmpret71 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_iexists_method
  tmpret71 = _ats2pypre_ML_list0_patsfun_32__closurerize(arg0)
  return tmpret71


def _ats2pypre_ML_list0_patsfun_32(env0, arg0):
  tmpret72 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_32
  tmpret72 = ats2pypre_ML_list0_iexists(env0, arg0)
  return tmpret72


def ats2pypre_ML_list0_forall(arg0, arg1):
  tmpret73 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_forall
  tmpret73 = ats2pypre_list_forall(arg0, arg1)
  return tmpret73


def ats2pypre_ML_list0_forall_method(arg0):
  tmpret74 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_forall_method
  tmpret74 = _ats2pypre_ML_list0_patsfun_35__closurerize(arg0)
  return tmpret74


def _ats2pypre_ML_list0_patsfun_35(env0, arg0):
  tmpret75 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_35
  tmpret75 = ats2pypre_ML_list0_forall(env0, arg0)
  return tmpret75


def ats2pypre_ML_list0_iforall(arg0, arg1):
  tmpret76 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_iforall
  tmpret76 = ats2pypre_list_iforall(arg0, arg1)
  return tmpret76


def ats2pypre_ML_list0_iforall_method(arg0):
  tmpret77 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_iforall_method
  tmpret77 = _ats2pypre_ML_list0_patsfun_38__closurerize(arg0)
  return tmpret77


def _ats2pypre_ML_list0_patsfun_38(env0, arg0):
  tmpret78 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_38
  tmpret78 = ats2pypre_ML_list0_iforall(env0, arg0)
  return tmpret78


def ats2pypre_ML_list0_app(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_app
  ats2pypre_ML_list0_foreach(arg0, arg1)
  return#_void


def ats2pypre_ML_list0_foreach(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_foreach
  ats2pypre_list_foreach(arg0, arg1)
  return#_void


def ats2pypre_ML_list0_foreach_method(arg0):
  tmpret81 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_foreach_method
  tmpret81 = _ats2pypre_ML_list0_patsfun_42__closurerize(arg0)
  return tmpret81


def _ats2pypre_ML_list0_patsfun_42(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_42
  ats2pypre_ML_list0_foreach(env0, arg0)
  return#_void


def ats2pypre_ML_list0_iforeach(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_iforeach
  ats2pypre_list_iforeach(arg0, arg1)
  return#_void


def ats2pypre_ML_list0_iforeach_method(arg0):
  tmpret84 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_iforeach_method
  tmpret84 = _ats2pypre_ML_list0_patsfun_45__closurerize(arg0)
  return tmpret84


def _ats2pypre_ML_list0_patsfun_45(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_45
  ats2pypre_ML_list0_iforeach(env0, arg0)
  return#_void


def ats2pypre_ML_list0_rforeach(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_rforeach
  ats2pypre_list_rforeach(arg0, arg1)
  return#_void


def ats2pypre_ML_list0_rforeach_method(arg0):
  tmpret87 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_rforeach_method
  tmpret87 = _ats2pypre_ML_list0_patsfun_48__closurerize(arg0)
  return tmpret87


def _ats2pypre_ML_list0_patsfun_48(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_48
  ats2pypre_ML_list0_rforeach(env0, arg0)
  return#_void


def ats2pypre_ML_list0_filter(arg0, arg1):
  tmpret89 = None
  tmp90 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_filter
  tmp90 = ats2pypre_list_filter(arg0, arg1)
  tmpret89 = tmp90
  return tmpret89


def ats2pypre_ML_list0_filter_method(arg0):
  tmpret91 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_filter_method
  tmpret91 = _ats2pypre_ML_list0_patsfun_51__closurerize(arg0)
  return tmpret91


def _ats2pypre_ML_list0_patsfun_51(env0, arg0):
  tmpret92 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_51
  tmpret92 = ats2pypre_ML_list0_filter(env0, arg0)
  return tmpret92


def _057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2py3_057_ATS2_055_0_056_3_056_2_057_SATS_057_ML_057_list0_056_sats__list0_labelize(arg0):
  tmpret93 = None
  tmp94 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_labelize
  tmp94 = _057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2py3_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_labelize(arg0)
  tmpret93 = tmp94
  return tmpret93


def ats2pypre_ML_list0_map(arg0, arg1):
  tmpret95 = None
  tmp96 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_map
  tmp96 = ats2pypre_list_map(arg0, arg1)
  tmpret95 = tmp96
  return tmpret95


def ats2pypre_ML_list0_map_method(arg0, arg1):
  tmpret97 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_map_method
  tmpret97 = _ats2pypre_ML_list0_patsfun_55__closurerize(arg0)
  return tmpret97


def _ats2pypre_ML_list0_patsfun_55(env0, arg0):
  tmpret98 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_55
  tmpret98 = ats2pypre_ML_list0_map(env0, arg0)
  return tmpret98


def ats2pypre_ML_list0_imap(arg0, arg1):
  tmpret99 = None
  tmp100 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_imap
  tmp100 = ats2pypre_list_imap(arg0, arg1)
  tmpret99 = tmp100
  return tmpret99


def ats2pypre_ML_list0_imap_method(arg0, arg1):
  tmpret101 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_imap_method
  tmpret101 = _ats2pypre_ML_list0_patsfun_58__closurerize(arg0)
  return tmpret101


def _ats2pypre_ML_list0_patsfun_58(env0, arg0):
  tmpret102 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_58
  tmpret102 = ats2pypre_ML_list0_imap(env0, arg0)
  return tmpret102


def ats2pypre_ML_list0_map2(arg0, arg1, arg2):
  tmpret103 = None
  tmp104 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_map2
  tmp104 = ats2pypre_list_map2(arg0, arg1, arg2)
  tmpret103 = tmp104
  return tmpret103


def ats2pypre_ML_list0_mapcons(arg0, arg1):
  tmpret105 = None
  tmp106 = None
  tmp107 = None
  tmp108 = None
  tmp109 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab38():
    nonlocal arg0, arg1
    nonlocal tmpret105, tmp106, tmp107, tmp108, tmp109
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab41
    __atstmplab39()
    return
  def __atstmplab39():
    nonlocal arg0, arg1
    nonlocal tmpret105, tmp106, tmp107, tmp108, tmp109
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret105 = None
    return
  def __atstmplab40():
    nonlocal arg0, arg1
    nonlocal tmpret105, tmp106, tmp107, tmp108, tmp109
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab41()
    return
  def __atstmplab41():
    nonlocal arg0, arg1
    nonlocal tmpret105, tmp106, tmp107, tmp108, tmp109
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp106 = arg1[0]
    tmp107 = arg1[1]
    tmp108 = (arg0, tmp106)
    tmp109 = ats2pypre_ML_list0_mapcons(arg0, tmp107)
    tmpret105 = (tmp108, tmp109)
    return
  mbranch_1 = { 1: __atstmplab38, 2: __atstmplab39, 3: __atstmplab40, 4: __atstmplab41 }
  #__patsflab_list0_mapcons
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret105


def ats2pypre_ML_list0_find_opt(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret110 = None
  tmp111 = None
  tmp112 = None
  tmp113 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab42():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret110, tmp111, tmp112, tmp113
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab45
    __atstmplab43()
    return
  def __atstmplab43():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret110, tmp111, tmp112, tmp113
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret110 = None
    return
  def __atstmplab44():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret110, tmp111, tmp112, tmp113
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab45()
    return
  def __atstmplab45():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret110, tmp111, tmp112, tmp113
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp111 = arg0[0]
    tmp112 = arg0[1]
    tmp113 = arg1[0](arg1, tmp111)
    if (tmp113):
      tmpret110 = (tmp111, )
    else:
      #ATStailcalseq_beg
      apy0 = tmp112
      apy1 = arg1
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab_list0_find_opt
      #ATStailcalseq_end
    #endif
    return
  mbranch_1 = { 1: __atstmplab42, 2: __atstmplab43, 3: __atstmplab44, 4: __atstmplab45 }
  while(1):
    funlab_py = 0
    #__patsflab_list0_find_opt
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret110


def ats2pypre_ML_list0_find_opt_method(arg0):
  tmpret114 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_find_opt_method
  tmpret114 = _ats2pypre_ML_list0_patsfun_63__closurerize(arg0)
  return tmpret114


def _ats2pypre_ML_list0_patsfun_63(env0, arg0):
  tmpret115 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_63
  tmpret115 = ats2pypre_ML_list0_find_opt(env0, arg0)
  return tmpret115


def ats2pypre_ML_list0_find_suffix(arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret116 = None
  tmp118 = None
  tmp119 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab46():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret116, tmp118, tmp119
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab49
    __atstmplab47()
    return
  def __atstmplab47():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret116, tmp118, tmp119
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret116 = None
    return
  def __atstmplab48():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret116, tmp118, tmp119
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab49()
    return
  def __atstmplab49():
    nonlocal arg0, arg1
    nonlocal apy0, apy1, tmpret116, tmp118, tmp119
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp118 = arg0[1]
    tmp119 = arg1[0](arg1, arg0)
    if (tmp119):
      tmpret116 = arg0
    else:
      #ATStailcalseq_beg
      apy0 = tmp118
      apy1 = arg1
      arg0 = apy0
      arg1 = apy1
      funlab_py = 1 #__patsflab_list0_find_suffix
      #ATStailcalseq_end
    #endif
    return
  mbranch_1 = { 1: __atstmplab46, 2: __atstmplab47, 3: __atstmplab48, 4: __atstmplab49 }
  while(1):
    funlab_py = 0
    #__patsflab_list0_find_suffix
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret116


def ats2pypre_ML_list0_find_suffix_method(arg0):
  tmpret120 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_find_suffix_method
  tmpret120 = _ats2pypre_ML_list0_patsfun_66__closurerize(arg0)
  return tmpret120


def _ats2pypre_ML_list0_patsfun_66(env0, arg0):
  tmpret121 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_66
  tmpret121 = ats2pypre_ML_list0_find_suffix(env0, arg0)
  return tmpret121


def ats2pypre_ML_list0_zip(arg0, arg1):
  tmpret122 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_zip
  tmpret122 = _ats2pypre_ML_list0_aux_68(arg0, arg1)
  return tmpret122


def _ats2pypre_ML_list0_aux_68(arg0, arg1):
  tmpret123 = None
  tmp124 = None
  tmp125 = None
  tmp126 = None
  tmp127 = None
  tmp128 = None
  tmp129 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  mbranch_2 = None
  def __atstmplab50():
    nonlocal arg0, arg1
    nonlocal tmpret123, tmp124, tmp125, tmp126, tmp127, tmp128, tmp129
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab53
    __atstmplab51()
    return
  def __atstmplab51():
    nonlocal arg0, arg1
    nonlocal tmpret123, tmp124, tmp125, tmp126, tmp127, tmp128, tmp129
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret123 = None
    return
  def __atstmplab52():
    nonlocal arg0, arg1
    nonlocal tmpret123, tmp124, tmp125, tmp126, tmp127, tmp128, tmp129
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab53()
    return
  def __atstmplab53():
    nonlocal arg0, arg1
    nonlocal tmpret123, tmp124, tmp125, tmp126, tmp127, tmp128, tmp129
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp124 = arg0[0]
    tmp125 = arg0[1]
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_2.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    return
  def __atstmplab54():
    nonlocal arg0, arg1
    nonlocal tmpret123, tmp124, tmp125, tmp126, tmp127, tmp128, tmp129
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab57
    __atstmplab55()
    return
  def __atstmplab55():
    nonlocal arg0, arg1
    nonlocal tmpret123, tmp124, tmp125, tmp126, tmp127, tmp128, tmp129
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret123 = None
    return
  def __atstmplab56():
    nonlocal arg0, arg1
    nonlocal tmpret123, tmp124, tmp125, tmp126, tmp127, tmp128, tmp129
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab57()
    return
  def __atstmplab57():
    nonlocal arg0, arg1
    nonlocal tmpret123, tmp124, tmp125, tmp126, tmp127, tmp128, tmp129
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp126 = arg1[0]
    tmp127 = arg1[1]
    tmp128 = (tmp124, tmp126)
    tmp129 = _ats2pypre_ML_list0_aux_68(tmp125, tmp127)
    tmpret123 = (tmp128, tmp129)
    return
  mbranch_1 = { 1: __atstmplab50, 2: __atstmplab51, 3: __atstmplab52, 4: __atstmplab53 }
  mbranch_2 = { 1: __atstmplab54, 2: __atstmplab55, 3: __atstmplab56, 4: __atstmplab57 }
  #__patsflab__ats2pypre_ML_list0_aux_68
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret123


def ats2pypre_ML_list0_zipwith(arg0, arg1, arg2):
  tmpret130 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_zipwith
  tmpret130 = _ats2pypre_ML_list0_aux_70(arg0, arg1, arg2)
  return tmpret130


def _ats2pypre_ML_list0_aux_70(arg0, arg1, arg2):
  tmpret131 = None
  tmp132 = None
  tmp133 = None
  tmp134 = None
  tmp135 = None
  tmp136 = None
  tmp137 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  mbranch_2 = None
  def __atstmplab58():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret131, tmp132, tmp133, tmp134, tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab61
    __atstmplab59()
    return
  def __atstmplab59():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret131, tmp132, tmp133, tmp134, tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret131 = None
    return
  def __atstmplab60():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret131, tmp132, tmp133, tmp134, tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab61()
    return
  def __atstmplab61():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret131, tmp132, tmp133, tmp134, tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp132 = arg0[0]
    tmp133 = arg0[1]
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_2.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    return
  def __atstmplab62():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret131, tmp132, tmp133, tmp134, tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab65
    __atstmplab63()
    return
  def __atstmplab63():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret131, tmp132, tmp133, tmp134, tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmpret131 = None
    return
  def __atstmplab64():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret131, tmp132, tmp133, tmp134, tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    __atstmplab65()
    return
  def __atstmplab65():
    nonlocal arg0, arg1, arg2
    nonlocal tmpret131, tmp132, tmp133, tmp134, tmp135, tmp136, tmp137
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1, mbranch_2
    tmplab_py = 0
    tmp134 = arg1[0]
    tmp135 = arg1[1]
    tmp136 = arg2[0](arg2, tmp132, tmp134)
    tmp137 = _ats2pypre_ML_list0_aux_70(tmp133, tmp135, arg2)
    tmpret131 = (tmp136, tmp137)
    return
  mbranch_1 = { 1: __atstmplab58, 2: __atstmplab59, 3: __atstmplab60, 4: __atstmplab61 }
  mbranch_2 = { 1: __atstmplab62, 2: __atstmplab63, 3: __atstmplab64, 4: __atstmplab65 }
  #__patsflab__ats2pypre_ML_list0_aux_70
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret131


def ats2pypre_ML_list0_zipwith_method(arg0, arg1):
  tmpret138 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_zipwith_method
  tmpret138 = _ats2pypre_ML_list0_patsfun_72__closurerize(arg0, arg1)
  return tmpret138


def _ats2pypre_ML_list0_patsfun_72(env0, env1, arg0):
  tmpret139 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_list0_patsfun_72
  tmpret139 = ats2pypre_ML_list0_zipwith(env0, env1, arg0)
  return tmpret139


def ats2pypre_ML_list0_foldleft(arg0, arg1, arg2):
  tmpret140 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_foldleft
  tmpret140 = _ats2pypre_ML_list0_aux_74(arg2, arg1, arg0)
  return tmpret140


def _ats2pypre_ML_list0_aux_74(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret141 = None
  tmp142 = None
  tmp143 = None
  tmp144 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab66():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret141, tmp142, tmp143, tmp144
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg1)): tmplab_py = 4 ; return#__atstmplab69
    __atstmplab67()
    return
  def __atstmplab67():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret141, tmp142, tmp143, tmp144
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret141 = arg0
    return
  def __atstmplab68():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret141, tmp142, tmp143, tmp144
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab69()
    return
  def __atstmplab69():
    nonlocal env0, arg0, arg1
    nonlocal apy0, apy1, tmpret141, tmp142, tmp143, tmp144
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp142 = arg1[0]
    tmp143 = arg1[1]
    tmp144 = env0[0](env0, arg0, tmp142)
    #ATStailcalseq_beg
    apy0 = tmp144
    apy1 = tmp143
    arg0 = apy0
    arg1 = apy1
    funlab_py = 1 #__patsflab__ats2pypre_ML_list0_aux_74
    #ATStailcalseq_end
    return
  mbranch_1 = { 1: __atstmplab66, 2: __atstmplab67, 3: __atstmplab68, 4: __atstmplab69 }
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_ML_list0_aux_74
    #ATScaseofseq_beg
    tmplab_py = 1
    while(1):
      mbranch_1.get(tmplab_py)()
      if (tmplab_py == 0): break
    #ATScaseofseq_end
    if (funlab_py == 0): break
  return tmpret141


def ats2pypre_ML_list0_foldright(arg0, arg1, arg2):
  tmpret145 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_foldright
  tmpret145 = _ats2pypre_ML_list0_aux_76(arg1, arg2, arg0, arg2)
  return tmpret145


def _ats2pypre_ML_list0_aux_76(env0, env1, arg0, arg1):
  tmpret146 = None
  tmp147 = None
  tmp148 = None
  tmp149 = None
  funlab_py = None
  tmplab_py = None
  mbranch_1 = None
  def __atstmplab70():
    nonlocal env0, env1, arg0, arg1
    nonlocal tmpret146, tmp147, tmp148, tmp149
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    if(ATSCKptriscons(arg0)): tmplab_py = 4 ; return#__atstmplab73
    __atstmplab71()
    return
  def __atstmplab71():
    nonlocal env0, env1, arg0, arg1
    nonlocal tmpret146, tmp147, tmp148, tmp149
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmpret146 = arg1
    return
  def __atstmplab72():
    nonlocal env0, env1, arg0, arg1
    nonlocal tmpret146, tmp147, tmp148, tmp149
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    __atstmplab73()
    return
  def __atstmplab73():
    nonlocal env0, env1, arg0, arg1
    nonlocal tmpret146, tmp147, tmp148, tmp149
    nonlocal funlab_py, tmplab_py
    nonlocal mbranch_1
    tmplab_py = 0
    tmp147 = arg0[0]
    tmp148 = arg0[1]
    tmp149 = _ats2pypre_ML_list0_aux_76(env0, env1, tmp148, env1)
    tmpret146 = env0[0](env0, tmp147, tmp149)
    return
  mbranch_1 = { 1: __atstmplab70, 2: __atstmplab71, 3: __atstmplab72, 4: __atstmplab73 }
  #__patsflab__ats2pypre_ML_list0_aux_76
  #ATScaseofseq_beg
  tmplab_py = 1
  while(1):
    mbranch_1.get(tmplab_py)()
    if (tmplab_py == 0): break
  #ATScaseofseq_end
  return tmpret146


def ats2pypre_ML_list0_sort_2(arg0, arg1):
  tmpret152 = None
  tmp153 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_sort_2
  tmp153 = ats2pypre_list_sort_2(arg0, arg1)
  tmpret152 = tmp153
  return tmpret152


def ats2pypre_ML_list0_mergesort(arg0, arg1):
  tmpret154 = None
  tmp155 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_list0_mergesort
  tmp155 = ats2pypre_list_mergesort(arg0, arg1)
  tmpret154 = tmp155
  return tmpret154


def ats2pypre_ML_streamize_list0_zip(arg0, arg1):
  tmpret156 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_streamize_list0_zip
  tmpret156 = ats2pypre_streamize_list_zip(arg0, arg1)
  return tmpret156


def ats2pypre_ML_streamize_list0_cross(arg0, arg1):
  tmpret157 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_streamize_list0_cross
  tmpret157 = ats2pypre_streamize_list_cross(arg0, arg1)
  return tmpret157

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######

def _ats2pypre_ML_array0_patsfun_8__closurerize(env0):
  def _ats2pypre_ML_array0_patsfun_8__cfun(cenv, arg0): return _ats2pypre_ML_array0_patsfun_8(cenv[1], arg0)
  return (_ats2pypre_ML_array0_patsfun_8__cfun, env0)

def _ats2pypre_ML_array0_patsfun_11__closurerize(env0):
  def _ats2pypre_ML_array0_patsfun_11__cfun(cenv, arg0): return _ats2pypre_ML_array0_patsfun_11(cenv[1], arg0)
  return (_ats2pypre_ML_array0_patsfun_11__cfun, env0)

def _ats2pypre_ML_array0_patsfun_17__closurerize(env0):
  def _ats2pypre_ML_array0_patsfun_17__cfun(cenv, arg0): return _ats2pypre_ML_array0_patsfun_17(cenv[1], arg0)
  return (_ats2pypre_ML_array0_patsfun_17__cfun, env0)

def ats2pypre_ML_array0_make_elt(arg0, arg1):
  tmpret0 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_make_elt
  tmpret0 = ats2pypre_arrszref_make_elt(arg0, arg1)
  return tmpret0


def ats2pypre_ML_array0_size(arg0):
  tmpret1 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_size
  tmpret1 = ats2pypre_arrszref_size(arg0)
  return tmpret1


def ats2pypre_ML_array0_length(arg0):
  tmpret2 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_length
  tmpret2 = ats2pypre_arrszref_size(arg0)
  return tmpret2


def ats2pypre_ML_array0_get_at(arg0, arg1):
  tmpret3 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_get_at
  tmpret3 = ats2pypre_arrszref_get_at(arg0, arg1)
  return tmpret3


def ats2pypre_ML_array0_set_at(arg0, arg1, arg2):
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_set_at
  ats2pypre_arrszref_set_at(arg0, arg1, arg2)
  return#_void


def ats2pypre_ML_array0_exch_at(arg0, arg1, arg2):
  tmpret5 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_exch_at
  tmpret5 = ats2pypre_arrszref_exch_at(arg0, arg1, arg2)
  return tmpret5


def ats2pypre_ML_array0_exists(arg0, arg1):
  tmpret6 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_exists
  tmpret6 = ats2pypre_arrszref_exists_cloref(arg0, arg1)
  return tmpret6


def ats2pypre_ML_array0_exists_method(arg0):
  tmpret7 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_exists_method
  tmpret7 = _ats2pypre_ML_array0_patsfun_8__closurerize(arg0)
  return tmpret7


def _ats2pypre_ML_array0_patsfun_8(env0, arg0):
  tmpret8 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_array0_patsfun_8
  tmpret8 = ats2pypre_ML_array0_exists(env0, arg0)
  return tmpret8


def ats2pypre_ML_array0_forall(arg0, arg1):
  tmpret9 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_forall
  tmpret9 = ats2pypre_arrszref_forall_cloref(arg0, arg1)
  return tmpret9


def ats2pypre_ML_array0_forall_method(arg0):
  tmpret10 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_forall_method
  tmpret10 = _ats2pypre_ML_array0_patsfun_11__closurerize(arg0)
  return tmpret10


def _ats2pypre_ML_array0_patsfun_11(env0, arg0):
  tmpret11 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_array0_patsfun_11
  tmpret11 = ats2pypre_ML_array0_forall(env0, arg0)
  return tmpret11


def array0_find_index(arg0, arg1):
  tmpret12 = None
  tmp17 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_find_index
  tmp17 = ats2pypre_ML_array0_size(arg0)
  tmpret12 = _ats2pypre_ML_array0_loop_13(arg1, 0, tmp17)
  return tmpret12


def _ats2pypre_ML_array0_loop_13(env0, arg0, arg1):
  apy0 = None
  apy1 = None
  tmpret13 = None
  tmp14 = None
  tmp15 = None
  tmp16 = None
  funlab_py = None
  tmplab_py = None
  while(1):
    funlab_py = 0
    #__patsflab__ats2pypre_ML_array0_loop_13
    tmp14 = ats2pypre_lt_int0_int0(arg0, arg1)
    if (tmp14):
      tmp15 = env0[0](env0, arg0)
      if (tmp15):
        tmpret13 = arg0
      else:
        tmp16 = ats2pypre_add_int1_int1(arg0, 1)
        #ATStailcalseq_beg
        apy0 = tmp16
        apy1 = arg1
        arg0 = apy0
        arg1 = apy1
        funlab_py = 1 #__patsflab__ats2pypre_ML_array0_loop_13
        #ATStailcalseq_end
      #endif
    else:
      tmpret13 = ats2pypre_neg_int1(1)
    #endif
    if (funlab_py == 0): break
  return tmpret13


def ats2pypre_ML_array0_app(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_app
  ats2pypre_ML_array0_foreach(arg0, arg1)
  return#_void


def ats2pypre_ML_array0_foreach(arg0, arg1):
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_foreach
  ats2pypre_arrszref_foreach_cloref(arg0, arg1)
  return#_void


def ats2pypre_ML_array0_foreach_method(arg0):
  tmpret20 = None
  funlab_py = None
  tmplab_py = None
  #__patsflab_array0_foreach_method
  tmpret20 = _ats2pypre_ML_array0_patsfun_17__closurerize(arg0)
  return tmpret20


def _ats2pypre_ML_array0_patsfun_17(env0, arg0):
  funlab_py = None
  tmplab_py = None
  #__patsflab__ats2pypre_ML_array0_patsfun_17
  ats2pypre_ML_array0_foreach(env0, arg0)
  return#_void

######
##
## end-of-compilation-unit
##
######
######
##
## The Python3 code
## is generated from ATS source by atscc2py3
## The starting compilation time is: 2017-10-23: 15h:54m
##
######
######
##
## end-of-compilation-unit
##
######

## ###### ###### ##

## end of [libatscc2py3_all.py] ##
