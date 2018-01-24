
######
#
# Time of Generation:
# Mon Oct 23 14:29:56 EDT 2017
#
######

######
#
# HX-2014-11:
# for Perl code translated from ATS
#
######

######
#beg of [basics_cats.pl]
######

############################################

sub
ATSCKiseqz($) { return ($_[0] == 0); }
sub
ATSCKisneqz($) { return ($_[0] != 0); }

############################################

sub
ATSCKptrisnull($) { return ($_[0] == 0); }
sub
ATSCKptriscons($) { return ($_[0] != 0); }

############################################

sub
ATSCKpat_int($$) { return ($_[0] == $_[1]); }
sub
ATSCKpat_bool($$) { return ($_[0] == $_[1]); }
sub
ATSCKpat_char($$) { return ($_[0] == $_[1]); }
sub
ATSCKpat_float($$) { return ($_[0] == $_[1]); }

############################################
#
sub
ATSCKpat_con0($$)
  { return ($_[0] == $_[1]); }
sub
ATSCKpat_con1($$)
  { my $con = $_[0]; return ($con->[0] == $_[1]); }
#
############################################
#
sub
ATSINScaseof_fail($)
{
STDERR->printflush("ATSINScaseof_fail:$_[0]"); exit(1); return;
}
#
sub
ATSINSdeadcode_fail()
  { STDERR->printflush("ATSINSdeadcode_fail"); exit(1); return; }
#
############################################
#
sub
ATSPMVempty(){ return; }
#
############################################

sub
ATSPMVlazyval($){ return [0, $_[0]]; }

############################################

sub
ATSPMVlazyval_eval($)
{
#
  my($lazyval) = @_;
  my($flag);
  my($mythunk);
#
  $flag = $lazyval->[0];
#
  if($flag==0)
  {
    $lazyval->[0] = 1;
    $mythunk = $lazyval->[1];
    $lazyval->[1] = &{$mythunk->[0]}($mythunk);
  } else {
    $lazyval->[0] = $flag + 1;
  } #end-of-[if]
  return ($lazyval->[1]);
#
} #end-of-[ATSPMVlazyval_eval]

############################################

sub
ATSPMVllazyval($){ return $_[0]; }

############################################
#
sub
ATSPMVllazyval_eval($)
{
  my($lazyval) = @_;
  return &{$lazyval->[0]}($lazyval, 1);
}
#
sub
atspre_lazy_vt_free($)
{
  my($lazyval) = @_;
  return &{$lazyval->[0]}($lazyval, 0);
}
#
############################################

sub
ats2plpre_lazy2cloref($) { return $_[0]->[1]; }

############################################
#
sub
ats2plpre_assert_bool0($)
{
  my($tfv) = @_;
  if (!$tfv) { exit(1); }
  return;
}
sub
ats2plpre_assert_bool1($$)
{
  ats2plpre_assert_bool0($_[0]); return;
}
#
sub
ats2plpre_assert_errmsg_bool0($$)
{
  my($tfv, $errmsg) = @_;
  if (!$tfv) { STDERR->printflush($errmsg); exit(1); }
  return;
}
sub
ats2plpre_assert_errmsg_bool1($$)
{
  ats2plpre_assert_errmsg_bool0($_[0], $_[1]); return;
}
#
############################################

######
1; #note that it is needed by 'use' or 'require'
######

######
#end of [basics_cats.js]
######
######
#
# HX-2014-11:
# for Perl code translated from ATS
#
######

######
#beg of [integer_cats.pl]
######

############################################

sub
ats2plpre_abs_int0($) { return abs($_[0]); }

############################################

sub
ats2plpre_neg_int0($) { return ( -($_[0]) ); }
sub
ats2plpre_neg_int1($) { return ( -($_[0]) ); }

############################################

sub
ats2plpre_succ_int0($) { return ($_[0] + 1); }
sub
ats2plpre_pred_int0($) { return ($_[0] - 1); }

############################################

sub
ats2plpre_half_int0($) { return int($_[0] / 2); }
sub
ats2plpre_half_int1($) { return int($_[0] / 2); }

############################################

sub
ats2plpre_add_int0_int0($$) { return ($_[0] + $_[1]); }
sub
ats2plpre_sub_int0_int0($$) { return ($_[0] - $_[1]); }
sub
ats2plpre_mul_int0_int0($$) { return ($_[0] * $_[1]); }
sub
ats2plpre_div_int0_int0($$) { return int($_[0] / $_[1]); }
sub
ats2plpre_mod_int0_int0($$) { return ($_[0] % $_[1]); }

############################################
#
sub
ats2plpre_add_int1_int1($$) { return ($_[0] + $_[1]); }
sub
ats2plpre_sub_int1_int1($$) { return ($_[0] - $_[1]); }
sub
ats2plpre_mul_int1_int1($$) { return ($_[0] * $_[1]); }
sub
ats2plpre_div_int1_int1($$) { return int($_[0] / $_[1]); }
#
sub
ats2plpre_mod_int1_int1($$) { return ($_[0] % $_[1]); }
sub
ats2plpre_nmod_int1_int1($$) { return ($_[0] % $_[1]); }
#
############################################

sub
ats2plpre_lt_int0_int0($$) { return ($_[0] < $_[1]); }
sub
ats2plpre_lte_int0_int0($$) { return ($_[0] <= $_[1]); }
sub
ats2plpre_gt_int0_int0($$) { return ($_[0] > $_[1]); }
sub
ats2plpre_gte_int0_int0($$) { return ($_[0] >= $_[1]); }
sub
ats2plpre_eq_int0_int0($$) { return ($_[0] == $_[1]); }
sub
ats2plpre_neq_int0_int0($$) { return ($_[0] != $_[1]); }

############################################

sub
ats2plpre_lt_int1_int1($$) { return ($_[0] < $_[1]); }
sub
ats2plpre_lte_int1_int1($$) { return ($_[0] <= $_[1]); }
sub
ats2plpre_gt_int1_int1($$) { return ($_[0] > $_[1]); }
sub
ats2plpre_gte_int1_int1($$) { return ($_[0] >= $_[1]); }
sub
ats2plpre_eq_int1_int1($$) { return ($_[0] == $_[1]); }
sub
ats2plpre_neq_int1_int1($$) { return ($_[0] != $_[1]); }

############################################

######
1; #note that it is needed by 'use' or 'require'
######

############################################
#end of [integer_cats.pl]
############################################

######
#
# HX-2014-11:
# for Perl code translated from ATS
#
######

######
#beg of [bool_cats.pl]
######

############################################

sub
ats2plpre_neg_bool0($) { return !($_[0]); }
sub
ats2plpre_neg_bool1($) { return !($_[0]); }

############################################

######
1; #note that it is needed by 'use' or 'require'
######

######
#end of [bool_cats.pl]
######

######
#
# HX-2014-11:
# for Perl code translated from ATS
#
######

######
#beg of [float_cats.pl]
######

############################################
#
sub
ats2plpre_double2int($) { return int($_[0]); }
sub
ats2plpre_int_of_double($) { return int($_[0]); }
#
sub
ats2plpre_int2double($) { return ($_[0]) ; }
sub
ats2plpre_double_of_int($) { return ($_[0]) ; }
#
############################################
#
sub
ats2plpre_abs_double($) { return abs($_[0]); }
sub
ats2plpre_neg_double($) { return ( -$_[0] ); }
#
sub
ats2plpre_succ_double($) { return ($_[0] + 1); }
sub
ats2plpre_pred_double($) { return ($_[0] + 1); }
#
############################################
#
sub
ats2plpre_add_int_double($$) { return ($_[0] + $_[1]); }
sub
ats2plpre_sub_int_double($$) { return ($_[0] - $_[1]); }
sub
ats2plpre_mul_int_double($$) { return ($_[0] * $_[1]); }
sub
ats2plpre_div_int_double($$) { return ($_[0] / $_[1]); }
#
############################################
#
sub
ats2plpre_add_double_int($$) { return ($_[0] + $_[1]); }
sub
ats2plpre_sub_double_int($$) { return ($_[0] - $_[1]); }
sub
ats2plpre_mul_double_int($$) { return ($_[0] * $_[1]); }
sub
ats2plpre_div_double_int($$) { return ($_[0] / $_[1]); }
#
############################################
#
sub
ats2plpre_add_double_double($$) { return ($_[0] + $_[1]); }
sub
ats2plpre_sub_double_double($$) { return ($_[0] - $_[1]); }
sub
ats2plpre_mul_double_double($$) { return ($_[0] * $_[1]); }
sub
ats2plpre_div_double_double($$) { return ($_[0] / $_[1]); }
#
############################################
#
sub
ats2plpre_lt_double_double($$) { return ($_[0] < $_[1]); }
sub
ats2plpre_lte_double_double($$) { return ($_[0] <= $_[1]); }
sub
ats2plpre_gt_double_double($$) { return ($_[0] > $_[1]); }
sub
ats2plpre_gte_double_double($$) { return ($_[0] >= $_[1]); }
#
sub
ats2plpre_eq_double_double($$) { return ($_[0] == $_[1]); }
sub
ats2plpre_neq_double_double($$) { return ($_[0] != $_[1]); }
#
############################################

######
1; #note that it is needed by 'use' or 'require'
######

######
#end of [float_cats.pl]
######
######
#
# HX-2014-11:
# for Perl code translated from ATS
#
######

######
#beg of [string_cats.pl]
######

############################################

sub
ats2plpre_lt_string_string($$) { return ($_[0] < $_[1]); }
sub
ats2plpre_lte_string_string($$) { return ($_[0] <= $_[1]); }
sub
ats2plpre_gt_string_string($$) { return ($_[0] > $_[1]); }
sub
ats2plpre_gte_string_string($$) { return ($_[0] >= $_[1]); }
sub
ats2plpre_eq_string_string($$) { return ($_[0] == $_[1]); }
sub
ats2plpre_neq_string_string($$) { return ($_[0] != $_[1]); }

############################################

######
1; #note that it is needed by 'use' or 'require'
######

###### end of [string_cats.pl] ######
######
#
# HX-2014-11:
# for Perl code translated from ATS
#
######

######
#beg of [print_cats.pl]
######

############################################
#
sub
ats2plpre_print_int($)
{
  ats2plpre_fprint_int(STDOUT, $_[0]); return;
}
sub
ats2plpre_prerr_int($)
{
  ats2plpre_fprint_int(STDERR, $_[0]); return;
}
#
sub
ats2plpre_fprint_int($$) { print {$_[0]} $_[1]; return; }
#
############################################
#
sub
ats2plpre_print_bool($)
{
  ats2plpre_fprint_bool(STDOUT, $_[0]); return;
}
sub
ats2plpre_prerr_bool($)
{
  ats2plpre_fprint_bool(STDERR, $_[0]); return;
}
#
sub
ats2plpre_fprint_bool($$)
{
  if($_[1]) { print {$_[0]} "true"; } else { print {$_[0]} "false"; }; return;
}
#
############################################
#
sub
ats2plpre_print_double($)
{
  ats2plpre_fprint_double(STDOUT, $_[0]); return;
}
sub
ats2plpre_prerr_double($)
{
  ats2plpre_fprint_double(STDERR, $_[0]); return;
}
#
sub
ats2plpre_fprint_double($$) { print {$_[0]} $_[1]; return; }
#
############################################
#
sub
ats2plpre_print_string($)
{
  ats2plpre_fprint_string(STDOUT, $_[0]); return;
}
sub
ats2plpre_prerr_string($)
{
  ats2plpre_fprint_string(STDERR, $_[0]); return;
}
#
sub
ats2plpre_fprint_string($$) { print {$_[0]} "$_[1]"; return; }
#
############################################
#
sub
ats2plpre_print_obj($)
{
  ats2plpre_fprint_obj(STDOUT, $_[0]); return;
}
sub
ats2plpre_prerr_obj($)
{
  ats2plpre_fprint_obj(STDERR, $_[0]); return;
}
#
sub
ats2plpre_fprint_obj($$) { print {$_[0]} $_[1]; return;  }
#
############################################
#
sub
ats2plpre_print_newline()
{
  STDOUT->printflush("\n"); return;
}
sub
ats2plpre_prerr_newline()
{
  STDERR->printflush("\n"); return;
}
sub
ats2plpre_fprint_newline($)
{
  $_[0]->printflush("\n"); return;
}
#
############################################

######
1; #note that it is needed by 'use' or 'require'
######

######
#end of [print_cats.pl]
######
######
#
# HX-2014-11:
# for Perl code translated from ATS
#
######

######
#beg of [PLarray_cats.pl]
######

sub
ats2plpre_PLarray_nil() { return []; }
sub
ats2plpre_PLarray_sing($) { return [$_[0]]; }
sub
ats2plpre_PLarray_pair($$) { return [$_[0], $_[1]]; }

######

sub
ats2plpre_PLarray_get_at($$)
{
  my($A, $i) = @_; return $A->[$i];
}
sub
ats2plpre_PLarray_set_at($$$)
{
  my($A, $i, $x) = @_; $A->[$i] = $x; return;
}

######

sub
ats2plpre_PLarray_length($)
{
  return scalar(@{$_[0]});
}

######

sub
ats2plpre_PLarray_pop_0($) { return pop(@{$_[0]}); }
sub
ats2plpre_PLarray_pop_1($$) { return splice(@{$_[0]}, $_[1], 1); }

######

sub
ats2plpre_PLarray_push($$) { return push(@{$_[0]}, $_[1]); }

######

sub
ats2plpre_PLarray_extend($$) { push(@{$_[0]}, $_[1]); return; }

######
#
sub
ats2plpre_PLarray_reverse($)
{
  my $A = @_;
  my $i = 0 ;
  my $j = scalar(@{$A}) - 1;
  while ($i < $j)
  {
    my $tmp = $A->[i]; $A->[i] = $A->[j]; $A->[j] = $tmp; $i++; $j--;
  }
  return;
} #ats2plpre_PLarray_reverse
#
######

sub
ats2plpre_PLarray_copy($)
{
  my @A2 = @{$_[0]}; return \@A2;
}
sub
ats2plpre_PLarray_revcopy($)
{
  my @A2 = reverse(@{$_[0]}); return \@A2;
}

######

sub
ats2plpre_PLarray_append_2($$)
{
  my @res = (@{$_[0]}, @{$_[1]}); return \@res;
}
sub
ats2plpre_PLarray_append_3($$$)
{
  my @res = (@{$_[0]}, @{$_[1]}, @{$_[2]}); return \@res;
}

######

######
1; #note that it is needed by 'use' or 'require'
######

######
#end of [PLarray_cats.pl]
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######
sub
_ats2plpre_list_patsfun_40__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_40($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_44__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_44($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_47__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_47($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_51__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_51($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_55__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_55($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_59__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_59($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_62__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_62($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_66__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_66($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_68__closurerize()
{
  #my() = @_;
  return [sub{ my($cenv, $arg0, $arg1) = @_; return _ats2plpre_list_patsfun_68($arg0, $arg1); }];
}

sub
_ats2plpre_list_patsfun_72__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_72($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_76__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_76($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_81__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_81($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_list_patsfun_85__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_85($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_list_patsfun_89__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_89($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_list_patsfun_93__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_93($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_list_patsfun_101__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_101($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_list_patsfun_104__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_104($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_list_patsfun_107__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_107($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_list_patsfun_109__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_list_patsfun_109($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}


sub
ats2plpre_list_make_elt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret2;
  my $tmp7;
##
  __patsflab_list_make_elt:
  $tmp7 = 0;
  $tmpret2 = _ats2plpre_list_loop_3($arg1, $arg0, $tmp7);
  return $tmpret2;
} #end-of-function


sub
_ats2plpre_list_loop_3($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret3;
  my $tmp4;
  my $tmp5;
  my $tmp6;
##
  __patsflab__ats2plpre_list_loop_3:
  $tmp4 = ats2plpre_gt_int1_int1($arg0, 0);
  if($tmp4) {
    $tmp5 = ats2plpre_sub_int1_int1($arg0, 1);
    $tmp6 = [$env0, $arg1];
    #ATStailcalseq_beg
    $apy0 = $tmp5;
    $apy1 = $tmp6;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_list_loop_3;
    #ATStailcalseq_end
  } else {
    $tmpret3 = $arg1;
  } #endif
  return $tmpret3;
} #end-of-function


sub
ats2plpre_list_make_intrange_2($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret8;
##
  __patsflab_list_make_intrange_2:
  $tmpret8 = ats2plpre_list_make_intrange_3($arg0, $arg1, 1);
  return $tmpret8;
} #end-of-function


sub
ats2plpre_list_make_intrange_3($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret9;
  my $tmp20;
  my $tmp21;
  my $tmp22;
  my $tmp23;
  my $tmp24;
  my $tmp25;
  my $tmp26;
  my $tmp27;
  my $tmp28;
  my $tmp29;
  my $tmp30;
  my $tmp31;
  my $tmp32;
  my $tmp33;
  my $tmp34;
  my $tmp35;
  my $tmp36;
  my $tmp37;
  my $tmp38;
  my $tmp39;
  my $tmp40;
##
  __patsflab_list_make_intrange_3:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab6:
    $tmp20 = ats2plpre_gt_int0_int0($arg2, 0);
    if(!ATSCKpat_bool($tmp20, 1)) { goto __atstmplab7; }
    $tmp21 = ats2plpre_lt_int0_int0($arg0, $arg1);
    if($tmp21) {
      $tmp25 = ats2plpre_sub_int0_int0($arg1, $arg0);
      $tmp24 = ats2plpre_add_int0_int0($tmp25, $arg2);
      $tmp23 = ats2plpre_sub_int0_int0($tmp24, 1);
      $tmp22 = ats2plpre_div_int0_int0($tmp23, $arg2);
      $tmp28 = ats2plpre_sub_int0_int0($tmp22, 1);
      $tmp27 = ats2plpre_mul_int0_int0($tmp28, $arg2);
      $tmp26 = ats2plpre_add_int0_int0($arg0, $tmp27);
      $tmp29 = 0;
      $tmpret9 = _ats2plpre_list_loop1_6($tmp22, $tmp26, $arg2, $tmp29);
    } else {
      $tmpret9 = 0;
    } #endif
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab7:
    $tmp30 = ats2plpre_lt_int0_int0($arg2, 0);
    if(!ATSCKpat_bool($tmp30, 1)) { goto __atstmplab8; }
    $tmp31 = ats2plpre_gt_int0_int0($arg0, $arg1);
    if($tmp31) {
      $tmp32 = ats2plpre_neg_int0($arg2);
      $tmp36 = ats2plpre_sub_int0_int0($arg0, $arg1);
      $tmp35 = ats2plpre_add_int0_int0($tmp36, $tmp32);
      $tmp34 = ats2plpre_sub_int0_int0($tmp35, 1);
      $tmp33 = ats2plpre_div_int0_int0($tmp34, $tmp32);
      $tmp39 = ats2plpre_sub_int0_int0($tmp33, 1);
      $tmp38 = ats2plpre_mul_int0_int0($tmp39, $tmp32);
      $tmp37 = ats2plpre_sub_int0_int0($arg0, $tmp38);
      $tmp40 = 0;
      $tmpret9 = _ats2plpre_list_loop2_7($tmp33, $tmp37, $tmp32, $tmp40);
    } else {
      $tmpret9 = 0;
    } #endif
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab8:
    $tmpret9 = 0;
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret9;
} #end-of-function


sub
_ats2plpre_list_loop1_6($$$$)
{
##
  my($arg0, $arg1, $arg2, $arg3) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $apy3;
  my $tmpret10;
  my $tmp11;
  my $tmp12;
  my $tmp13;
  my $tmp14;
##
  __patsflab__ats2plpre_list_loop1_6:
  $tmp11 = ats2plpre_gt_int0_int0($arg0, 0);
  if($tmp11) {
    $tmp12 = ats2plpre_sub_int0_int0($arg0, 1);
    $tmp13 = ats2plpre_sub_int0_int0($arg1, $arg2);
    $tmp14 = [$arg1, $arg3];
    #ATStailcalseq_beg
    $apy0 = $tmp12;
    $apy1 = $tmp13;
    $apy2 = $arg2;
    $apy3 = $tmp14;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    $arg3 = $apy3;
    goto __patsflab__ats2plpre_list_loop1_6;
    #ATStailcalseq_end
  } else {
    $tmpret10 = $arg3;
  } #endif
  return $tmpret10;
} #end-of-function


sub
_ats2plpre_list_loop2_7($$$$)
{
##
  my($arg0, $arg1, $arg2, $arg3) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $apy3;
  my $tmpret15;
  my $tmp16;
  my $tmp17;
  my $tmp18;
  my $tmp19;
##
  __patsflab__ats2plpre_list_loop2_7:
  $tmp16 = ats2plpre_gt_int0_int0($arg0, 0);
  if($tmp16) {
    $tmp17 = ats2plpre_sub_int0_int0($arg0, 1);
    $tmp18 = ats2plpre_add_int0_int0($arg1, $arg2);
    $tmp19 = [$arg1, $arg3];
    #ATStailcalseq_beg
    $apy0 = $tmp17;
    $apy1 = $tmp18;
    $apy2 = $arg2;
    $apy3 = $tmp19;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    $arg3 = $apy3;
    goto __patsflab__ats2plpre_list_loop2_7;
    #ATStailcalseq_end
  } else {
    $tmpret15 = $arg3;
  } #endif
  return $tmpret15;
} #end-of-function


sub
ats2plpre_list_length($)
{
##
  my($arg0) = @_;
##
  my $tmpret52;
##
  __patsflab_list_length:
  $tmpret52 = _ats2plpre_list_loop_14($arg0, 0);
  return $tmpret52;
} #end-of-function


sub
_ats2plpre_list_loop_14($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret53;
  my $tmp55;
  my $tmp56;
##
  __patsflab__ats2plpre_list_loop_14:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab13:
    if(ATSCKptriscons($arg0)) { goto __atstmplab16; }
    __atstmplab14:
    $tmpret53 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab15:
    __atstmplab16:
    $tmp55 = $arg0->[1];
    $tmp56 = ats2plpre_add_int1_int1($arg1, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp55;
    $apy1 = $tmp56;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_list_loop_14;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret53;
} #end-of-function


sub
_057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2pl_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_length_gte($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret57;
  my $tmp58;
##
  __patsflab_list_length_gte:
  $tmp58 = _057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2pl_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_length_compare($arg0, $arg1);
  $tmpret57 = ats2plpre_gte_int1_int1($tmp58, 0);
  return $tmpret57;
} #end-of-function


sub
_057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2pl_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_length_compare($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret59;
##
  __patsflab_list_length_compare:
  $tmpret59 = _ats2plpre_list_loop_17($arg0, $arg1);
  return $tmpret59;
} #end-of-function


sub
_ats2plpre_list_loop_17($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret60;
  my $tmp61;
  my $tmp63;
  my $tmp64;
  my $tmp65;
##
  __patsflab__ats2plpre_list_loop_17:
  $tmp61 = ats2plpre_lt_int1_int1($arg1, 0);
  if($tmp61) {
    $tmpret60 = 1;
  } else {
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab17:
      if(ATSCKptrisnull($arg0)) { goto __atstmplab19; }
      __atstmplab18:
      $tmp63 = $arg0->[1];
      $tmp64 = ats2plpre_sub_int1_int1($arg1, 1);
      #ATStailcalseq_beg
      $apy0 = $tmp63;
      $apy1 = $tmp64;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab__ats2plpre_list_loop_17;
      #ATStailcalseq_end
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab19:
      $tmp65 = ats2plpre_eq_int1_int1($arg1, 0);
      if($tmp65) {
        $tmpret60 = 0;
      } else {
        $tmpret60 = ats2plpre_neg_int1(1);
      } #endif
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } #endif
  return $tmpret60;
} #end-of-function


sub
ats2plpre_list_head($)
{
##
  my($arg0) = @_;
##
  my $tmpret66;
  my $tmp67;
##
  __patsflab_list_head:
  $tmp67 = $arg0->[0];
  $tmpret66 = $tmp67;
  return $tmpret66;
} #end-of-function


sub
ats2plpre_list_tail($)
{
##
  my($arg0) = @_;
##
  my $tmpret68;
  my $tmp69;
##
  __patsflab_list_tail:
  $tmp69 = $arg0->[1];
  $tmpret68 = $tmp69;
  return $tmpret68;
} #end-of-function


sub
ats2plpre_list_last($)
{
##
  my($arg0) = @_;
##
  my $apy0;
  my $tmpret70;
  my $tmp71;
  my $tmp72;
##
  __patsflab_list_last:
  $tmp71 = $arg0->[0];
  $tmp72 = $arg0->[1];
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab20:
    if(ATSCKptriscons($tmp72)) { goto __atstmplab23; }
    __atstmplab21:
    $tmpret70 = $tmp71;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab22:
    __atstmplab23:
    #ATStailcalseq_beg
    $apy0 = $tmp72;
    $arg0 = $apy0;
    goto __patsflab_list_last;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret70;
} #end-of-function


sub
ats2plpre_list_get_at($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret73;
  my $tmp74;
  my $tmp75;
  my $tmp76;
  my $tmp77;
##
  __patsflab_list_get_at:
  $tmp74 = ats2plpre_eq_int1_int1($arg1, 0);
  if($tmp74) {
    $tmp75 = $arg0->[0];
    $tmpret73 = $tmp75;
  } else {
    $tmp76 = $arg0->[1];
    $tmp77 = ats2plpre_sub_int1_int1($arg1, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp76;
    $apy1 = $tmp77;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab_list_get_at;
    #ATStailcalseq_end
  } #endif
  return $tmpret73;
} #end-of-function


sub
ats2plpre_list_snoc($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret78;
  my $tmp79;
  my $tmp80;
##
  __patsflab_list_snoc:
  $tmp80 = 0;
  $tmp79 = [$arg1, $tmp80];
  $tmpret78 = ats2plpre_list_append($arg0, $tmp79);
  return $tmpret78;
} #end-of-function


sub
ats2plpre_list_extend($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret81;
  my $tmp82;
  my $tmp83;
##
  __patsflab_list_extend:
  $tmp83 = 0;
  $tmp82 = [$arg1, $tmp83];
  $tmpret81 = ats2plpre_list_append($arg0, $tmp82);
  return $tmpret81;
} #end-of-function


sub
ats2plpre_list_append($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret84;
  my $tmp85;
  my $tmp86;
  my $tmp87;
##
  __patsflab_list_append:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab24:
    if(ATSCKptriscons($arg0)) { goto __atstmplab27; }
    __atstmplab25:
    $tmpret84 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab26:
    __atstmplab27:
    $tmp85 = $arg0->[0];
    $tmp86 = $arg0->[1];
    $tmp87 = ats2plpre_list_append($tmp86, $arg1);
    $tmpret84 = [$tmp85, $tmp87];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret84;
} #end-of-function


sub
ats2plpre_mul_int_list($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret88;
  my $tmp93;
##
  __patsflab_mul_int_list:
  $tmp93 = 0;
  $tmpret88 = _ats2plpre_list_loop_26($arg1, $arg0, $tmp93);
  return $tmpret88;
} #end-of-function


sub
_ats2plpre_list_loop_26($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret89;
  my $tmp90;
  my $tmp91;
  my $tmp92;
##
  __patsflab__ats2plpre_list_loop_26:
  $tmp90 = ats2plpre_gt_int1_int1($arg0, 0);
  if($tmp90) {
    $tmp91 = ats2plpre_sub_int1_int1($arg0, 1);
    $tmp92 = ats2plpre_list_append($env0, $arg1);
    #ATStailcalseq_beg
    $apy0 = $tmp91;
    $apy1 = $tmp92;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_list_loop_26;
    #ATStailcalseq_end
  } else {
    $tmpret89 = $arg1;
  } #endif
  return $tmpret89;
} #end-of-function


sub
ats2plpre_list_reverse($)
{
##
  my($arg0) = @_;
##
  my $tmpret94;
  my $tmp95;
##
  __patsflab_list_reverse:
  $tmp95 = 0;
  $tmpret94 = ats2plpre_list_reverse_append($arg0, $tmp95);
  return $tmpret94;
} #end-of-function


sub
ats2plpre_list_reverse_append($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret96;
##
  __patsflab_list_reverse_append:
  $tmpret96 = _ats2plpre_list_loop_29($arg0, $arg1);
  return $tmpret96;
} #end-of-function


sub
_ats2plpre_list_loop_29($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret97;
  my $tmp98;
  my $tmp99;
  my $tmp100;
##
  __patsflab__ats2plpre_list_loop_29:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab28:
    if(ATSCKptriscons($arg0)) { goto __atstmplab31; }
    __atstmplab29:
    $tmpret97 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab30:
    __atstmplab31:
    $tmp98 = $arg0->[0];
    $tmp99 = $arg0->[1];
    $tmp100 = [$tmp98, $arg1];
    #ATStailcalseq_beg
    $apy0 = $tmp99;
    $apy1 = $tmp100;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_list_loop_29;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret97;
} #end-of-function


sub
ats2plpre_list_concat($)
{
##
  my($arg0) = @_;
##
  my $tmpret101;
##
  __patsflab_list_concat:
  $tmpret101 = _ats2plpre_list_auxlst_31($arg0);
  return $tmpret101;
} #end-of-function


sub
_ats2plpre_list_auxlst_31($)
{
##
  my($arg0) = @_;
##
  my $tmpret102;
  my $tmp103;
  my $tmp104;
  my $tmp105;
##
  __patsflab__ats2plpre_list_auxlst_31:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab32:
    if(ATSCKptriscons($arg0)) { goto __atstmplab35; }
    __atstmplab33:
    $tmpret102 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab34:
    __atstmplab35:
    $tmp103 = $arg0->[0];
    $tmp104 = $arg0->[1];
    $tmp105 = _ats2plpre_list_auxlst_31($tmp104);
    $tmpret102 = ats2plpre_list_append($tmp103, $tmp105);
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret102;
} #end-of-function


sub
ats2plpre_list_take($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret106;
  my $tmp107;
  my $tmp108;
  my $tmp109;
  my $tmp110;
  my $tmp111;
##
  __patsflab_list_take:
  $tmp107 = ats2plpre_gt_int1_int1($arg1, 0);
  if($tmp107) {
    $tmp108 = $arg0->[0];
    $tmp109 = $arg0->[1];
    $tmp111 = ats2plpre_sub_int1_int1($arg1, 1);
    $tmp110 = ats2plpre_list_take($tmp109, $tmp111);
    $tmpret106 = [$tmp108, $tmp110];
  } else {
    $tmpret106 = 0;
  } #endif
  return $tmpret106;
} #end-of-function


sub
ats2plpre_list_drop($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret112;
  my $tmp113;
  my $tmp114;
  my $tmp115;
##
  __patsflab_list_drop:
  $tmp113 = ats2plpre_gt_int1_int1($arg1, 0);
  if($tmp113) {
    $tmp114 = $arg0->[1];
    $tmp115 = ats2plpre_sub_int1_int1($arg1, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp114;
    $apy1 = $tmp115;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab_list_drop;
    #ATStailcalseq_end
  } else {
    $tmpret112 = $arg0;
  } #endif
  return $tmpret112;
} #end-of-function


sub
ats2plpre_list_split_at($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret116;
  my $tmp117;
  my $tmp118;
##
  __patsflab_list_split_at:
  $tmp117 = ats2plpre_list_take($arg0, $arg1);
  $tmp118 = ats2plpre_list_drop($arg0, $arg1);
  $tmpret116 = [$tmp117, $tmp118];
  return $tmpret116;
} #end-of-function


sub
ats2plpre_list_insert_at($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret119;
  my $tmp120;
  my $tmp121;
  my $tmp122;
  my $tmp123;
  my $tmp124;
##
  __patsflab_list_insert_at:
  $tmp120 = ats2plpre_gt_int1_int1($arg1, 0);
  if($tmp120) {
    $tmp121 = $arg0->[0];
    $tmp122 = $arg0->[1];
    $tmp124 = ats2plpre_sub_int1_int1($arg1, 1);
    $tmp123 = ats2plpre_list_insert_at($tmp122, $tmp124, $arg2);
    $tmpret119 = [$tmp121, $tmp123];
  } else {
    $tmpret119 = [$arg2, $arg0];
  } #endif
  return $tmpret119;
} #end-of-function


sub
ats2plpre_list_remove_at($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret125;
  my $tmp126;
  my $tmp127;
  my $tmp128;
  my $tmp129;
  my $tmp130;
##
  __patsflab_list_remove_at:
  $tmp126 = $arg0->[0];
  $tmp127 = $arg0->[1];
  $tmp128 = ats2plpre_gt_int1_int1($arg1, 0);
  if($tmp128) {
    $tmp130 = ats2plpre_sub_int1_int1($arg1, 1);
    $tmp129 = ats2plpre_list_remove_at($tmp127, $tmp130);
    $tmpret125 = [$tmp126, $tmp129];
  } else {
    $tmpret125 = $tmp127;
  } #endif
  return $tmpret125;
} #end-of-function


sub
ats2plpre_list_takeout_at($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret131;
  my $tmp132;
  my $tmp133;
  my $tmp134;
  my $tmp135;
  my $tmp136;
  my $tmp137;
  my $tmp138;
  my $tmp139;
##
  __patsflab_list_takeout_at:
  $tmp132 = $arg0->[0];
  $tmp133 = $arg0->[1];
  $tmp134 = ats2plpre_gt_int1_int1($arg1, 0);
  if($tmp134) {
    $tmp136 = ats2plpre_sub_int1_int1($arg1, 1);
    $tmp135 = ats2plpre_list_takeout_at($tmp133, $tmp136);
    $tmp137 = $tmp135->[0];
    $tmp138 = $tmp135->[1];
    $tmp139 = [$tmp132, $tmp138];
    $tmpret131 = [$tmp137, $tmp139];
  } else {
    $tmpret131 = [$tmp132, $tmp133];
  } #endif
  return $tmpret131;
} #end-of-function


sub
ats2plpre_list_exists($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret140;
  my $tmp141;
  my $tmp142;
  my $tmp143;
##
  __patsflab_list_exists:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab36:
    if(ATSCKptriscons($arg0)) { goto __atstmplab39; }
    __atstmplab37:
    $tmpret140 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab38:
    __atstmplab39:
    $tmp141 = $arg0->[0];
    $tmp142 = $arg0->[1];
    $tmp143 = &{$arg1->[0]}($arg1, $tmp141);
    if($tmp143) {
      $tmpret140 = 1;
    } else {
      #ATStailcalseq_beg
      $apy0 = $tmp142;
      $apy1 = $arg1;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab_list_exists;
      #ATStailcalseq_end
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret140;
} #end-of-function


sub
ats2plpre_list_exists_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret144;
##
  __patsflab_list_exists_method:
  $tmpret144 = _ats2plpre_list_patsfun_40__closurerize($arg0);
  return $tmpret144;
} #end-of-function


sub
_ats2plpre_list_patsfun_40($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret145;
##
  __patsflab__ats2plpre_list_patsfun_40:
  $tmpret145 = ats2plpre_list_exists($env0, $arg0);
  return $tmpret145;
} #end-of-function


sub
ats2plpre_list_iexists($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret146;
##
  __patsflab_list_iexists:
  $tmpret146 = _ats2plpre_list_loop_42($arg1, 0, $arg0);
  return $tmpret146;
} #end-of-function


sub
_ats2plpre_list_loop_42($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret147;
  my $tmp148;
  my $tmp149;
  my $tmp150;
  my $tmp151;
##
  __patsflab__ats2plpre_list_loop_42:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab40:
    if(ATSCKptriscons($arg1)) { goto __atstmplab43; }
    __atstmplab41:
    $tmpret147 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab42:
    __atstmplab43:
    $tmp148 = $arg1->[0];
    $tmp149 = $arg1->[1];
    $tmp150 = &{$env0->[0]}($env0, $arg0, $tmp148);
    if($tmp150) {
      $tmpret147 = 1;
    } else {
      $tmp151 = ats2plpre_add_int1_int1($arg0, 1);
      #ATStailcalseq_beg
      $apy0 = $tmp151;
      $apy1 = $tmp149;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab__ats2plpre_list_loop_42;
      #ATStailcalseq_end
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret147;
} #end-of-function


sub
ats2plpre_list_iexists_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret152;
##
  __patsflab_list_iexists_method:
  $tmpret152 = _ats2plpre_list_patsfun_44__closurerize($arg0);
  return $tmpret152;
} #end-of-function


sub
_ats2plpre_list_patsfun_44($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret153;
##
  __patsflab__ats2plpre_list_patsfun_44:
  $tmpret153 = ats2plpre_list_iexists($env0, $arg0);
  return $tmpret153;
} #end-of-function


sub
ats2plpre_list_forall($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret154;
  my $tmp155;
  my $tmp156;
  my $tmp157;
##
  __patsflab_list_forall:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab44:
    if(ATSCKptriscons($arg0)) { goto __atstmplab47; }
    __atstmplab45:
    $tmpret154 = 1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab46:
    __atstmplab47:
    $tmp155 = $arg0->[0];
    $tmp156 = $arg0->[1];
    $tmp157 = &{$arg1->[0]}($arg1, $tmp155);
    if($tmp157) {
      #ATStailcalseq_beg
      $apy0 = $tmp156;
      $apy1 = $arg1;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab_list_forall;
      #ATStailcalseq_end
    } else {
      $tmpret154 = 0;
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret154;
} #end-of-function


sub
ats2plpre_list_forall_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret158;
##
  __patsflab_list_forall_method:
  $tmpret158 = _ats2plpre_list_patsfun_47__closurerize($arg0);
  return $tmpret158;
} #end-of-function


sub
_ats2plpre_list_patsfun_47($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret159;
##
  __patsflab__ats2plpre_list_patsfun_47:
  $tmpret159 = ats2plpre_list_forall($env0, $arg0);
  return $tmpret159;
} #end-of-function


sub
ats2plpre_list_iforall($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret160;
##
  __patsflab_list_iforall:
  $tmpret160 = _ats2plpre_list_loop_49($arg1, 0, $arg0);
  return $tmpret160;
} #end-of-function


sub
_ats2plpre_list_loop_49($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret161;
  my $tmp162;
  my $tmp163;
  my $tmp164;
  my $tmp165;
##
  __patsflab__ats2plpre_list_loop_49:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab48:
    if(ATSCKptriscons($arg1)) { goto __atstmplab51; }
    __atstmplab49:
    $tmpret161 = 1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab50:
    __atstmplab51:
    $tmp162 = $arg1->[0];
    $tmp163 = $arg1->[1];
    $tmp164 = &{$env0->[0]}($env0, $arg0, $tmp162);
    if($tmp164) {
      $tmp165 = ats2plpre_add_int1_int1($arg0, 1);
      #ATStailcalseq_beg
      $apy0 = $tmp165;
      $apy1 = $tmp163;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab__ats2plpre_list_loop_49;
      #ATStailcalseq_end
    } else {
      $tmpret161 = 0;
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret161;
} #end-of-function


sub
ats2plpre_list_iforall_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret166;
##
  __patsflab_list_iforall_method:
  $tmpret166 = _ats2plpre_list_patsfun_51__closurerize($arg0);
  return $tmpret166;
} #end-of-function


sub
_ats2plpre_list_patsfun_51($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret167;
##
  __patsflab__ats2plpre_list_patsfun_51:
  $tmpret167 = ats2plpre_list_iforall($env0, $arg0);
  return $tmpret167;
} #end-of-function


sub
ats2plpre_list_app($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_list_app:
  ats2plpre_list_foreach($arg0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_list_foreach($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmp170;
  my $tmp171;
##
  __patsflab_list_foreach:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab52:
    if(ATSCKptriscons($arg0)) { goto __atstmplab55; }
    __atstmplab53:
    #ATSINSmove_void;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab54:
    __atstmplab55:
    $tmp170 = $arg0->[0];
    $tmp171 = $arg0->[1];
    &{$arg1->[0]}($arg1, $tmp170);
    #ATStailcalseq_beg
    $apy0 = $tmp171;
    $apy1 = $arg1;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab_list_foreach;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return;#_void
} #end-of-function


sub
ats2plpre_list_foreach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret173;
##
  __patsflab_list_foreach_method:
  $tmpret173 = _ats2plpre_list_patsfun_55__closurerize($arg0);
  return $tmpret173;
} #end-of-function


sub
_ats2plpre_list_patsfun_55($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_list_patsfun_55:
  ats2plpre_list_foreach($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_list_iforeach($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_list_iforeach:
  _ats2plpre_list_aux_57($arg1, 0, $arg0);
  return;#_void
} #end-of-function


sub
_ats2plpre_list_aux_57($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmp177;
  my $tmp178;
  my $tmp180;
##
  __patsflab__ats2plpre_list_aux_57:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab56:
    if(ATSCKptriscons($arg1)) { goto __atstmplab59; }
    __atstmplab57:
    #ATSINSmove_void;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab58:
    __atstmplab59:
    $tmp177 = $arg1->[0];
    $tmp178 = $arg1->[1];
    &{$env0->[0]}($env0, $arg0, $tmp177);
    $tmp180 = ats2plpre_add_int1_int1($arg0, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp180;
    $apy1 = $tmp178;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_list_aux_57;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return;#_void
} #end-of-function


sub
ats2plpre_list_iforeach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret181;
##
  __patsflab_list_iforeach_method:
  $tmpret181 = _ats2plpre_list_patsfun_59__closurerize($arg0);
  return $tmpret181;
} #end-of-function


sub
_ats2plpre_list_patsfun_59($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_list_patsfun_59:
  ats2plpre_list_iforeach($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_list_rforeach($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmp184;
  my $tmp185;
##
  __patsflab_list_rforeach:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab60:
    if(ATSCKptriscons($arg0)) { goto __atstmplab63; }
    __atstmplab61:
    #ATSINSmove_void;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab62:
    __atstmplab63:
    $tmp184 = $arg0->[0];
    $tmp185 = $arg0->[1];
    ats2plpre_list_rforeach($tmp185, $arg1);
    &{$arg1->[0]}($arg1, $tmp184);
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return;#_void
} #end-of-function


sub
ats2plpre_list_rforeach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret187;
##
  __patsflab_list_rforeach_method:
  $tmpret187 = _ats2plpre_list_patsfun_62__closurerize($arg0);
  return $tmpret187;
} #end-of-function


sub
_ats2plpre_list_patsfun_62($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_list_patsfun_62:
  ats2plpre_list_rforeach($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_list_filter($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret189;
##
  __patsflab_list_filter:
  $tmpret189 = _ats2plpre_list_aux_64($arg1, $arg0);
  return $tmpret189;
} #end-of-function


sub
_ats2plpre_list_aux_64($$)
{
##
  my($env0, $arg0) = @_;
##
  my $apy0;
  my $tmpret190;
  my $tmp191;
  my $tmp192;
  my $tmp193;
  my $tmp194;
##
  __patsflab__ats2plpre_list_aux_64:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab64:
    if(ATSCKptriscons($arg0)) { goto __atstmplab67; }
    __atstmplab65:
    $tmpret190 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab66:
    __atstmplab67:
    $tmp191 = $arg0->[0];
    $tmp192 = $arg0->[1];
    $tmp193 = &{$env0->[0]}($env0, $tmp191);
    if($tmp193) {
      $tmp194 = _ats2plpre_list_aux_64($env0, $tmp192);
      $tmpret190 = [$tmp191, $tmp194];
    } else {
      #ATStailcalseq_beg
      $apy0 = $tmp192;
      $arg0 = $apy0;
      goto __patsflab__ats2plpre_list_aux_64;
      #ATStailcalseq_end
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret190;
} #end-of-function


sub
ats2plpre_list_filter_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret195;
##
  __patsflab_list_filter_method:
  $tmpret195 = _ats2plpre_list_patsfun_66__closurerize($arg0);
  return $tmpret195;
} #end-of-function


sub
_ats2plpre_list_patsfun_66($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret196;
##
  __patsflab__ats2plpre_list_patsfun_66:
  $tmpret196 = ats2plpre_list_filter($env0, $arg0);
  return $tmpret196;
} #end-of-function


sub
_057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2pl_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_labelize($)
{
##
  my($arg0) = @_;
##
  my $tmpret197;
##
  __patsflab_list_labelize:
  $tmpret197 = ats2plpre_list_imap($arg0, _ats2plpre_list_patsfun_68__closurerize());
  return $tmpret197;
} #end-of-function


sub
_ats2plpre_list_patsfun_68($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret198;
##
  __patsflab__ats2plpre_list_patsfun_68:
  $tmpret198 = [$arg0, $arg1];
  return $tmpret198;
} #end-of-function


sub
ats2plpre_list_map($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret199;
##
  __patsflab_list_map:
  $tmpret199 = _ats2plpre_list_aux_70($arg1, $arg0);
  return $tmpret199;
} #end-of-function


sub
_ats2plpre_list_aux_70($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret200;
  my $tmp201;
  my $tmp202;
  my $tmp203;
  my $tmp204;
##
  __patsflab__ats2plpre_list_aux_70:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab68:
    if(ATSCKptriscons($arg0)) { goto __atstmplab71; }
    __atstmplab69:
    $tmpret200 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab70:
    __atstmplab71:
    $tmp201 = $arg0->[0];
    $tmp202 = $arg0->[1];
    $tmp203 = &{$env0->[0]}($env0, $tmp201);
    $tmp204 = _ats2plpre_list_aux_70($env0, $tmp202);
    $tmpret200 = [$tmp203, $tmp204];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret200;
} #end-of-function


sub
ats2plpre_list_map_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret205;
##
  __patsflab_list_map_method:
  $tmpret205 = _ats2plpre_list_patsfun_72__closurerize($arg0);
  return $tmpret205;
} #end-of-function


sub
_ats2plpre_list_patsfun_72($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret206;
##
  __patsflab__ats2plpre_list_patsfun_72:
  $tmpret206 = ats2plpre_list_map($env0, $arg0);
  return $tmpret206;
} #end-of-function


sub
ats2plpre_list_imap($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret207;
##
  __patsflab_list_imap:
  $tmpret207 = _ats2plpre_list_aux_74($arg1, 0, $arg0);
  return $tmpret207;
} #end-of-function


sub
_ats2plpre_list_aux_74($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret208;
  my $tmp209;
  my $tmp210;
  my $tmp211;
  my $tmp212;
  my $tmp213;
##
  __patsflab__ats2plpre_list_aux_74:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab72:
    if(ATSCKptriscons($arg1)) { goto __atstmplab75; }
    __atstmplab73:
    $tmpret208 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab74:
    __atstmplab75:
    $tmp209 = $arg1->[0];
    $tmp210 = $arg1->[1];
    $tmp211 = &{$env0->[0]}($env0, $arg0, $tmp209);
    $tmp213 = ats2plpre_add_int1_int1($arg0, 1);
    $tmp212 = _ats2plpre_list_aux_74($env0, $tmp213, $tmp210);
    $tmpret208 = [$tmp211, $tmp212];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret208;
} #end-of-function


sub
ats2plpre_list_imap_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret214;
##
  __patsflab_list_imap_method:
  $tmpret214 = _ats2plpre_list_patsfun_76__closurerize($arg0);
  return $tmpret214;
} #end-of-function


sub
_ats2plpre_list_patsfun_76($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret215;
##
  __patsflab__ats2plpre_list_patsfun_76:
  $tmpret215 = ats2plpre_list_imap($env0, $arg0);
  return $tmpret215;
} #end-of-function


sub
ats2plpre_list_map2($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret216;
  my $tmp217;
  my $tmp218;
  my $tmp219;
  my $tmp220;
  my $tmp221;
  my $tmp222;
##
  __patsflab_list_map2:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab76:
    if(ATSCKptriscons($arg0)) { goto __atstmplab79; }
    __atstmplab77:
    $tmpret216 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab78:
    __atstmplab79:
    $tmp217 = $arg0->[0];
    $tmp218 = $arg0->[1];
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab80:
      if(ATSCKptriscons($arg1)) { goto __atstmplab83; }
      __atstmplab81:
      $tmpret216 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab82:
      __atstmplab83:
      $tmp219 = $arg1->[0];
      $tmp220 = $arg1->[1];
      $tmp221 = &{$arg2->[0]}($arg2, $tmp217, $tmp219);
      $tmp222 = ats2plpre_list_map2($tmp218, $tmp220, $arg2);
      $tmpret216 = [$tmp221, $tmp222];
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret216;
} #end-of-function


sub
ats2plpre_list_foldleft($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret223;
##
  __patsflab_list_foldleft:
  $tmpret223 = _ats2plpre_list_loop_79($arg2, $arg1, $arg0);
  return $tmpret223;
} #end-of-function


sub
_ats2plpre_list_loop_79($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret224;
  my $tmp225;
  my $tmp226;
  my $tmp227;
##
  __patsflab__ats2plpre_list_loop_79:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab84:
    if(ATSCKptriscons($arg1)) { goto __atstmplab87; }
    __atstmplab85:
    $tmpret224 = $arg0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab86:
    __atstmplab87:
    $tmp225 = $arg1->[0];
    $tmp226 = $arg1->[1];
    $tmp227 = &{$env0->[0]}($env0, $arg0, $tmp225);
    #ATStailcalseq_beg
    $apy0 = $tmp227;
    $apy1 = $tmp226;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_list_loop_79;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret224;
} #end-of-function


sub
ats2plpre_list_foldleft_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret228;
##
  __patsflab_list_foldleft_method:
  $tmpret228 = _ats2plpre_list_patsfun_81__closurerize($arg0, $arg1);
  return $tmpret228;
} #end-of-function


sub
_ats2plpre_list_patsfun_81($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret229;
##
  __patsflab__ats2plpre_list_patsfun_81:
  $tmpret229 = ats2plpre_list_foldleft($env0, $env1, $arg0);
  return $tmpret229;
} #end-of-function


sub
ats2plpre_list_ifoldleft($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret230;
##
  __patsflab_list_ifoldleft:
  $tmpret230 = _ats2plpre_list_loop_83($arg2, 0, $arg1, $arg0);
  return $tmpret230;
} #end-of-function


sub
_ats2plpre_list_loop_83($$$$)
{
##
  my($env0, $arg0, $arg1, $arg2) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $tmpret231;
  my $tmp232;
  my $tmp233;
  my $tmp234;
  my $tmp235;
##
  __patsflab__ats2plpre_list_loop_83:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab88:
    if(ATSCKptriscons($arg2)) { goto __atstmplab91; }
    __atstmplab89:
    $tmpret231 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab90:
    __atstmplab91:
    $tmp232 = $arg2->[0];
    $tmp233 = $arg2->[1];
    $tmp234 = ats2plpre_add_int1_int1($arg0, 1);
    $tmp235 = &{$env0->[0]}($env0, $arg0, $arg1, $tmp232);
    #ATStailcalseq_beg
    $apy0 = $tmp234;
    $apy1 = $tmp235;
    $apy2 = $tmp233;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    goto __patsflab__ats2plpre_list_loop_83;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret231;
} #end-of-function


sub
ats2plpre_list_ifoldleft_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret236;
##
  __patsflab_list_ifoldleft_method:
  $tmpret236 = _ats2plpre_list_patsfun_85__closurerize($arg0, $arg1);
  return $tmpret236;
} #end-of-function


sub
_ats2plpre_list_patsfun_85($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret237;
##
  __patsflab__ats2plpre_list_patsfun_85:
  $tmpret237 = ats2plpre_list_ifoldleft($env0, $env1, $arg0);
  return $tmpret237;
} #end-of-function


sub
ats2plpre_list_foldright($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret238;
##
  __patsflab_list_foldright:
  $tmpret238 = _ats2plpre_list_aux_87($arg1, $arg0, $arg2);
  return $tmpret238;
} #end-of-function


sub
_ats2plpre_list_aux_87($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret239;
  my $tmp240;
  my $tmp241;
  my $tmp242;
##
  __patsflab__ats2plpre_list_aux_87:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab92:
    if(ATSCKptriscons($arg0)) { goto __atstmplab95; }
    __atstmplab93:
    $tmpret239 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab94:
    __atstmplab95:
    $tmp240 = $arg0->[0];
    $tmp241 = $arg0->[1];
    $tmp242 = _ats2plpre_list_aux_87($env0, $tmp241, $arg1);
    $tmpret239 = &{$env0->[0]}($env0, $tmp240, $tmp242);
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret239;
} #end-of-function


sub
ats2plpre_list_foldright_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret243;
##
  __patsflab_list_foldright_method:
  $tmpret243 = _ats2plpre_list_patsfun_89__closurerize($arg0, $arg1);
  return $tmpret243;
} #end-of-function


sub
_ats2plpre_list_patsfun_89($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret244;
##
  __patsflab__ats2plpre_list_patsfun_89:
  $tmpret244 = ats2plpre_list_foldright($env0, $arg0, $env1);
  return $tmpret244;
} #end-of-function


sub
ats2plpre_list_ifoldright($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret245;
##
  __patsflab_list_ifoldright:
  $tmpret245 = _ats2plpre_list_aux_91($arg1, 0, $arg0, $arg2);
  return $tmpret245;
} #end-of-function


sub
_ats2plpre_list_aux_91($$$$)
{
##
  my($env0, $arg0, $arg1, $arg2) = @_;
##
  my $tmpret246;
  my $tmp247;
  my $tmp248;
  my $tmp249;
  my $tmp250;
##
  __patsflab__ats2plpre_list_aux_91:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab96:
    if(ATSCKptriscons($arg1)) { goto __atstmplab99; }
    __atstmplab97:
    $tmpret246 = $arg2;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab98:
    __atstmplab99:
    $tmp247 = $arg1->[0];
    $tmp248 = $arg1->[1];
    $tmp250 = ats2plpre_add_int1_int1($arg0, 1);
    $tmp249 = _ats2plpre_list_aux_91($env0, $tmp250, $tmp248, $arg2);
    $tmpret246 = &{$env0->[0]}($env0, $arg0, $tmp247, $tmp249);
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret246;
} #end-of-function


sub
ats2plpre_list_ifoldright_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret251;
##
  __patsflab_list_ifoldright_method:
  $tmpret251 = _ats2plpre_list_patsfun_93__closurerize($arg0, $arg1);
  return $tmpret251;
} #end-of-function


sub
_ats2plpre_list_patsfun_93($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret252;
##
  __patsflab__ats2plpre_list_patsfun_93:
  $tmpret252 = ats2plpre_list_ifoldright($env0, $arg0, $env1);
  return $tmpret252;
} #end-of-function


sub
ats2plpre_list_mergesort($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret255;
  my $tmp274;
##
  __patsflab_list_mergesort:
  $tmp274 = ats2plpre_list_length($arg0);
  $tmpret255 = _ats2plpre_list_msort_97($arg1, $arg0, $tmp274);
  return $tmpret255;
} #end-of-function


sub
_ats2plpre_list_msort_97($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret256;
  my $tmp257;
  my $tmp258;
  my $tmp259;
  my $tmp260;
  my $tmp261;
  my $tmp262;
  my $tmp263;
  my $tmp264;
##
  __patsflab__ats2plpre_list_msort_97:
  $tmp257 = ats2plpre_lt_int1_int1($arg1, 2);
  if($tmp257) {
    $tmpret256 = $arg0;
  } else {
    $tmp258 = ats2plpre_half_int1($arg1);
    $tmp259 = ats2plpre_list_split_at($arg0, $tmp258);
    $tmp260 = $tmp259->[0];
    $tmp261 = $tmp259->[1];
    $tmp262 = _ats2plpre_list_msort_97($env0, $tmp260, $tmp258);
    $tmp264 = ats2plpre_sub_int1_int1($arg1, $tmp258);
    $tmp263 = _ats2plpre_list_msort_97($env0, $tmp261, $tmp264);
    $tmpret256 = _ats2plpre_list_merge_98($env0, $tmp262, $tmp263);
  } #endif
  return $tmpret256;
} #end-of-function


sub
_ats2plpre_list_merge_98($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret265;
  my $tmp266;
  my $tmp267;
  my $tmp268;
  my $tmp269;
  my $tmp270;
  my $tmp271;
  my $tmp272;
  my $tmp273;
##
  __patsflab__ats2plpre_list_merge_98:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab100:
    if(ATSCKptriscons($arg0)) { goto __atstmplab103; }
    __atstmplab101:
    $tmpret265 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab102:
    __atstmplab103:
    $tmp266 = $arg0->[0];
    $tmp267 = $arg0->[1];
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab104:
      if(ATSCKptriscons($arg1)) { goto __atstmplab107; }
      __atstmplab105:
      $tmpret265 = $arg0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab106:
      __atstmplab107:
      $tmp268 = $arg1->[0];
      $tmp269 = $arg1->[1];
      $tmp270 = &{$env0->[0]}($env0, $tmp266, $tmp268);
      $tmp271 = ats2plpre_lte_int0_int0($tmp270, 0);
      if($tmp271) {
        $tmp272 = _ats2plpre_list_merge_98($env0, $tmp267, $arg1);
        $tmpret265 = [$tmp266, $tmp272];
      } else {
        $tmp273 = _ats2plpre_list_merge_98($env0, $arg0, $tmp269);
        $tmpret265 = [$tmp268, $tmp273];
      } #endif
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret265;
} #end-of-function


sub
ats2plpre_streamize_list_elt($)
{
##
  my($arg0) = @_;
##
  my $tmpret275;
##
  __patsflab_streamize_list_elt:
  $tmpret275 = _ats2plpre_list_auxmain_100($arg0);
  return $tmpret275;
} #end-of-function


sub
_ats2plpre_list_auxmain_100($)
{
##
  my($arg0) = @_;
##
  my $tmpret276;
##
  __patsflab__ats2plpre_list_auxmain_100:
  $tmpret276 = ATSPMVllazyval(_ats2plpre_list_patsfun_101__closurerize($arg0));
  return $tmpret276;
} #end-of-function


sub
_ats2plpre_list_patsfun_101($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret277;
  my $tmp278;
  my $tmp279;
  my $tmp280;
##
  __patsflab__ats2plpre_list_patsfun_101:
  if($arg0) {
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab108:
      if(ATSCKptriscons($env0)) { goto __atstmplab111; }
      __atstmplab109:
      $tmpret277 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab110:
      __atstmplab111:
      $tmp278 = $env0->[0];
      $tmp279 = $env0->[1];
      $tmp280 = _ats2plpre_list_auxmain_100($tmp279);
      $tmpret277 = [$tmp278, $tmp280];
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
  } #endif
  return $tmpret277;
} #end-of-function


sub
ats2plpre_streamize_list_zip($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret281;
##
  __patsflab_streamize_list_zip:
  $tmpret281 = _ats2plpre_list_auxmain_103($arg0, $arg1);
  return $tmpret281;
} #end-of-function


sub
_ats2plpre_list_auxmain_103($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret282;
##
  __patsflab__ats2plpre_list_auxmain_103:
  $tmpret282 = ATSPMVllazyval(_ats2plpre_list_patsfun_104__closurerize($arg0, $arg1));
  return $tmpret282;
} #end-of-function


sub
_ats2plpre_list_patsfun_104($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret283;
  my $tmp284;
  my $tmp285;
  my $tmp286;
  my $tmp287;
  my $tmp288;
  my $tmp289;
##
  __patsflab__ats2plpre_list_patsfun_104:
  if($arg0) {
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab112:
      if(ATSCKptriscons($env0)) { goto __atstmplab115; }
      __atstmplab113:
      $tmpret283 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab114:
      __atstmplab115:
      $tmp284 = $env0->[0];
      $tmp285 = $env0->[1];
      #ATScaseofseq_beg
      while(1)
      {
        #ATSbranchseq_beg
        __atstmplab116:
        if(ATSCKptriscons($env1)) { goto __atstmplab119; }
        __atstmplab117:
        $tmpret283 = 0;
        last;
        #ATSbranchseq_end
        #ATSbranchseq_beg
        __atstmplab118:
        __atstmplab119:
        $tmp286 = $env1->[0];
        $tmp287 = $env1->[1];
        $tmp288 = [$tmp284, $tmp286];
        $tmp289 = _ats2plpre_list_auxmain_103($tmp285, $tmp287);
        $tmpret283 = [$tmp288, $tmp289];
        last;
        #ATSbranchseq_end
      } #end-of-while-loop;
      #ATScaseofseq_end
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
  } #endif
  return $tmpret283;
} #end-of-function


sub
ats2plpre_streamize_list_cross($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret290;
##
  __patsflab_streamize_list_cross:
  $tmpret290 = _ats2plpre_list_auxmain_108($arg0, $arg1);
  return $tmpret290;
} #end-of-function


sub
_ats2plpre_list_auxone_106($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret291;
##
  __patsflab__ats2plpre_list_auxone_106:
  $tmpret291 = ATSPMVllazyval(_ats2plpre_list_patsfun_107__closurerize($arg0, $arg1));
  return $tmpret291;
} #end-of-function


sub
_ats2plpre_list_patsfun_107($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret292;
  my $tmp293;
  my $tmp294;
  my $tmp295;
  my $tmp296;
##
  __patsflab__ats2plpre_list_patsfun_107:
  if($arg0) {
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab120:
      if(ATSCKptriscons($env1)) { goto __atstmplab123; }
      __atstmplab121:
      $tmpret292 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab122:
      __atstmplab123:
      $tmp293 = $env1->[0];
      $tmp294 = $env1->[1];
      $tmp295 = [$env0, $tmp293];
      $tmp296 = _ats2plpre_list_auxone_106($env0, $tmp294);
      $tmpret292 = [$tmp295, $tmp296];
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
  } #endif
  return $tmpret292;
} #end-of-function


sub
_ats2plpre_list_auxmain_108($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret297;
##
  __patsflab__ats2plpre_list_auxmain_108:
  $tmpret297 = ATSPMVllazyval(_ats2plpre_list_patsfun_109__closurerize($arg0, $arg1));
  return $tmpret297;
} #end-of-function


sub
_ats2plpre_list_patsfun_109($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret298;
  my $tmp299;
  my $tmp300;
  my $tmp301;
  my $tmp302;
  my $tmp303;
##
  __patsflab__ats2plpre_list_patsfun_109:
  if($arg0) {
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab124:
      if(ATSCKptriscons($env0)) { goto __atstmplab127; }
      __atstmplab125:
      $tmpret298 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab126:
      __atstmplab127:
      $tmp299 = $env0->[0];
      $tmp300 = $env0->[1];
      $tmp302 = _ats2plpre_list_auxone_106($tmp299, $env1);
      $tmp303 = _ats2plpre_list_auxmain_108($tmp300, $env1);
      $tmp301 = ats2plpre_stream_vt_append($tmp302, $tmp303);
      $tmpret298 = ATSPMVllazyval_eval($tmp301);
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
  } #endif
  return $tmpret298;
} #end-of-function


######
#ATSextcode_beg()
######
######
1; ##note that it is needed by 'use' or 'require'
######
######
#ATSextcode_end()
######
######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######

sub
ats2plpre_option_some($)
{
##
  my($arg0) = @_;
##
  my $tmpret0;
##
  __patsflab_option_some:
  $tmpret0 = [$arg0];
  return $tmpret0;
} #end-of-function


sub
ats2plpre_option_none()
{
##
  #argless
##
  my $tmpret1;
##
  __patsflab_option_none:
  $tmpret1 = 0;
  return $tmpret1;
} #end-of-function


sub
ats2plpre_option_unsome($)
{
##
  my($arg0) = @_;
##
  my $tmpret2;
  my $tmp3;
##
  __patsflab_option_unsome:
  $tmp3 = $arg0->[0];
  $tmpret2 = $tmp3;
  return $tmpret2;
} #end-of-function


sub
ats2plpre_option_is_some($)
{
##
  my($arg0) = @_;
##
  my $tmpret4;
##
  __patsflab_option_is_some:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab0:
    if(ATSCKptrisnull($arg0)) { goto __atstmplab3; }
    __atstmplab1:
    $tmpret4 = 1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab2:
    __atstmplab3:
    $tmpret4 = 0;
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret4;
} #end-of-function


sub
ats2plpre_option_is_none($)
{
##
  my($arg0) = @_;
##
  my $tmpret5;
##
  __patsflab_option_is_none:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab4:
    if(ATSCKptriscons($arg0)) { goto __atstmplab7; }
    __atstmplab5:
    $tmpret5 = 1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab6:
    __atstmplab7:
    $tmpret5 = 0;
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret5;
} #end-of-function

######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######
sub
_ats2plpre_stream_patsfun_6__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_6($cenv->[1]); }, $env0];
}

sub
_ats2plpre_stream_patsfun_16__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_patsfun_16($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_stream_patsfun_22__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_22($cenv->[1], $cenv->[2]); }, $env0, $env1];
}

sub
_ats2plpre_stream_patsfun_24__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_24($cenv->[1]); }, $env0];
}

sub
_ats2plpre_stream_patsfun_26__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_26($cenv->[1], $cenv->[2]); }, $env0, $env1];
}

sub
_ats2plpre_stream_patsfun_28__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_patsfun_28($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_patsfun_30__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_30($cenv->[1], $cenv->[2]); }, $env0, $env1];
}

sub
_ats2plpre_stream_patsfun_32__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_patsfun_32($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_patsfun_35__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_patsfun_35($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_patsfun_38__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_patsfun_38($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_patsfun_41__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_patsfun_41($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_patsfun_45__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_patsfun_45($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_patsfun_48__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_48($cenv->[1], $cenv->[2]); }, $env0, $env1];
}

sub
_ats2plpre_stream_patsfun_51__closurerize($$$$)
{
  my($env0, $env1, $env2, $env3) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_51($cenv->[1], $cenv->[2], $cenv->[3], $cenv->[4]); }, $env0, $env1, $env2, $env3];
}

sub
_ats2plpre_stream_patsfun_52__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_52($cenv->[1], $cenv->[2]); }, $env0, $env1];
}

sub
_ats2plpre_stream_patsfun_55__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_55($cenv->[1]); }, $env0];
}

sub
_ats2plpre_stream_patsfun_57__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_57($cenv->[1]); }, $env0];
}

sub
_ats2plpre_stream_patsfun_59__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_59($cenv->[1], $cenv->[2]); }, $env0, $env1];
}

sub
_ats2plpre_stream_patsfun_64__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0, $arg1) = @_; return _ats2plpre_stream_patsfun_64($cenv->[1], $arg0, $arg1); }, $env0];
}

sub
_ats2plpre_stream_patsfun_66__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0, $arg1) = @_; return _ats2plpre_stream_patsfun_66($cenv->[1], $arg0, $arg1); }, $env0];
}

sub
_ats2plpre_stream_patsfun_69__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_69($cenv->[1], $cenv->[2]); }, $env0, $env1];
}

sub
_ats2plpre_stream_patsfun_71__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_patsfun_71($cenv->[1], $cenv->[2]); }, $env0, $env1];
}


sub
ats2plpre_stream_make_list($)
{
##
  my($arg0) = @_;
##
  my $tmpret7;
##
  __patsflab_stream_make_list:
  $tmpret7 = ATSPMVlazyval(_ats2plpre_stream_patsfun_6__closurerize($arg0));
  return $tmpret7;
} #end-of-function


sub
_ats2plpre_stream_patsfun_6($)
{
##
  my($env0) = @_;
##
  my $tmpret8;
  my $tmp9;
  my $tmp10;
  my $tmp11;
##
  __patsflab__ats2plpre_stream_patsfun_6:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab0:
    if(ATSCKptriscons($env0)) { goto __atstmplab3; }
    __atstmplab1:
    $tmpret8 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab2:
    __atstmplab3:
    $tmp9 = $env0->[0];
    $tmp10 = $env0->[1];
    $tmp11 = ats2plpre_stream_make_list($tmp10);
    $tmpret8 = [$tmp9, $tmp11];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret8;
} #end-of-function


sub
ats2plpre_stream_make_list0($)
{
##
  my($arg0) = @_;
##
  my $tmpret12;
##
  __patsflab_stream_make_list0:
  $tmpret12 = ats2plpre_stream_make_list($arg0);
  return $tmpret12;
} #end-of-function


sub
ats2plpre_stream_nth_opt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret13;
##
  __patsflab_stream_nth_opt:
  $tmpret13 = _ats2plpre_stream_loop_9($arg0, $arg1);
  return $tmpret13;
} #end-of-function


sub
_ats2plpre_stream_loop_9($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret14;
  my $tmp15;
  my $tmp16;
  my $tmp17;
  my $tmp18;
  my $tmp19;
##
  __patsflab__ats2plpre_stream_loop_9:
  $tmp15 = ATSPMVlazyval_eval($arg0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab4:
    if(ATSCKptriscons($tmp15)) { goto __atstmplab7; }
    __atstmplab5:
    $tmpret14 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab6:
    __atstmplab7:
    $tmp16 = $tmp15->[0];
    $tmp17 = $tmp15->[1];
    $tmp18 = ats2plpre_gt_int1_int1($arg1, 0);
    if($tmp18) {
      $tmp19 = ats2plpre_pred_int1($arg1);
      #ATStailcalseq_beg
      $apy0 = $tmp17;
      $apy1 = $tmp19;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab__ats2plpre_stream_loop_9;
      #ATStailcalseq_end
    } else {
      $tmpret14 = [$tmp16];
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret14;
} #end-of-function


sub
ats2plpre_stream_length($)
{
##
  my($arg0) = @_;
##
  my $tmpret20;
##
  __patsflab_stream_length:
  $tmpret20 = _ats2plpre_stream_loop_11($arg0, 0);
  return $tmpret20;
} #end-of-function


sub
_ats2plpre_stream_loop_11($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret21;
  my $tmp22;
  my $tmp24;
  my $tmp25;
##
  __patsflab__ats2plpre_stream_loop_11:
  $tmp22 = ATSPMVlazyval_eval($arg0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab8:
    if(ATSCKptriscons($tmp22)) { goto __atstmplab11; }
    __atstmplab9:
    $tmpret21 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab10:
    __atstmplab11:
    $tmp24 = $tmp22->[1];
    $tmp25 = ats2plpre_add_int1_int1($arg1, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp24;
    $apy1 = $tmp25;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_stream_loop_11;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret21;
} #end-of-function


sub
ats2plpre_stream2list($)
{
##
  my($arg0) = @_;
##
  my $tmpret26;
  my $tmp27;
##
  __patsflab_stream2list:
  $tmp27 = ats2plpre_stream2list_rev($arg0);
  $tmpret26 = ats2plpre_list_reverse($tmp27);
  return $tmpret26;
} #end-of-function


sub
ats2plpre_stream2list_rev($)
{
##
  my($arg0) = @_;
##
  my $tmpret28;
  my $tmp34;
##
  __patsflab_stream2list_rev:
  $tmp34 = 0;
  $tmpret28 = _ats2plpre_stream_loop_14($arg0, $tmp34);
  return $tmpret28;
} #end-of-function


sub
_ats2plpre_stream_loop_14($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret29;
  my $tmp30;
  my $tmp31;
  my $tmp32;
  my $tmp33;
##
  __patsflab__ats2plpre_stream_loop_14:
  $tmp30 = ATSPMVlazyval_eval($arg0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab12:
    if(ATSCKptriscons($tmp30)) { goto __atstmplab15; }
    __atstmplab13:
    $tmpret29 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab14:
    __atstmplab15:
    $tmp31 = $tmp30->[0];
    $tmp32 = $tmp30->[1];
    $tmp33 = [$tmp31, $arg1];
    #ATStailcalseq_beg
    $apy0 = $tmp32;
    $apy1 = $tmp33;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_stream_loop_14;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret29;
} #end-of-function


sub
ats2plpre_stream_takeLte($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret35;
##
  __patsflab_stream_takeLte:
  $tmpret35 = ATSPMVllazyval(_ats2plpre_stream_patsfun_16__closurerize($arg0, $arg1));
  return $tmpret35;
} #end-of-function


sub
_ats2plpre_stream_patsfun_16($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret36;
  my $tmp37;
  my $tmp38;
  my $tmp39;
  my $tmp40;
  my $tmp41;
  my $tmp42;
##
  __patsflab__ats2plpre_stream_patsfun_16:
  if($arg0) {
    $tmp37 = ats2plpre_gt_int1_int1($env1, 0);
    if($tmp37) {
      $tmp38 = ATSPMVlazyval_eval($env0); 
      #ATScaseofseq_beg
      while(1)
      {
        #ATSbranchseq_beg
        __atstmplab16:
        if(ATSCKptriscons($tmp38)) { goto __atstmplab19; }
        __atstmplab17:
        $tmpret36 = 0;
        last;
        #ATSbranchseq_end
        #ATSbranchseq_beg
        __atstmplab18:
        __atstmplab19:
        $tmp39 = $tmp38->[0];
        $tmp40 = $tmp38->[1];
        $tmp42 = ats2plpre_sub_int1_int1($env1, 1);
        $tmp41 = ats2plpre_stream_takeLte($tmp40, $tmp42);
        $tmpret36 = [$tmp39, $tmp41];
        last;
        #ATSbranchseq_end
      } #end-of-while-loop;
      #ATScaseofseq_end
    } else {
      $tmpret36 = 0;
    } #endif
  } else {
  } #endif
  return $tmpret36;
} #end-of-function


sub
ats2plpre_stream_take_opt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret43;
  my $tmp52;
##
  __patsflab_stream_take_opt:
  $tmp52 = 0;
  $tmpret43 = _ats2plpre_stream_auxmain_18($arg1, $arg0, 0, $tmp52);
  return $tmpret43;
} #end-of-function


sub
_ats2plpre_stream_auxmain_18($$$$)
{
##
  my($env0, $arg0, $arg1, $arg2) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $tmpret44;
  my $tmp45;
  my $tmp46;
  my $tmp47;
  my $tmp48;
  my $tmp49;
  my $tmp50;
  my $tmp51;
##
  __patsflab__ats2plpre_stream_auxmain_18:
  $tmp45 = ats2plpre_lt_int1_int1($arg1, $env0);
  if($tmp45) {
    $tmp46 = ATSPMVlazyval_eval($arg0); 
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab20:
      if(ATSCKptriscons($tmp46)) { goto __atstmplab23; }
      __atstmplab21:
      $tmpret44 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab22:
      __atstmplab23:
      $tmp47 = $tmp46->[0];
      $tmp48 = $tmp46->[1];
      $tmp49 = ats2plpre_add_int1_int1($arg1, 1);
      $tmp50 = [$tmp47, $arg2];
      #ATStailcalseq_beg
      $apy0 = $tmp48;
      $apy1 = $tmp49;
      $apy2 = $tmp50;
      $arg0 = $apy0;
      $arg1 = $apy1;
      $arg2 = $apy2;
      goto __patsflab__ats2plpre_stream_auxmain_18;
      #ATStailcalseq_end
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
    $tmp51 = ats2plpre_list_reverse($arg2);
    $tmpret44 = [$tmp51];
  } #endif
  return $tmpret44;
} #end-of-function


sub
ats2plpre_stream_drop_opt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret53;
##
  __patsflab_stream_drop_opt:
  $tmpret53 = _ats2plpre_stream_auxmain_20($arg1, $arg0, 0);
  return $tmpret53;
} #end-of-function


sub
_ats2plpre_stream_auxmain_20($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret54;
  my $tmp55;
  my $tmp56;
  my $tmp58;
  my $tmp59;
##
  __patsflab__ats2plpre_stream_auxmain_20:
  $tmp55 = ats2plpre_lt_int1_int1($arg1, $env0);
  if($tmp55) {
    $tmp56 = ATSPMVlazyval_eval($arg0); 
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab24:
      if(ATSCKptriscons($tmp56)) { goto __atstmplab27; }
      __atstmplab25:
      $tmpret54 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab26:
      __atstmplab27:
      $tmp58 = $tmp56->[1];
      $tmp59 = ats2plpre_add_int1_int1($arg1, 1);
      #ATStailcalseq_beg
      $apy0 = $tmp58;
      $apy1 = $tmp59;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab__ats2plpre_stream_auxmain_20;
      #ATStailcalseq_end
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
    $tmpret54 = [$arg0];
  } #endif
  return $tmpret54;
} #end-of-function


sub
ats2plpre_stream_append($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret60;
##
  __patsflab_stream_append:
  $tmpret60 = ATSPMVlazyval(_ats2plpre_stream_patsfun_22__closurerize($arg0, $arg1));
  return $tmpret60;
} #end-of-function


sub
_ats2plpre_stream_patsfun_22($$)
{
##
  my($env0, $env1) = @_;
##
  my $tmpret61;
  my $tmp62;
  my $tmp63;
  my $tmp64;
  my $tmp65;
##
  __patsflab__ats2plpre_stream_patsfun_22:
  $tmp62 = ATSPMVlazyval_eval($env0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab28:
    if(ATSCKptriscons($tmp62)) { goto __atstmplab31; }
    __atstmplab29:
    $tmpret61 = ATSPMVlazyval_eval($env1); 
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab30:
    __atstmplab31:
    $tmp63 = $tmp62->[0];
    $tmp64 = $tmp62->[1];
    $tmp65 = ats2plpre_stream_append($tmp64, $env1);
    $tmpret61 = [$tmp63, $tmp65];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret61;
} #end-of-function


sub
ats2plpre_stream_concat($)
{
##
  my($arg0) = @_;
##
  my $tmpret66;
##
  __patsflab_stream_concat:
  $tmpret66 = ATSPMVlazyval(_ats2plpre_stream_patsfun_24__closurerize($arg0));
  return $tmpret66;
} #end-of-function


sub
_ats2plpre_stream_patsfun_24($)
{
##
  my($env0) = @_;
##
  my $tmpret67;
  my $tmp68;
  my $tmp69;
  my $tmp70;
  my $tmp71;
  my $tmp72;
##
  __patsflab__ats2plpre_stream_patsfun_24:
  $tmp68 = ATSPMVlazyval_eval($env0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab32:
    if(ATSCKptriscons($tmp68)) { goto __atstmplab35; }
    __atstmplab33:
    $tmpret67 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab34:
    __atstmplab35:
    $tmp69 = $tmp68->[0];
    $tmp70 = $tmp68->[1];
    $tmp72 = ats2plpre_stream_concat($tmp70);
    $tmp71 = ats2plpre_stream_append($tmp69, $tmp72);
    $tmpret67 = ATSPMVlazyval_eval($tmp71); 
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret67;
} #end-of-function


sub
ats2plpre_stream_map_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret73;
##
  __patsflab_stream_map_cloref:
  $tmpret73 = ATSPMVlazyval(_ats2plpre_stream_patsfun_26__closurerize($arg0, $arg1));
  return $tmpret73;
} #end-of-function


sub
_ats2plpre_stream_patsfun_26($$)
{
##
  my($env0, $env1) = @_;
##
  my $tmpret74;
  my $tmp75;
  my $tmp76;
  my $tmp77;
  my $tmp78;
  my $tmp79;
##
  __patsflab__ats2plpre_stream_patsfun_26:
  $tmp75 = ATSPMVlazyval_eval($env0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab36:
    if(ATSCKptriscons($tmp75)) { goto __atstmplab39; }
    __atstmplab37:
    $tmpret74 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab38:
    __atstmplab39:
    $tmp76 = $tmp75->[0];
    $tmp77 = $tmp75->[1];
    $tmp78 = &{$env1->[0]}($env1, $tmp76);
    $tmp79 = ats2plpre_stream_map_cloref($tmp77, $env1);
    $tmpret74 = [$tmp78, $tmp79];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret74;
} #end-of-function


sub
ats2plpre_stream_map_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret80;
##
  __patsflab_stream_map_method:
  $tmpret80 = _ats2plpre_stream_patsfun_28__closurerize($arg0);
  return $tmpret80;
} #end-of-function


sub
_ats2plpre_stream_patsfun_28($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret81;
##
  __patsflab__ats2plpre_stream_patsfun_28:
  $tmpret81 = ats2plpre_stream_map_cloref($env0, $arg0);
  return $tmpret81;
} #end-of-function


sub
ats2plpre_stream_filter_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret82;
##
  __patsflab_stream_filter_cloref:
  $tmpret82 = ATSPMVlazyval(_ats2plpre_stream_patsfun_30__closurerize($arg0, $arg1));
  return $tmpret82;
} #end-of-function


sub
_ats2plpre_stream_patsfun_30($$)
{
##
  my($env0, $env1) = @_;
##
  my $tmpret83;
  my $tmp84;
  my $tmp85;
  my $tmp86;
  my $tmp87;
  my $tmp88;
  my $tmp89;
##
  __patsflab__ats2plpre_stream_patsfun_30:
  $tmp84 = ATSPMVlazyval_eval($env0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab40:
    if(ATSCKptriscons($tmp84)) { goto __atstmplab43; }
    __atstmplab41:
    $tmpret83 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab42:
    __atstmplab43:
    $tmp85 = $tmp84->[0];
    $tmp86 = $tmp84->[1];
    $tmp87 = &{$env1->[0]}($env1, $tmp85);
    if($tmp87) {
      $tmp88 = ats2plpre_stream_filter_cloref($tmp86, $env1);
      $tmpret83 = [$tmp85, $tmp88];
    } else {
      $tmp89 = ats2plpre_stream_filter_cloref($tmp86, $env1);
      $tmpret83 = ATSPMVlazyval_eval($tmp89); 
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret83;
} #end-of-function


sub
ats2plpre_stream_filter_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret90;
##
  __patsflab_stream_filter_method:
  $tmpret90 = _ats2plpre_stream_patsfun_32__closurerize($arg0);
  return $tmpret90;
} #end-of-function


sub
_ats2plpre_stream_patsfun_32($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret91;
##
  __patsflab__ats2plpre_stream_patsfun_32:
  $tmpret91 = ats2plpre_stream_filter_cloref($env0, $arg0);
  return $tmpret91;
} #end-of-function


sub
ats2plpre_stream_forall_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret92;
  my $tmp93;
  my $tmp94;
  my $tmp95;
  my $tmp96;
##
  __patsflab_stream_forall_cloref:
  $tmp93 = ATSPMVlazyval_eval($arg0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab44:
    if(ATSCKptriscons($tmp93)) { goto __atstmplab47; }
    __atstmplab45:
    $tmpret92 = 1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab46:
    __atstmplab47:
    $tmp94 = $tmp93->[0];
    $tmp95 = $tmp93->[1];
    $tmp96 = &{$arg1->[0]}($arg1, $tmp94);
    if($tmp96) {
      #ATStailcalseq_beg
      $apy0 = $tmp95;
      $apy1 = $arg1;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab_stream_forall_cloref;
      #ATStailcalseq_end
    } else {
      $tmpret92 = 0;
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret92;
} #end-of-function


sub
ats2plpre_stream_forall_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret97;
##
  __patsflab_stream_forall_method:
  $tmpret97 = _ats2plpre_stream_patsfun_35__closurerize($arg0);
  return $tmpret97;
} #end-of-function


sub
_ats2plpre_stream_patsfun_35($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret98;
##
  __patsflab__ats2plpre_stream_patsfun_35:
  $tmpret98 = ats2plpre_stream_forall_cloref($env0, $arg0);
  return $tmpret98;
} #end-of-function


sub
ats2plpre_stream_exists_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret99;
  my $tmp100;
  my $tmp101;
  my $tmp102;
  my $tmp103;
##
  __patsflab_stream_exists_cloref:
  $tmp100 = ATSPMVlazyval_eval($arg0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab48:
    if(ATSCKptriscons($tmp100)) { goto __atstmplab51; }
    __atstmplab49:
    $tmpret99 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab50:
    __atstmplab51:
    $tmp101 = $tmp100->[0];
    $tmp102 = $tmp100->[1];
    $tmp103 = &{$arg1->[0]}($arg1, $tmp101);
    if($tmp103) {
      $tmpret99 = 1;
    } else {
      #ATStailcalseq_beg
      $apy0 = $tmp102;
      $apy1 = $arg1;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab_stream_exists_cloref;
      #ATStailcalseq_end
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret99;
} #end-of-function


sub
ats2plpre_stream_exists_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret104;
##
  __patsflab_stream_exists_method:
  $tmpret104 = _ats2plpre_stream_patsfun_38__closurerize($arg0);
  return $tmpret104;
} #end-of-function


sub
_ats2plpre_stream_patsfun_38($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret105;
##
  __patsflab__ats2plpre_stream_patsfun_38:
  $tmpret105 = ats2plpre_stream_exists_cloref($env0, $arg0);
  return $tmpret105;
} #end-of-function


sub
ats2plpre_stream_foreach_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmp107;
  my $tmp108;
  my $tmp109;
##
  __patsflab_stream_foreach_cloref:
  $tmp107 = ATSPMVlazyval_eval($arg0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab52:
    if(ATSCKptriscons($tmp107)) { goto __atstmplab55; }
    __atstmplab53:
    #ATSINSmove_void;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab54:
    __atstmplab55:
    $tmp108 = $tmp107->[0];
    $tmp109 = $tmp107->[1];
    &{$arg1->[0]}($arg1, $tmp108);
    #ATStailcalseq_beg
    $apy0 = $tmp109;
    $apy1 = $arg1;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab_stream_foreach_cloref;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return;#_void
} #end-of-function


sub
ats2plpre_stream_foreach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret111;
##
  __patsflab_stream_foreach_method:
  $tmpret111 = _ats2plpre_stream_patsfun_41__closurerize($arg0);
  return $tmpret111;
} #end-of-function


sub
_ats2plpre_stream_patsfun_41($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_stream_patsfun_41:
  ats2plpre_stream_foreach_cloref($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_stream_iforeach_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_stream_iforeach_cloref:
  _ats2plpre_stream_loop_43($arg1, 0, $arg0);
  return;#_void
} #end-of-function


sub
_ats2plpre_stream_loop_43($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmp115;
  my $tmp116;
  my $tmp117;
  my $tmp119;
##
  __patsflab__ats2plpre_stream_loop_43:
  $tmp115 = ATSPMVlazyval_eval($arg1); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab56:
    if(ATSCKptriscons($tmp115)) { goto __atstmplab59; }
    __atstmplab57:
    #ATSINSmove_void;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab58:
    __atstmplab59:
    $tmp116 = $tmp115->[0];
    $tmp117 = $tmp115->[1];
    &{$env0->[0]}($env0, $arg0, $tmp116);
    $tmp119 = ats2plpre_add_int1_int1($arg0, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp119;
    $apy1 = $tmp117;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_stream_loop_43;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return;#_void
} #end-of-function


sub
ats2plpre_stream_iforeach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret120;
##
  __patsflab_stream_iforeach_method:
  $tmpret120 = _ats2plpre_stream_patsfun_45__closurerize($arg0);
  return $tmpret120;
} #end-of-function


sub
_ats2plpre_stream_patsfun_45($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_stream_patsfun_45:
  ats2plpre_stream_iforeach_cloref($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_stream_tabulate_cloref($)
{
##
  my($arg0) = @_;
##
  my $tmpret122;
##
  __patsflab_stream_tabulate_cloref:
  $tmpret122 = _ats2plpre_stream_auxmain_47($arg0, 0);
  return $tmpret122;
} #end-of-function


sub
_ats2plpre_stream_auxmain_47($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret123;
##
  __patsflab__ats2plpre_stream_auxmain_47:
  $tmpret123 = ATSPMVlazyval(_ats2plpre_stream_patsfun_48__closurerize($env0, $arg0));
  return $tmpret123;
} #end-of-function


sub
_ats2plpre_stream_patsfun_48($$)
{
##
  my($env0, $env1) = @_;
##
  my $tmpret124;
  my $tmp125;
  my $tmp126;
  my $tmp127;
##
  __patsflab__ats2plpre_stream_patsfun_48:
  $tmp125 = &{$env0->[0]}($env0, $env1);
  $tmp127 = ats2plpre_add_int1_int1($env1, 1);
  $tmp126 = _ats2plpre_stream_auxmain_47($env0, $tmp127);
  $tmpret124 = [$tmp125, $tmp126];
  return $tmpret124;
} #end-of-function


sub
ats2plpre_cross_stream_list($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret128;
##
  __patsflab_cross_stream_list:
  $tmpret128 = ATSPMVlazyval(_ats2plpre_stream_patsfun_52__closurerize($arg0, $arg1));
  return $tmpret128;
} #end-of-function


sub
_ats2plpre_stream_auxmain_50($$$$)
{
##
  my($arg0, $arg1, $arg2, $arg3) = @_;
##
  my $tmpret129;
##
  __patsflab__ats2plpre_stream_auxmain_50:
  $tmpret129 = ATSPMVlazyval(_ats2plpre_stream_patsfun_51__closurerize($arg0, $arg1, $arg2, $arg3));
  return $tmpret129;
} #end-of-function


sub
_ats2plpre_stream_patsfun_51($$$$)
{
##
  my($env0, $env1, $env2, $env3) = @_;
##
  my $tmpret130;
  my $tmp131;
  my $tmp132;
  my $tmp133;
  my $tmp134;
  my $tmp135;
##
  __patsflab__ats2plpre_stream_patsfun_51:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab60:
    if(ATSCKptriscons($env3)) { goto __atstmplab63; }
    __atstmplab61:
    $tmp133 = ats2plpre_cross_stream_list($env1, $env2);
    $tmpret130 = ATSPMVlazyval_eval($tmp133); 
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab62:
    __atstmplab63:
    $tmp131 = $env3->[0];
    $tmp132 = $env3->[1];
    $tmp134 = [$env0, $tmp131];
    $tmp135 = _ats2plpre_stream_auxmain_50($env0, $env1, $env2, $tmp132);
    $tmpret130 = [$tmp134, $tmp135];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret130;
} #end-of-function


sub
_ats2plpre_stream_patsfun_52($$)
{
##
  my($env0, $env1) = @_;
##
  my $tmpret136;
  my $tmp137;
  my $tmp138;
  my $tmp139;
  my $tmp140;
##
  __patsflab__ats2plpre_stream_patsfun_52:
  $tmp137 = ATSPMVlazyval_eval($env0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab64:
    if(ATSCKptriscons($tmp137)) { goto __atstmplab67; }
    __atstmplab65:
    $tmpret136 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab66:
    if(ATSCKptrisnull($tmp137)) { ATSINScaseof_fail("/home/hwxi/Research/ATS-Postiats/contrib/libatscc/ATS2-0.3.2/DATS/stream.dats: 7014(line=463, offs=1) -- 7106(line=465, offs=50)"); }
    __atstmplab67:
    $tmp138 = $tmp137->[0];
    $tmp139 = $tmp137->[1];
    $tmp140 = _ats2plpre_stream_auxmain_50($tmp138, $tmp139, $env1, $env1);
    $tmpret136 = ATSPMVlazyval_eval($tmp140); 
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret136;
} #end-of-function


sub
ats2plpre_cross_stream_list0($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret141;
##
  __patsflab_cross_stream_list0:
  $tmpret141 = ats2plpre_cross_stream_list($arg0, $arg1);
  return $tmpret141;
} #end-of-function


sub
ats2plpre_stream2cloref_exn($)
{
##
  my($arg0) = @_;
##
  my $tmpret142;
  my $tmp143;
##
  __patsflab_stream2cloref_exn:
  $tmp143 = ats2plpre_ref($arg0);
  $tmpret142 = _ats2plpre_stream_patsfun_55__closurerize($tmp143);
  return $tmpret142;
} #end-of-function


sub
_ats2plpre_stream_patsfun_55($)
{
##
  my($env0) = @_;
##
  my $tmpret144;
  my $tmp145;
  my $tmp146;
  my $tmp147;
  my $tmp148;
##
  __patsflab__ats2plpre_stream_patsfun_55:
  $tmp145 = ats2plpre_ref_get_elt($env0);
  $tmp146 = ATSPMVlazyval_eval($tmp145); 
  if(ATSCKptrisnull($tmp146)) { ATSINScaseof_fail("/home/hwxi/Research/ATS-Postiats/contrib/libatscc/ATS2-0.3.2/DATS/stream.dats: 7407(line=491, offs=5) -- 7431(line=491, offs=29)"); }
  $tmp147 = $tmp146->[0];
  $tmp148 = $tmp146->[1];
  ats2plpre_ref_set_elt($env0, $tmp148);
  $tmpret144 = $tmp147;
  return $tmpret144;
} #end-of-function


sub
ats2plpre_stream2cloref_opt($)
{
##
  my($arg0) = @_;
##
  my $tmpret150;
  my $tmp151;
##
  __patsflab_stream2cloref_opt:
  $tmp151 = ats2plpre_ref($arg0);
  $tmpret150 = _ats2plpre_stream_patsfun_57__closurerize($tmp151);
  return $tmpret150;
} #end-of-function


sub
_ats2plpre_stream_patsfun_57($)
{
##
  my($env0) = @_;
##
  my $tmpret152;
  my $tmp153;
  my $tmp154;
  my $tmp155;
  my $tmp156;
##
  __patsflab__ats2plpre_stream_patsfun_57:
  $tmp153 = ats2plpre_ref_get_elt($env0);
  $tmp154 = ATSPMVlazyval_eval($tmp153); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab68:
    if(ATSCKptriscons($tmp154)) { goto __atstmplab71; }
    __atstmplab69:
    $tmpret152 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab70:
    __atstmplab71:
    $tmp155 = $tmp154->[0];
    $tmp156 = $tmp154->[1];
    ats2plpre_ref_set_elt($env0, $tmp156);
    $tmpret152 = [$tmp155];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret152;
} #end-of-function


sub
ats2plpre_stream2cloref_last($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret158;
  my $tmp159;
  my $tmp160;
##
  __patsflab_stream2cloref_last:
  $tmp159 = ats2plpre_ref($arg0);
  $tmp160 = ats2plpre_ref($arg1);
  $tmpret158 = _ats2plpre_stream_patsfun_59__closurerize($tmp159, $tmp160);
  return $tmpret158;
} #end-of-function


sub
_ats2plpre_stream_patsfun_59($$)
{
##
  my($env0, $env1) = @_;
##
  my $tmpret161;
  my $tmp162;
  my $tmp163;
  my $tmp164;
  my $tmp165;
##
  __patsflab__ats2plpre_stream_patsfun_59:
  $tmp162 = ats2plpre_ref_get_elt($env0);
  $tmp163 = ATSPMVlazyval_eval($tmp162); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab72:
    if(ATSCKptriscons($tmp163)) { goto __atstmplab75; }
    __atstmplab73:
    $tmpret161 = ats2plpre_ref_get_elt($env1);
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab74:
    __atstmplab75:
    $tmp164 = $tmp163->[0];
    $tmp165 = $tmp163->[1];
    ats2plpre_ref_set_elt($env0, $tmp165);
    ats2plpre_ref_set_elt($env1, $tmp164);
    $tmpret161 = $tmp164;
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret161;
} #end-of-function


sub
ats2plpre_stream_take_while_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret168;
  my $tmp169;
  my $tmp170;
  my $tmp171;
  my $tmp172;
##
  __patsflab_stream_take_while_cloref:
  $tmp169 = ats2plpre_stream_rtake_while_cloref($arg0, $arg1);
  $tmp170 = $tmp169->[0];
  $tmp171 = $tmp169->[1];
  $tmp172 = ats2plpre_list_reverse($tmp171);
  $tmpret168 = [$tmp170, $tmp172];
  return $tmpret168;
} #end-of-function


sub
ats2plpre_stream_rtake_while_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret173;
  my $tmp181;
##
  __patsflab_stream_rtake_while_cloref:
  $tmp181 = 0;
  $tmpret173 = _ats2plpre_stream_loop_62($arg1, $arg0, 0, $tmp181);
  return $tmpret173;
} #end-of-function


sub
_ats2plpre_stream_loop_62($$$$)
{
##
  my($env0, $arg0, $arg1, $arg2) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $tmpret174;
  my $tmp175;
  my $tmp176;
  my $tmp177;
  my $tmp178;
  my $tmp179;
  my $tmp180;
##
  __patsflab__ats2plpre_stream_loop_62:
  $tmp175 = ATSPMVlazyval_eval($arg0); 
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab76:
    if(ATSCKptriscons($tmp175)) { goto __atstmplab79; }
    __atstmplab77:
    $tmpret174 = [$arg0, $arg2];
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab78:
    __atstmplab79:
    $tmp176 = $tmp175->[0];
    $tmp177 = $tmp175->[1];
    $tmp178 = &{$env0->[0]}($env0, $arg1, $tmp176);
    if($tmp178) {
      $tmp179 = ats2plpre_add_int1_int1($arg1, 1);
      $tmp180 = [$tmp176, $arg2];
      #ATStailcalseq_beg
      $apy0 = $tmp177;
      $apy1 = $tmp179;
      $apy2 = $tmp180;
      $arg0 = $apy0;
      $arg1 = $apy1;
      $arg2 = $apy2;
      goto __patsflab__ats2plpre_stream_loop_62;
      #ATStailcalseq_end
    } else {
      $tmpret174 = [$arg0, $arg2];
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret174;
} #end-of-function


sub
ats2plpre_stream_take_until_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret182;
##
  __patsflab_stream_take_until_cloref:
  $tmpret182 = ats2plpre_stream_take_while_cloref($arg0, _ats2plpre_stream_patsfun_64__closurerize($arg1));
  return $tmpret182;
} #end-of-function


sub
_ats2plpre_stream_patsfun_64($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret183;
  my $tmp184;
##
  __patsflab__ats2plpre_stream_patsfun_64:
  $tmp184 = &{$env0->[0]}($env0, $arg0, $arg1);
  $tmpret183 = atspre_neg_bool0($tmp184);
  return $tmpret183;
} #end-of-function


sub
ats2plpre_stream_rtake_until_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret185;
##
  __patsflab_stream_rtake_until_cloref:
  $tmpret185 = ats2plpre_stream_rtake_while_cloref($arg0, _ats2plpre_stream_patsfun_66__closurerize($arg1));
  return $tmpret185;
} #end-of-function


sub
_ats2plpre_stream_patsfun_66($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret186;
  my $tmp187;
##
  __patsflab__ats2plpre_stream_patsfun_66:
  $tmp187 = &{$env0->[0]}($env0, $arg0, $arg1);
  $tmpret186 = atspre_neg_bool0($tmp187);
  return $tmpret186;
} #end-of-function


sub
ats2plpre_stream_list_xprod2($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret188;
##
  __patsflab_stream_list_xprod2:
  $tmpret188 = _ats2plpre_stream_auxlst_70($arg0, $arg1);
  return $tmpret188;
} #end-of-function


sub
_ats2plpre_stream_aux_68($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret189;
##
  __patsflab__ats2plpre_stream_aux_68:
  $tmpret189 = ATSPMVlazyval(_ats2plpre_stream_patsfun_69__closurerize($arg0, $arg1));
  return $tmpret189;
} #end-of-function


sub
_ats2plpre_stream_patsfun_69($$)
{
##
  my($env0, $env1) = @_;
##
  my $tmpret190;
  my $tmp191;
  my $tmp192;
  my $tmp193;
  my $tmp194;
##
  __patsflab__ats2plpre_stream_patsfun_69:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab80:
    if(ATSCKptriscons($env1)) { goto __atstmplab83; }
    __atstmplab81:
    $tmpret190 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab82:
    __atstmplab83:
    $tmp191 = $env1->[0];
    $tmp192 = $env1->[1];
    $tmp193 = [$env0, $tmp191];
    $tmp194 = _ats2plpre_stream_aux_68($env0, $tmp192);
    $tmpret190 = [$tmp193, $tmp194];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret190;
} #end-of-function


sub
_ats2plpre_stream_auxlst_70($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret195;
##
  __patsflab__ats2plpre_stream_auxlst_70:
  $tmpret195 = ATSPMVlazyval(_ats2plpre_stream_patsfun_71__closurerize($arg0, $arg1));
  return $tmpret195;
} #end-of-function


sub
_ats2plpre_stream_patsfun_71($$)
{
##
  my($env0, $env1) = @_;
##
  my $tmpret196;
  my $tmp197;
  my $tmp198;
  my $tmp199;
  my $tmp200;
  my $tmp201;
##
  __patsflab__ats2plpre_stream_patsfun_71:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab84:
    if(ATSCKptriscons($env0)) { goto __atstmplab87; }
    __atstmplab85:
    $tmpret196 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab86:
    __atstmplab87:
    $tmp197 = $env0->[0];
    $tmp198 = $env0->[1];
    $tmp200 = _ats2plpre_stream_aux_68($tmp197, $env1);
    $tmp201 = _ats2plpre_stream_auxlst_70($tmp198, $env1);
    $tmp199 = ats2plpre_stream_append($tmp200, $tmp201);
    $tmpret196 = ATSPMVlazyval_eval($tmp199); 
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret196;
} #end-of-function


######
#ATSextcode_beg()
######
######
1; ##note that it is needed by 'use' or 'require'
######
######
#ATSextcode_end()
######
######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######
sub
_ats2plpre_stream_vt_patsfun_10__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_stream_vt_patsfun_10($cenv->[1]); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_13__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_13($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_stream_vt_patsfun_21__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_21($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_stream_vt_patsfun_24__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_24($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_27__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_27($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_stream_vt_patsfun_29__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_29($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_32__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_32($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_stream_vt_patsfun_34__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_34($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_37__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_37($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}

sub
_ats2plpre_stream_vt_patsfun_39__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_39($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_43__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_43($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_47__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_47($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_51__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_51($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_55__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_55($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_59__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_59($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_stream_vt_patsfun_62__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_stream_vt_patsfun_62($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}


sub
ats2plpre_stream_vt_free($)
{
##
  my($arg0) = @_;
##
##
  __patsflab_stream_vt_free:
  atspre_lazy_vt_free($arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_stream_vt2t($)
{
##
  my($arg0) = @_;
##
  my $tmpret11;
##
  __patsflab_stream_vt2t:
  $tmpret11 = _ats2plpre_stream_vt_aux_9($arg0);
  return $tmpret11;
} #end-of-function


sub
_ats2plpre_stream_vt_aux_9($)
{
##
  my($arg0) = @_;
##
  my $tmpret12;
##
  __patsflab__ats2plpre_stream_vt_aux_9:
  $tmpret12 = ATSPMVlazyval(_ats2plpre_stream_vt_patsfun_10__closurerize($arg0));
  return $tmpret12;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_10($)
{
##
  my($env0) = @_;
##
  my $tmpret13;
  my $tmp14;
  my $tmp15;
  my $tmp16;
  my $tmp17;
##
  __patsflab__ats2plpre_stream_vt_patsfun_10:
  $tmp14 = ATSPMVllazyval_eval($env0);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab0:
    if(ATSCKptriscons($tmp14)) { goto __atstmplab3; }
    __atstmplab1:
    $tmpret13 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab2:
    __atstmplab3:
    $tmp15 = $tmp14->[0];
    $tmp16 = $tmp14->[1];
    #ATSINSfreecon($tmp14);
    $tmp17 = _ats2plpre_stream_vt_aux_9($tmp16);
    $tmpret13 = [$tmp15, $tmp17];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret13;
} #end-of-function


sub
ats2plpre_stream_vt_takeLte($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret18;
##
  __patsflab_stream_vt_takeLte:
  $tmpret18 = _ats2plpre_stream_vt_auxmain_12($arg0, $arg1);
  return $tmpret18;
} #end-of-function


sub
_ats2plpre_stream_vt_auxmain_12($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret19;
##
  __patsflab__ats2plpre_stream_vt_auxmain_12:
  $tmpret19 = ATSPMVllazyval(_ats2plpre_stream_vt_patsfun_13__closurerize($arg0, $arg1));
  return $tmpret19;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_13($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret20;
  my $tmp21;
  my $tmp22;
  my $tmp23;
  my $tmp24;
  my $tmp25;
  my $tmp26;
##
  __patsflab__ats2plpre_stream_vt_patsfun_13:
  if($arg0) {
    $tmp21 = ats2plpre_gt_int1_int1($env1, 0);
    if($tmp21) {
      $tmp22 = ATSPMVllazyval_eval($env0);
      #ATScaseofseq_beg
      while(1)
      {
        #ATSbranchseq_beg
        __atstmplab4:
        if(ATSCKptriscons($tmp22)) { goto __atstmplab7; }
        __atstmplab5:
        $tmpret20 = 0;
        last;
        #ATSbranchseq_end
        #ATSbranchseq_beg
        __atstmplab6:
        __atstmplab7:
        $tmp23 = $tmp22->[0];
        $tmp24 = $tmp22->[1];
        #ATSINSfreecon($tmp22);
        $tmp26 = ats2plpre_sub_int1_int1($env1, 1);
        $tmp25 = _ats2plpre_stream_vt_auxmain_12($tmp24, $tmp26);
        $tmpret20 = [$tmp23, $tmp25];
        last;
        #ATSbranchseq_end
      } #end-of-while-loop;
      #ATScaseofseq_end
    } else {
      atspre_lazy_vt_free($env0);
      $tmpret20 = 0;
    } #endif
  } else {
    atspre_lazy_vt_free($env0);
  } #endif
  return $tmpret20;
} #end-of-function


sub
ats2plpre_stream_vt_length($)
{
##
  my($arg0) = @_;
##
  my $tmpret29;
##
  __patsflab_stream_vt_length:
  $tmpret29 = _ats2plpre_stream_vt_loop_15($arg0, 0);
  return $tmpret29;
} #end-of-function


sub
_ats2plpre_stream_vt_loop_15($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret30;
  my $tmp31;
  my $tmp33;
  my $tmp34;
##
  __patsflab__ats2plpre_stream_vt_loop_15:
  $tmp31 = ATSPMVllazyval_eval($arg0);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab8:
    if(ATSCKptriscons($tmp31)) { goto __atstmplab11; }
    __atstmplab9:
    $tmpret30 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab10:
    __atstmplab11:
    $tmp33 = $tmp31->[1];
    #ATSINSfreecon($tmp31);
    $tmp34 = ats2plpre_add_int1_int1($arg1, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp33;
    $apy1 = $tmp34;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_stream_vt_loop_15;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret30;
} #end-of-function


sub
ats2plpre_stream2list_vt($)
{
##
  my($arg0) = @_;
##
  my $tmpret35;
  my $tmp36;
##
  __patsflab_stream2list_vt:
  $tmp36 = ats2plpre_stream2list_vt_rev($arg0);
  $tmpret35 = ats2plpre_list_vt_reverse($tmp36);
  return $tmpret35;
} #end-of-function


sub
ats2plpre_stream2list_vt_rev($)
{
##
  my($arg0) = @_;
##
  my $tmpret37;
  my $tmp43;
##
  __patsflab_stream2list_vt_rev:
  $tmp43 = 0;
  $tmpret37 = _ats2plpre_stream_vt_loop_18($arg0, $tmp43);
  return $tmpret37;
} #end-of-function


sub
_ats2plpre_stream_vt_loop_18($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret38;
  my $tmp39;
  my $tmp40;
  my $tmp41;
  my $tmp42;
##
  __patsflab__ats2plpre_stream_vt_loop_18:
  $tmp39 = ATSPMVllazyval_eval($arg0);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab12:
    if(ATSCKptriscons($tmp39)) { goto __atstmplab15; }
    __atstmplab13:
    $tmpret38 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab14:
    __atstmplab15:
    $tmp40 = $tmp39->[0];
    $tmp41 = $tmp39->[1];
    #ATSINSfreecon($tmp39);
    $tmp42 = [$tmp40, $arg1];
    #ATStailcalseq_beg
    $apy0 = $tmp41;
    $apy1 = $tmp42;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_stream_vt_loop_18;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret38;
} #end-of-function


sub
ats2plpre_stream_vt_append($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret44;
##
  __patsflab_stream_vt_append:
  $tmpret44 = _ats2plpre_stream_vt_auxmain_20($arg0, $arg1);
  return $tmpret44;
} #end-of-function


sub
_ats2plpre_stream_vt_auxmain_20($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret45;
##
  __patsflab__ats2plpre_stream_vt_auxmain_20:
  $tmpret45 = ATSPMVllazyval(_ats2plpre_stream_vt_patsfun_21__closurerize($arg0, $arg1));
  return $tmpret45;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_21($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret46;
  my $tmp47;
  my $tmp48;
  my $tmp49;
  my $tmp50;
##
  __patsflab__ats2plpre_stream_vt_patsfun_21:
  if($arg0) {
    $tmp47 = ATSPMVllazyval_eval($env0);
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab16:
      if(ATSCKptriscons($tmp47)) { goto __atstmplab19; }
      __atstmplab17:
      $tmpret46 = ATSPMVllazyval_eval($env1);
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab18:
      __atstmplab19:
      $tmp48 = $tmp47->[0];
      $tmp49 = $tmp47->[1];
      #ATSINSfreecon($tmp47);
      $tmp50 = _ats2plpre_stream_vt_auxmain_20($tmp49, $env1);
      $tmpret46 = [$tmp48, $tmp50];
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
    atspre_lazy_vt_free($env0);
    atspre_lazy_vt_free($env1);
  } #endif
  return $tmpret46;
} #end-of-function


sub
ats2plpre_stream_vt_concat($)
{
##
  my($arg0) = @_;
##
  my $tmpret53;
##
  __patsflab_stream_vt_concat:
  $tmpret53 = _ats2plpre_stream_vt_auxmain_23($arg0);
  return $tmpret53;
} #end-of-function


sub
_ats2plpre_stream_vt_auxmain_23($)
{
##
  my($arg0) = @_;
##
  my $tmpret54;
##
  __patsflab__ats2plpre_stream_vt_auxmain_23:
  $tmpret54 = ATSPMVllazyval(_ats2plpre_stream_vt_patsfun_24__closurerize($arg0));
  return $tmpret54;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_24($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret55;
  my $tmp56;
  my $tmp57;
  my $tmp58;
  my $tmp59;
  my $tmp60;
##
  __patsflab__ats2plpre_stream_vt_patsfun_24:
  if($arg0) {
    $tmp56 = ATSPMVllazyval_eval($env0);
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab20:
      if(ATSCKptriscons($tmp56)) { goto __atstmplab23; }
      __atstmplab21:
      $tmpret55 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab22:
      __atstmplab23:
      $tmp57 = $tmp56->[0];
      $tmp58 = $tmp56->[1];
      #ATSINSfreecon($tmp56);
      $tmp60 = _ats2plpre_stream_vt_auxmain_23($tmp58);
      $tmp59 = ats2plpre_stream_vt_append($tmp57, $tmp60);
      $tmpret55 = ATSPMVllazyval_eval($tmp59);
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
    atspre_lazy_vt_free($env0);
  } #endif
  return $tmpret55;
} #end-of-function


sub
ats2plpre_stream_vt_map_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret62;
##
  __patsflab_stream_vt_map_cloref:
  $tmpret62 = _ats2plpre_stream_vt_auxmain_26($arg1, $arg0);
  return $tmpret62;
} #end-of-function


sub
_ats2plpre_stream_vt_auxmain_26($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret63;
##
  __patsflab__ats2plpre_stream_vt_auxmain_26:
  $tmpret63 = ATSPMVllazyval(_ats2plpre_stream_vt_patsfun_27__closurerize($env0, $arg0));
  return $tmpret63;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_27($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret64;
  my $tmp65;
  my $tmp66;
  my $tmp67;
  my $tmp68;
  my $tmp69;
##
  __patsflab__ats2plpre_stream_vt_patsfun_27:
  if($arg0) {
    $tmp65 = ATSPMVllazyval_eval($env1);
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab24:
      if(ATSCKptriscons($tmp65)) { goto __atstmplab27; }
      __atstmplab25:
      $tmpret64 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab26:
      __atstmplab27:
      $tmp66 = $tmp65->[0];
      $tmp67 = $tmp65->[1];
      #ATSINSfreecon($tmp65);
      $tmp68 = &{$env0->[0]}($env0, $tmp66);
      $tmp69 = _ats2plpre_stream_vt_auxmain_26($env0, $tmp67);
      $tmpret64 = [$tmp68, $tmp69];
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
    atspre_lazy_vt_free($env1);
  } #endif
  return $tmpret64;
} #end-of-function


sub
ats2plpre_stream_vt_map_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret71;
##
  __patsflab_stream_vt_map_method:
  $tmpret71 = _ats2plpre_stream_vt_patsfun_29__closurerize($arg0);
  return $tmpret71;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_29($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret72;
##
  __patsflab__ats2plpre_stream_vt_patsfun_29:
  $tmpret72 = ats2plpre_stream_vt_map_cloref($env0, $arg0);
  return $tmpret72;
} #end-of-function


sub
ats2plpre_stream_vt_mapopt_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret73;
##
  __patsflab_stream_vt_mapopt_cloref:
  $tmpret73 = _ats2plpre_stream_vt_auxmain_31($arg1, $arg0);
  return $tmpret73;
} #end-of-function


sub
_ats2plpre_stream_vt_auxmain_31($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret74;
##
  __patsflab__ats2plpre_stream_vt_auxmain_31:
  $tmpret74 = ATSPMVllazyval(_ats2plpre_stream_vt_patsfun_32__closurerize($env0, $arg0));
  return $tmpret74;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_32($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret75;
  my $tmp76;
  my $tmp77;
  my $tmp78;
  my $tmp79;
  my $tmp80;
  my $tmp81;
  my $tmp82;
##
  __patsflab__ats2plpre_stream_vt_patsfun_32:
  if($arg0) {
    $tmp76 = ATSPMVllazyval_eval($env1);
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab28:
      if(ATSCKptriscons($tmp76)) { goto __atstmplab31; }
      __atstmplab29:
      $tmpret75 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab30:
      __atstmplab31:
      $tmp77 = $tmp76->[0];
      $tmp78 = $tmp76->[1];
      #ATSINSfreecon($tmp76);
      $tmp79 = &{$env0->[0]}($env0, $tmp77);
      #ATScaseofseq_beg
      while(1)
      {
        #ATSbranchseq_beg
        __atstmplab32:
        if(ATSCKptriscons($tmp79)) { goto __atstmplab35; }
        __atstmplab33:
        $tmp81 = _ats2plpre_stream_vt_auxmain_31($env0, $tmp78);
        $tmpret75 = ATSPMVllazyval_eval($tmp81);
        last;
        #ATSbranchseq_end
        #ATSbranchseq_beg
        __atstmplab34:
        __atstmplab35:
        $tmp80 = $tmp79->[0];
        #ATSINSfreecon($tmp79);
        $tmp82 = _ats2plpre_stream_vt_auxmain_31($env0, $tmp78);
        $tmpret75 = [$tmp80, $tmp82];
        last;
        #ATSbranchseq_end
      } #end-of-while-loop;
      #ATScaseofseq_end
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
    atspre_lazy_vt_free($env1);
  } #endif
  return $tmpret75;
} #end-of-function


sub
ats2plpre_stream_vt_mapopt_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret84;
##
  __patsflab_stream_vt_mapopt_method:
  $tmpret84 = _ats2plpre_stream_vt_patsfun_34__closurerize($arg0);
  return $tmpret84;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_34($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret85;
##
  __patsflab__ats2plpre_stream_vt_patsfun_34:
  $tmpret85 = ats2plpre_stream_vt_mapopt_cloref($env0, $arg0);
  return $tmpret85;
} #end-of-function


sub
ats2plpre_stream_vt_filter_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret86;
##
  __patsflab_stream_vt_filter_cloref:
  $tmpret86 = _ats2plpre_stream_vt_auxmain_36($arg1, $arg0);
  return $tmpret86;
} #end-of-function


sub
_ats2plpre_stream_vt_auxmain_36($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret87;
##
  __patsflab__ats2plpre_stream_vt_auxmain_36:
  $tmpret87 = ATSPMVllazyval(_ats2plpre_stream_vt_patsfun_37__closurerize($env0, $arg0));
  return $tmpret87;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_37($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret88;
  my $tmp89;
  my $tmp90;
  my $tmp91;
  my $tmp92;
  my $tmp93;
  my $tmp94;
##
  __patsflab__ats2plpre_stream_vt_patsfun_37:
  if($arg0) {
    $tmp89 = ATSPMVllazyval_eval($env1);
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab36:
      if(ATSCKptriscons($tmp89)) { goto __atstmplab39; }
      __atstmplab37:
      $tmpret88 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab38:
      __atstmplab39:
      $tmp90 = $tmp89->[0];
      $tmp91 = $tmp89->[1];
      #ATSINSfreecon($tmp89);
      $tmp92 = &{$env0->[0]}($env0, $tmp90);
      if($tmp92) {
        $tmp93 = _ats2plpre_stream_vt_auxmain_36($env0, $tmp91);
        $tmpret88 = [$tmp90, $tmp93];
      } else {
        $tmp94 = _ats2plpre_stream_vt_auxmain_36($env0, $tmp91);
        $tmpret88 = ATSPMVllazyval_eval($tmp94);
      } #endif
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
  } else {
    atspre_lazy_vt_free($env1);
  } #endif
  return $tmpret88;
} #end-of-function


sub
ats2plpre_stream_vt_filter_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret96;
##
  __patsflab_stream_vt_filter_method:
  $tmpret96 = _ats2plpre_stream_vt_patsfun_39__closurerize($arg0);
  return $tmpret96;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_39($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret97;
##
  __patsflab__ats2plpre_stream_vt_patsfun_39:
  $tmpret97 = ats2plpre_stream_vt_filter_cloref($env0, $arg0);
  return $tmpret97;
} #end-of-function


sub
ats2plpre_stream_vt_exists_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret98;
##
  __patsflab_stream_vt_exists_cloref:
  $tmpret98 = _ats2plpre_stream_vt_loop_41($arg1, $arg0);
  return $tmpret98;
} #end-of-function


sub
_ats2plpre_stream_vt_loop_41($$)
{
##
  my($env0, $arg0) = @_;
##
  my $apy0;
  my $tmpret99;
  my $tmp100;
  my $tmp101;
  my $tmp102;
  my $tmp103;
##
  __patsflab__ats2plpre_stream_vt_loop_41:
  $tmp100 = ATSPMVllazyval_eval($arg0);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab40:
    if(ATSCKptriscons($tmp100)) { goto __atstmplab43; }
    __atstmplab41:
    $tmpret99 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab42:
    __atstmplab43:
    $tmp101 = $tmp100->[0];
    $tmp102 = $tmp100->[1];
    #ATSINSfreecon($tmp100);
    $tmp103 = &{$env0->[0]}($env0, $tmp101);
    if($tmp103) {
      atspre_lazy_vt_free($tmp102);
      $tmpret99 = 1;
    } else {
      #ATStailcalseq_beg
      $apy0 = $tmp102;
      $arg0 = $apy0;
      goto __patsflab__ats2plpre_stream_vt_loop_41;
      #ATStailcalseq_end
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret99;
} #end-of-function


sub
ats2plpre_stream_vt_exists_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret105;
##
  __patsflab_stream_vt_exists_method:
  $tmpret105 = _ats2plpre_stream_vt_patsfun_43__closurerize($arg0);
  return $tmpret105;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_43($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret106;
##
  __patsflab__ats2plpre_stream_vt_patsfun_43:
  $tmpret106 = ats2plpre_stream_vt_exists_cloref($env0, $arg0);
  return $tmpret106;
} #end-of-function


sub
ats2plpre_stream_vt_forall_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret107;
##
  __patsflab_stream_vt_forall_cloref:
  $tmpret107 = _ats2plpre_stream_vt_loop_45($arg1, $arg0);
  return $tmpret107;
} #end-of-function


sub
_ats2plpre_stream_vt_loop_45($$)
{
##
  my($env0, $arg0) = @_;
##
  my $apy0;
  my $tmpret108;
  my $tmp109;
  my $tmp110;
  my $tmp111;
  my $tmp112;
##
  __patsflab__ats2plpre_stream_vt_loop_45:
  $tmp109 = ATSPMVllazyval_eval($arg0);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab44:
    if(ATSCKptriscons($tmp109)) { goto __atstmplab47; }
    __atstmplab45:
    $tmpret108 = 1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab46:
    __atstmplab47:
    $tmp110 = $tmp109->[0];
    $tmp111 = $tmp109->[1];
    #ATSINSfreecon($tmp109);
    $tmp112 = &{$env0->[0]}($env0, $tmp110);
    if($tmp112) {
      #ATStailcalseq_beg
      $apy0 = $tmp111;
      $arg0 = $apy0;
      goto __patsflab__ats2plpre_stream_vt_loop_45;
      #ATStailcalseq_end
    } else {
      atspre_lazy_vt_free($tmp111);
      $tmpret108 = 0;
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret108;
} #end-of-function


sub
ats2plpre_stream_vt_forall_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret114;
##
  __patsflab_stream_vt_forall_method:
  $tmpret114 = _ats2plpre_stream_vt_patsfun_47__closurerize($arg0);
  return $tmpret114;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_47($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret115;
##
  __patsflab__ats2plpre_stream_vt_patsfun_47:
  $tmpret115 = ats2plpre_stream_vt_forall_cloref($env0, $arg0);
  return $tmpret115;
} #end-of-function


sub
ats2plpre_stream_vt_foreach_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_stream_vt_foreach_cloref:
  _ats2plpre_stream_vt_loop_49($arg1, $arg0);
  return;#_void
} #end-of-function


sub
_ats2plpre_stream_vt_loop_49($$)
{
##
  my($env0, $arg0) = @_;
##
  my $apy0;
  my $tmp118;
  my $tmp119;
  my $tmp120;
##
  __patsflab__ats2plpre_stream_vt_loop_49:
  $tmp118 = ATSPMVllazyval_eval($arg0);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab48:
    if(ATSCKptriscons($tmp118)) { goto __atstmplab51; }
    __atstmplab49:
    #ATSINSmove_void;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab50:
    __atstmplab51:
    $tmp119 = $tmp118->[0];
    $tmp120 = $tmp118->[1];
    #ATSINSfreecon($tmp118);
    &{$env0->[0]}($env0, $tmp119);
    #ATStailcalseq_beg
    $apy0 = $tmp120;
    $arg0 = $apy0;
    goto __patsflab__ats2plpre_stream_vt_loop_49;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return;#_void
} #end-of-function


sub
ats2plpre_stream_vt_foreach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret122;
##
  __patsflab_stream_vt_foreach_method:
  $tmpret122 = _ats2plpre_stream_vt_patsfun_51__closurerize($arg0);
  return $tmpret122;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_51($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_stream_vt_patsfun_51:
  ats2plpre_stream_vt_foreach_cloref($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_stream_vt_iforeach_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_stream_vt_iforeach_cloref:
  _ats2plpre_stream_vt_loop_53($arg1, 0, $arg0);
  return;#_void
} #end-of-function


sub
_ats2plpre_stream_vt_loop_53($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmp126;
  my $tmp127;
  my $tmp128;
  my $tmp130;
##
  __patsflab__ats2plpre_stream_vt_loop_53:
  $tmp126 = ATSPMVllazyval_eval($arg1);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab52:
    if(ATSCKptriscons($tmp126)) { goto __atstmplab55; }
    __atstmplab53:
    #ATSINSmove_void;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab54:
    __atstmplab55:
    $tmp127 = $tmp126->[0];
    $tmp128 = $tmp126->[1];
    #ATSINSfreecon($tmp126);
    &{$env0->[0]}($env0, $arg0, $tmp127);
    $tmp130 = ats2plpre_add_int1_int1($arg0, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp130;
    $apy1 = $tmp128;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_stream_vt_loop_53;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return;#_void
} #end-of-function


sub
ats2plpre_stream_vt_iforeach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret131;
##
  __patsflab_stream_vt_iforeach_method:
  $tmpret131 = _ats2plpre_stream_vt_patsfun_55__closurerize($arg0);
  return $tmpret131;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_55($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_stream_vt_patsfun_55:
  ats2plpre_stream_vt_iforeach_cloref($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_stream_vt_rforeach_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_stream_vt_rforeach_cloref:
  _ats2plpre_stream_vt_auxmain_57($arg1, $arg0);
  return;#_void
} #end-of-function


sub
_ats2plpre_stream_vt_auxmain_57($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmp135;
  my $tmp136;
  my $tmp137;
##
  __patsflab__ats2plpre_stream_vt_auxmain_57:
  $tmp135 = ATSPMVllazyval_eval($arg0);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab56:
    if(ATSCKptriscons($tmp135)) { goto __atstmplab59; }
    __atstmplab57:
    #ATSINSmove_void;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab58:
    __atstmplab59:
    $tmp136 = $tmp135->[0];
    $tmp137 = $tmp135->[1];
    #ATSINSfreecon($tmp135);
    _ats2plpre_stream_vt_auxmain_57($env0, $tmp137);
    &{$env0->[0]}($env0, $tmp136);
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return;#_void
} #end-of-function


sub
ats2plpre_stream_vt_rforeach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret139;
##
  __patsflab_stream_vt_rforeach_method:
  $tmpret139 = _ats2plpre_stream_vt_patsfun_59__closurerize($arg0);
  return $tmpret139;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_59($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_stream_vt_patsfun_59:
  ats2plpre_stream_vt_rforeach_cloref($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_stream_vt_tabulate_cloref($)
{
##
  my($arg0) = @_;
##
  my $tmpret141;
##
  __patsflab_stream_vt_tabulate_cloref:
  $tmpret141 = _ats2plpre_stream_vt_auxmain_61($arg0, 0);
  return $tmpret141;
} #end-of-function


sub
_ats2plpre_stream_vt_auxmain_61($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret142;
##
  __patsflab__ats2plpre_stream_vt_auxmain_61:
  $tmpret142 = ATSPMVllazyval(_ats2plpre_stream_vt_patsfun_62__closurerize($env0, $arg0));
  return $tmpret142;
} #end-of-function


sub
_ats2plpre_stream_vt_patsfun_62($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret143;
  my $tmp144;
  my $tmp145;
  my $tmp146;
##
  __patsflab__ats2plpre_stream_vt_patsfun_62:
  if($arg0) {
    $tmp144 = &{$env0->[0]}($env0, $env1);
    $tmp146 = ats2plpre_add_int1_int1($env1, 1);
    $tmp145 = _ats2plpre_stream_vt_auxmain_61($env0, $tmp146);
    $tmpret143 = [$tmp144, $tmp145];
  } else {
  } #endif
  return $tmpret143;
} #end-of-function


######
#ATSextcode_beg()
######
######
1; ##note that it is needed by 'use' or 'require'
######
######
#ATSextcode_end()
######
######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######
sub
_ats2plpre_intrange_patsfun_4__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_4($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_8__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_8($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_10__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_10($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_14__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_14($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_17__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0, $arg1) = @_; return _ats2plpre_intrange_patsfun_17($cenv->[1], $arg0, $arg1); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_20__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0, $arg1) = @_; return _ats2plpre_intrange_patsfun_20($cenv->[1], $arg0, $arg1); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_24__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_24($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_27__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_27($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_30__closurerize($$$)
{
  my($env0, $env1, $env2) = @_;
  return [sub{ my($cenv) = @_; return _ats2plpre_intrange_patsfun_30($cenv->[1], $cenv->[2], $cenv->[3]); }, $env0, $env1, $env2];
}

sub
_ats2plpre_intrange_patsfun_32__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_32($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_35__closurerize($$$)
{
  my($env0, $env1, $env2) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_35($cenv->[1], $cenv->[2], $cenv->[3], $arg0); }, $env0, $env1, $env2];
}

sub
_ats2plpre_intrange_patsfun_37__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_37($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_44__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_44($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_48__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_48($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_52__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_52($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_56__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_intrange_patsfun_56($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_intrange_patsfun_60__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0, $arg1) = @_; return _ats2plpre_intrange_patsfun_60($cenv->[1], $cenv->[2], $arg0, $arg1); }, $env0, $env1];
}

sub
_ats2plpre_intrange_patsfun_64__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0, $arg1) = @_; return _ats2plpre_intrange_patsfun_64($cenv->[1], $cenv->[2], $arg0, $arg1); }, $env0, $env1];
}


sub
ats2plpre_int_repeat_lazy($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmp1;
##
  __patsflab_int_repeat_lazy:
  $tmp1 = ats2plpre_lazy2cloref($arg1);
  ats2plpre_int_repeat_cloref($arg0, $tmp1);
  return;#_void
} #end-of-function


sub
ats2plpre_int_repeat_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_int_repeat_cloref:
  _ats2plpre_intrange_loop_2($arg0, $arg1);
  return;#_void
} #end-of-function


sub
_ats2plpre_intrange_loop_2($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmp4;
  my $tmp6;
##
  __patsflab__ats2plpre_intrange_loop_2:
  $tmp4 = ats2plpre_gt_int0_int0($arg0, 0);
  if($tmp4) {
    &{$arg1->[0]}($arg1);
    $tmp6 = ats2plpre_sub_int0_int0($arg0, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp6;
    $apy1 = $arg1;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_intrange_loop_2;
    #ATStailcalseq_end
  } else {
    #ATSINSmove_void;
  } #endif
  return;#_void
} #end-of-function


sub
ats2plpre_int_repeat_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret7;
##
  __patsflab_int_repeat_method:
  $tmpret7 = _ats2plpre_intrange_patsfun_4__closurerize($arg0);
  return $tmpret7;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_4($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_intrange_patsfun_4:
  ats2plpre_int_repeat_cloref($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_int_exists_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret9;
##
  __patsflab_int_exists_cloref:
  $tmpret9 = ats2plpre_intrange_exists_cloref(0, $arg0, $arg1);
  return $tmpret9;
} #end-of-function


sub
ats2plpre_int_forall_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret10;
##
  __patsflab_int_forall_cloref:
  $tmpret10 = ats2plpre_intrange_forall_cloref(0, $arg0, $arg1);
  return $tmpret10;
} #end-of-function


sub
ats2plpre_int_exists_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret11;
##
  __patsflab_int_exists_method:
  $tmpret11 = _ats2plpre_intrange_patsfun_8__closurerize($arg0);
  return $tmpret11;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_8($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret12;
##
  __patsflab__ats2plpre_intrange_patsfun_8:
  $tmpret12 = ats2plpre_int_exists_cloref($env0, $arg0);
  return $tmpret12;
} #end-of-function


sub
ats2plpre_int_forall_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret13;
##
  __patsflab_int_forall_method:
  $tmpret13 = _ats2plpre_intrange_patsfun_10__closurerize($arg0);
  return $tmpret13;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_10($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret14;
##
  __patsflab__ats2plpre_intrange_patsfun_10:
  $tmpret14 = ats2plpre_int_forall_cloref($env0, $arg0);
  return $tmpret14;
} #end-of-function


sub
ats2plpre_int_foreach_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_int_foreach_cloref:
  ats2plpre_intrange_foreach_cloref(0, $arg0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_int_rforeach_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_int_rforeach_cloref:
  ats2plpre_intrange_rforeach_cloref(0, $arg0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_int_foreach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret17;
##
  __patsflab_int_foreach_method:
  $tmpret17 = _ats2plpre_intrange_patsfun_14__closurerize($arg0);
  return $tmpret17;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_14($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_intrange_patsfun_14:
  ats2plpre_int_foreach_cloref($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_int_foldleft_cloref($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret19;
##
  __patsflab_int_foldleft_cloref:
  $tmpret19 = ats2plpre_intrange_foldleft_cloref(0, $arg0, $arg1, $arg2);
  return $tmpret19;
} #end-of-function


sub
ats2plpre_int_foldleft_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret20;
##
  __patsflab_int_foldleft_method:
  $tmpret20 = _ats2plpre_intrange_patsfun_17__closurerize($arg0);
  return $tmpret20;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_17($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret21;
##
  __patsflab__ats2plpre_intrange_patsfun_17:
  $tmpret21 = ats2plpre_int_foldleft_cloref($env0, $arg0, $arg1);
  return $tmpret21;
} #end-of-function


sub
ats2plpre_int_foldright_cloref($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret22;
##
  __patsflab_int_foldright_cloref:
  $tmpret22 = ats2plpre_intrange_foldright_cloref(0, $arg0, $arg1, $arg2);
  return $tmpret22;
} #end-of-function


sub
ats2plpre_int_foldright_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret23;
##
  __patsflab_int_foldright_method:
  $tmpret23 = _ats2plpre_intrange_patsfun_20__closurerize($arg0);
  return $tmpret23;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_20($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret24;
##
  __patsflab__ats2plpre_intrange_patsfun_20:
  $tmpret24 = ats2plpre_int_foldright_cloref($env0, $arg0, $arg1);
  return $tmpret24;
} #end-of-function


sub
ats2plpre_int_list_map_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret25;
##
  __patsflab_int_list_map_cloref:
  $tmpret25 = _ats2plpre_intrange_aux_22($arg0, $arg1, 0);
  return $tmpret25;
} #end-of-function


sub
_ats2plpre_intrange_aux_22($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret26;
  my $tmp27;
  my $tmp28;
  my $tmp29;
  my $tmp30;
##
  __patsflab__ats2plpre_intrange_aux_22:
  $tmp27 = ats2plpre_lt_int1_int1($arg0, $env0);
  if($tmp27) {
    $tmp28 = &{$env1->[0]}($env1, $arg0);
    $tmp30 = ats2plpre_add_int1_int1($arg0, 1);
    $tmp29 = _ats2plpre_intrange_aux_22($env0, $env1, $tmp30);
    $tmpret26 = [$tmp28, $tmp29];
  } else {
    $tmpret26 = 0;
  } #endif
  return $tmpret26;
} #end-of-function


sub
ats2plpre_int_list_map_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret31;
##
  __patsflab_int_list_map_method:
  $tmpret31 = _ats2plpre_intrange_patsfun_24__closurerize($arg0);
  return $tmpret31;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_24($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret32;
##
  __patsflab__ats2plpre_intrange_patsfun_24:
  $tmpret32 = ats2plpre_int_list_map_cloref($env0, $arg0);
  return $tmpret32;
} #end-of-function


sub
ats2plpre_int_list0_map_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret33;
  my $tmp34;
  my $tmp35;
##
  __patsflab_int_list0_map_cloref:
  $tmp34 = ats2plpre_gte_int1_int1($arg0, 0);
  if($tmp34) {
    $tmp35 = ats2plpre_int_list_map_cloref($arg0, $arg1);
    $tmpret33 = $tmp35;
  } else {
    $tmpret33 = 0;
  } #endif
  return $tmpret33;
} #end-of-function


sub
ats2plpre_int_list0_map_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret36;
##
  __patsflab_int_list0_map_method:
  $tmpret36 = _ats2plpre_intrange_patsfun_27__closurerize($arg0);
  return $tmpret36;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_27($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret37;
##
  __patsflab__ats2plpre_intrange_patsfun_27:
  $tmpret37 = ats2plpre_int_list0_map_cloref($env0, $arg0);
  return $tmpret37;
} #end-of-function


sub
ats2plpre_int_stream_map_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret38;
##
  __patsflab_int_stream_map_cloref:
  $tmpret38 = _ats2plpre_intrange_aux_29($arg0, $arg1, 0);
  return $tmpret38;
} #end-of-function


sub
_ats2plpre_intrange_aux_29($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret39;
##
  __patsflab__ats2plpre_intrange_aux_29:
  $tmpret39 = ATSPMVlazyval(_ats2plpre_intrange_patsfun_30__closurerize($env0, $env1, $arg0));
  return $tmpret39;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_30($$$)
{
##
  my($env0, $env1, $env2) = @_;
##
  my $tmpret40;
  my $tmp41;
  my $tmp42;
  my $tmp43;
  my $tmp44;
##
  __patsflab__ats2plpre_intrange_patsfun_30:
  $tmp41 = ats2plpre_lt_int1_int1($env2, $env0);
  if($tmp41) {
    $tmp42 = &{$env1->[0]}($env1, $env2);
    $tmp44 = ats2plpre_add_int1_int1($env2, 1);
    $tmp43 = _ats2plpre_intrange_aux_29($env0, $env1, $tmp44);
    $tmpret40 = [$tmp42, $tmp43];
  } else {
    $tmpret40 = 0;
  } #endif
  return $tmpret40;
} #end-of-function


sub
ats2plpre_int_stream_map_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret45;
##
  __patsflab_int_stream_map_method:
  $tmpret45 = _ats2plpre_intrange_patsfun_32__closurerize($arg0);
  return $tmpret45;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_32($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret46;
##
  __patsflab__ats2plpre_intrange_patsfun_32:
  $tmpret46 = ats2plpre_int_stream_map_cloref($env0, $arg0);
  return $tmpret46;
} #end-of-function


sub
ats2plpre_int_stream_vt_map_cloref($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret47;
##
  __patsflab_int_stream_vt_map_cloref:
  $tmpret47 = _ats2plpre_intrange_aux_34($arg0, $arg1, 0);
  return $tmpret47;
} #end-of-function


sub
_ats2plpre_intrange_aux_34($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret48;
##
  __patsflab__ats2plpre_intrange_aux_34:
  $tmpret48 = ATSPMVllazyval(_ats2plpre_intrange_patsfun_35__closurerize($env0, $env1, $arg0));
  return $tmpret48;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_35($$$$)
{
##
  my($env0, $env1, $env2, $arg0) = @_;
##
  my $tmpret49;
  my $tmp50;
  my $tmp51;
  my $tmp52;
  my $tmp53;
##
  __patsflab__ats2plpre_intrange_patsfun_35:
  if($arg0) {
    $tmp50 = ats2plpre_lt_int1_int1($env2, $env0);
    if($tmp50) {
      $tmp51 = &{$env1->[0]}($env1, $env2);
      $tmp53 = ats2plpre_add_int1_int1($env2, 1);
      $tmp52 = _ats2plpre_intrange_aux_34($env0, $env1, $tmp53);
      $tmpret49 = [$tmp51, $tmp52];
    } else {
      $tmpret49 = 0;
    } #endif
  } else {
  } #endif
  return $tmpret49;
} #end-of-function


sub
ats2plpre_int_stream_vt_map_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret54;
##
  __patsflab_int_stream_vt_map_method:
  $tmpret54 = _ats2plpre_intrange_patsfun_37__closurerize($arg0);
  return $tmpret54;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_37($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret55;
##
  __patsflab__ats2plpre_intrange_patsfun_37:
  $tmpret55 = ats2plpre_int_stream_vt_map_cloref($env0, $arg0);
  return $tmpret55;
} #end-of-function


sub
ats2plpre_int2_exists_cloref($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret56;
##
  __patsflab_int2_exists_cloref:
  $tmpret56 = ats2plpre_intrange2_exists_cloref(0, $arg0, 0, $arg1, $arg2);
  return $tmpret56;
} #end-of-function


sub
ats2plpre_int2_forall_cloref($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret57;
##
  __patsflab_int2_forall_cloref:
  $tmpret57 = ats2plpre_intrange2_forall_cloref(0, $arg0, 0, $arg1, $arg2);
  return $tmpret57;
} #end-of-function


sub
ats2plpre_int2_foreach_cloref($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
##
  __patsflab_int2_foreach_cloref:
  ats2plpre_intrange2_foreach_cloref(0, $arg0, 0, $arg1, $arg2);
  return;#_void
} #end-of-function


sub
ats2plpre_intrange_exists_cloref($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret59;
##
  __patsflab_intrange_exists_cloref:
  $tmpret59 = _ats2plpre_intrange_loop_42($arg0, $arg1, $arg2);
  return $tmpret59;
} #end-of-function


sub
_ats2plpre_intrange_loop_42($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $tmpret60;
  my $tmp61;
  my $tmp62;
  my $tmp63;
##
  __patsflab__ats2plpre_intrange_loop_42:
  $tmp61 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp61) {
    $tmp62 = &{$arg2->[0]}($arg2, $arg0);
    if($tmp62) {
      $tmpret60 = 1;
    } else {
      $tmp63 = ats2plpre_add_int0_int0($arg0, 1);
      #ATStailcalseq_beg
      $apy0 = $tmp63;
      $apy1 = $arg1;
      $apy2 = $arg2;
      $arg0 = $apy0;
      $arg1 = $apy1;
      $arg2 = $apy2;
      goto __patsflab__ats2plpre_intrange_loop_42;
      #ATStailcalseq_end
    } #endif
  } else {
    $tmpret60 = 0;
  } #endif
  return $tmpret60;
} #end-of-function


sub
ats2plpre_intrange_exists_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret64;
##
  __patsflab_intrange_exists_method:
  $tmpret64 = _ats2plpre_intrange_patsfun_44__closurerize($arg0);
  return $tmpret64;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_44($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret65;
##
  __patsflab__ats2plpre_intrange_patsfun_44:
  $tmpret65 = ats2plpre_intrange_exists_cloref($env0->[0], $env0->[1], $arg0);
  return $tmpret65;
} #end-of-function


sub
ats2plpre_intrange_forall_cloref($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret66;
##
  __patsflab_intrange_forall_cloref:
  $tmpret66 = _ats2plpre_intrange_loop_46($arg0, $arg1, $arg2);
  return $tmpret66;
} #end-of-function


sub
_ats2plpre_intrange_loop_46($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $tmpret67;
  my $tmp68;
  my $tmp69;
  my $tmp70;
##
  __patsflab__ats2plpre_intrange_loop_46:
  $tmp68 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp68) {
    $tmp69 = &{$arg2->[0]}($arg2, $arg0);
    if($tmp69) {
      $tmp70 = ats2plpre_add_int0_int0($arg0, 1);
      #ATStailcalseq_beg
      $apy0 = $tmp70;
      $apy1 = $arg1;
      $apy2 = $arg2;
      $arg0 = $apy0;
      $arg1 = $apy1;
      $arg2 = $apy2;
      goto __patsflab__ats2plpre_intrange_loop_46;
      #ATStailcalseq_end
    } else {
      $tmpret67 = 0;
    } #endif
  } else {
    $tmpret67 = 1;
  } #endif
  return $tmpret67;
} #end-of-function


sub
ats2plpre_intrange_forall_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret71;
##
  __patsflab_intrange_forall_method:
  $tmpret71 = _ats2plpre_intrange_patsfun_48__closurerize($arg0);
  return $tmpret71;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_48($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret72;
##
  __patsflab__ats2plpre_intrange_patsfun_48:
  $tmpret72 = ats2plpre_intrange_forall_cloref($env0->[0], $env0->[1], $arg0);
  return $tmpret72;
} #end-of-function


sub
ats2plpre_intrange_foreach_cloref($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
##
  __patsflab_intrange_foreach_cloref:
  _ats2plpre_intrange_loop_50($arg0, $arg1, $arg2);
  return;#_void
} #end-of-function


sub
_ats2plpre_intrange_loop_50($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $tmp75;
  my $tmp77;
##
  __patsflab__ats2plpre_intrange_loop_50:
  $tmp75 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp75) {
    &{$arg2->[0]}($arg2, $arg0);
    $tmp77 = ats2plpre_add_int0_int0($arg0, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp77;
    $apy1 = $arg1;
    $apy2 = $arg2;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    goto __patsflab__ats2plpre_intrange_loop_50;
    #ATStailcalseq_end
  } else {
    #ATSINSmove_void;
  } #endif
  return;#_void
} #end-of-function


sub
ats2plpre_intrange_foreach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret78;
##
  __patsflab_intrange_foreach_method:
  $tmpret78 = _ats2plpre_intrange_patsfun_52__closurerize($arg0);
  return $tmpret78;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_52($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_intrange_patsfun_52:
  ats2plpre_intrange_foreach_cloref($env0->[0], $env0->[1], $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_intrange_rforeach_cloref($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
##
  __patsflab_intrange_rforeach_cloref:
  _ats2plpre_intrange_loop_54($arg0, $arg1, $arg2);
  return;#_void
} #end-of-function


sub
_ats2plpre_intrange_loop_54($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $tmp82;
  my $tmp84;
  my $tmp85;
##
  __patsflab__ats2plpre_intrange_loop_54:
  $tmp82 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp82) {
    $tmp84 = ats2plpre_sub_int0_int0($arg1, 1);
    &{$arg2->[0]}($arg2, $tmp84);
    $tmp85 = ats2plpre_sub_int0_int0($arg1, 1);
    #ATStailcalseq_beg
    $apy0 = $arg0;
    $apy1 = $tmp85;
    $apy2 = $arg2;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    goto __patsflab__ats2plpre_intrange_loop_54;
    #ATStailcalseq_end
  } else {
    #ATSINSmove_void;
  } #endif
  return;#_void
} #end-of-function


sub
ats2plpre_intrange_rforeach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret86;
##
  __patsflab_intrange_rforeach_method:
  $tmpret86 = _ats2plpre_intrange_patsfun_56__closurerize($arg0);
  return $tmpret86;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_56($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_intrange_patsfun_56:
  ats2plpre_intrange_rforeach_cloref($env0->[0], $env0->[1], $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_intrange_foldleft_cloref($$$$)
{
##
  my($arg0, $arg1, $arg2, $arg3) = @_;
##
  my $tmpret88;
##
  __patsflab_intrange_foldleft_cloref:
  $tmpret88 = _ats2plpre_intrange_loop_58($arg3, $arg0, $arg1, $arg2);
  return $tmpret88;
} #end-of-function


sub
_ats2plpre_intrange_loop_58($$$$)
{
##
  my($env0, $arg0, $arg1, $arg2) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $tmpret89;
  my $tmp90;
  my $tmp91;
  my $tmp92;
##
  __patsflab__ats2plpre_intrange_loop_58:
  $tmp90 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp90) {
    $tmp91 = ats2plpre_add_int0_int0($arg0, 1);
    $tmp92 = &{$env0->[0]}($env0, $arg2, $arg0);
    #ATStailcalseq_beg
    $apy0 = $tmp91;
    $apy1 = $arg1;
    $apy2 = $tmp92;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    goto __patsflab__ats2plpre_intrange_loop_58;
    #ATStailcalseq_end
  } else {
    $tmpret89 = $arg2;
  } #endif
  return $tmpret89;
} #end-of-function


sub
ats2plpre_intrange_foldleft_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmp93;
  my $tmp94;
  my $tmpret95;
##
  __patsflab_intrange_foldleft_method:
  $tmp93 = $arg0->[0];
  $tmp94 = $arg0->[1];
  $tmpret95 = _ats2plpre_intrange_patsfun_60__closurerize($tmp93, $tmp94);
  return $tmpret95;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_60($$$$)
{
##
  my($env0, $env1, $arg0, $arg1) = @_;
##
  my $tmpret96;
##
  __patsflab__ats2plpre_intrange_patsfun_60:
  $tmpret96 = ats2plpre_intrange_foldleft_cloref($env0, $env1, $arg0, $arg1);
  return $tmpret96;
} #end-of-function


sub
ats2plpre_intrange_foldright_cloref($$$$)
{
##
  my($arg0, $arg1, $arg2, $arg3) = @_;
##
  my $tmpret97;
##
  __patsflab_intrange_foldright_cloref:
  $tmpret97 = _ats2plpre_intrange_loop_62($arg2, $arg0, $arg1, $arg3);
  return $tmpret97;
} #end-of-function


sub
_ats2plpre_intrange_loop_62($$$$)
{
##
  my($env0, $arg0, $arg1, $arg2) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $tmpret98;
  my $tmp99;
  my $tmp100;
  my $tmp101;
  my $tmp102;
##
  __patsflab__ats2plpre_intrange_loop_62:
  $tmp99 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp99) {
    $tmp100 = ats2plpre_sub_int0_int0($arg1, 1);
    $tmp102 = ats2plpre_sub_int0_int0($arg1, 1);
    $tmp101 = &{$env0->[0]}($env0, $tmp102, $arg2);
    #ATStailcalseq_beg
    $apy0 = $arg0;
    $apy1 = $tmp100;
    $apy2 = $tmp101;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    goto __patsflab__ats2plpre_intrange_loop_62;
    #ATStailcalseq_end
  } else {
    $tmpret98 = $arg2;
  } #endif
  return $tmpret98;
} #end-of-function


sub
ats2plpre_intrange_foldright_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmp103;
  my $tmp104;
  my $tmpret105;
##
  __patsflab_intrange_foldright_method:
  $tmp103 = $arg0->[0];
  $tmp104 = $arg0->[1];
  $tmpret105 = _ats2plpre_intrange_patsfun_64__closurerize($tmp103, $tmp104);
  return $tmpret105;
} #end-of-function


sub
_ats2plpre_intrange_patsfun_64($$$$)
{
##
  my($env0, $env1, $arg0, $arg1) = @_;
##
  my $tmpret106;
##
  __patsflab__ats2plpre_intrange_patsfun_64:
  $tmpret106 = ats2plpre_intrange_foldright_cloref($env0, $env1, $arg0, $arg1);
  return $tmpret106;
} #end-of-function


sub
ats2plpre_intrange2_exists_cloref($$$$$)
{
##
  my($arg0, $arg1, $arg2, $arg3, $arg4) = @_;
##
  my $tmpret107;
##
  __patsflab_intrange2_exists_cloref:
  $tmpret107 = _ats2plpre_intrange_loop1_66($arg2, $arg3, $arg4, $arg0, $arg1, $arg2, $arg3, $arg4);
  return $tmpret107;
} #end-of-function


sub
_ats2plpre_intrange_loop1_66($$$$$$$$)
{
##
  my($env0, $env1, $env2, $arg0, $arg1, $arg2, $arg3, $arg4) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $apy3;
  my $apy4;
  my $tmpret108;
  my $tmp109;
  my $a2rg0;
  my $a2rg1;
  my $a2rg2;
  my $a2rg3;
  my $a2rg4;
  my $a2py0;
  my $a2py1;
  my $a2py2;
  my $a2py3;
  my $a2py4;
  my $tmpret110;
  my $tmp111;
  my $tmp112;
  my $tmp113;
  my $tmp114;
##
  __patsflab__ats2plpre_intrange_loop1_66:
  $tmp109 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp109) {
    #ATStailcalseq_beg
    $a2py0 = $arg0;
    $a2py1 = $arg1;
    $a2py2 = $arg2;
    $a2py3 = $arg3;
    $a2py4 = $env2;
    $a2rg0 = $a2py0;
    $a2rg1 = $a2py1;
    $a2rg2 = $a2py2;
    $a2rg3 = $a2py3;
    $a2rg4 = $a2py4;
    goto __patsflab__ats2plpre_intrange_loop2_67;
    #ATStailcalseq_end
  } else {
    $tmpret108 = 0;
  } #endif
  return $tmpret108;
##
  __patsflab__ats2plpre_intrange_loop2_67:
  $tmp111 = ats2plpre_lt_int0_int0($a2rg2, $a2rg3);
  if($tmp111) {
    $tmp112 = &{$a2rg4->[0]}($a2rg4, $a2rg0, $a2rg2);
    if($tmp112) {
      $tmpret110 = 1;
    } else {
      $tmp113 = ats2plpre_add_int0_int0($a2rg2, 1);
      #ATStailcalseq_beg
      $a2py0 = $a2rg0;
      $a2py1 = $a2rg1;
      $a2py2 = $tmp113;
      $a2py3 = $a2rg3;
      $a2py4 = $a2rg4;
      $a2rg0 = $a2py0;
      $a2rg1 = $a2py1;
      $a2rg2 = $a2py2;
      $a2rg3 = $a2py3;
      $a2rg4 = $a2py4;
      goto __patsflab__ats2plpre_intrange_loop2_67;
      #ATStailcalseq_end
    } #endif
  } else {
    $tmp114 = ats2plpre_add_int0_int0($a2rg0, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp114;
    $apy1 = $a2rg1;
    $apy2 = $env0;
    $apy3 = $env1;
    $apy4 = $a2rg4;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    $arg3 = $apy3;
    $arg4 = $apy4;
    goto __patsflab__ats2plpre_intrange_loop1_66;
    #ATStailcalseq_end
  } #endif
  return $tmpret110;
} #end-of-function


sub
ats2plpre_intrange2_forall_cloref($$$$$)
{
##
  my($arg0, $arg1, $arg2, $arg3, $arg4) = @_;
##
  my $tmpret115;
##
  __patsflab_intrange2_forall_cloref:
  $tmpret115 = _ats2plpre_intrange_loop1_69($arg2, $arg3, $arg0, $arg1, $arg2, $arg3, $arg4);
  return $tmpret115;
} #end-of-function


sub
_ats2plpre_intrange_loop1_69($$$$$$$)
{
##
  my($env0, $env1, $arg0, $arg1, $arg2, $arg3, $arg4) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $apy3;
  my $apy4;
  my $tmpret116;
  my $tmp117;
  my $a2rg0;
  my $a2rg1;
  my $a2rg2;
  my $a2rg3;
  my $a2rg4;
  my $a2py0;
  my $a2py1;
  my $a2py2;
  my $a2py3;
  my $a2py4;
  my $tmpret118;
  my $tmp119;
  my $tmp120;
  my $tmp121;
  my $tmp122;
##
  __patsflab__ats2plpre_intrange_loop1_69:
  $tmp117 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp117) {
    #ATStailcalseq_beg
    $a2py0 = $arg0;
    $a2py1 = $arg1;
    $a2py2 = $arg2;
    $a2py3 = $arg3;
    $a2py4 = $arg4;
    $a2rg0 = $a2py0;
    $a2rg1 = $a2py1;
    $a2rg2 = $a2py2;
    $a2rg3 = $a2py3;
    $a2rg4 = $a2py4;
    goto __patsflab__ats2plpre_intrange_loop2_70;
    #ATStailcalseq_end
  } else {
    $tmpret116 = 1;
  } #endif
  return $tmpret116;
##
  __patsflab__ats2plpre_intrange_loop2_70:
  $tmp119 = ats2plpre_lt_int0_int0($a2rg2, $a2rg3);
  if($tmp119) {
    $tmp120 = &{$a2rg4->[0]}($a2rg4, $a2rg0, $a2rg2);
    if($tmp120) {
      $tmp121 = ats2plpre_add_int0_int0($a2rg2, 1);
      #ATStailcalseq_beg
      $a2py0 = $a2rg0;
      $a2py1 = $a2rg1;
      $a2py2 = $tmp121;
      $a2py3 = $a2rg3;
      $a2py4 = $a2rg4;
      $a2rg0 = $a2py0;
      $a2rg1 = $a2py1;
      $a2rg2 = $a2py2;
      $a2rg3 = $a2py3;
      $a2rg4 = $a2py4;
      goto __patsflab__ats2plpre_intrange_loop2_70;
      #ATStailcalseq_end
    } else {
      $tmpret118 = 0;
    } #endif
  } else {
    $tmp122 = ats2plpre_add_int0_int0($a2rg0, 1);
    #ATStailcalseq_beg
    $apy0 = $tmp122;
    $apy1 = $a2rg1;
    $apy2 = $env0;
    $apy3 = $env1;
    $apy4 = $a2rg4;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    $arg3 = $apy3;
    $arg4 = $apy4;
    goto __patsflab__ats2plpre_intrange_loop1_69;
    #ATStailcalseq_end
  } #endif
  return $tmpret118;
} #end-of-function


sub
ats2plpre_intrange2_foreach_cloref($$$$$)
{
##
  my($arg0, $arg1, $arg2, $arg3, $arg4) = @_;
##
##
  __patsflab_intrange2_foreach_cloref:
  _ats2plpre_intrange_loop1_72($arg2, $arg3, $arg0, $arg1, $arg2, $arg3, $arg4);
  return;#_void
} #end-of-function


sub
_ats2plpre_intrange_loop1_72($$$$$$$)
{
##
  my($env0, $env1, $arg0, $arg1, $arg2, $arg3, $arg4) = @_;
##
  my $apy0;
  my $apy1;
  my $apy2;
  my $apy3;
  my $apy4;
  my $tmp125;
  my $a2rg0;
  my $a2rg1;
  my $a2rg2;
  my $a2rg3;
  my $a2rg4;
  my $a2py0;
  my $a2py1;
  my $a2py2;
  my $a2py3;
  my $a2py4;
  my $tmp127;
  my $tmp129;
  my $tmp130;
##
  __patsflab__ats2plpre_intrange_loop1_72:
  $tmp125 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp125) {
    #ATStailcalseq_beg
    $a2py0 = $arg0;
    $a2py1 = $arg1;
    $a2py2 = $arg2;
    $a2py3 = $arg3;
    $a2py4 = $arg4;
    $a2rg0 = $a2py0;
    $a2rg1 = $a2py1;
    $a2rg2 = $a2py2;
    $a2rg3 = $a2py3;
    $a2rg4 = $a2py4;
    goto __patsflab__ats2plpre_intrange_loop2_73;
    #ATStailcalseq_end
  } else {
    #ATSINSmove_void;
  } #endif
  return;#_void
##
  __patsflab__ats2plpre_intrange_loop2_73:
  $tmp127 = ats2plpre_lt_int0_int0($a2rg2, $a2rg3);
  if($tmp127) {
    &{$a2rg4->[0]}($a2rg4, $a2rg0, $a2rg2);
    $tmp129 = ats2plpre_add_int0_int0($a2rg2, 1);
    #ATStailcalseq_beg
    $a2py0 = $a2rg0;
    $a2py1 = $a2rg1;
    $a2py2 = $tmp129;
    $a2py3 = $a2rg3;
    $a2py4 = $a2rg4;
    $a2rg0 = $a2py0;
    $a2rg1 = $a2py1;
    $a2rg2 = $a2py2;
    $a2rg3 = $a2py3;
    $a2rg4 = $a2py4;
    goto __patsflab__ats2plpre_intrange_loop2_73;
    #ATStailcalseq_end
  } else {
    $tmp130 = ats2plpre_succ_int0($a2rg0);
    #ATStailcalseq_beg
    $apy0 = $tmp130;
    $apy1 = $a2rg1;
    $apy2 = $env0;
    $apy3 = $env1;
    $apy4 = $a2rg4;
    $arg0 = $apy0;
    $arg1 = $apy1;
    $arg2 = $apy2;
    $arg3 = $apy3;
    $arg4 = $apy4;
    goto __patsflab__ats2plpre_intrange_loop1_72;
    #ATStailcalseq_end
  } #endif
  return;#_void
} #end-of-function


######
#ATSextcode_beg()
######
######
1; ##note that it is needed by 'use' or 'require'
######
######
#ATSextcode_end()
######
######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######

sub
ats2plpre_ref($)
{
##
  my($arg0) = @_;
##
  my $tmpret0;
##
  __patsflab_ref:
  $tmpret0 = ats2plpre_ref_make_elt($arg0);
  return $tmpret0;
} #end-of-function


sub
ats2plpre_ref_make_elt($)
{
##
  my($arg0) = @_;
##
  my $tmpret1;
  my $tmp2;
##
  __patsflab_ref_make_elt:
  $tmp2 = ats2plpre_PLarray_sing($arg0);
  $tmpret1 = $tmp2;
  return $tmpret1;
} #end-of-function


sub
ats2plpre_ref_get_elt($)
{
##
  my($arg0) = @_;
##
  my $tmpret3;
##
  __patsflab_ref_get_elt:
  $tmpret3 = ats2plpre_PLarray_get_at($arg0, 0);
  return $tmpret3;
} #end-of-function


sub
ats2plpre_ref_set_elt($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_ref_set_elt:
  ats2plpre_PLarray_set_at($arg0, 0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_ref_exch_elt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret5;
  my $tmp6;
##
  __patsflab_ref_exch_elt:
  $tmp6 = ats2plpre_PLarray_get_at($arg0, 0);
  ats2plpre_PLarray_set_at($arg0, 0, $arg1);
  $tmpret5 = $tmp6;
  return $tmpret5;
} #end-of-function

######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######

sub
slistref_make_nil()
{
##
  #argless
##
  my $tmpret0;
  my $tmp1;
##
  __patsflab_slistref_make_nil:
  $tmp1 = 0;
  $tmpret0 = ats2plpre_ref($tmp1);
  return $tmpret0;
} #end-of-function


sub
slistref_length($)
{
##
  my($arg0) = @_;
##
  my $tmpret2;
  my $tmp3;
##
  __patsflab_slistref_length:
  $tmp3 = ats2plpre_ref_get_elt($arg0);
  $tmpret2 = ats2plpre_list_length($tmp3);
  return $tmpret2;
} #end-of-function


sub
slistref_push($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmp5;
  my $tmp6;
##
  __patsflab_slistref_push:
  $tmp6 = ats2plpre_ref_get_elt($arg0);
  $tmp5 = [$arg1, $tmp6];
  ats2plpre_ref_set_elt($arg0, $tmp5);
  return;#_void
} #end-of-function


sub
slistref_pop_opt($)
{
##
  my($arg0) = @_;
##
  my $tmpret7;
  my $tmp8;
  my $tmp9;
  my $tmp10;
##
  __patsflab_slistref_pop_opt:
  $tmp8 = ats2plpre_ref_get_elt($arg0);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab0:
    if(ATSCKptriscons($tmp8)) { goto __atstmplab3; }
    __atstmplab1:
    $tmpret7 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab2:
    __atstmplab3:
    $tmp9 = $tmp8->[0];
    $tmp10 = $tmp8->[1];
    ats2plpre_ref_set_elt($arg0, $tmp10);
    $tmpret7 = [$tmp9];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret7;
} #end-of-function


sub
slistref_foldleft($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret12;
  my $tmp13;
##
  __patsflab_slistref_foldleft:
  $tmp13 = ats2plpre_ref_get_elt($arg0);
  $tmpret12 = ats2plpre_list_foldleft($tmp13, $arg1, $arg2);
  return $tmpret12;
} #end-of-function


sub
slistref_foldright($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret14;
  my $tmp15;
##
  __patsflab_slistref_foldright:
  $tmp15 = ats2plpre_ref_get_elt($arg0);
  $tmpret14 = ats2plpre_list_foldright($tmp15, $arg1, $arg2);
  return $tmpret14;
} #end-of-function

######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######

sub
ats2plpre_qlistref_make_nil()
{
##
  #argless
##
  my $tmpret0;
  my $tmp1;
  my $tmp2;
  my $tmp3;
  my $tmp4;
##
  __patsflab_qlistref_make_nil:
  $tmp2 = 0;
  $tmp1 = ats2plpre_ref($tmp2);
  $tmp4 = 0;
  $tmp3 = ats2plpre_ref($tmp4);
  $tmpret0 = [$tmp1, $tmp3];
  return $tmpret0;
} #end-of-function


sub
ats2plpre_qlistref_length($)
{
##
  my($arg0) = @_;
##
  my $tmpret5;
  my $tmp6;
  my $tmp7;
  my $tmp8;
  my $tmp9;
  my $tmp10;
  my $tmp11;
##
  __patsflab_qlistref_length:
  $tmp6 = $arg0->[0];
  $tmp7 = $arg0->[1];
  $tmp9 = ats2plpre_ref_get_elt($tmp6);
  $tmp8 = ats2plpre_list_length($tmp9);
  $tmp11 = ats2plpre_ref_get_elt($tmp7);
  $tmp10 = ats2plpre_list_length($tmp11);
  $tmpret5 = ats2plpre_add_int1_int1($tmp8, $tmp10);
  return $tmpret5;
} #end-of-function


sub
ats2plpre_qlistref_enqueue($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmp13;
  my $tmp14;
  my $tmp15;
  my $tmp16;
##
  __patsflab_qlistref_enqueue:
  $tmp13 = $arg0->[0];
  $tmp14 = $arg0->[1];
  $tmp16 = ats2plpre_ref_get_elt($tmp13);
  $tmp15 = [$arg1, $tmp16];
  ats2plpre_ref_set_elt($tmp13, $tmp15);
  return;#_void
} #end-of-function


sub
ats2plpre_qlistref_dequeue_opt($)
{
##
  my($arg0) = @_;
##
  my $tmpret17;
  my $tmp18;
  my $tmp19;
  my $tmp20;
  my $tmp21;
  my $tmp22;
  my $tmp23;
  my $tmp25;
  my $tmp26;
  my $tmp27;
##
  __patsflab_qlistref_dequeue_opt:
  $tmp18 = $arg0->[0];
  $tmp19 = $arg0->[1];
  $tmp20 = ats2plpre_ref_get_elt($tmp19);
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab0:
    if(ATSCKptriscons($tmp20)) { goto __atstmplab3; }
    __atstmplab1:
    $tmp23 = ats2plpre_ref_get_elt($tmp18);
    $tmp25 = 0;
    ats2plpre_ref_set_elt($tmp18, $tmp25);
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab4:
      if(ATSCKptriscons($tmp23)) { goto __atstmplab7; }
      __atstmplab5:
      $tmpret17 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab6:
      __atstmplab7:
      $tmp26 = $tmp23->[0];
      $tmp27 = $tmp23->[1];
      ats2plpre_ref_set_elt($tmp19, $tmp27);
      $tmpret17 = [$tmp26];
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab2:
    __atstmplab3:
    $tmp21 = $tmp20->[0];
    $tmp22 = $tmp20->[1];
    ats2plpre_ref_set_elt($tmp19, $tmp22);
    $tmpret17 = [$tmp21];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret17;
} #end-of-function


sub
ats2plpre_qlistref_foldleft($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret30;
  my $tmp31;
  my $tmp32;
  my $tmp41;
  my $tmp42;
  my $tmp43;
##
  __patsflab_qlistref_foldleft:
  $tmp31 = $arg0->[0];
  $tmp32 = $arg0->[1];
  $tmp41 = ats2plpre_ref_get_elt($tmp31);
  $tmp43 = ats2plpre_ref_get_elt($tmp32);
  $tmp42 = _ats2plpre_qlistref_auxl_5($arg2, $arg1, $tmp43);
  $tmpret30 = _ats2plpre_qlistref_auxr_6($arg2, $tmp41, $tmp42);
  return $tmpret30;
} #end-of-function


sub
_ats2plpre_qlistref_auxl_5($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret33;
  my $tmp34;
  my $tmp35;
  my $tmp36;
##
  __patsflab__ats2plpre_qlistref_auxl_5:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab8:
    if(ATSCKptriscons($arg1)) { goto __atstmplab11; }
    __atstmplab9:
    $tmpret33 = $arg0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab10:
    __atstmplab11:
    $tmp34 = $arg1->[0];
    $tmp35 = $arg1->[1];
    $tmp36 = &{$env0->[0]}($env0, $arg0, $tmp34);
    #ATStailcalseq_beg
    $apy0 = $tmp36;
    $apy1 = $tmp35;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_qlistref_auxl_5;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret33;
} #end-of-function


sub
_ats2plpre_qlistref_auxr_6($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret37;
  my $tmp38;
  my $tmp39;
  my $tmp40;
##
  __patsflab__ats2plpre_qlistref_auxr_6:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab12:
    if(ATSCKptriscons($arg0)) { goto __atstmplab15; }
    __atstmplab13:
    $tmpret37 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab14:
    __atstmplab15:
    $tmp38 = $arg0->[0];
    $tmp39 = $arg0->[1];
    $tmp40 = _ats2plpre_qlistref_auxr_6($env0, $tmp39, $arg1);
    $tmpret37 = &{$env0->[0]}($env0, $tmp40, $tmp38);
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret37;
} #end-of-function


sub
ats2plpre_qlistref_foldright($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret44;
  my $tmp45;
  my $tmp46;
  my $tmp55;
  my $tmp56;
  my $tmp57;
##
  __patsflab_qlistref_foldright:
  $tmp45 = $arg0->[0];
  $tmp46 = $arg0->[1];
  $tmp55 = ats2plpre_ref_get_elt($tmp46);
  $tmp57 = ats2plpre_ref_get_elt($tmp45);
  $tmp56 = _ats2plpre_qlistref_auxl_8($arg1, $arg2, $tmp57);
  $tmpret44 = _ats2plpre_qlistref_auxr_9($arg1, $tmp55, $tmp56);
  return $tmpret44;
} #end-of-function


sub
_ats2plpre_qlistref_auxl_8($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret47;
  my $tmp48;
  my $tmp49;
  my $tmp50;
##
  __patsflab__ats2plpre_qlistref_auxl_8:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab16:
    if(ATSCKptriscons($arg1)) { goto __atstmplab19; }
    __atstmplab17:
    $tmpret47 = $arg0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab18:
    __atstmplab19:
    $tmp48 = $arg1->[0];
    $tmp49 = $arg1->[1];
    $tmp50 = &{$env0->[0]}($env0, $tmp48, $arg0);
    #ATStailcalseq_beg
    $apy0 = $tmp50;
    $apy1 = $tmp49;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_qlistref_auxl_8;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret47;
} #end-of-function


sub
_ats2plpre_qlistref_auxr_9($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $tmpret51;
  my $tmp52;
  my $tmp53;
  my $tmp54;
##
  __patsflab__ats2plpre_qlistref_auxr_9:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab20:
    if(ATSCKptriscons($arg0)) { goto __atstmplab23; }
    __atstmplab21:
    $tmpret51 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab22:
    __atstmplab23:
    $tmp52 = $arg0->[0];
    $tmp53 = $arg0->[1];
    $tmp54 = _ats2plpre_qlistref_auxr_9($env0, $tmp53, $arg1);
    $tmpret51 = &{$env0->[0]}($env0, $tmp52, $tmp54);
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret51;
} #end-of-function

######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######
sub
_ats2plpre_ML_list0_patsfun_29__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_29($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_32__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_32($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_35__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_35($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_38__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_38($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_42__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_42($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_45__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_45($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_48__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_48($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_51__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_51($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_55__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_55($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_58__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_58($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_63__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_63($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_66__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_66($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_list0_patsfun_72__closurerize($$)
{
  my($env0, $env1) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_list0_patsfun_72($cenv->[1], $cenv->[2], $arg0); }, $env0, $env1];
}


sub
ats2plpre_ML_list0_head_opt($)
{
##
  my($arg0) = @_;
##
  my $tmpret7;
  my $tmp8;
##
  __patsflab_list0_head_opt:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab6:
    if(ATSCKptriscons($arg0)) { goto __atstmplab9; }
    __atstmplab7:
    $tmpret7 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab8:
    __atstmplab9:
    $tmp8 = $arg0->[0];
    $tmpret7 = [$tmp8];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret7;
} #end-of-function


sub
ats2plpre_ML_list0_tail_opt($)
{
##
  my($arg0) = @_;
##
  my $tmpret10;
  my $tmp12;
##
  __patsflab_list0_tail_opt:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab10:
    if(ATSCKptriscons($arg0)) { goto __atstmplab13; }
    __atstmplab11:
    $tmpret10 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab12:
    __atstmplab13:
    $tmp12 = $arg0->[1];
    $tmpret10 = [$tmp12];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret10;
} #end-of-function


sub
ats2plpre_ML_list0_length($)
{
##
  my($arg0) = @_;
##
  my $tmpret13;
##
  __patsflab_list0_length:
  $tmpret13 = ats2plpre_list_length($arg0);
  return $tmpret13;
} #end-of-function


sub
ats2plpre_ML_list0_last_opt($)
{
##
  my($arg0) = @_;
##
  my $tmpret14;
  my $tmp18;
  my $tmp19;
  my $tmp20;
##
  __patsflab_list0_last_opt:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab18:
    if(ATSCKptriscons($arg0)) { goto __atstmplab21; }
    __atstmplab19:
    $tmpret14 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab20:
    __atstmplab21:
    $tmp18 = $arg0->[0];
    $tmp19 = $arg0->[1];
    $tmp20 = _ats2plpre_ML_list0_loop_8($tmp18, $tmp19);
    $tmpret14 = [$tmp20];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret14;
} #end-of-function


sub
_ats2plpre_ML_list0_loop_8($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret15;
  my $tmp16;
  my $tmp17;
##
  __patsflab__ats2plpre_ML_list0_loop_8:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab14:
    if(ATSCKptriscons($arg1)) { goto __atstmplab17; }
    __atstmplab15:
    $tmpret15 = $arg0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab16:
    __atstmplab17:
    $tmp16 = $arg1->[0];
    $tmp17 = $arg1->[1];
    #ATStailcalseq_beg
    $apy0 = $tmp16;
    $apy1 = $tmp17;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_ML_list0_loop_8;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret15;
} #end-of-function


sub
ats2plpre_ML_list0_get_at_opt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret21;
  my $tmp22;
  my $tmp23;
  my $tmp24;
  my $tmp25;
##
  __patsflab_list0_get_at_opt:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab22:
    if(ATSCKptriscons($arg0)) { goto __atstmplab25; }
    __atstmplab23:
    $tmpret21 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab24:
    __atstmplab25:
    $tmp22 = $arg0->[0];
    $tmp23 = $arg0->[1];
    $tmp24 = ats2plpre_gt_int1_int1($arg1, 0);
    if($tmp24) {
      $tmp25 = ats2plpre_sub_int1_int1($arg1, 1);
      #ATStailcalseq_beg
      $apy0 = $tmp23;
      $apy1 = $tmp25;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab_list0_get_at_opt;
      #ATStailcalseq_end
    } else {
      $tmpret21 = [$tmp22];
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret21;
} #end-of-function


sub
ats2plpre_ML_list0_make_elt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret26;
  my $tmp27;
  my $tmp28;
##
  __patsflab_list0_make_elt:
  $tmp27 = ats2plpre_gte_int1_int1($arg0, 0);
  if($tmp27) {
    $tmp28 = ats2plpre_list_make_elt($arg0, $arg1);
    $tmpret26 = $tmp28;
  } else {
    $tmpret26 = 0;
  } #endif
  return $tmpret26;
} #end-of-function


sub
ats2plpre_ML_list0_make_intrange_2($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret29;
  my $tmp30;
##
  __patsflab_list0_make_intrange_2:
  $tmp30 = ats2plpre_list_make_intrange_2($arg0, $arg1);
  $tmpret29 = $tmp30;
  return $tmpret29;
} #end-of-function


sub
ats2plpre_ML_list0_make_intrange_3($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret31;
  my $tmp32;
##
  __patsflab_list0_make_intrange_3:
  $tmp32 = ats2plpre_list_make_intrange_3($arg0, $arg1, $arg2);
  $tmpret31 = $tmp32;
  return $tmpret31;
} #end-of-function


sub
ats2plpre_ML_list0_snoc($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret44;
  my $tmp45;
##
  __patsflab_list0_snoc:
  $tmp45 = ats2plpre_list_snoc($arg0, $arg1);
  $tmpret44 = $tmp45;
  return $tmpret44;
} #end-of-function


sub
ats2plpre_ML_list0_extend($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret46;
  my $tmp47;
##
  __patsflab_list0_extend:
  $tmp47 = ats2plpre_list_extend($arg0, $arg1);
  $tmpret46 = $tmp47;
  return $tmpret46;
} #end-of-function


sub
ats2plpre_ML_list0_append($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret48;
  my $tmp49;
##
  __patsflab_list0_append:
  $tmp49 = ats2plpre_list_append($arg0, $arg1);
  $tmpret48 = $tmp49;
  return $tmpret48;
} #end-of-function


sub
ats2plpre_ML_mul_int_list0($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret50;
  my $tmp51;
##
  __patsflab_mul_int_list0:
  $tmp51 = ats2plpre_mul_int_list($arg0, $arg1);
  $tmpret50 = $tmp51;
  return $tmpret50;
} #end-of-function


sub
ats2plpre_ML_list0_reverse($)
{
##
  my($arg0) = @_;
##
  my $tmpret52;
  my $tmp53;
##
  __patsflab_list0_reverse:
  $tmp53 = ats2plpre_list_reverse($arg0);
  $tmpret52 = $tmp53;
  return $tmpret52;
} #end-of-function


sub
ats2plpre_ML_list0_reverse_append($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret54;
  my $tmp55;
##
  __patsflab_list0_reverse_append:
  $tmp55 = ats2plpre_list_reverse_append($arg0, $arg1);
  $tmpret54 = $tmp55;
  return $tmpret54;
} #end-of-function


sub
ats2plpre_ML_list0_concat($)
{
##
  my($arg0) = @_;
##
  my $tmpret56;
  my $tmp57;
##
  __patsflab_list0_concat:
  $tmp57 = ats2plpre_list_concat($arg0);
  $tmpret56 = $tmp57;
  return $tmpret56;
} #end-of-function


sub
ats2plpre_ML_list0_remove_at_opt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret58;
##
  __patsflab_list0_remove_at_opt:
  $tmpret58 = _ats2plpre_ML_list0_aux_26($arg0, 0);
  return $tmpret58;
} #end-of-function


sub
_ats2plpre_ML_list0_aux_26($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret59;
  my $tmp60;
  my $tmp61;
  my $tmp62;
  my $tmp63;
  my $tmp64;
  my $tmp65;
  my $tmp66;
##
  __patsflab__ats2plpre_ML_list0_aux_26:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab30:
    if(ATSCKptriscons($arg0)) { goto __atstmplab33; }
    __atstmplab31:
    $tmpret59 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab32:
    __atstmplab33:
    $tmp60 = $arg0->[0];
    $tmp61 = $arg0->[1];
    $tmp62 = ats2plpre_gt_int1_int1($arg1, 0);
    if($tmp62) {
      $tmp64 = ats2plpre_sub_int1_int1($arg1, 1);
      $tmp63 = _ats2plpre_ML_list0_aux_26($tmp61, $tmp64);
      #ATScaseofseq_beg
      while(1)
      {
        #ATSbranchseq_beg
        __atstmplab34:
        if(ATSCKptriscons($tmp63)) { goto __atstmplab37; }
        __atstmplab35:
        $tmpret59 = 0;
        last;
        #ATSbranchseq_end
        #ATSbranchseq_beg
        __atstmplab36:
        __atstmplab37:
        $tmp65 = $tmp63->[0];
        #ATSINSfreecon($tmp63);
        $tmp66 = [$tmp60, $tmp65];
        $tmpret59 = [$tmp66];
        last;
        #ATSbranchseq_end
      } #end-of-while-loop;
      #ATScaseofseq_end
    } else {
      $tmpret59 = [$tmp61];
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret59;
} #end-of-function


sub
ats2plpre_ML_list0_exists($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret67;
##
  __patsflab_list0_exists:
  $tmpret67 = ats2plpre_list_exists($arg0, $arg1);
  return $tmpret67;
} #end-of-function


sub
ats2plpre_ML_list0_exists_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret68;
##
  __patsflab_list0_exists_method:
  $tmpret68 = _ats2plpre_ML_list0_patsfun_29__closurerize($arg0);
  return $tmpret68;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_29($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret69;
##
  __patsflab__ats2plpre_ML_list0_patsfun_29:
  $tmpret69 = ats2plpre_ML_list0_exists($env0, $arg0);
  return $tmpret69;
} #end-of-function


sub
ats2plpre_ML_list0_iexists($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret70;
##
  __patsflab_list0_iexists:
  $tmpret70 = ats2plpre_list_iexists($arg0, $arg1);
  return $tmpret70;
} #end-of-function


sub
ats2plpre_ML_list0_iexists_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret71;
##
  __patsflab_list0_iexists_method:
  $tmpret71 = _ats2plpre_ML_list0_patsfun_32__closurerize($arg0);
  return $tmpret71;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_32($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret72;
##
  __patsflab__ats2plpre_ML_list0_patsfun_32:
  $tmpret72 = ats2plpre_ML_list0_iexists($env0, $arg0);
  return $tmpret72;
} #end-of-function


sub
ats2plpre_ML_list0_forall($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret73;
##
  __patsflab_list0_forall:
  $tmpret73 = ats2plpre_list_forall($arg0, $arg1);
  return $tmpret73;
} #end-of-function


sub
ats2plpre_ML_list0_forall_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret74;
##
  __patsflab_list0_forall_method:
  $tmpret74 = _ats2plpre_ML_list0_patsfun_35__closurerize($arg0);
  return $tmpret74;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_35($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret75;
##
  __patsflab__ats2plpre_ML_list0_patsfun_35:
  $tmpret75 = ats2plpre_ML_list0_forall($env0, $arg0);
  return $tmpret75;
} #end-of-function


sub
ats2plpre_ML_list0_iforall($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret76;
##
  __patsflab_list0_iforall:
  $tmpret76 = ats2plpre_list_iforall($arg0, $arg1);
  return $tmpret76;
} #end-of-function


sub
ats2plpre_ML_list0_iforall_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret77;
##
  __patsflab_list0_iforall_method:
  $tmpret77 = _ats2plpre_ML_list0_patsfun_38__closurerize($arg0);
  return $tmpret77;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_38($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret78;
##
  __patsflab__ats2plpre_ML_list0_patsfun_38:
  $tmpret78 = ats2plpre_ML_list0_iforall($env0, $arg0);
  return $tmpret78;
} #end-of-function


sub
ats2plpre_ML_list0_app($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_list0_app:
  ats2plpre_ML_list0_foreach($arg0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_list0_foreach($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_list0_foreach:
  ats2plpre_list_foreach($arg0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_list0_foreach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret81;
##
  __patsflab_list0_foreach_method:
  $tmpret81 = _ats2plpre_ML_list0_patsfun_42__closurerize($arg0);
  return $tmpret81;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_42($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_ML_list0_patsfun_42:
  ats2plpre_ML_list0_foreach($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_list0_iforeach($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_list0_iforeach:
  ats2plpre_list_iforeach($arg0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_list0_iforeach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret84;
##
  __patsflab_list0_iforeach_method:
  $tmpret84 = _ats2plpre_ML_list0_patsfun_45__closurerize($arg0);
  return $tmpret84;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_45($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_ML_list0_patsfun_45:
  ats2plpre_ML_list0_iforeach($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_list0_rforeach($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_list0_rforeach:
  ats2plpre_list_rforeach($arg0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_list0_rforeach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret87;
##
  __patsflab_list0_rforeach_method:
  $tmpret87 = _ats2plpre_ML_list0_patsfun_48__closurerize($arg0);
  return $tmpret87;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_48($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_ML_list0_patsfun_48:
  ats2plpre_ML_list0_rforeach($env0, $arg0);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_list0_filter($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret89;
  my $tmp90;
##
  __patsflab_list0_filter:
  $tmp90 = ats2plpre_list_filter($arg0, $arg1);
  $tmpret89 = $tmp90;
  return $tmpret89;
} #end-of-function


sub
ats2plpre_ML_list0_filter_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret91;
##
  __patsflab_list0_filter_method:
  $tmpret91 = _ats2plpre_ML_list0_patsfun_51__closurerize($arg0);
  return $tmpret91;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_51($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret92;
##
  __patsflab__ats2plpre_ML_list0_patsfun_51:
  $tmpret92 = ats2plpre_ML_list0_filter($env0, $arg0);
  return $tmpret92;
} #end-of-function


sub
_057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2pl_057_ATS2_055_0_056_3_056_2_057_SATS_057_ML_057_list0_056_sats__list0_labelize($)
{
##
  my($arg0) = @_;
##
  my $tmpret93;
  my $tmp94;
##
  __patsflab_list0_labelize:
  $tmp94 = _057_home_057_hwxi_057_Research_057_ATS_055_Postiats_057_contrib_057_libatscc2pl_057_ATS2_055_0_056_3_056_2_057_SATS_057_list_056_sats__list_labelize($arg0);
  $tmpret93 = $tmp94;
  return $tmpret93;
} #end-of-function


sub
ats2plpre_ML_list0_map($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret95;
  my $tmp96;
##
  __patsflab_list0_map:
  $tmp96 = ats2plpre_list_map($arg0, $arg1);
  $tmpret95 = $tmp96;
  return $tmpret95;
} #end-of-function


sub
ats2plpre_ML_list0_map_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret97;
##
  __patsflab_list0_map_method:
  $tmpret97 = _ats2plpre_ML_list0_patsfun_55__closurerize($arg0);
  return $tmpret97;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_55($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret98;
##
  __patsflab__ats2plpre_ML_list0_patsfun_55:
  $tmpret98 = ats2plpre_ML_list0_map($env0, $arg0);
  return $tmpret98;
} #end-of-function


sub
ats2plpre_ML_list0_imap($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret99;
  my $tmp100;
##
  __patsflab_list0_imap:
  $tmp100 = ats2plpre_list_imap($arg0, $arg1);
  $tmpret99 = $tmp100;
  return $tmpret99;
} #end-of-function


sub
ats2plpre_ML_list0_imap_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret101;
##
  __patsflab_list0_imap_method:
  $tmpret101 = _ats2plpre_ML_list0_patsfun_58__closurerize($arg0);
  return $tmpret101;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_58($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret102;
##
  __patsflab__ats2plpre_ML_list0_patsfun_58:
  $tmpret102 = ats2plpre_ML_list0_imap($env0, $arg0);
  return $tmpret102;
} #end-of-function


sub
ats2plpre_ML_list0_map2($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret103;
  my $tmp104;
##
  __patsflab_list0_map2:
  $tmp104 = ats2plpre_list_map2($arg0, $arg1, $arg2);
  $tmpret103 = $tmp104;
  return $tmpret103;
} #end-of-function


sub
ats2plpre_ML_list0_mapcons($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret105;
  my $tmp106;
  my $tmp107;
  my $tmp108;
  my $tmp109;
##
  __patsflab_list0_mapcons:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab38:
    if(ATSCKptriscons($arg1)) { goto __atstmplab41; }
    __atstmplab39:
    $tmpret105 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab40:
    __atstmplab41:
    $tmp106 = $arg1->[0];
    $tmp107 = $arg1->[1];
    $tmp108 = [$arg0, $tmp106];
    $tmp109 = ats2plpre_ML_list0_mapcons($arg0, $tmp107);
    $tmpret105 = [$tmp108, $tmp109];
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret105;
} #end-of-function


sub
ats2plpre_ML_list0_find_opt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret110;
  my $tmp111;
  my $tmp112;
  my $tmp113;
##
  __patsflab_list0_find_opt:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab42:
    if(ATSCKptriscons($arg0)) { goto __atstmplab45; }
    __atstmplab43:
    $tmpret110 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab44:
    __atstmplab45:
    $tmp111 = $arg0->[0];
    $tmp112 = $arg0->[1];
    $tmp113 = &{$arg1->[0]}($arg1, $tmp111);
    if($tmp113) {
      $tmpret110 = [$tmp111];
    } else {
      #ATStailcalseq_beg
      $apy0 = $tmp112;
      $apy1 = $arg1;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab_list0_find_opt;
      #ATStailcalseq_end
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret110;
} #end-of-function


sub
ats2plpre_ML_list0_find_opt_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret114;
##
  __patsflab_list0_find_opt_method:
  $tmpret114 = _ats2plpre_ML_list0_patsfun_63__closurerize($arg0);
  return $tmpret114;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_63($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret115;
##
  __patsflab__ats2plpre_ML_list0_patsfun_63:
  $tmpret115 = ats2plpre_ML_list0_find_opt($env0, $arg0);
  return $tmpret115;
} #end-of-function


sub
ats2plpre_ML_list0_find_suffix($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret116;
  my $tmp118;
  my $tmp119;
##
  __patsflab_list0_find_suffix:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab46:
    if(ATSCKptriscons($arg0)) { goto __atstmplab49; }
    __atstmplab47:
    $tmpret116 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab48:
    __atstmplab49:
    $tmp118 = $arg0->[1];
    $tmp119 = &{$arg1->[0]}($arg1, $arg0);
    if($tmp119) {
      $tmpret116 = $arg0;
    } else {
      #ATStailcalseq_beg
      $apy0 = $tmp118;
      $apy1 = $arg1;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab_list0_find_suffix;
      #ATStailcalseq_end
    } #endif
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret116;
} #end-of-function


sub
ats2plpre_ML_list0_find_suffix_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret120;
##
  __patsflab_list0_find_suffix_method:
  $tmpret120 = _ats2plpre_ML_list0_patsfun_66__closurerize($arg0);
  return $tmpret120;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_66($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret121;
##
  __patsflab__ats2plpre_ML_list0_patsfun_66:
  $tmpret121 = ats2plpre_ML_list0_find_suffix($env0, $arg0);
  return $tmpret121;
} #end-of-function


sub
ats2plpre_ML_list0_zip($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret122;
##
  __patsflab_list0_zip:
  $tmpret122 = _ats2plpre_ML_list0_aux_68($arg0, $arg1);
  return $tmpret122;
} #end-of-function


sub
_ats2plpre_ML_list0_aux_68($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret123;
  my $tmp124;
  my $tmp125;
  my $tmp126;
  my $tmp127;
  my $tmp128;
  my $tmp129;
##
  __patsflab__ats2plpre_ML_list0_aux_68:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab50:
    if(ATSCKptriscons($arg0)) { goto __atstmplab53; }
    __atstmplab51:
    $tmpret123 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab52:
    __atstmplab53:
    $tmp124 = $arg0->[0];
    $tmp125 = $arg0->[1];
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab54:
      if(ATSCKptriscons($arg1)) { goto __atstmplab57; }
      __atstmplab55:
      $tmpret123 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab56:
      __atstmplab57:
      $tmp126 = $arg1->[0];
      $tmp127 = $arg1->[1];
      $tmp128 = [$tmp124, $tmp126];
      $tmp129 = _ats2plpre_ML_list0_aux_68($tmp125, $tmp127);
      $tmpret123 = [$tmp128, $tmp129];
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret123;
} #end-of-function


sub
ats2plpre_ML_list0_zipwith($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret130;
##
  __patsflab_list0_zipwith:
  $tmpret130 = _ats2plpre_ML_list0_aux_70($arg0, $arg1, $arg2);
  return $tmpret130;
} #end-of-function


sub
_ats2plpre_ML_list0_aux_70($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret131;
  my $tmp132;
  my $tmp133;
  my $tmp134;
  my $tmp135;
  my $tmp136;
  my $tmp137;
##
  __patsflab__ats2plpre_ML_list0_aux_70:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab58:
    if(ATSCKptriscons($arg0)) { goto __atstmplab61; }
    __atstmplab59:
    $tmpret131 = 0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab60:
    __atstmplab61:
    $tmp132 = $arg0->[0];
    $tmp133 = $arg0->[1];
    #ATScaseofseq_beg
    while(1)
    {
      #ATSbranchseq_beg
      __atstmplab62:
      if(ATSCKptriscons($arg1)) { goto __atstmplab65; }
      __atstmplab63:
      $tmpret131 = 0;
      last;
      #ATSbranchseq_end
      #ATSbranchseq_beg
      __atstmplab64:
      __atstmplab65:
      $tmp134 = $arg1->[0];
      $tmp135 = $arg1->[1];
      $tmp136 = &{$arg2->[0]}($arg2, $tmp132, $tmp134);
      $tmp137 = _ats2plpre_ML_list0_aux_70($tmp133, $tmp135, $arg2);
      $tmpret131 = [$tmp136, $tmp137];
      last;
      #ATSbranchseq_end
    } #end-of-while-loop;
    #ATScaseofseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret131;
} #end-of-function


sub
ats2plpre_ML_list0_zipwith_method($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret138;
##
  __patsflab_list0_zipwith_method:
  $tmpret138 = _ats2plpre_ML_list0_patsfun_72__closurerize($arg0, $arg1);
  return $tmpret138;
} #end-of-function


sub
_ats2plpre_ML_list0_patsfun_72($$$)
{
##
  my($env0, $env1, $arg0) = @_;
##
  my $tmpret139;
##
  __patsflab__ats2plpre_ML_list0_patsfun_72:
  $tmpret139 = ats2plpre_ML_list0_zipwith($env0, $env1, $arg0);
  return $tmpret139;
} #end-of-function


sub
ats2plpre_ML_list0_foldleft($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret140;
##
  __patsflab_list0_foldleft:
  $tmpret140 = _ats2plpre_ML_list0_aux_74($arg2, $arg1, $arg0);
  return $tmpret140;
} #end-of-function


sub
_ats2plpre_ML_list0_aux_74($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret141;
  my $tmp142;
  my $tmp143;
  my $tmp144;
##
  __patsflab__ats2plpre_ML_list0_aux_74:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab66:
    if(ATSCKptriscons($arg1)) { goto __atstmplab69; }
    __atstmplab67:
    $tmpret141 = $arg0;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab68:
    __atstmplab69:
    $tmp142 = $arg1->[0];
    $tmp143 = $arg1->[1];
    $tmp144 = &{$env0->[0]}($env0, $arg0, $tmp142);
    #ATStailcalseq_beg
    $apy0 = $tmp144;
    $apy1 = $tmp143;
    $arg0 = $apy0;
    $arg1 = $apy1;
    goto __patsflab__ats2plpre_ML_list0_aux_74;
    #ATStailcalseq_end
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret141;
} #end-of-function


sub
ats2plpre_ML_list0_foldright($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret145;
##
  __patsflab_list0_foldright:
  $tmpret145 = _ats2plpre_ML_list0_aux_76($arg1, $arg2, $arg0, $arg2);
  return $tmpret145;
} #end-of-function


sub
_ats2plpre_ML_list0_aux_76($$$$)
{
##
  my($env0, $env1, $arg0, $arg1) = @_;
##
  my $tmpret146;
  my $tmp147;
  my $tmp148;
  my $tmp149;
##
  __patsflab__ats2plpre_ML_list0_aux_76:
  #ATScaseofseq_beg
  while(1)
  {
    #ATSbranchseq_beg
    __atstmplab70:
    if(ATSCKptriscons($arg0)) { goto __atstmplab73; }
    __atstmplab71:
    $tmpret146 = $arg1;
    last;
    #ATSbranchseq_end
    #ATSbranchseq_beg
    __atstmplab72:
    __atstmplab73:
    $tmp147 = $arg0->[0];
    $tmp148 = $arg0->[1];
    $tmp149 = _ats2plpre_ML_list0_aux_76($env0, $env1, $tmp148, $env1);
    $tmpret146 = &{$env0->[0]}($env0, $tmp147, $tmp149);
    last;
    #ATSbranchseq_end
  } #end-of-while-loop;
  #ATScaseofseq_end
  return $tmpret146;
} #end-of-function


sub
ats2plpre_ML_list0_sort_2($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret152;
  my $tmp153;
##
  __patsflab_list0_sort_2:
  $tmp153 = ats2plpre_list_sort_2($arg0, $arg1);
  $tmpret152 = $tmp153;
  return $tmpret152;
} #end-of-function


sub
ats2plpre_ML_list0_mergesort($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret154;
  my $tmp155;
##
  __patsflab_list0_mergesort:
  $tmp155 = ats2plpre_list_mergesort($arg0, $arg1);
  $tmpret154 = $tmp155;
  return $tmpret154;
} #end-of-function


sub
ats2plpre_ML_streamize_list0_zip($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret156;
##
  __patsflab_streamize_list0_zip:
  $tmpret156 = ats2plpre_streamize_list_zip($arg0, $arg1);
  return $tmpret156;
} #end-of-function


sub
ats2plpre_ML_streamize_list0_cross($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret157;
##
  __patsflab_streamize_list0_cross:
  $tmpret157 = ats2plpre_streamize_list_cross($arg0, $arg1);
  return $tmpret157;
} #end-of-function

######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######
sub
_ats2plpre_ML_array0_patsfun_8__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_array0_patsfun_8($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_array0_patsfun_11__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_array0_patsfun_11($cenv->[1], $arg0); }, $env0];
}

sub
_ats2plpre_ML_array0_patsfun_17__closurerize($)
{
  my($env0) = @_;
  return [sub{ my($cenv, $arg0) = @_; return _ats2plpre_ML_array0_patsfun_17($cenv->[1], $arg0); }, $env0];
}


sub
ats2plpre_ML_array0_make_elt($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret0;
##
  __patsflab_array0_make_elt:
  $tmpret0 = ats2plpre_arrszref_make_elt($arg0, $arg1);
  return $tmpret0;
} #end-of-function


sub
ats2plpre_ML_array0_size($)
{
##
  my($arg0) = @_;
##
  my $tmpret1;
##
  __patsflab_array0_size:
  $tmpret1 = ats2plpre_arrszref_size($arg0);
  return $tmpret1;
} #end-of-function


sub
ats2plpre_ML_array0_length($)
{
##
  my($arg0) = @_;
##
  my $tmpret2;
##
  __patsflab_array0_length:
  $tmpret2 = ats2plpre_arrszref_size($arg0);
  return $tmpret2;
} #end-of-function


sub
ats2plpre_ML_array0_get_at($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret3;
##
  __patsflab_array0_get_at:
  $tmpret3 = ats2plpre_arrszref_get_at($arg0, $arg1);
  return $tmpret3;
} #end-of-function


sub
ats2plpre_ML_array0_set_at($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
##
  __patsflab_array0_set_at:
  ats2plpre_arrszref_set_at($arg0, $arg1, $arg2);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_array0_exch_at($$$)
{
##
  my($arg0, $arg1, $arg2) = @_;
##
  my $tmpret5;
##
  __patsflab_array0_exch_at:
  $tmpret5 = ats2plpre_arrszref_exch_at($arg0, $arg1, $arg2);
  return $tmpret5;
} #end-of-function


sub
ats2plpre_ML_array0_exists($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret6;
##
  __patsflab_array0_exists:
  $tmpret6 = ats2plpre_arrszref_exists_cloref($arg0, $arg1);
  return $tmpret6;
} #end-of-function


sub
ats2plpre_ML_array0_exists_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret7;
##
  __patsflab_array0_exists_method:
  $tmpret7 = _ats2plpre_ML_array0_patsfun_8__closurerize($arg0);
  return $tmpret7;
} #end-of-function


sub
_ats2plpre_ML_array0_patsfun_8($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret8;
##
  __patsflab__ats2plpre_ML_array0_patsfun_8:
  $tmpret8 = ats2plpre_ML_array0_exists($env0, $arg0);
  return $tmpret8;
} #end-of-function


sub
ats2plpre_ML_array0_forall($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret9;
##
  __patsflab_array0_forall:
  $tmpret9 = ats2plpre_arrszref_forall_cloref($arg0, $arg1);
  return $tmpret9;
} #end-of-function


sub
ats2plpre_ML_array0_forall_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret10;
##
  __patsflab_array0_forall_method:
  $tmpret10 = _ats2plpre_ML_array0_patsfun_11__closurerize($arg0);
  return $tmpret10;
} #end-of-function


sub
_ats2plpre_ML_array0_patsfun_11($$)
{
##
  my($env0, $arg0) = @_;
##
  my $tmpret11;
##
  __patsflab__ats2plpre_ML_array0_patsfun_11:
  $tmpret11 = ats2plpre_ML_array0_forall($env0, $arg0);
  return $tmpret11;
} #end-of-function


sub
array0_find_index($$)
{
##
  my($arg0, $arg1) = @_;
##
  my $tmpret12;
  my $tmp17;
##
  __patsflab_array0_find_index:
  $tmp17 = ats2plpre_ML_array0_size($arg0);
  $tmpret12 = _ats2plpre_ML_array0_loop_13($arg1, 0, $tmp17);
  return $tmpret12;
} #end-of-function


sub
_ats2plpre_ML_array0_loop_13($$$)
{
##
  my($env0, $arg0, $arg1) = @_;
##
  my $apy0;
  my $apy1;
  my $tmpret13;
  my $tmp14;
  my $tmp15;
  my $tmp16;
##
  __patsflab__ats2plpre_ML_array0_loop_13:
  $tmp14 = ats2plpre_lt_int0_int0($arg0, $arg1);
  if($tmp14) {
    $tmp15 = &{$env0->[0]}($env0, $arg0);
    if($tmp15) {
      $tmpret13 = $arg0;
    } else {
      $tmp16 = ats2plpre_add_int1_int1($arg0, 1);
      #ATStailcalseq_beg
      $apy0 = $tmp16;
      $apy1 = $arg1;
      $arg0 = $apy0;
      $arg1 = $apy1;
      goto __patsflab__ats2plpre_ML_array0_loop_13;
      #ATStailcalseq_end
    } #endif
  } else {
    $tmpret13 = ats2plpre_neg_int1(1);
  } #endif
  return $tmpret13;
} #end-of-function


sub
ats2plpre_ML_array0_app($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_array0_app:
  ats2plpre_ML_array0_foreach($arg0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_array0_foreach($$)
{
##
  my($arg0, $arg1) = @_;
##
##
  __patsflab_array0_foreach:
  ats2plpre_arrszref_foreach_cloref($arg0, $arg1);
  return;#_void
} #end-of-function


sub
ats2plpre_ML_array0_foreach_method($)
{
##
  my($arg0) = @_;
##
  my $tmpret20;
##
  __patsflab_array0_foreach_method:
  $tmpret20 = _ats2plpre_ML_array0_patsfun_17__closurerize($arg0);
  return $tmpret20;
} #end-of-function


sub
_ats2plpre_ML_array0_patsfun_17($$)
{
##
  my($env0, $arg0) = @_;
##
##
  __patsflab__ats2plpre_ML_array0_patsfun_17:
  ats2plpre_ML_array0_foreach($env0, $arg0);
  return;#_void
} #end-of-function

######
##
## end-of-compilation-unit
##
######
######
##
## The Perl code is generated by atscc2pl
## The starting compilation time is: 2017-10-23: 14h:29m
##
######
######
##
## end-of-compilation-unit
##
######

## ###### ###### ##

## end of [libatscc2pl_all.pl] ##
