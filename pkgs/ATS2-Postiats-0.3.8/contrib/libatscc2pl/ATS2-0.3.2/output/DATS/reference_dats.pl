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
