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
