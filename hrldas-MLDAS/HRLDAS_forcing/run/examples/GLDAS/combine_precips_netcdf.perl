#!/usr/bin/perl

$filename = "combine_precips_netcdf.exe";
system ("make clean");
system ("make");
@nums = ("00","01","02","03","04","05","06","07","08","09", 
         "10","11","12","13","14","15","16","17","18","19", 
	 "20","21","22","23","24","25","26","27","28","29", 
	 "30","31");
	 
@yrs = ("17");

$day_start = 114;
$day_end = 116;

@hrs = ("00","03","06","09","12","15","18","21");

@noleap_days = (0,31,59,90,120,151,181,212,243,273,304,334,365);
@leap_days   = (0,31,60,91,121,152,182,213,244,274,305,335,366);

$data_dir = "/glade/work/zhezhang/GLDAS/extracted/";
$results_dir = "/glade/work/zhezhang/GLDAS/extracted/";

for $yy (@yrs)
 {
# This will be the jday time loop

for($julday=$day_start;$julday<=$day_end;$julday++)

 { 
 
 # This little section finds the text month and day
 
 @modays = @noleap_days;
 if($yy == "00" || $yy == "04" || $yy == "08" || $yy == "12") {@modays = @leap_days}

 for($mo=1;$mo<=12;$mo++)
  {
    if($julday>$modays[$mo-1] && $julday<=$modays[$mo]) 
     {
       $mon = $mo;
       $day = $julday - $modays[$mo-1];
     }
  }

for $hr (@hrs)
 {
  $file1_in  =    "$data_dir/Rainf/GLDAS_Rainf_tavg_20$yy$nums[$mon]$nums[$day]$hr";
  $file2_in  =    "$data_dir/Snowf/GLDAS_Snowf_tavg_20$yy$nums[$mon]$nums[$day]$hr";
  $file_out  =  "$results_dir/Precip/GLDAS_Precip_20$yy$nums[$mon]$nums[$day]$hr";
#  print ("$file_out \n");
  system ("./$filename $file1_in $file2_in $file_out");
 }
 }
 }

system("rm $filename");
