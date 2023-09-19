#!/bin/bash

#################################################################################################
#                                                                                               #
#                             Alessandro Anav         JULY 2023                                 #
#                                                                                               #
#      This script extracts and prepares the variables needed to run NOAH-MP from ERA5-Land     # 
#      The program expects one single file per year, namely data should span from               #
#                  YYYY-01-01_00:00:00 to YYYY-12-31_23:00:00                                   #
#                                                                                               #
#                   For questions or issues: alessandro.anav@enea.it                            #
#                                                                                               #
#################################################################################################


#################################################################################################
#                                                                                               #
#                                        USER DEFINED INPUT FILES                               # 
#                                                                                               #
#################################################################################################

year=1980
INFILE=/fas4/anav/INPUTS/ERA5_for_Noah-MP/YEARLY/ERA5-Land_Noah-MP_$year.grb

let last_year=$year-1
PREVIOUS_YEAR=/fas4/anav/INPUTS/ERA5_for_Noah-MP/YEARLY/ERA5-Land_Noah-MP_$last_year.grb

#################################################################################################
#                                                                                               #
#                                        INPUT VARIABLES                                        # 
#                                                                                               #
#################################################################################################

vars_name=("10U" "10V" "2T" "2D" "SP" "SSRD" "STRD" "TP")
vars_nout=("10U" "10V" "2T" "2D" "SP" "ACSSRD" "ACSTRD" "ACTP")
vars_code=("var165" "var166" "var167" "var168" "var134" "var169" "var175" "var228")

#################################################################################################
#                                                                                               #
#                                        START PROGRAM                                          # 
#                                                                                               #
#################################################################################################

echo ""
echo -e "\033[33m Processing YEAR:  $year  FILE: $INFILE  \033[0m"
echo ""

#-------------------------------------------------------------------------- CREATING OUTPUT DIRECTORIES

mkdir -p OUTPUTS

for i in ${!vars_name[@]}; do
  mkdir -p OUTPUTS/${vars_name[$i]}
done

mkdir -p OUTPUTS/E
mkdir -p OUTPUTS/Q

#################################################################################################
#                                                                                               #
#                                    PART 1: EXTRACT TIME-STEP VARIABLES                        # 
#                                                                                               #
#################################################################################################

echo -e "\033[32m o-----------------> PART 1: EXTRACTING TIME-STEP VARIABLES \033[0m"

#-------------------------------------------------------------------------- SPLIT THE VARIABLES FROM INPUT FILE
grib_copy $INFILE   OUTPUTS/[shortName].grb

for i in ${!vars_name[@]}; do
   echo -e "\033[36m PROCESSING ${vars_nout[$i]} \033[0m"
   var=${vars_name[$i]}
   #-------------------------------------------------------------------------- SPLIT TIME
   codes_split_file -1 OUTPUTS/${var,,}.grb

   #-------------------------------------------------------------------------- LOOP OVER FILES TO RENAME THEM ACCORDING TO THE DATE 
   for f1 in $(ls OUTPUTS/${var,,}.grb_*); do
      date=(`cdo -s showtimestamp $f1`)
      timestamp=$(sed -e 's/T/''/g' -e 's/:00:00/''/g' -e 's/-/''/g' <<< $date)
      mv $f1 OUTPUTS/${vars_name[$i]}/ERA5_"${vars_nout[$i]}"_"${timestamp}" 
   done
done
echo ""

#################################################################################################
#                                                                                               #
#                                    PART 2: DECUMULATE FIELDS                                  # 
#                                                                                               #
#################################################################################################

echo -e "\033[32m o-----------------> PART 2: DECUMULATE FIELDS \033[0m"

#-------------------------------------------------------------------------- EXTRACT THE ACCUMULATED VARIABLES FROM THE LAST TIME STEP OF PREVIOUS YEAR

cdo -s -selvar,var169 -seldate,$last_year-12-31T23:00:00,$last_year-12-31T23:00:00 $PREVIOUS_YEAR OUTPUTS/SSRD/ERA5_ACSSRD_"$last_year"123123
cdo -s -selvar,var175 -seldate,$last_year-12-31T23:00:00,$last_year-12-31T23:00:00 $PREVIOUS_YEAR OUTPUTS/STRD/ERA5_ACSTRD_"$last_year"123123
cdo -s -selvar,var228 -seldate,$last_year-12-31T23:00:00,$last_year-12-31T23:00:00 $PREVIOUS_YEAR OUTPUTS/TP/ERA5_ACTP_"$last_year"123123

#-------------------------------------------------------------------------- DECUMULATE VARIABLES
for i in ${!vars_nout[@]}; do
   if [ ${vars_nout[$i]} = "ACSSRD" ] || [ ${vars_nout[$i]} = "ACSTRD" ] || [ ${vars_nout[$i]} = "ACTP" ] ; then
            
      echo -e "\033[36m PROCESSING ${vars_nout[$i]} \033[0m"

      FILES=OUTPUTS/${vars_name[$i]}/ERA5_"${vars_nout[$i]}"_"$year"* 
      prev_file="OUTPUTS/${vars_name[$i]}/ERA5_"${vars_nout[$i]}"_""$last_year"123123 

      for f in $FILES; do
         # Find the hour reading the last two digits of the filename
         file=$(basename "$f")
         hour="${file: -2}"

         # Create the output file name replacing the name of accumulated variable with instantaneous variable  
         outfile=$(sed 's/'${vars_nout[$i]}'/'${vars_name[$i]}'/g' <<< $f)

         # Decumulate the 02-24 fields and copy the 01 data from accumulated to instantaneous variable
         if [[ "$hour" = "01" ]]; then
            echo -e "\033[32m We are at 01: cp $f to $outfile  \033[0m"
            cp $f $outfile  
         else
            echo -e "\033[33m Decumulating: $f less $prev_file saved to $outfile  \033[0m"
            cdo -s -sub $f $prev_file $outfile
         fi
         prev_file=$f
      done
      echo ""
   fi
done
echo ""

#################################################################################################
#                                                                                               #
#                               PART 3: COMPUTE SPECIFIC HUMIDITY                               # 
#                                                                                               #
#################################################################################################

# Three steps are taken for calculating specific humidity
# (1) calculate saturated water pressure e from Td 
# (2) combine e and SP
# (3) calculate q from e and SP

echo -e "\033[32m o-----------------> PART 3: COMPUTING SPECIFIC HUMIDITY \033[0m"

FILES=OUTPUTS/2D/ERA5_2D_"$year"*
 
for f in $FILES; do
   outfile1=$(sed 's/'2D'/'E'/g' <<< $f)
   echo -e "\033[33m Computing saturated water pressure from $f and save results to $outfile1  \033[0m"
   cdo -s -expr,"var1=6.112*exp(17.67*(var168-273.15)/(var168-273.15+243.5))" $f $outfile1 

   mkdir -p OUTPUTS/TMP
   SP_file=$(sed 's/'2D'/'SP'/g' <<< $f)
   outfile2=$(sed 's/'2D'/'TMP'/g' <<< $f)
   echo -e "\033[33m Merging $outfile1 and $SP_file to  $outfile2 \033[0m"
   cdo -s merge $outfile1 $SP_file $outfile2

   outfile3=$(sed 's/'2D'/'Q'/g' <<< $f)
   cdo -s -setparam,133.128 -expr,"var133=(0.622*var1)/(var134/100.-(0.378*var1))/1000." $outfile2 $outfile3
done
echo ""

#################################################################################################
#                                                                                               #
#                                       CLEANING UP                                             # 
#                                                                                               #
#################################################################################################

rm OUTPUTS/*.grb
rm OUTPUTS/TP/ERA5_ACTP_"$year"*
rm OUTPUTS/SSRD/ERA5_ACSSRD_"$year"*
rm OUTPUTS/STRD/ERA5_ACSTRD_"$year"*
rm -rf OUTPUTS/TMP
rm -rf OUTPUTS/E

#################################################################################################
#                                                                                               #
#                                        NICE ENDING MESSAGE                                    # 
#                                                                                               #
#################################################################################################

echo -e "\033[32m o-----------------> ALL DONE  \033[0m"
echo ""
