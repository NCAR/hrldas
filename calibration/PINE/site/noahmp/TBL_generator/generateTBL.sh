#PBS -l walltime=4:00:00
#PBS -P 11004044
#PBS -l select=1:ncpus=16:mem=32gb
###PBS -q ic007
#PBS -e generateTBL.sh_err.txt
#PBS -o generateTBL.sh_out.txt
#PBS -N generateTBL

module load cray-mpich
module load cray-pals
module load cray-netcdf cray-hdf5
module list

module load miniforge3
conda activate general

CODE_BASE="$HOME/scratch/NOAHMP/testrun"
TBL_BASE="$CODE_BASE/TBL_generator"

cd $TBL_BASE

python noahmp_apply_samples.py --samples noahmp_1000samples.txt --base_table NoahmpTable.TBL --out_root ./TropicalSites/ --n_rows 1000


