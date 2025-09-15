### Project Intro

HRLDAS + Noah‑MP (Fortran) for simulations and Python ML for analysis/training on Derecho/Casper. Keep code safe in Home, run I/O in Scratch, retain results in Work, and promote long‑lived assets to Campaign.

### Storage Layout (Best Practice)

- Home (`/glade/u/home/wukoutian`)
  - `hrldas/` — driver repo (with git branches)
  - `hrldas/noahmp/` — model repo
  -  `../Ori_RPM/hrldas_phs/` - phs hrldas repo
- Work (`/glade/work/wukoutian`)
  - `noahmp/inputs/` — simulation inputs (authoritative copy)
  - `noahmp/results/` — retained outputs by run_id
  - `ml/datasets/` — curated training/validation/test shards
  - `ml/checkpoints/` — active model checkpoints
- Scratch (`/glade/derecho/scratch/wukoutian`)
  - `noahmp/runs/<run_id>/` — per-simulation working dirs
  - `ml/experiments/<exp_id>/` — per-training run workspaces
- Campaign (`/glade/campaign/univ/utaa0012/`)
  - `datasets/{raw,curated}/`, `models/releases/`, `manifests/`, `results/<YYYY>/<run_id>/`

Guideline: Stage inputs to Scratch for each run; write intermediates to Scratch; copy minimal results to Work; promote stable datasets/models to Campaign. Do not train directly from Campaign.

### HRLDAS + Noah‑MP Workflow

- Repos in Home; builds and runs in Scratch; inputs from Work; archive results to Work.
- If Noah‑MP is separate from HRLDAS, pin versions via Git submodule.

### Git/worktree plan

Use HRLDAS as the superproject with a single `noahmp/` submodule; create and develop Noah‑MP feature branches on your fork (`feature/wood`, `feature/rock`, `feature/ai`) for model changes; create thin HRLDAS branches (`main`, `wood`, `rock`, `ai`) and worktrees at `/glade/u/home/$USER/hrldas-{main,wood,rock,ai}` to pin each variant to a specific `noahmp` commit; in each worktree `git submodule update --init`, check out the target `noahmp` feature commit in detached HEAD, commit the submodule pointer, then build/run there; never create `noahmp_*` directories—always use `noahmp/`; push only to your fork; put build/output under Scratch per branch.

/glade/u/home/wukoutian/
├── hrldas-env/                    # Shared environment configs
│   ├── environment.yml            # Conda/mamba environment
│   ├── modules.sh                 # Module loads for Derecho/Casper
│   ├── paths.env                  # Common paths (Work/Scratch/Campaign)
│   └── build-config.sh            # Compiler flags, build options
├── hrldas/                        # Main worktree
│   └── noahmp/
├── hrldas-wood/                   # Wood variant worktree
│   └── noahmp/
├── hrldas-rock/                   # Rock variant worktree
│   └── noahmp/
└── hrldas-ai/                     # AI variant worktree
    └── noahmp/

- HRLDAS (superproject)
  - `main` → `noahmp` @ `NCAR/noahmp:develop` (baseline)
  - `wood` → `noahmp` @ `fork/feature/wood`
  - `rock` → `noahmp` @ `fork/feature/rock`
  - `ai`   → `noahmp` @ `fork/feature/ai`
- Worktrees
  - `/glade/u/home/$USER/hrldas-main` (branch `main`)
  - `/glade/u/home/$USER/hrldas-wood` (branch `wood`)
  - `/glade/u/home/$USER/hrldas-rock` (branch `rock`)
  - `/glade/u/home/$USER/hrldas-ai`   (branch `ai`)
- Environment config
  - `/glade/u/home/$USER/hrldas-env/` (separate Git repo: `environment.yml`, `modules.sh`, `paths.env`)
  - Symlink `env/ -> ../hrldas-env/` in each worktree; source in build/run scripts

### Hardened Job Templates (Derecho/Casper)

Slurm (Derecho) — Noah‑MP CPU run with provenance
```bash
#!/bin/bash
#SBATCH -J noahmp
#SBATCH -A <account>
#SBATCH -N 1
#SBATCH -t 02:00:00
RUN_ID=$(date +%Y%m%d_%H%M%S)
SCR=/glade/derecho/scratch/$USER/noahmp/runs/$RUN_ID
WRK=/glade/work/$USER/noahmp
mkdir -p "$SCR" "$WRK/results"
cp -r "$WRK/inputs" "$SCR"
cd "$SCR"
# srun ./noahmp.exe ...
rsync -av output/ "$WRK/results/$RUN_ID/"
```

PBS (Derecho) — ML GPU training with provenance
```bash
#!/bin/bash
#PBS -N noahmp_ml
#PBS -A <account>
#PBS -q derecho
#PBS -l select=1:ncpus=16:ngpus=1:mem=64GB
#PBS -l walltime=02:00:00
set -euo pipefail
module load conda || true
source activate noahmp-ml || conda activate noahmp-ml || true

EXP=$(date +%Y%m%d_%H%M%S)
SCR=/glade/derecho/scratch/$USER/ml/experiments/$EXP
WRK=/glade/work/$USER/ml
mkdir -p "$SCR" "$WRK/checkpoints"
rsync -a "$WRK/datasets/" "$SCR/datasets/"
cd "$SCR"

# Example training
python -m train --data ./datasets --out ./outputs
rsync -av "$SCR/outputs/best.ckpt" "$WRK/checkpoints/$EXP.ckpt"
```

### Reproducibility

- Use a conda/mamba env tracked by `environment.yml` or an Apptainer SIF image. Record toolchain, package versions, and checksums for promoted datasets/models.

### Campaign Guidance (Placeholder)

- Confirm root with CISL; suggested structure above. Campaign is not backed up and not for active training; use it to preserve curated datasets, released models, and manifests.

 

