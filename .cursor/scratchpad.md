### Project Intro

HRLDAS + Noah‑MP (Fortran) for simulations and Python ML for analysis/training on Derecho/Casper. Keep code safe in Home, run I/O in Scratch, retain results in Work, and promote long‑lived assets to Campaign.

### Storage Layout (Best Practice)

- Home (`/glade/u/home/wukoutian`)
  - `hrldas/` — driver repo
  - `hrldas/noahmp/` — model repo
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

### Git Layout (variants: main, wood, rock, ai)

- Use one repo with branches `main`, `wood`, `rock`, `ai`.
- Create parallel worktrees for simultaneous builds/runs:
```bash
# from repo root (e.g., ~/noahmp)
git checkout -b main
git checkout -b phs
git checkout -b wood
git checkout -b rock
git checkout -b ai
mkdir -p ~/noahmp-worktrees
git worktree add ~/noahmp-worktrees/main     main
git worktree add ~/noahmp-worktrees/phs      phs
git worktree add ~/noahmp-worktrees/wood     wood
git worktree add ~/noahmp-worktrees/rock     rock
git worktree add ~/noahmp-worktrees/ai       ai
```
Build artifacts should go to Scratch (e.g., `/glade/derecho/scratch/$USER/noahmp/build-wood/`).

### Minimal Job Templates

Slurm (Derecho) — Noah‑MP CPU run
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

PBS (Casper) — ML GPU training
```bash
#!/bin/bash
#PBS -N noahmp_ml
#PBS -A <account>
#PBS -q casper
#PBS -l select=1:ncpus=16:ngpus=1:mem=64GB
#PBS -l walltime=02:00:00
module load conda
source activate noahmp-ml
EXP=$(date +%Y%m%d_%H%M%S)
SCR=/glade/derecho/scratch/$USER/ml/experiments/$EXP
WRK=/glade/work/$USER/ml
mkdir -p "$SCR" "$WRK/checkpoints"
rsync -av "$WRK/datasets/" "$SCR/datasets/"
cd "$SCR"
python -m train --data ./datasets --out ./outputs
rsync -av "$SCR/outputs/best.ckpt" "$WRK/checkpoints/$EXP.ckpt"
```

### Reproducibility

- Use a conda/mamba env tracked by `environment.yml` or an Apptainer SIF image. Record toolchain, package versions, and checksums for promoted datasets/models.

### Campaign Guidance (Placeholder)

- Confirm root with CISL; suggested structure above. Campaign is not backed up and not for active training; use it to preserve curated datasets, released models, and manifests.
