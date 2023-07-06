################################################################
## `param_names` is a list of variable names whose values will
## be inserted into every script yaml header, over-riding
## values of the same name from the resources/'section'.qmd file,
## which in turn over-rides the values in resources/project.yml.
## Subsection-specific values in resources/'section'.yml
## will take precedence over any of the other three sources.
################################################################

param_names=res_dir VERSION TAG staging_dir


################################################################
## `sections` will get used to pick up script names in given 
## order, so name everything in the 'source-dir' 
## (`./resources` by default) after one one of these
################################################################

sections=Heatmap
sections=Serology


################################################################
## SLURM
## 
## Have default but customisable slurm parameters 
################################################################

SLURM--time=0-02:00:00
SLURM--mem=64G
SLURM--cpus-per-task=8
SLURM--partition=cpu

define slurm
#! /usr/bin/bash
#SBATCH --partition=$(SLURM--partition)
#SBATCH --time='$(SLURM--time)'
#SBATCH --cpus-per-task=$(SLURM--cpus-per-task)
#SBATCH --mem=$(SLURM--mem)
#SBATCH --job-name=$(notdir $(CURDIR))
#SBATCH --output=slurm-%x-%A_%a.out
endef
export slurm
