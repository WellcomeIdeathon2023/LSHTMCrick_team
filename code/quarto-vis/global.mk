.DEFAULT_GOAL=help

R=R
RVERSION=4.2.2
QUARTO=quarto
GIT=git
EXECUTOR=singularity
EXEC=bash
DOCKER = gavinpaulkelly/verse-boost

source_dir=resources
staging_dir=staging-${project}
RESULTS_DIR = results-${project}
res_dir = $(RESULTS_DIR)/$(VERSION)
publish_intranet=www_internal
publish_internet=www_external
publish_outputs=outputs
log_dir=logs

# Nothing beyond this point _should_ need customising
ml = module is-loaded $1 || module load $1
# Git variables
TAG := _$(shell $(GIT) describe --tags --dirty=_altered --always --long 2>/dev/null || echo "uncontrolled")# e.g. v1.0.2-2-ace1729a
VERSION := $(shell $(GIT) describe --tags --abbrev=0 2>/dev/null || echo "vX.Y.Z")#e.g. v1.0.2
git-ignore=touch .gitignore && grep -qxF '$(1)' .gitignore || echo '$(1)' >> .gitignore

make_rwx = setfacl -m u::rwx

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

################################################################
## Wrappers
################################################################
define bash_head
#!/bin/bash
set -e
endef
export bash_head

slurm-%:
	bash_script=$$(mktemp -u $(staging_dir)/bash_XXXXX) ;\
	echo "$$bash_head" > $${bash_script} ;\
	make -n -s $* >> $${bash_script} ;\
	echo "$$slurm" > $${bash_script}_slurm ;\
	echo "$(EXEC) $${bash_script}" >> $${bash_script}_slurm ;\
	echo 'r=$$?' >>  $${bash_script}_slurm ;\
	echo '$(notification)' >> $${bash_script}_slurm ;\
	echo "rm -f $${bash_script}" >> $${bash_script}_slurm ;\
	sbatch $${bash_script}_slurm ;\
	rm -f $${bash_script}_slurm

scontain-% contain-%: EXEC=$(CONTAINER) $(CONTAINER_FLAGS) $(CONTAINER_IMAGE) bash
scontain-%: slurm-% 
	true

contain-%: $(CONTAINER_IMAGE)
	bash_script=$$(mktemp -u $(staging_dir)/bash_XXXXX) ;\
	echo "$$bash_head" > $${bash_script} ;\
	make -n -s $* >> $${bash_script} ;\
	$(EXEC) "$${bash_script}" ;\
	r=$$? ;\
	rm -f $${bash_script}} ;\
	exit $$r

################################################################
## Reproducible containers
##
## Above, we set a default value of EXECUTOR. This can be over-
## ridden at the command line, e.g.
## `make contain-target EXECUTOR=singularity|docker`
################################################################
BIND_DIR = $(shell ${GIT} rev-parse --show-toplevel || echo ${CURDIR})

ifeq (${EXECUTOR},singularity)
CONTAINER= $(call ml,Singularity/3.6.4); singularity
CONTAINER_IMAGE=$(SINGULARITY_ROOT)/$(notdir $(DOCKER))_$(RVERSION).sif
CONTAINER_BIND=--bind $(BIND_DIR),/tmp,$(RENV_PATHS_ROOT),$(CURDIR)/rocker.Renviron:/usr/local/lib/R/etc/Renviron.site
CONTAINER_ENV=--env SQLITE_TMPDIR=/tmp
CONTAINER_FLAGS= exec $(CONTAINER_BIND) --pwd $(CURDIR) --containall --cleanenv $(CONTAINER_ENV)
CONTAINER_FLAGS_INTERACTIVE= exec $(CONTAINER_BIND),$${HOME}/.emacs.d,$${HOME}/.Xauthority --pwd $(CURDIR) --containall --cleanenv $(CONTAINER_ENV),DISPLAY=$${DISPLAY}
CONTAINER_SHELL = $(CONTAINER) $(patsubst exec,shell,$(CONTAINER_FLAGS_INTERACTIVE)) $(CONTAINER_IMAGE)
$(CONTAINER_IMAGE): | rocker.Renviron
	cd $(dir $(CONTAINER_IMAGE)) ;\
	$(CONTAINER) pull docker://$(DOCKER):$(RVERSION)

else ifeq (${EXECUTOR},docker)
CONTAINER=docker
CONTAINER_IMAGE=$(DOCKER):$(RVERSION)
CONTAINER_FLAGS=run \
--mount type=bind,source="$(BIND_DIR)",target="$(BIND_DIR)" \
--mount type=bind,source="/tmp",target="/tmp" \
--mount type=bind,source="$(CURDIR)/rocker.Renviron",target="/usr/local/lib/R/etc/Renviron.site" \
--workdir="$(CURDIR)" $(CONTAINER_IMAGE)
	mkdir -p $(dir $(CONTAINER_IMAGE))
	touch $(CONTAINER_IMAGE)
CONTAINER_SHELL = $(CONTAINER) $(patsubst run,exec -it,$(CONTAINER_FLAGS_INTERACTIVE)) $(CONTAINER_IMAGE) /bin/bash
$(CONTAINER_IMAGE): | rocker.Renviron
	$(CONTAINER) pull docker://$(DOCKER):$(RVERSION)
	echo "Proxy for docker image" > $(DOCKER):$(RVERSION)

endif


#Standard makefile hacks
MAKEFLAGS += --no-builtin-rules

log=2>&1 | tee $2 $(log_dir)/$1.log

comma:= ,
space:= $() $()
empty:= $()
define newline

$(empty)
endef

ifdef NTFY
notification= [[ $$r = 0 ]] && \
curl -H "Title: SLURM submission complete" -H "Tags: +1" -d "Finished $*" ntfy.sh/$(NTFY) || \
curl -H "Title: SLURM submission failed"   -H "Tags: warning" -d "Failed $*: status $$r" ntfy.sh/$(NTFY)
endif


################################################################
# Standard Goals
################################################################

print-%: ## `make print-varname` will show varname's value
	@echo "$*"="$($*)"

$(V).SILENT: 

.PHONY: help
help: ## Show help message
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m\033[0m\n"} /^[$$()% 0-9a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)
