project?=SDY180
.PHONY: makefile global.mk secrets.mk $(project).mk
include global.mk          #Sysadmin settings for executibles, standard folders  etc.
include $(project).mk #Provides analysis parametrisation - primarily the `sections` variable
include secrets.mk         #define SINGULARITY_ROOT, RENV_PATHS_ROOT, RENV_PATHS_PREFIX

# Include info about what each section's html and qmd files are
include $(patsubst %,$(staging_dir)/.make-%,$(sections))
# And aggregate them into an overall list
html_reports=$(strip $(foreach section,$(sections),$($(section)_htmls)))
scripts=$(strip $(foreach section,$(sections),$($(section)_scripts)))


.PHONY: analyses
analyses: $(scripts)  $(staging_dir)/_quarto.yml $(staging_dir)/index.qmd | $(log_dir) data $(RESULTS_DIR)/$(VERSION) R.bib## Generate the analysis report
	$(QUARTO) render '$(staging_dir)' \
	 --output-dir $(RESULTS_DIR)/$(VERSION)\
	 --execute-dir $(CURDIR) $(call log,render)
	cp -rT $(staging_dir)/$(RESULTS_DIR)/$(VERSION) $(RESULTS_DIR)/$(VERSION)
	ln -sfn  $(VERSION) $(RESULTS_DIR)/latest $(call log,$@)

$(html_reports) : $(staging_dir)/$(RESULTS_DIR)/$(VERSION)/%$(TAG).html : $(staging_dir)/%$(TAG).qmd R.bib $(staging_dir)/_quarto.yml $(staging_dir)/index.qmd
	$(QUARTO) render '$<' \
	  --output-dir $(RESULTS_DIR)/$(VERSION) \
	  --execute-dir $(CURDIR) \
	$(call log,html-$*)

html-%: $(staging_dir)/$(RESULTS_DIR)/$(VERSION)/%$(TAG).html ## Create a single html file to be placed in the staging directory, '%' is a simplified filename e.g. `make html-PCA_1`.

R.bib: renv.lock
	$(R) -e 'knitr::write_bib(names(renv:::lockfile("renv.lock")$$data()$$Packages), file="$@",prefix="pkg_")' $(call log,bib)
	$(call git-ignore,/$@)

renv.lock: rocker.Renviron | $(log_dir)
	$(R) -e "library(renv); init(bioconductor=TRUE)" $(call log,renv)

.PHONY: scripts
scripts: $(scripts) ## Generate all the parametrised scripts, into the staging directory. But don't render
$(scripts) : $(staging_dir)/%$(TAG).qmd : $(staging_dir)/$(project).yml $(source_dir)/$(project).yml
	$(QUARTO) run $(source_dir)/typescript/merge_yml.ts\
	 --script=$(source_dir)/$(section).qmd\
	 --yml=$(source_dir)/$(section).yml\
	 --subsection=$(patsubst $(section)_%,%,$*)\
	 --section=$(section)\
	 --fixed=$(source_dir)/$(project).yml\
	 --generated=$(staging_dir)/$(project).yml > $@
	sed -i -e 's#load_params("$(source_dir)/$(section)", extra=TRUE)#load_params("$(staging_dir)/$*$(TAG)")#' $@

# Generate the sidebar grouped by sections
${staging_dir}/_quarto.yml: $(source_dir)/_quarto.yml $(scripts)
	$(QUARTO) run $(source_dir)/typescript/sidebar.ts\
	 --sections="$(subst $(space),$(comma),$(sections))"\
	 --template=$< \
	 --tag=$(TAG)\
	 --staging=$(staging_dir)\
	 --repo="$(repo)" > $@

$(source_dir)/$(project).yml:
	echo "" > $@

$(staging_dir)/$(project).yml:
	echo '{$(foreach p,$(param_names),$p: "${$p}"$(comma))}' > $@

${staging_dir}/index.qmd : $(source_dir)/index.qmd $(scripts)
	sed  's#^  contents: .*#  contents: [$(subst $(space),$(comma),$(patsubst $(staging_dir)/%,%,$(scripts)))]#' $< > $@

$(log_dir) $(staging_dir) data $(RESULTS_DIR)/$(VERSION) :
	mkdir -p $@

# Create a makefile per section, that contains a variable derived from the
# source-file yaml stanzas.  The variable `section`_pages, will
# contain a list of all subsections, from which are derived variables,
# containing filenames of the qmd scripts and html pages. Ensure the
# scripts have dependencies on the parental qmd and yml file
define section_make
$(1)_pages=$(2)
$(1)_scripts=$$(patsubst %,$(staging_dir)/%$$(TAG).qmd,$${$(1)_pages})
$(1)_htmls=$$(patsubst %,$(staging_dir)/$(RESULTS_DIR)/$$(VERSION)/%$$(TAG).html,$${$(1)_pages})
$(1)_htmls: $$($(1)_htmls)
$${$(1)_scripts}: section=$(1)
$${$(1)_scripts}: $(source_dir)/$(1).qmd
$${$(1)_scripts}: $$(wildcard $(source_dir)/$(1).yml)
endef

mky=$(patsubst $(source_dir)/%.yml,$(staging_dir)/.make-%,$(wildcard $(source_dir)/*.yml))
$(mky): $(staging_dir)/.make-%: $(source_dir)/%.yml | $(staging_dir)
	echo -e '$(subst $(newline),\n,$(call section_make,$*,$*_$(shell seq -s " $*_" `grep ^- $< | wc -l`)))' > $@

$(staging_dir)/.make-% : | $(staging_dir)
	echo -e '$(subst $(newline),\n,$(call section_make,$*,$*_1))' > $@



location=outputs
shortcut=$(if $(redirect_$(location)),shortcuts/,)
pubdir = $(or $(shortcut)$(publish_$(location)),published)
publish:  ## Copy results to final location. Either 'outputs' folder (default), intranet (`make publish location=intranet`) or internet
ifdef redirect_$(location)
	mkdir -p $(redirect_$(location))
ifdef url_$(location)
	mkdir -p $(shortcut)
	echo "[InternetShortcut]" > $(shortcut)$(location).url
	echo "URL=$(url_$(location))/latest" >> $(shortcut)$(location).url
endif
	ln -sfn $(redirect_$(location)) $(pubdir)
endif
	mkdir -p $(pubdir)/$(VERSION)
	cp --no-preserve=mode -r $(RESULTS_DIR)/$(VERSION)/. $(pubdir)/$(VERSION)
	ln -sfn $(VERSION) $(pubdir)/latest
	[ ! -f .htaccess ] || [ "$(location)" == "outputs" ] || cp --no-preserve=mode -r .htaccess $(pubdir)/
	[ -f .htaccess ] || [ "$(location)" == "outputs" ] || echo "Warning: no .htaccess files, so unauthorised people may be able to view these results"
	ln -sfn $$(realpath $(pubdir)/$(VERSION)) $(RESULTS_DIR)/published_$(location)


rocker.Renviron: 
	echo "RENV_PATHS_PREFIX=$(RENV_PATHS_PREFIX)" > $@
	echo "RENV_PATHS_ROOT=$(RENV_PATHS_ROOT)" >> $@
	echo "RENV_PATHS_LIBRARY=renv/library" >> $@

.PHONY: R-local
R-local: $(CONTAINER_IMAGE) rocker.Renviron ## Create a local shell script that will run R (optional, but helpful for interactive analyses)
	echo "#!/bin/bash" > $@
	echo 'function R { $(CONTAINER) $(CONTAINER_FLAGS_INTERACTIVE) $(CONTAINER_IMAGE) R' \$$@  " ; }" >> $@
	echo 'function Rscript { $(CONTAINER) $(CONTAINER_FLAGS_INTERACTIVE) $(CONTAINER_IMAGE) Rscript' \$$@  " ; }" >> $@
	echo 'function conshell { $(CONTAINER_SHELL) ; }' >> $@
	echo "[[ "'$$BASH_SOURCE'" == "'$$0'" ]] && R " \$$@ >> $@
	$(make_rwx) $@
	$(call git-ignore,$@)


################################################################
## Cleaners
################################################################
clean: ## Remove caches and purely intermediate files
	rm -f $(log_dir)/*.log
	rm -rf $(staging_dir)
	rm -f slurm-$(notdir $(CURDIR))*.out



