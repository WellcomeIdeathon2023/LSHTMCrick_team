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

