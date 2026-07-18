#'
#' Update Quarto website
#'

## Install required packages ----

# install.packages("pak")
pak::local_install_deps(dependencies = TRUE)


## Load project ----

pkgload::load_all()


## Update content ----

# update_bibliography()
# update_publication_stats()

update_software_metadata()


## Update website ----

