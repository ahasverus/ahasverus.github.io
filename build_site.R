###
### BUILD WEBSITE
###


## Install 'devtools' package ----

if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")


## Install required packages ----

devtools::install_deps()


## Load functions and packages ----

devtools::load_all()


## Create HTML header ----

html <- make_htmlheader()


## Add profile ----

html <- make_profile(html)


## Add sections ----

index    <- yaml::yaml.load_file(file.path("data", "_profile.yml"))
sections <- names(index$"sections"[which(index$"sections" == TRUE)])

if (length(sections) > 0) {

  for (i in sections) {

    make_sectionheader(section = i)
    html <- eval(parse(text = paste0("make_", i, "(html)")))
    make_sectionfooter(section = i)
  }
}


## Add page footer ----

html <- make_pagefooter(html)


## Add JavaScript loading ----

html <- make_htmlfooter(html)


## Write 'index.html' file ----

cat(html, file = "index.html", append = FALSE)
