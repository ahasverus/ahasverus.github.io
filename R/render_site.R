###
###
###



# setwd("~/MEGA/webdesign/ahasverus.github.io")


### ===   LOAD FUNCTIONS
library(yaml)
scripts <- list.files(path = "R", pattern = "^make_[a-z]{1,}\\.R$", full.names = TRUE)
scripts <- sapply(scripts, function(x) source(x))


### ===   CREATE HTML HEADER

html <- make_htmlheader()


### ===   ADD PROFILE

html <- make_profile(html)


### ===   ADD SECTIONS

index      <- yaml.load_file(file.path("data", "_profile.yml"))
sections   <- names(index$sections[which(index$sections == TRUE)])

if (length(sections) > 0){

  for (i in sections){

    make_sectionheader(section = i)
    html <- eval(parse(text = paste0("make_", i, "(html)")))
    make_sectionfooter(section = i)
  }
}


### ===   ADD PAGE FOOTER

html <- make_pagefooter(html)


### ===   ADD JS LOADING

html <- make_htmlfooter(html)


### ===   WRITE INDEX PAGE

cat(html, file = "index.html", append = FALSE)


### ===   OPEN WEBSITE
# system("open index.html")


### ===   PUBLISH ON GITHUB
# if (length(geterrmessage()) == 0){
  # system("git add -A")
  # system("git commit -m ''")
  # system("git push")
# }
