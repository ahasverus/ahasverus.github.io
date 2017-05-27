make_sectionfooter <- function(section){

  # ===   ADD JAVASCRIPT LOADING

  asset <- ""
  url <- list.files(pattern = paste0("^language-selector\\.js$"), recursive = TRUE)
  if (length(url) > 0){
    asset <- paste0(asset, '\n')
    asset <- paste0(asset, '<script type="text/javascript" src="', url, '"></script>\n')
  } else {
    stop("Missing javascript file.")
  }


  # ===   EXPORT ASSET

  dir.create("assets", showWarnings = FALSE)
  cat(asset, file = file.path("assets", paste0(section, ".html")), append = TRUE)
}
