make_sectionheader <- function(section){

  yaml <- yaml.load_file(file.path("data", paste0("_", section, ".yml")))
  info <- yaml.load_file(file.path("data", "_profile.yml"))
  lang <- names(info$languages[which(info$languages == TRUE)])

  header <- which(names(yaml) == "header")
  if (length(header) > 0){


    # ===   ADD SECTION HEADER

    classe  <- ifelse(
      test = names(info$sections[which(info$sections == TRUE)])[1] == section,
      yes  = ' class="first" ',
      no   = " ")

    asset   <- ""
    icon <- which(names(yaml$header) == "icon")
    if (length(icon) > 0){
      size    <- yaml$header[[icon]]$size
      package <- yaml$header[[icon]]$package
      icon    <- yaml$header[[icon]]$name
      icon    <- paste0('<i class="', package, ' ', package, '-', icon, ' ', package, '-', size, 'x fa-fw"></i>&nbsp;&nbsp;&nbsp;')
    } else {
      icon <- ""
    }

    if (length(yaml$header$title) == 1){
      asset <- paste0(asset, '<h1', classe, '>')
      asset <- paste0(asset, icon)
      asset <- paste0(asset, yaml$header$title[[i]])
      asset <- paste0(asset, '</h1>\n')
    } else {
      langs <- names(yaml$header$title)
      langs <- langs[which(langs %in% lang)]
      for (i in lang){
        asset <- paste0(asset, '<h1', classe, 'lang="', substr(i, 1, 2), '">')
        asset <- paste0(asset, icon)
        asset <- paste0(asset, yaml$header$title[[i]])
        asset <- paste0(asset, '</h1>\n')
      }
    }
  } else {
    asset <- ""
  }


  # ===   EXPORT ASSET

  dir.create("assets", showWarnings = FALSE)
  cat(asset, file = file.path("assets", paste0(section, ".html")), append = FALSE)

}
