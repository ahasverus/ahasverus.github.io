make_button <- function(button){

  info <- yaml.load_file(file.path("data", "_profile.yml"))
  lang <- names(info$languages[which(info$languages == TRUE)])

  code <- ""
  if (length(button$name) == 1){
    if (button$available){
      if (length(grep("^http", button$url)) == 0){
        url <- list.files(pattern = paste0("^", button$url, "$"), recursive = TRUE)
        if (length(url) > 0){
          code <- paste0(code, '    <a class="btn-xs" href="', url, '">', button$name, '</a>\n')
        } else {
          stop(paste0("Unable to find ", button$url, "."))
        }
      } else {
        code <- paste0(code, '    <a class="btn-xs" href="', button$url, '">', button$name, '</a>\n')
      }
    } else {
      code <- paste0(code, '    <a class="btn-xs-not">', button$name, '</a>\n')
    }
  } else {
    langs <- names(button$name)
    langs <- langs[which(langs %in% lang)]
    for (k in langs){
      if (button$available){
        if (length(grep("^http", button$url)) == 0){
          url <- list.files(pattern = paste0("^", button$url, "$"), recursive = TRUE)
          if (length(url) > 0){
            code <- paste0(code, '    <a class="btn-xs" lang="', substr(k, 1, 2), '" href="', url, '">', button$name[[k]], '</a>\n')
          } else {
            stop(paste0("Unable to find ", button$url, "."))
          }
        } else {
          code <- paste0(code, '    <a class="btn-xs" lang="', substr(k, 1, 2), '" href="', button$url, '">', button$name[[k]], '</a>\n')
        }
      } else {
        code <- paste0(code, '    <a class="btn-xs-not" lang="', substr(k, 1, 2), '">', button$name[[k]], '</a>\n')
      }
    }
  }
  code
}
