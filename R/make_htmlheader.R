make_htmlheader <- function(){

  yaml   <- yaml.load_file(file.path("data", "_profile.yml"))
  header <- which(names(yaml) == "header")
  html   <- "<!DOCTYPE html>\n<html>\n  <head>\n"


  # ===   HEADER SPECIFICATIONS

  if (length(header) > 0){


    # ===   ENCODING

    encoding <- which(names(yaml[[header]]) == "encoding")
    if (length(encoding) > 0){
      encoding <- yaml[[header]][[encoding]]
    } else {
      encoding <- "UTF-8"
    }
    html <- paste0(html, paste0('    <meta charset="', encoding, '">\n'))


    # ===   OTHER < yaml > TAGS

    for (i in c("author", "description", "keywords")){
      tag <- which(names(yaml[[header]]) == i)
      if (length(tag) > 0){
        html <- paste0(html, paste0('    <meta name="', i, '" content="', yaml[[header]][[tag]], '">\n'))
      }
    }


  # ===   NO HEADER SPECIFICATION

  } else {
    html <- paste0(html, '    <meta charset="UTF-8">\n')
  }


  # ===   ADD VIEWPORT

  html <- paste0(html, '    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1">\n')


  # ===   ADD TITLE

  if (length(header) > 0){
    title <- which(names(yaml[[header]]) == "title")
    if (length(title) > 0){
      html <- paste0(html, '\n')
      html <- paste0(html, '    <title>', yaml[[header]][[title]], '</title>\n')
    }
  }


  # ===   ADD STYLESHEETS

  css <- list.files(pattern = ".css$", recursive = TRUE)
  if (length(css) > 5){
    html <- paste0(html, '\n')
    for (i in css){
      html <- paste0(html, '    <link type="text/css" rel="stylesheet" href="', i, '">\n')
    }
  } else {
    stop("Missing stylesheets in layout/css/")
  }


  # ===   ADD FAVICON

  if (length(header) > 0){
    favicon <- which(names(yaml[[header]]) == "favicon")
    if (length(favicon) > 0){
      html <- paste0(html, '\n')
      favicon <- list.files(pattern = paste0("^", yaml[[header]][[favicon]], "$"), recursive = TRUE)
      if (length(favicon) > 0){
        html <- paste0(html, '    <link type="favicon" rel="shorcut icon" href="', favicon, '">', '</title>\n')
      } else {
        stop("Unable to find favicon.")
      }
    }
  }


  # ===   FOR THIS F****** IE

  html <- paste0(html, '\n')
  html <- paste0(html, '    <!--[if lt IE 9]>\n')
  html <- paste0(html, '      <script src="http://html5shim.googlecode.com/svn/trunk/html5.js"></script>\n')
  html <- paste0(html, '    <![endif]-->\n')


  # ===   CLOSE HEADER

  html <- paste0(html, '\n')
  paste0(html, '  </head>\n')

}
