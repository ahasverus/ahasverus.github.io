make_hobbies <- function(html){

  section <- "hobbies"

  yaml <- yaml::yaml.load_file(file.path("data", paste0("_", section, ".yml")))
  info <- yaml::yaml.load_file(file.path("data", "_profile.yml"))
  lang <- names(info$languages[which(info$languages == TRUE)])

  header <- which(names(yaml) == "header")
  if (length(header) == 0){
    stop(paste0("Missing ", section, " header."))
  }


  asset <- ""

  asset <- paste0(asset, '<article>\n')
  asset <- paste0(asset, '  <p>')
  softs <- yaml$content$items
  for (j in 1:length(softs)){
    if ((length(softs[[j]]$item$name) == 1)){
      url <- which(names(softs[[j]]$item) == "url")
      if (length(url) > 0){
        if (j == 1){
          asset <- paste0(asset, '<a class="falink dotted" href="', softs[[j]]$item$url, '">')
          asset <- paste0(asset, softs[[j]]$item$name)
          asset <- paste0(asset, '</a>')
        } else {
          asset <- paste0(asset, '&nbsp;&middot;&nbsp;<a class="falink dotted" href="', softs[[j]]$item$url, '">')
          asset <- paste0(asset, softs[[j]]$item$name)
          asset <- paste0(asset, '</a>')
        }
      } else {
        if (j == 1){
          asset <- paste0(asset, softs[[j]]$item$name)
        } else {
          asset <- paste0(asset, '&nbsp;&middot;&nbsp;', softs[[j]]$item)
        }
      }
    }

    if (length(softs[[j]]$item$name) > 1){
      langs <- names(softs[[j]]$item$name)
      langs <- langs[which(langs %in% lang)]
      url <- which(names(softs[[j]]$item) == "url")
      for (k in langs){
        if (length(url) > 0){
          if (j == 1){
            asset <- paste0(asset, '<span lang="', substr(k, 1, 2), '"><a class="falink dotted" href="', softs[[j]]$item$url, '">')
            asset <- paste0(asset, softs[[j]]$item$name[[k]])
            asset <- paste0(asset, '</a></span>')
          } else {
            asset <- paste0(asset, '<span lang="', substr(k, 1, 2), '">&nbsp;&middot;&nbsp;<a class="falink dotted" href="', softs[[j]]$item$url, '">')
            asset <- paste0(asset, softs[[j]]$item$name[[k]])
            asset <- paste0(asset, '</a></span>')
          }
        } else {
          if (j == 1){
            asset <- paste0(asset, '<span lang="', substr(k, 1, 2), '">')
            asset <- paste0(asset, softs[[j]]$item$name[[k]])
            asset <- paste0(asset, '</span>')
          } else {
            asset <- paste0(asset, '<span lang="', substr(k, 1, 2), '">&nbsp;&middot;&nbsp;')
            asset <- paste0(asset, softs[[j]]$item$name[[k]])
            asset <- paste0(asset, '</span>')
          }
        }
      }
    }
  }
  asset <- paste0(asset, '</p>\n')
  asset <- paste0(asset, '  <div style="margin-bottom: 40px;"></div>')
  asset <- paste0(asset, '</article>\n')



  # ===   EXPORT ASSET

  dir.create("assets", showWarnings = FALSE)
  cat(asset, file = file.path("assets", paste0(section, ".html")), append = TRUE)
  paste0(html, '        <section id="', section, '" class="twelve columns"></section>\n')
}
