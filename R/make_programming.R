make_programming <- function(html){

  section <- "programming"

  yaml <- yaml.load_file(file.path("data", paste0("_", section, ".yml")))
  info <- yaml.load_file(file.path("data", "_profile.yml"))
  lang <- names(info$languages[which(info$languages == TRUE)])

  header <- which(names(yaml) == "header")
  if (length(header) == 0){
    stop(paste0("Missing ", section, " header."))
  }


  asset <- ""


  # ===   GRADE

  categories <- yaml$content

  for (i in 1:length(categories)){

    asset <- paste0(asset, "\n")
    asset <- paste0(asset, "<!-- NEW CATEGORY           -->\n")
    asset <- paste0(asset, "<article>\n")
    asset <- paste0(asset, "  <header>\n")


    # ===   ADD CATEGORY NAME

    classe <- ifelse(
      test = (i == 1),
      yes  = ' class="family"',
      no   = ' class="family notfirst"'
      )

    if (length(yaml$content[[i]]$category$name) > 1){
      langs <- names(yaml$content[[i]]$category$name)
      langs <- langs[which(langs %in% lang)]
      for (k in langs){
        asset <- paste0(asset, '    <h1 lang="', substr(k, 1, 2), '"', classe, '>')
        asset <- paste0(asset, yaml$content[[i]]$category$name[[k]])
        asset <- paste0(asset, '</h1>\n')
      }
    } else {
      asset <- paste0(asset, '    <h1', classe, '>', yaml$content[[i]]$category$name, '</h1>\n')
    }

    asset <- paste0(asset, "  </header>\n")


    # ===   ADD SOFTWARES

    if (yaml$content[[i]]$category$type == "progressbar"){

      softs <- yaml$content[[i]]$category$items
      for (j in 1:length(softs)){
        asset <- paste0(asset, '\n')
        asset <- paste0(asset, '  <div style="float:left;">\n')
        asset <- paste0(asset, '    <div style="width:150px;float:left;margin-right:20px;">\n')
        asset <- paste0(asset, '      <div class="progress">\n')
        asset <- paste0(asset, '        <div style="width:', softs[[j]]$item$length, '%;" class="progress-bar">\n')
        asset <- paste0(asset, '          <em2>', softs[[j]]$item$length, '%</em2>\n')
        asset <- paste0(asset, '        </div>\n')
        asset <- paste0(asset, '      </div>\n')
        asset <- paste0(asset, '    </div>\n')
        asset <- paste0(asset, '    <div style="width:130px;float:left;">\n')
        asset <- paste0(asset, '      <em>', softs[[j]]$item$name, '</em>\n')
        asset <- paste0(asset, '    </div>\n')
        asset <- paste0(asset, '  </div>\n')

        if (((j/2) == floor(j/2) || j == length(softs))){
          asset <- paste0(asset, '\n')
          asset <- paste0(asset, '  <div style="clear:both;"></div>\n')
          asset <- paste0(asset, '\n')
        }
      }
    }

    if (yaml$content[[i]]$category$type == "list"){

      asset <- paste0(asset, '  <p>')

      softs <- yaml$content[[i]]$category$items
      for (j in 1:length(softs)){
        if ((length(softs[[j]]$item) == 1) && !is.null(softs[[j]]$item)){
          if (j == 1){
            asset <- paste0(asset, softs[[j]]$item)
          } else {
            asset <- paste0(asset, ' - ', softs[[j]]$item)
          }
        }
        if ((length(softs[[j]]$item) == 1) && is.null(softs[[j]]$item)){
          if (j == 1){
            asset <- paste0(asset, softs[[j]]$item$name)
          } else {
            asset <- paste0(asset, ', ', softs[[j]]$item$name)
          }
        }
        if (length(softs[[j]]$item) > 1){
          if (j > 1){
            asset <- paste0(asset, ', ')
          }
          url <- which(names(softs[[j]]$item) == "url")
          if (length(url) > 0){
            asset <- paste0(asset, '<a class="falink dotted" href="', softs[[j]]$item$url, '">')
            asset <- paste0(asset, softs[[j]]$item$name)
            asset <- paste0(asset, '</a>')
          } else {
            asset <- paste0(asset, softs[[j]]$item$name)
          }
        }
      }
      asset <- paste0(asset, '.</p>\n')

    }

    asset <- paste0(asset, "</article>\n")
    asset <- paste0(asset, '<div style="clear:both;"></div>')
  }


  # ===   EXPORT ASSET

  dir.create("assets", showWarnings = FALSE)
  cat(asset, file = file.path("assets", paste0(section, ".html")), append = TRUE)
  paste0(html, '        <section id="', section, '" class="twelve columns"></section>\n')
}
