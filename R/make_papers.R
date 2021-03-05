make_papers <- function(html){

  section <- "papers"

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


    # === SORT ITEMS

    idx     <- 1:length(yaml$content[[i]]$category$items)
    authors <- unlist(lapply(yaml$content[[i]]$category$items, function(x) x$item$authors))
    authors <- factor(authors, levels = sort(unique(authors)))
    years   <- unlist(lapply(yaml$content[[i]]$category$items, function(x) x$item$year))
    years   <- factor(years, levels = rev(sort(unique(years))))
    status  <- unlist(lapply(yaml$content[[i]]$category$items, function(x) x$item$status))
    status  <- factor(status, levels = c("submitted", "accepted", "published"))

    dat <- data.frame(idx, years, status, authors)
    dat <- dat[with(dat, order(years, status, authors)), ]
    yaml$content[[i]]$category$items <- yaml$content[[i]]$category$items[dat$idx]


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


    items <- yaml$content[[i]]$category$items

    for (j in 1:length(items)){

      if (items[[j]]$item$type == "article"){

        asset <- paste0(asset, '  <p class="project">\n')
        if (!is.null(yaml$style$emphaze)){
          authors <- gsub(yaml$style$emphaze, paste0("<em>", yaml$style$emphaze, "</em>"), items[[j]]$item$authors)
        }
        asset <- paste0(asset, authors, ' ')
        asset <- paste0(asset, '(', items[[j]]$item$year, ') ')
        asset <- paste0(asset, items[[j]]$item$title, '. ')
        asset <- paste0(asset, '<em>', items[[j]]$item$editor, '</em>, ')

        if (items[[j]]$item$status == "published"){
          if (!is.null(items[[j]]$item$volume))
            asset <- paste0(asset, items[[j]]$item$volume, ', ')
          asset <- paste0(asset, items[[j]]$item$pages, '.\n')
        }
        if (items[[j]]$item$status %in% c("accepted", "submitted")){
          asset <- paste0(asset, items[[j]]$item$status, '.\n')
        }

        asset <- paste0(asset, '  </p>')
      }

      if (items[[j]]$item$type == "book"){

        asset <- paste0(asset, '  <p class="project">\n')
        if (!is.null(yaml$style$emphaze)){
          authors <- gsub(yaml$style$emphaze, paste0("<em>", yaml$style$emphaze, "</em>"), items[[j]]$item$authors)
        }
        asset <- paste0(asset, authors, ' ')
        asset <- paste0(asset, '(', items[[j]]$item$year, ') ')
        asset <- paste0(asset, items[[j]]$item$title, '. ')
        asset <- paste0(asset, '<em>', items[[j]]$item$editor, '</em>, ')

        if (items[[j]]$item$status == "published"){
          asset <- paste0(asset, items[[j]]$item$city, ', ')
          asset <- paste0(asset, items[[j]]$item$country, ', ')
          asset <- paste0(asset, items[[j]]$item$pages, ' pp.')
        }
        if (items[[j]]$item$status %in% c("accepted", "submitted")){
          asset <- paste0(asset, items[[j]]$item$status, '.\n')
        }
        asset <- paste0(asset, '  </p>')
      }

      if (items[[j]]$item$type == "chapter"){

        asset <- paste0(asset, '  <p class="project">\n')
        if (!is.null(yaml$style$emphaze)){
          authors <- gsub(yaml$style$emphaze, paste0("<em>", yaml$style$emphaze, "</em>"), items[[j]]$item$authors)
        }
        asset <- paste0(asset, authors, ' ')
        asset <- paste0(asset, '(', items[[j]]$item$year, ') ')
        asset <- paste0(asset, items[[j]]$item$title, '. In ')
        asset <- paste0(asset, items[[j]]$item$book, '. ')
        asset <- paste0(asset, '<em>', items[[j]]$item$editor, '</em>, pp ')

        if (items[[j]]$item$status == "published"){
          asset <- paste0(asset, items[[j]]$item$pages, '.')
        }
        if (items[[j]]$item$status %in% c("accepted", "submitted")){
          asset <- paste0(asset, items[[j]]$item$status, '.\n')
        }
        asset <- paste0(asset, '  </p>')
      }


      # ===   ADD BUTTONS

      buttons <- items[[j]]$item$buttons
      if (!is.null(buttons)){
        asset <- paste0(asset, '\n')
        asset <- paste0(asset, '  <div class="btn3">\n')
        for (z in 1:length(buttons)){
          asset <- paste0(asset, make_button(buttons[[z]]$button))
        }
        asset <- paste0(asset, '  </div>\n')
      }

      asset <- paste0(asset, '  <div style="margin-bottom: 20px;"></div>\n')
    }
    asset <- paste0(asset, "</article>\n")
  }

  asset <- gsub('\\?\\.', '?', asset)
  asset <- gsub('\\!\\.', '!', asset)
  asset <- gsub('\\.\\.', '.', asset)

  # ===   EXPORT ASSET

  dir.create("assets", showWarnings = FALSE)
  cat(asset, file = file.path("assets", paste0(section, ".html")), append = TRUE)
  paste0(html, '        <section id="', section, '" class="twelve columns"></section>\n')
}
