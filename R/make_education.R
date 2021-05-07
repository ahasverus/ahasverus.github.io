make_education <- function(html){

  section <- "education"

  yaml <- yaml.load_file(file.path("data", paste0("_", section, ".yml")))
  info <- yaml.load_file(file.path("data", "_profile.yml"))
  lang <- names(info$languages[which(info$languages == TRUE)])

  header <- which(names(yaml) == "header")
  if (length(header) == 0){
    stop(paste0("Missing ", section, " header."))
  }


  asset <- ""


  # ===   GRADE

  grades <- yaml$content

  for (i in 1:length(grades)){

    asset <- paste0(asset, "\n")
    asset <- paste0(asset, "<!-- NEW GRADE           -->\n")
    asset <- paste0(asset, "<article>\n")
    asset <- paste0(asset, "  <header>\n")


    # ===   ADD grade NAME

    if (length(yaml$content[[i]]$grade[["title"]]) > 1){
      langs <- names(yaml$content[[i]]$grade[["title"]])
      langs <- langs[which(langs %in% lang)]
      for (k in langs){
        asset <- paste0(asset, '    <h1 lang="', substr(k, 1, 2), '">')
        asset <- paste0(asset, yaml$content[[i]]$grade[["title"]][[k]])
        asset <- paste0(asset, '</h1>\n')
      }
    } else {
      asset <- paste0(asset, '    <h1>', yaml$content[[i]]$grade[["title"]], '</h1>\n')
    }


    # ===   ADD INSTITUTION NAME AND URL

    pos <- which(names(yaml$content[[i]]$grade$institution) %in% lang)

    # No NAME name found and multi-languages
    if (length(pos) > 0){
      langs <- names(yaml$content[[i]]$grade$institution)
      langs <- langs[which(langs %in% lang)]
      for (k in langs){
        asset <- paste0(asset, '    <span lang="', substr(k, 1, 2), '">')
        asset <- paste0(asset, yaml$content[[i]]$grade$institution[[k]])
        asset <- paste0(asset, '</span>\n')
      }

    # Tag NAME found
    } else {
      name <- which(names(yaml$content[[i]]$grade$institution) == "name")
      if (length(name) > 0){

        # Multi-languages
        if (length(yaml$content[[i]]$grade$institution$name) > 1){
          langs <- names(yaml$content[[i]]$grade$institution$name)
          langs <- langs[which(langs %in% lang)]
          for (k in langs){
            asset <- paste0(asset, '    <span lang="', substr(k, 1, 2), '">')
            url <- which(names(yaml$content[[i]]$grade$institution) == "url")

            # Institution URL
            if (length(url) > 0){
              asset <- paste0(asset, '<a class="falink dotted" href="', yaml$content[[i]]$grade$institution$url, '">')
              asset <- paste0(asset, yaml$content[[i]]$grade$institution$name[[k]])
              asset <- paste0(asset, '</a>')

            # No institution URL
            } else {
              asset <- paste0(asset, yaml$content[[i]]$grade$institution$name[[k]])
            }
            asset <- paste0(asset, '</span>\n')
          }

        # One single language
        } else {
          asset <- paste0(asset, '    <span>')
          url <- which(names(yaml$content[[i]]$grade$institution) == "url")

          # Institution URL
          if (length(url) > 0){
            asset <- paste0(asset, '<a class="falink dotted" href="', yaml$content[[i]]$grade$institution$url, '">')
            asset <- paste0(asset, yaml$content[[i]]$grade$institution$name)
            asset <- paste0(asset, '</a>')


          # No institution URL
          } else {
            asset <- paste0(asset, yaml$content[[i]]$grade$institution$name)
          }
          asset <- paste0(asset, '</span>\n')
        }

      # No tag found (institution: nameof institution)
      } else {
        if (length(yaml$content[[i]]$grade$institution) == 1){
          asset <- paste0(asset, '    <span>', yaml$content[[i]]$grade$institution, '</span>\n')
        }
      }


      # ===   ADD INSTITUTION CITY, COUNTRY AND TIME

      pos <- which(names(yaml$content[[i]]$grade$institution) %in% c("city", "country", "period"))
      if (length(pos) > 0){

        ncity    <- length(yaml$content[[i]]$grade$institution$city)
        ncountry <- length(yaml$content[[i]]$grade$institution$country)
        nperiod  <- length(yaml$content[[i]]$grade$institution$period)

        # Multi-languages
        if (ncity > 1 | ncountry > 1 | nperiod > 1){
          langs <- unique(c(
            names(yaml$content[[i]]$grade$institution$city),
            names(yaml$content[[i]]$grade$institution$country),
            names(yaml$content[[i]]$grade$institution$period)))
          langs <- langs[which(langs %in% lang)]
          for (k in langs){
            asset <- paste0(asset, '    <span lang="', substr(k, 1, 2), '">')
            if (ncity > 0){
              if (ncity > 1){
                asset <- paste0(asset, '<address>', yaml$content[[i]]$grade$institution$city[[k]])
              } else {
                asset <- paste0(asset, '<address>', yaml$content[[i]]$grade$institution$city)
              }
              if (ncountry > 0){
                if (ncountry > 1){
                  asset <- paste0(asset, ', ', yaml$content[[i]]$grade$institution$country[[k]])
                } else {
                  asset <- paste0(asset, ', ', yaml$content[[i]]$grade$institution$country)
                }
              }
              asset <- paste0(asset, '</address>')
            } else {
              if (ncountry > 0){
                if (ncountry > 1){
                  asset <- paste0(asset, '<address>', yaml$content[[i]]$grade$institution$country[[k]], '</address>')
                } else {
                  asset <- paste0(asset, '<address>', yaml$content[[i]]$grade$institution$country, '</address>')
                }
              }
            }
            if (nperiod > 0){
              if (nperiod > 1){
                asset <- paste0(asset, '<time>', yaml$content[[i]]$grade$institution$period[[k]], '</time>')
              } else {
                asset <- paste0(asset, '<time>', yaml$content[[i]]$grade$institution$period, '</time>')
              }
            }
            asset <- paste0(asset, '</span>\n')
          }

        # One single language
        } else {
          asset <- paste0(asset, '    <span>')
          if (ncity > 0){
            asset <- paste0(asset, '<address>', yaml$content[[i]]$grade$institution$city)
            if (ncountry > 0){
              asset <- paste0(asset, ', ', yaml$content[[i]]$grade$institution$country)
            }
            asset <- paste0(asset, '</address>')
          } else {
            if (ncountry > 0){
              asset <- paste0(asset, '<address>', yaml$content[[i]]$grade$institution$country, '</address>')
            }
          }
          if (nperiod > 0){
            asset <- paste0(asset, '<time>', yaml$content[[i]]$grade$institution$period, '</time>')
          }
          asset <- paste0(asset, '</span>\n')
        }
      }
    }
    asset <- paste0(asset, "  </header>\n")


    # ===   ADD THESIS

    if (length(yaml$content[[i]]$grade$thesis) > 0){
      asset <- paste0(asset, "\n")
      asset <- paste0(asset, '  <div style="margin-bottom: 10px;"></div>\n')
      asset <- paste0(asset, "\n")
      asset <- paste0(asset, "  <!-- THESIS           -->\n")

      # One single language
      if (length(yaml$content[[i]]$grade$thesis) == 1){
        asset <- paste0(asset, '  <p></em>', yaml$content[[i]]$grade$thesis, '</em></p>\n')

      # Multi-languages
      } else {
        langs <- names(yaml$content[[i]]$grade$thesis)
        langs <- langs[which(langs %in% lang)]
        for (k in langs){
          asset <- paste0(asset, '  <p lang="', substr(k, 1, 2), '"><em>', yaml$content[[i]]$grade$thesis[[k]], '</em></p>\n')
        }
      }
    }


    # ===   ADD BUTTONS

    buttons <- yaml$content[[i]]$grade$buttons
    if (!is.null(buttons)){
      asset <- paste0(asset, '\n')
      asset <- paste0(asset, '  <div class="btn1">\n')
      for (z in 1:length(buttons)){
        asset <- paste0(asset, make_button(buttons[[z]]$button))
      }
      asset <- paste0(asset, '  </div>\n')
    }

    asset <- paste0(asset, "</article>\n")
  }


  # ===   EXPORT ASSET

  dir.create("assets", showWarnings = FALSE)
  cat(asset, file = file.path("assets", paste0(section, ".html")), append = TRUE)
  paste0(html, '        <section id="', section, '" class="twelve columns"></section>\n')
}
