make_teaching <- function(html){

  section <- "teaching"

  yaml <- yaml::yaml.load_file(file.path("data", paste0("_", section, ".yml")))
  info <- yaml::yaml.load_file(file.path("data", "_profile.yml"))
  lang <- names(info$languages[which(info$languages == TRUE)])

  header <- which(names(yaml) == "header")
  if (length(header) == 0){
    stop(paste0("Missing ", section, " header."))
  }


  asset <- ""


  # ===   course

  courses <- yaml$content

  for (i in 1:length(courses)){

    asset <- paste0(asset, "\n")
    asset <- paste0(asset, "<!-- NEW COURSE           -->\n")
    asset <- paste0(asset, "<article>\n")
    asset <- paste0(asset, "  <header>\n")


    # ===   ADD course NAME

    if (length(yaml$content[[i]]$course[["title"]]) > 1){
      langs <- names(yaml$content[[i]]$course[["title"]])
      langs <- langs[which(langs %in% lang)]
      for (k in langs){
        asset <- paste0(asset, '    <h1 lang="', substr(k, 1, 2), '">')
        asset <- paste0(asset, yaml$content[[i]]$course[["title"]][[k]])
        asset <- paste0(asset, '</h1>\n')
      }
    } else {
      asset <- paste0(asset, '    <h1>', yaml$content[[i]]$course[["title"]], '</h1>\n')
    }


    # ===   ADD INSTITUTION NAME AND URL

    pos <- which(names(yaml$content[[i]]$course$institution) %in% lang)

    # No NAME name found and multi-languages
    if (length(pos) > 0){
      langs <- names(yaml$content[[i]]$course$institution)
      langs <- langs[which(langs %in% lang)]
      for (k in langs){
        asset <- paste0(asset, '    <span lang="', substr(k, 1, 2), '">')
        asset <- paste0(asset, yaml$content[[i]]$course$institution[[k]])
        asset <- paste0(asset, '</span>\n')
      }

    # Tag NAME found
    } else {
      name <- which(names(yaml$content[[i]]$course$institution) == "name")
      if (length(name) > 0){

        # Multi-languages
        if (length(yaml$content[[i]]$course$institution$name) > 1){
          langs <- names(yaml$content[[i]]$course$institution$name)
          langs <- langs[which(langs %in% lang)]
          for (k in langs){
            asset <- paste0(asset, '    <span lang="', substr(k, 1, 2), '">')
            asset <- paste0(asset, yaml$content[[i]]$course$institution$name[[k]])
            asset <- paste0(asset, '</span>\n')
          }

        # One single language
        } else {
          asset <- paste0(asset, '    <span>')
          asset <- paste0(asset, yaml$content[[i]]$course$institution$name)
          asset <- paste0(asset, '</span>\n')
        }

      # No tag found (institution: nameof institution)
      } else {
        if (length(yaml$content[[i]]$course$institution) == 1){
          asset <- paste0(asset, '    <span>', yaml$content[[i]]$course$institution, '</span>\n')
        }
      }


      # ===   ADD INSTITUTION CITY, COUNTRY AND TIME

      pos <- which(names(yaml$content[[i]]$course$institution) %in% c("city", "country", "period"))
      if (length(pos) > 0){

        ncity    <- length(yaml$content[[i]]$course$institution$city)
        ncountry <- length(yaml$content[[i]]$course$institution$country)
        nperiod  <- length(yaml$content[[i]]$course$institution$period)

        # Multi-languages
        if (ncity > 1 | ncountry > 1 | nperiod > 1){
          langs <- unique(c(
            names(yaml$content[[i]]$course$institution$city),
            names(yaml$content[[i]]$course$institution$country),
            names(yaml$content[[i]]$course$institution$period)))
          langs <- langs[which(langs %in% lang)]
          for (k in langs){
            asset <- paste0(asset, '    <span lang="', substr(k, 1, 2), '">')
            if (ncity > 0){
              if (ncity > 1){
                asset <- paste0(asset, '<address>', yaml$content[[i]]$course$institution$city[[k]])
              } else {
                asset <- paste0(asset, '<address>', yaml$content[[i]]$course$institution$city)
              }
              if (ncountry > 0){
                if (ncountry > 1){
                  asset <- paste0(asset, ', ', yaml$content[[i]]$course$institution$country[[k]])
                } else {
                  asset <- paste0(asset, ', ', yaml$content[[i]]$course$institution$country)
                }
              }
              asset <- paste0(asset, '</address>')
            } else {
              if (ncountry > 0){
                if (ncountry > 1){
                  asset <- paste0(asset, '<address>', yaml$content[[i]]$course$institution$country[[k]], '</address>')
                } else {
                  asset <- paste0(asset, '<address>', yaml$content[[i]]$course$institution$country, '</address>')
                }
              }
            }
            if (nperiod > 0){
              if (nperiod > 1){
                asset <- paste0(asset, '<time>', yaml$content[[i]]$course$institution$period[[k]], '</time>')
              } else {
                asset <- paste0(asset, '<time>', yaml$content[[i]]$course$institution$period, '</time>')
              }
            }
            asset <- paste0(asset, '</span>\n')
          }

        # One single language
        } else {
          asset <- paste0(asset, '    <span>')
          if (ncity > 0){
            asset <- paste0(asset, '<address>', yaml$content[[i]]$course$institution$city)
            if (ncountry > 0){
              asset <- paste0(asset, ', ', yaml$content[[i]]$course$institution$country)
            }
            asset <- paste0(asset, '</address>')
          } else {
            if (ncountry > 0){
              asset <- paste0(asset, '<address>', yaml$content[[i]]$course$institution$country, '</address>')
            }
          }
          if (nperiod > 0){
            asset <- paste0(asset, '<time>', yaml$content[[i]]$course$institution$period, '</time>')
          }
          asset <- paste0(asset, '</span>\n')
        }
      }
    }
    asset <- paste0(asset, "  </header>\n")


    # ===   ADD COLLABORATORS

    collabos <- yaml$content[[i]]$course$collaborator

    if (length(collabos) > 0){

      # One single language
      if (length(collabos[[1]]$collaborator$prefixe) == 1){

        asset <- paste0(asset, '  <p>\n    ')

        for (j in 1:length(collabos)){

          pretexte <- collabos[[j]]$collaborator$prefixe
          asset <- paste0(asset, pretexte, '\n')
          texte    <- collabos[[j]]$collaborator$name
          if (!is.null(collabos[[j]]$collaborator$url)){
            url   <- collabos[[j]]$collaborator$url
            asset <- paste0(asset, '    <a class="falink dotted" href="', url, '">', texte, '</a>\n')
          }
          if (!is.null(collabos[[j]]$collaborator$github)){
            url   <- paste0("https://www.github.com/", collabos[[j]]$collaborator$github)
            asset <- paste0(asset, '    <a alt="GitHub" title="GitHub" href="', url, '">')
            asset <- paste0(asset, '<i class="fa fa-github fa-1x fa-fw falink"></i></a>\n')
          } else {
            asset <- paste0(asset, '\n')
          }
        }
        asset <- paste0(asset, '  </p>\n')


      # Multi-languages
      } else {

        langs <- names(collabos[[1]]$collaborator$prefixe)
        langs <- langs[which(langs %in% lang)]

        for (k in langs){

          asset <- paste0(asset, '  <p lang="', substr(k, 1, 2), '">\n')

          for (j in 1:length(collabos)){

            pretexte <- collabos[[j]]$collaborator$prefixe[[k]]
            asset <- paste0(asset, '    ', pretexte, '\n')
            texte    <- collabos[[j]]$collaborator$name
            if (!is.null(collabos[[j]]$collaborator$url)){
              url   <- collabos[[j]]$collaborator$url
              asset <- paste0(asset, '    <a class="falink dotted" href="', url, '">', texte, '</a>\n')
            }
            if (!is.null(collabos[[j]]$collaborator$github)){
              url   <- paste0("https://www.github.com/", collabos[[j]]$collaborator$github)
              asset <- paste0(asset, '    <a alt="GitHub" title="GitHub" href="', url, '">')
              asset <- paste0(asset, '<i class="fa fa-github fa-1x fa-fw falink"></i></a>\n')
            } else {
              asset <- paste0(asset, '\n')
            }
          }
          asset <- paste0(asset, '  </p>\n')
        }
      }
    }


    # ===   ADD BUTTONS

    buttons <- yaml$content[[i]]$course$buttons
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
