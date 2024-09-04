make_experience <- function(html){

  section <- "experience"

  yaml <- yaml::yaml.load_file(file.path("data", paste0("_", section, ".yml")))
  info <- yaml::yaml.load_file(file.path("data", "_profile.yml"))
  lang <- names(info$languages[which(info$languages == TRUE)])

  header <- which(names(yaml) == "header")
  if (length(header) == 0){
    stop(paste0("Missing ", section, " header."))
  }


  asset <- ""


  # ===   JOB

  jobs <- yaml$content

  for (i in 1:length(jobs)){

    asset <- paste0(asset, "\n")
    asset <- paste0(asset, "<!-- NEW JOB           -->\n")
    asset <- paste0(asset, "<article>\n")
    asset <- paste0(asset, "  <header>\n")


    # ===   ADD JOB NAME

    if (length(yaml$content[[i]]$job[["title"]]) > 1){
      langs <- names(yaml$content[[i]]$job[["title"]])
      langs <- langs[which(langs %in% lang)]
      for (k in langs){
        asset <- paste0(asset, '    <h1 lang="', substr(k, 1, 2), '">')
        asset <- paste0(asset, "&diams;&nbsp;&nbsp;", yaml$content[[i]]$job[["title"]][[k]])
        asset <- paste0(asset, '</h1>\n')
      }
    } else {
      asset <- paste0(asset, '    <h1>', "&diams;&nbsp;&nbsp;", yaml$content[[i]]$job[["title"]], '</h1>\n')
    }


    # ===   ADD INSTITUTION NAME AND URL

    pos <- which(names(yaml$content[[i]]$job$institution) %in% lang)

    # No NAME name found and multi-languages
    if (length(pos) > 0){
      langs <- names(yaml$content[[i]]$job$institution)
      langs <- langs[which(langs %in% lang)]
      for (k in langs){
        asset <- paste0(asset, '    <span lang="', substr(k, 1, 2), '">')
        asset <- paste0(asset, yaml$content[[i]]$job$institution[[k]])
        asset <- paste0(asset, '</span>\n')
      }

    # Tag NAME found
    } else {
      name <- which(names(yaml$content[[i]]$job$institution) == "name")
      if (length(name) > 0){

        # Multi-languages
        if (length(yaml$content[[i]]$job$institution$name) > 1){
          langs <- names(yaml$content[[i]]$job$institution$name)
          langs <- langs[which(langs %in% lang)]
          for (k in langs){
            asset <- paste0(asset, '    <span lang="', substr(k, 1, 2), '">')
            url <- which(names(yaml$content[[i]]$job$institution) == "url")

            # Institution URL
            if (length(url) > 0){
              asset <- paste0(asset, '<a class="falink dotted" href="', yaml$content[[i]]$job$institution$url, '">')
              asset <- paste0(asset, yaml$content[[i]]$job$institution$name[[k]])
              asset <- paste0(asset, '</a>')

            # No institution URL
            } else {
              asset <- paste0(asset, yaml$content[[i]]$job$institution$name[[k]])
            }
            asset <- paste0(asset, '</span>\n')
          }

        # One single language
        } else {
          asset <- paste0(asset, '    <span>')
          url <- which(names(yaml$content[[i]]$job$institution) == "url")

          # Institution URL
          if (length(url) > 0){
            asset <- paste0(asset, '<a class="falink dotted" href="', yaml$content[[i]]$job$institution$url, '">')
            asset <- paste0(asset, yaml$content[[i]]$job$institution$name)
            asset <- paste0(asset, '</a>')


          # No institution URL
          } else {
            asset <- paste0(asset, yaml$content[[i]]$job$institution$name)
          }
          asset <- paste0(asset, '</span>\n')
        }

      # No tag found (institution: nameof institution)
      } else {
        if (length(yaml$content[[i]]$job$institution) == 1){
          asset <- paste0(asset, '    <span>', yaml$content[[i]]$job$institution, '</span>\n')
        }
      }


      # ===   ADD INSTITUTION CITY, COUNTRY AND TIME

      pos <- which(names(yaml$content[[i]]$job$institution) %in% c("city", "country", "period"))
      if (length(pos) > 0){

        ncity    <- length(yaml$content[[i]]$job$institution$city)
        ncountry <- length(yaml$content[[i]]$job$institution$country)
        nperiod  <- length(yaml$content[[i]]$job$institution$period)

        # Multi-languages
        if (ncity > 1 | ncountry > 1 | nperiod > 1){
          langs <- unique(c(
            names(yaml$content[[i]]$job$institution$city),
            names(yaml$content[[i]]$job$institution$country),
            names(yaml$content[[i]]$job$institution$period)))
          langs <- langs[which(langs %in% lang)]
          for (k in langs){
            asset <- paste0(asset, '    <span lang="', substr(k, 1, 2), '">')
            if (ncity > 0){
              if (ncity > 1){
                asset <- paste0(asset, '<address>', yaml$content[[i]]$job$institution$city[[k]])
              } else {
                asset <- paste0(asset, '<address>', yaml$content[[i]]$job$institution$city)
              }
              if (ncountry > 0){
                if (ncountry > 1){
                  asset <- paste0(asset, ', ', yaml$content[[i]]$job$institution$country[[k]])
                } else {
                  asset <- paste0(asset, ', ', yaml$content[[i]]$job$institution$country)
                }
              }
              asset <- paste0(asset, '</address>')
            } else {
              if (ncountry > 0){
                if (ncountry > 1){
                  asset <- paste0(asset, '<address>', yaml$content[[i]]$job$institution$country[[k]], '</address>')
                } else {
                  asset <- paste0(asset, '<address>', yaml$content[[i]]$job$institution$country, '</address>')
                }
              }
            }
            if (nperiod > 0){
              if (nperiod > 1){
                asset <- paste0(asset, '<time>', yaml$content[[i]]$job$institution$period[[k]], '</time>')
              } else {
                asset <- paste0(asset, '<time>', yaml$content[[i]]$job$institution$period, '</time>')
              }
            }
            asset <- paste0(asset, '</span>\n')
          }

        # One single language
        } else {
          asset <- paste0(asset, '    <span>')
          if (ncity > 0){
            asset <- paste0(asset, '<address>', yaml$content[[i]]$job$institution$city)
            if (ncountry > 0){
              asset <- paste0(asset, ', ', yaml$content[[i]]$job$institution$country)
            }
            asset <- paste0(asset, '</address>')
          } else {
            if (ncountry > 0){
              asset <- paste0(asset, '<address>', yaml$content[[i]]$job$institution$country, '</address>')
            }
          }
          if (nperiod > 0){
            asset <- paste0(asset, '<time>', yaml$content[[i]]$job$institution$period, '</time>')
          }
          asset <- paste0(asset, '</span>\n')
        }
      }
    }
    asset <- paste0(asset, "  </header>\n")


    # ===   PROJECTS

    projects <- yaml$content[[i]]$job$projects

    for (j in 1:length(projects)){

      asset <- paste0(asset, '\n')
      asset <- paste0(asset, '  <div style="margin-bottom: 20px;"></div>\n')
      asset <- paste0(asset, '\n')
      asset <- paste0(asset, "  <!-- NEW PROJECT           -->\n")


      # ===   ADD PROJECT NAME

      if (!is.null(yaml$style$project$before)){
        prefixe <- yaml$style$project$before
      } else {
        prefixe <- ""
      }

      if (prefixe == ""){
        space <- ""
      } else {
        space <- "&nbsp;&nbsp;"
      }

      # One single language
      if (length(yaml$content[[i]]$job$projects[[j]]$project$name) == 1){
        texte <- yaml$content[[i]]$job$projects[[j]]$project$name
        asset <- paste0(asset, '  <p class="project"><em><i><span style="color:#aaa;">', prefixe, '</span>', space, texte, '</i></em>')
        if (!is.null(yaml$content[[i]]$job$projects[[j]]$project$url)){
          url <- yaml$content[[i]]$job$projects[[j]]$project$url
          asset <- paste0(asset, '<a href="', url, '">')
          asset <- paste0(asset, '<span style="padding-left:10px;"><i class="fa fa-globe fa-1x fa-fw falink"></i></span>')
          asset <- paste0(asset, '</a>')
        }
        asset <- paste0(asset, '</p>\n')

      # Multi-languages
      } else {
        langs <- names(yaml$content[[i]]$job$projects[[j]]$project$name)
        langs <- langs[which(langs %in% lang)]
        for (k in langs){
          texte <- yaml$content[[i]]$job$projects[[j]]$project$name[[k]]
          asset <- paste0(asset, '  <p class="project" lang="', substr(k, 1, 2), '"><em><span style="color:#aaa;">', prefixe, '</span>', space, texte, '</em>')
          if (!is.null(yaml$content[[i]]$job$projects[[j]]$project$url)){
            url <- yaml$content[[i]]$job$projects[[j]]$project$url
            asset <- paste0(asset, '<a href="', url, '">')
            asset <- paste0(asset, '<span style="padding-left:15px;"><i class="fa fa-globe fa-1x fa-fw falink"></i></span>')
            asset <- paste0(asset, '</a>')
          }
          asset <- paste0(asset, '</p>\n')
        }
      }


      # ===   ADD LEADERSHIP

      asset <- paste0(asset, '\n')

      if (!is.null(yaml$style$collaborator$before)){
        prefixe <- yaml$style$collaborator$before
      } else {
        prefixe <- ""
      }

      if (prefixe == ""){
        space <- ""
      } else {
        space <- "&nbsp;&nbsp;"
      }

      # One single language
      if (length(yaml$content[[i]]$job$projects[[j]]$project$collaborator$prefixe) == 1){
        pretexte <- yaml$content[[i]]$job$projects[[j]]$project$collaborator$prefixe
        texte    <- yaml$content[[i]]$job$projects[[j]]$project$collaborator$name
        asset <- paste0(asset, '  <p class="collaborator">', prefixe, space, pretexte)
        if (!is.null(yaml$content[[i]]$job$projects[[j]]$project$collaborator$url)){
          url <- yaml$content[[i]]$job$projects[[j]]$project$collaborator$url
          asset <- paste0(asset, ' <a class="falink dotted" href="', url, '">')
          asset <- paste0(asset, texte)
          asset <- paste0(asset, '</a>')
        }
        asset <- paste0(asset, '</p>\n')


      # Multi-languages
      } else {
        langs <- names(yaml$content[[i]]$job$projects[[j]]$project$collaborator$prefixe)
        langs <- langs[which(langs %in% lang)]
        texte <- yaml$content[[i]]$job$projects[[j]]$project$collaborator$name
        for (k in langs){
          pretexte <- yaml$content[[i]]$job$projects[[j]]$project$collaborator$prefixe[[k]]
          asset <- paste0(asset, '  <p class="collaborator" lang="', substr(k, 1, 2), '">', prefixe, space, pretexte)
          if (!is.null(yaml$content[[i]]$job$projects[[j]]$project$collaborator$url)){
            url <- yaml$content[[i]]$job$projects[[j]]$project$collaborator$url
            asset <- paste0(asset, ' <a class="falink dotted" href="', url, '">')
            asset <- paste0(asset, texte)
            asset <- paste0(asset, '</a>')
          }
          asset <- paste0(asset, '</p>\n')
        }
      }


      # ===   ADD DESCRIPTION

      # One single language
      if (length(yaml$content[[i]]$job$projects[[j]]$project$description) == 1){
        texte <- yaml$content[[i]]$job$projects[[j]]$project$description
        asset <- paste0(asset, '  <p class="keywords">', texte, '</p>\n')
      } else {
        langs <- names(yaml$content[[i]]$job$projects[[j]]$project$description)
        langs <- langs[which(langs %in% lang)]
        for (k in langs){
          texte <- yaml$content[[i]]$job$projects[[j]]$project$description[[k]]
          asset <- paste0(asset, '  <p class="keywords" lang="', substr(k, 1, 2), '">', texte, '</p>\n')
        }
      }


      # ===   ADD BUTTONS

      buttons <- yaml$content[[i]]$job$projects[[j]]$project$buttons
      if (!is.null(buttons)){
        asset <- paste0(asset, '\n')
        asset <- paste0(asset, '  <div class="btn3">\n')
        for (z in 1:length(buttons)){
          asset <- paste0(asset, make_button(buttons[[z]]$button))
        }
        asset <- paste0(asset, '  </div>\n')
      }
    }
    asset <- paste0(asset, "</article>\n")
  }


  # ===   EXPORT ASSET

  dir.create("assets", showWarnings = FALSE)
  cat(asset, file = file.path("assets", paste0(section, ".html")), append = TRUE)
  paste0(html, '        <section id="', section, '" class="twelve columns"></section>\n')
}
