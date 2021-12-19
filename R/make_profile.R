make_profile <- function(html){

  yaml <- yaml::yaml.load_file(file.path("data", "_profile.yml"))
  lang <- names(yaml$languages[which(yaml$languages == TRUE)])

  html <- paste0(html, '\n')
  html <- paste0(html, '  <body>\n')
  html <- paste0(html, '    <div class="container">\n')
  html <- paste0(html, '      <header class="four columns profile">\n')
  html <- paste0(html, '\n')


  # ===   ADD FLAGS (LANGUAGES SELECTOR)

  if (length(lang > 0)){
    html <- paste0(html, '        <!-- Language selection [flags icon] -->\n')
    html <- paste0(html, '        <div class="flag">\n')
    for (i in lang){
      html <- paste0(html, '          <a alt="', i, '" title="', i, '" id="lang-', substr(i, 1, 2), '">\n')
      html <- paste0(html, '            <i class="flag-icon flag-icon-', substr(i, 1, 2), '"></i>\n')
      html <- paste0(html, '          </a>\n')
    }
    html <- paste0(html, '        </div>\n')
  }


  # ===   ADD PROFILE IMAGE

  profile <- which(names(yaml) == "profile")
  if (length(profile) == 0){
    stop("Missing profile informations.")
  }

  image <- which(names(yaml[[profile]]) == "image")
  if (length(image) > 0){
    image <- list.files(pattern = paste0("^", yaml[[profile]][[image]], "$"), recursive = TRUE)
    if (length(image) > 0){
      html <- paste0(html, '\n')
      html <- paste0(html, '        <!-- Profile image -->\n')
      html <- paste0(html, '        <div style="margin-bottom: 10px;"></div>\n')
      html <- paste0(html, '        <img src="', image, '" class="me" />\n')
      html <- paste0(html, '        <div style="margin-bottom: 40px;"></div>\n')
    } else {
      stop("Unable to find profile image.")
    }
  }


  # ===   ADD AUTHOR NAME

  author <- which(names(yaml[[profile]]) == "author")
  if (length(author) > 0){
    html <- paste0(html, '\n')
    html <- paste0(html, '        <!-- Profile infos -->\n')
    html <- paste0(html, '        <h1>', yaml[[profile]][[author]], '</h1>\n')
  }


  # ===   ADD JOB NAME

  job <- which(names(yaml[[profile]]) == "job")
  if (length(job) > 0){
    html <- paste0(html, '        <h2>', yaml[[profile]][[job]], '</h2>\n')
  }


  # ===   ADD ADDRESS AND CONTACT

  for (j in c("institution", "contact")){
    pos <- which(names(yaml[[profile]]) == j)
    if (length(pos) > 0){
      html <- paste0(html, '        <p>')
      tags <- names(yaml[[profile]][[pos]])
      for (i in tags){
        tag <- which(names(yaml[[profile]][[pos]]) == i)
        if (length(tag) > 0){
          html <- paste0(html, yaml[[profile]][[pos]][[tag]], '<br />')
        }
      }
      html <- paste0(html, '</p>\n')
    }
  }


  # ===   ADD SOCIAL NETWORKS

  social <- which(names(yaml) == "social")
  if (length(social) > 0){
    html <- paste0(html, '\n')
    html <- paste0(html, '        <div style="margin-bottom: 20px;"></div>\n')
    html <- paste0(html, '\n')
    html <- paste0(html, '        <!-- Social networks links and icons -->\n')
    html <- paste0(html, '        <p>\n')
    tags <- names(yaml[[social]])

    n <- 1

    for (i in tags){
      if (n == 5) html <- paste0(html, '          <br /><br />\n')
      if (i == "github"){
        href    <- "https://www.github.com/"
        package <- "fa"
      }
      if (i == "google-scholar"){
        href    <- "https://scholar.google.fr/citations?user="
        package <- "ai"
      }
      if (i == "researchgate"){
        href    <- "https://www.researchgate.net/profile/"
        package <- "ai"
      }
      if (i == "orcid"){
        href    <- "http://orcid.org/"
        package <- "ai"
      }
      if (i == "linkedin"){
        href    <- "https://www.linkedin.com/in/"
        package <- "fa"
      }
      if (i == "mendeley"){
        href    <- "https://www.mendeley.com/profiles/"
        package <- "ai"
      }
      if (i == "twitter"){
        href    <- "http://twitter.com/"
        package <- "fa"
      }
      if (i == "facebook"){
        href    <- "https://www.facebook.com/"
        package <- "fa"
      }
      if (i == "envelope"){
        href    <- "mailto:"
        package <- "fa"
      }
      if (i == "skype"){
        href    <- "https://join.skype.com/invite/"
        package <- "fa"
      }
      if (i == "phone"){
        href    <- "tel:"
        package <- "fa"
      }


      href <- paste0(href, yaml[[social]][[i]])
      html <- paste0(html, '          <a alt="', i, '" title="', i, '" href="', href, '">\n')
      html <- paste0(html, '            <i class="', package, ' ', package, '-', i, ' ', package, '-2x ', package, '-fw falink"></i>\n')
      html <- paste0(html, '          </a>\n')
      n <- n + 1
    }
    html <- paste0(html, '        </p>\n')
    html <- paste0(html, '        <div style="margin-bottom: 40px;"></div>\n')
  }

  html <- paste0(html, '      </header>\n\n')
  paste0(html, '      <div class="twelve columns clearfix">\n')
}
