make_pagefooter <- function(html){

  yaml <- yaml.load_file(file.path("data", "_profile.yml"))

  html <- paste0(html, '\n')
  html <- paste0(html, '        <!-- SCROLL-TO-TOP BUTTON -->\n')
  html <- paste0(html, '        <a href="javascript:" id="return-to-top"><i class="fa fa-chevron-up"></i></a>\n')
  html <- paste0(html, '\n')
  html <- paste0(html, '        <!-- PAGE FOOTER -->\n')
  html <- paste0(html, '        <footer class="twelve columns">\n')
  html <- paste0(html, '          <p>\n')


  # ===   INSERT LICENSE INFORMATIONS

  license <- which(names(yaml) == "license")
  if (length(license) > 0){
    icon <- which(names(yaml[[license]]) == "icon")
    if (length(icon) > 0){
      html <- paste0(html, '<i class="fa fa-', yaml[[license]][[icon]], ' fa-1x fa-fw"></i>&nbsp;\n')
    }
    name <- which(names(yaml[[license]]) == "name")
    if (length(name) > 0){
      html  <- paste0(html, 'Licensed under ')
      texte <- yaml[[license]][[name]]
      url <- which(names(yaml[[license]]) == "url")
      if (length(url) > 0){
        html <- paste0(html, '<a href="', yaml[[license]][[url]], '" class="falink dotted">')
        html <- paste0(html, texte)
        html <- paste0(html, '</a>&nbsp;&nbsp;&middot;&nbsp;\n')
      } else {
        html <- paste0(html, texte, '&nbsp;&middot;&nbsp;')
      }
    }
  }


  # ===   ADD JEKYLL VITAE THEME LINK
  html  <- paste0(html, 'Modified from the <a href="https://www.github.com/biomadeira/vitae" class="falink dotted">Vitae theme</a>&nbsp;&nbsp;&middot;&nbsp;\n')


  # ===   ADD LAST UPDATE


  html <- paste0(html, 'Made with the <a href="https://cran.r-project.org/" class="falink dotted">R</a> package <a href="https://CRAN.R-project.org/package=yaml" class="falink dotted">yaml</a><br />\n')

  if (!is.null(yaml$"header"$"last-modified") && yaml$"header"$"last-modified"){
      html <- paste0(html, 'Last modified: ', Sys.time(), '\n')
  }

  html <- paste0(html, '          </p>\n')
  html <- paste0(html, '        </footer>\n')
  html <- paste0(html, '      </div>\n')
  html <- paste0(html, '    </div>\n')
}
