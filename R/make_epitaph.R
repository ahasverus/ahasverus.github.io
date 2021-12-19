make_epitaph <- function(html){

  section <- "epitaph"

  yaml <- yaml::yaml.load_file(file.path("data", paste0("_", section, ".yml")))
  info <- yaml::yaml.load_file(file.path("data", "_profile.yml"))
  lang <- names(info$languages[which(info$languages == TRUE)])

  asset <- ""
  asset <- paste0(asset, '<article>\n')
  asset <- paste0(asset, '  <div class="epitaph">\n')
  asset <- paste0(asset, '    <h4>', yaml$content, '</h4>\n')
  asset <- paste0(asset, '  </div>\n')
  asset <- paste0(asset, '</article>\n')


  # ===   EXPORT ASSET

  dir.create("assets", showWarnings = FALSE)
  cat(asset, file = file.path("assets", paste0(section, ".html")), append = TRUE)
  paste0(html, '        <section id="', section, '" class="twelve columns"></section>\n')
}
