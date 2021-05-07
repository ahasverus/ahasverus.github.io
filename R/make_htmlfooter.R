make_htmlfooter <- function(html){

  yaml      <- yaml.load_file(file.path("data", "_profile.yml"))
  sections   <- names(yaml$sections[which(yaml$sections == TRUE)])

  html <- paste0(html, '\n')
  html <- paste0(html, '    <!-- Import jQuery -->\n')
  html <- paste0(html, '    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>\n')
  html <- paste0(html, '    <script type="text/javascript">\n')
  html <- paste0(html, '\n')
  html <- paste0(html, '      // ===== Load jQuery =====\n')
  html <- paste0(html, '      $(document).ready(function() {\n')
  html <- paste0(html, '\n')
  html <- paste0(html, '        // ===== Include Assets =====\n')

  for (i in sections){
    url <- list.files(pattern = paste0("^", i, "\\.html$"), recursive = TRUE)
    if (length(url) == 1){
      html <- paste0(html, "        $('#", i, "').load('", url, "');\n")
    }
  }
  html <- paste0(html, '\n')

  html <- paste0(html, '        // ===== Scroll to Top =====\n')
  html <- paste0(html, '        $(window).scroll(function() {\n')
  html <- paste0(html, '          if ($(this).scrollTop() >= 200) {\n')
  html <- paste0(html, "            $('#return-to-top').fadeIn(200);\n")
  html <- paste0(html, '          } else {\n')
  html <- paste0(html, "            $('#return-to-top').fadeOut(200);\n")
  html <- paste0(html, '          }\n')
  html <- paste0(html, '        });\n')
  html <- paste0(html, "        $('#return-to-top').click(function() {\n")
  html <- paste0(html, "          $('body,html').animate({ scrollTop : 0 }, 500);\n")
  html <- paste0(html, '        });\n')
  html <- paste0(html, '      });\n')
  html <- paste0(html, '    </script>\n')
  html <- paste0(html, '\n')

  html <- paste0(html, '    <!-- Change language on flag click (default FR) -->\n')
  url <- list.files(pattern = paste0("^language-selector\\.js$"), recursive = TRUE)
  if (length(url) > 0){
    html <- paste0(html, '    <script type="text/javascript" src="', url, '"></script>')
  } else {
    stop("Missing javascript file.")
  }

  html <- paste0(html, '\n')
  html <- paste0(html, '    <noscript>Your browser does not support JavaScript!</noscript>\n')

  html <- paste0(html, '  </body>\n')
  paste0(html, '</html>\n')
}
