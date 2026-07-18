#' Get the path to the publications directory
#'
#' Get the path to the directory containing publications Markdown files.
#'
#' @return
#' A length-one character vector giving the absolute path to the
#' `content/publications` directory.

path_publications <- function() {
  here::here("content", "publications")
}

#' Normalize DOI strings
#'
#' Normalize DOI strings by removing common prefixes and surrounding
#' whitespace.
#'
#' @param doi A character vector of DOIs.
#'
#' @return A character vector of normalized DOIs.

normalize_doi <- function(doi) {
  doi <- trimws(doi)
  doi <- sub("^https?://doi\\.org/", "", doi)
  doi <- sub("^doi:", "", doi, ignore.case = TRUE)
  doi
}


#' Retrieve publication metadata from OpenAlex
#'
#' Retrieve one or more OpenAlex works from a vector of DOIs using the
#' OpenAlex REST API. Requests are performed in batches to reduce the
#' number of API calls.
#'
#' @param dois A character vector of DOIs.
#' @param mailto Optional email address used for the OpenAlex polite pool.
#' @param batch_size Number of DOIs queried per request.
#'
#' @return A list of OpenAlex work objects.

fetch_openalex_works <- function(dois, mailto = NULL, batch_size = 50) {
  dois <- normalize_doi(dois)

  select <- paste(
    c(
      "id",
      "doi",
      "cited_by_count",
      "counts_by_year",
      "fwci",
      "citation_normalized_percentile",
      "open_access",
      "primary_location",
      "apc_list"
    ),
    collapse = ","
  )

  batches <- split(dois, ceiling(seq_along(dois) / batch_size))

  results <- list()

  for (batch in batches) {
    filter <- paste0("doi:", paste0("https://doi.org/", batch, collapse = "|"))

    req <- httr2::request("https://api.openalex.org/works") |>
      httr2::req_url_query(filter = filter, select = select)

    if (!is.null(mailto)) {
      req <- req |>
        httr2::req_url_query(mailto = mailto)
    }

    response <- req |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    results <- c(results, response$results)
  }

  results
}


#' Extract publication metrics from an OpenAlex work
#'
#' Extract citation-based metrics from an OpenAlex work object.
#'
#' @param work An OpenAlex work object.
#'
#' @return A named list containing:
#'   - `cited_by_count`: total number of citations received by the work
#'   - `counts_by_year`: data frame containing the number of citations per year
#'   - `average_citations_per_year`: mean number of citations per year
#'   - `fwci`: Field-Weighted Citation Impact (FWCI), measuring the citation
#'     impact of the work relative to similar publications in the same field,
#'     publication year, and document type. A value greater than 1 indicates
#'     above-average citation impact.
#'   - `percentile`: normalized citation percentile expressed as a value
#'     between 0 and 1. Higher values indicate a stronger citation impact
#'     compared with similar publications.
#'   - `top_10_percent`: logical value indicating whether the work belongs to
#'     the top 10% most cited publications in its comparison group.
#'   - `top_1_percent`: logical value indicating whether the work belongs to
#'     the top 1% most cited publications in its comparison group.

parse_openalex_metrics <- function(work) {
  counts_by_year <- data.frame()

  if (length(work$counts_by_year) > 0) {
    counts_by_year <- do.call(rbind.data.frame, work$counts_by_year)
  }

  average_citations_per_year <- NA_real_

  if (nrow(counts_by_year) > 0) {
    average_citations_per_year <- mean(counts_by_year$cited_by_count)
  }

  fwci <- work$fwci

  if (!is.null(fwci)) {
    fwci <- round(fwci, 2)
  }

  percentile <- NA_real_
  top_10_percent <- FALSE
  top_1_percent <- FALSE

  if (!is.null(work$citation_normalized_percentile)) {
    percentile <- work$citation_normalized_percentile$value
    top_10_percent <- work$citation_normalized_percentile$is_in_top_10_percent
    top_1_percent <- work$citation_normalized_percentile$is_in_top_1_percent
  }

  list(
    cited_by_count = work$cited_by_count,
    counts_by_year = counts_by_year,
    average_citations_per_year = average_citations_per_year,
    fwci = fwci,
    top_10_percent = top_10_percent,
    top_1_percent = top_1_percent
  )
}


#' Compute an Open Access publication badge
#'
#' Classify an OpenAlex work according to the publication model used.
#'
#' The returned badge reflects the journal business model rather than the
#' OpenAlex `oa_status` classification.
#'
#' Returned values are:
#'   - `Diamond OA`: fully open access journal without APC.
#'   - `Gold OA`: fully open access journal with APC.
#'   - `Hybrid OA`: subscription journal with an openly accessible article.
#'   - `Subscription`: subscription journal without openly accessible article.
#' }
#'
#' @param work An OpenAlex work object.
#'
#' @return A character string.

compute_oa_badge <- function(work) {
  journal_oa <- work$primary_location$source$is_oa
  apc <- work$apc_list$value
  article_oa <- work$open_access$is_oa

  if (!journal_oa) {
    if (article_oa) {
      oa_badge <- "Hybrid OA"
    } else {
      oa_badge <- "Subscription"
    }
  } else {
    if (is.null(apc) || apc == 0) {
      oa_badge <- "Diamond OA"
    } else {
      oa_badge <- "Gold OA"
    }
  }

  oa_badge
}


#' Retrieve publication metrics from OpenAlex
#'
#' Retrieve citation metrics and Open Access information for a collection
#' of publications identified by their DOI.
#'
#' @param dois A character vector of DOIs.
#' @param mailto Optional email address used for the OpenAlex polite pool.
#' @param batch_size Number of DOIs queried per request.
#'
#' @return A named list. Each element corresponds to one publication and
#' contains citation metrics together with an Open Access badge.

get_publication_metrics <- function(dois, mailto = NULL, batch_size = 50) {
  works <- fetch_openalex_works(
    dois = dois,
    mailto = mailto,
    batch_size = batch_size
  )

  metrics <- lapply(works, function(work) {
    meta <- parse_openalex_metrics(work)
    meta$oa_badge <- compute_oa_badge(work)

    meta
  })

  names(metrics) <- vapply(
    works,
    function(work) normalize_doi(work$doi),
    character(1)
  )

  metrics <- metrics[!duplicated(names(metrics))]

  metrics
}


#' Retrieve publication metadata from a Zotero collection
#'
#' Retrieves publication metadata from the local Zotero library, filters
#' references belonging to a given collection, and formats them.
#'
#' References are sorted by decreasing publication year and first author.
#' Author names are shortened using [shorten_authors()] and converted
#' to a human-readable format (e.g. "Smith, J., Doe, A. & Brown, C.").
#'
#' @param collection A character string. Name of the Zotero collection to
#'   retrieve. Defaults to `Me`.
#'
#' @return A data frame containing one row per publication with the following
#'   variables:
#'   - `category`: publication type (e.g. `journalArticle`, `book`, etc.)
#'   - `citation_key`: unique citation key used to identify the publication
#'   - `year`: publication year
#'   - `title`: publication title
#'   - `book_title`: book title (for chapters)
#'   - `author`: formatted author list
#'   - `volume`: journal volume
#'   - `pages`: page range
#'   - `num_pages`: number of pages
#'   - `journal`: journal title
#'   - `publisher`: publisher name
#'   - `place`: place of publication
#'   - `doi`: digital Object Identifier (DOI)
#'   - `url`: publication URL
#'   - `display_order`: order used to display publications in the portfolio

fetch_zotero_reference_metadata <- function(collection = "Me") {
  ref <- zoteror::get_zotero_data()
  ref <- ref[ref$collection == collection, ]
  ref <- ref[ref$category != "preprint", ]


  ref$year <- as.numeric(ref$year)
  ref <- dplyr::arrange(ref, dplyr::desc(year), author)

  columns <- c(
    "category",
    "citation_key",
    "year",
    "title",
    "book_title",
    "author",
    "volume",
    "pages",
    "num_pages",
    "journal",
    "publisher",
    "place",
    "doi",
    "url"
  )

  ref <- ref[, columns]

  ref_list <- lapply(seq_len(nrow(ref)), function(i) {
    x <- as.list(ref[i, ])

    x$author <- strsplit(x$author, " ; ")[[1]] |>
      shorten_authors() |>
      paste(collapse = ", ") |>
      sub(",([^,]*)$", " &\\1", x = _)

    x$display_order <- i
    x$weight <- i

    x
  })

  ref_list <- do.call(rbind.data.frame, ref_list)
  ref_list$display_order <- rev(ref_list$display_order)

  ref_list
}


format_pages <- function(pages) {

  if (is.na(pages) || pages == "") {
    return("")
  }

  if (pages == "published online (not yet assigned to an issue)") {
    return(pages)
  }

  pages <- gsub("[\u2013\u2014]", "-", pages)

  if (grepl("-", pages, fixed = TRUE)) {
    return(paste0("pp. ", pages))
  }

  paste0("art. ", pages)
}


#' Generate a stable publication identifier
#'
#' Generates a stable identifier from a publication key using the **xxhash32**
#' hashing algorithm on the DOI or URL (for book and book chapter).
#'
#' The identifier is used in Zotero as the **Citation Key** and as the base
#' filename for publication resources in the portfolio (Markdown page, full-text
#' PDF, and BibTeX citation).
#'
#' @param key A character string. A unique publication identifier, such as a
#'   DOI or URL.
#'
#' @return A character string of the form `pub-xxxxxxxx`, where `xxxxxxxx` is
#' the hexadecimal **xxhash32** digest of the input key.
#'
#' @examples
#' generate_publication_id("10.1016/j.oneear.2024.09.011")

generate_publication_id <- function(key) {
  paste0("pub-", digest::digest(key, algo = "xxhash32"))
}


key <- "pub-e0647ea0"

#' Read a publication Markdown file
#'
#' Reads the content of a Markdown file (if exists) from the publications/
#' directory.
#'
#' @param key A character string. The publication identifier
#'   (e.g. `"pub-e0647ea0"`).
#'
#' @return
#' A character vector containing the lines of the Markdown file, or `NULL` if
#' the file does not exist.

read_publication_file <- function(key) {
  stopifnot(
    length(key) == 1,
    is.character(key)
  )

  filename <- file.path(path_publications(), paste0(key, ".md"))

  if (!file.exists(filename)) {
    return(NULL)
  }

  readLines(filename, encoding = "UTF-8")
}


#' Read publication metadata entries
#'
#' Reads and parses all publication metadata files from the publications/
#' directory. Each Markdown file is expected to contain TOML front matter
#' describing a publication entry.
#'
#' @return
#' A named list of parsed TOML metadata entries. Names correspond to the
#' publication identifiers.

read_publication_entries <- function() {
  refs <- fetch_zotero_reference_metadata()
  keys <- refs[["citation_key"]]

  entries <- lapply(keys, function(key) {
    content <- read_publication_file(key)

    if (is.null(content)) {
      return(NULL)
    }

    parse_toml_frontmatter(content)
  })

  names(entries) <- keys
  entries
}


#' Replace missing values with empty strings
#'
#' Utility functions used to normalize missing values before generating
#' publication metadata. Missing values are converted to empty character
#' strings to ensure a consistent TOML structure.
#'
#' @param x An object containing potentially missing values.
#'
#' @return
#' A value where missing entries are replaced by an empty character string.
#'
#' @examples
#' empty_if_na(NA)
#' empty_if_null(NULL)

empty_if_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return("")
  }

  x
}

#' @rdname empty_if_na

empty_if_null <- function(x) {
  if (is.null(x)) {
    return("")
  }

  x
}


#' Build publication metadata for a Zola page
#'
#' Combines bibliographic metadata from Zotero and optional metrics from
#' OpenAlex into a nested list matching the TOML front matter structure used
#' by publication Markdown files in the Zola portfolio.
#'
#' Zotero metadata are stored under `[extra.zotero]`, OpenAlex metrics under
#' `[extra.openalex]` when available, and additional user-defined resources
#' under `[extra.resources]`.
#'
#' Missing values from Zotero and OpenAlex are converted to empty strings to
#' maintain a consistent TOML schema across publication types. OpenAlex
#' information is only added when metrics are available, for example for
#' DOI-indexed publications.
#'
#' @param metadata A named list containing cleaned Zotero metadata for a single
#'   publication. Expected fields include `citation_key`, `display_order`,
#'   `category`, `year`, `title`, `author`, bibliographic fields, and `doi`.
#'
#' @param metrics Optional named list containing OpenAlex metrics for a single
#'   publication. Expected fields include citation metrics, normalized citation
#'   indicators, and an Open Access badge.
#'
#' @return
#' A nested named list representing the TOML front matter of a publication
#' Markdown file. The structure contains:
#'
#' - `weight`: Publication display order in Zola.
#' - `extra.zotero`: Bibliographic metadata originating from Zotero, including
#'   generated paths for full text and citation files.
#' - `extra.openalex`: Citation metrics and Open Access information from
#'   OpenAlex, when available.
#' - `extra.resources`: Additional resources manually associated with the
#'   publication, such as code, package, or database.
#'
#' @examples
#' refs <- fetch_zotero_reference_metadata()
#' ref <- refs[1, ]
#'
#' metrics <- get_publication_metrics(ref$doi)
#'
#' content <- build_publication_metadata(
#'   metadata = ref,
#'   metrics = metrics[[normalize_doi(ref$doi)]]
#' )
#'
#' @export
build_publication_metadata <- function(metadata, metrics = NULL) {
  key <- metadata$citation_key

  extra <- list(
    zotero = list(
      key = key,
      number = metadata$display_order,

      category = metadata$category,
      section = ifelse(
        metadata$category %in% c("journalArticle", "preprint"),
        "Articles",
        "Books & Chapters"
      ),

      year = metadata$year,
      title = metadata$title,
      author = metadata$author,

      journal = empty_if_na(metadata$journal),
      volume = empty_if_na(metadata$volume),
      pages = format_pages(metadata$pages),

      book_title = empty_if_na(metadata$book_title),
      num_pages = empty_if_na(metadata$num_pages),
      publisher = empty_if_na(metadata$publisher),
      place = empty_if_na(metadata$place),

      doi = empty_if_na(metadata$doi),

      fulltext = paste0("resources/fulltexts/", key, ".pdf"),

      citation = paste0("resources/citations/", key, ".bib")
    ),

    resources = list(
      code = "",
      package = "",
      database = ""
    )
  )

  if (!is.null(metrics)) {
    extra$openalex <- list(
      cited_by_count = empty_if_null(metrics$cited_by_count),
      average_citations_per_year = empty_if_null(
        metrics$average_count_by_year
      ),

      fwci = empty_if_null(metrics$fwci),
      percentile = empty_if_null(metrics$percentile),
      top_10_percent = empty_if_null(metrics$top_10_percent),
      top_1_percent = empty_if_null(metrics$top_1_percent),
      oa_badge = empty_if_null(metrics$oa_badge)
    )
  }

  list(
    weight = metadata$weight,
    extra = extra
  )
}


#' Merge publication metadata
#'
#' Merge newly generated publication metadata with an existing TOML metadata
#' structure. Automatically generated sections from Zotero and OpenAlex are
#' replaced by the new values, while manually editable resources are preserved
#' from the existing metadata when available.
#'
#' @param old_metadata A named list containing the existing TOML metadata, or
#'   `NULL` if the publication file does not exist (new publication).
#'
#' @param new_metadata A named list containing the newly generated publication
#'   metadata.
#'
#' @return
#' A named list containing the merged publication metadata.
#'
#' @details
#' The following sections are regenerated:
#'   - `weight`
#'   - `extra.zotero`
#'   - `extra.openalex`
#'
#' The following section is preserved from the existing file when available:
#'   - `extra.resources`
#'

merge_publication_metadata <- function(old_metadata, new_metadata) {
  if (is.null(old_metadata)) {
    return(new_metadata)
  }

  if (!is.null(old_metadata$extra$resources)) {
    new_metadata$extra$resources <- old_metadata$extra$resources
  }

  new_metadata
}


#' Write a publication Markdown file
#'
#' Write the TOML front matter of a publication entry to a Markdown file.
#'
#' The generated file is wrapped with TOML front matter delimiters (`+++`).
#'
#' @param key A character string. The publication identifier
#'   (e.g. `"pub-e0647ea0"`).
#'
#' @param x A TOML expression containing the publication metadata.
#'
#' @return
#' No return value.

write_publication_md_file <- function(key, x) {
  stopifnot(
    length(key) == 1,
    is.character(key)
  )

  x <- c("+++", as.character(x), "+++")

  filename <- file.path(path_publications(), paste0(key, ".md"))

  writeLines(x, filename, useBytes = TRUE)

  invisible(NULL)
}


#' Update publication Markdown pages
#'
#' Update publication Markdown files from Zotero metadata and optional
#' OpenAlex metrics. Existing files are read before update in order to preserve
#' manually maintained resources.
#'
#' Zotero metadata and OpenAlex metrics replace previous generated values,
#' while the `[extra.resources]` section is preserved when already present.
#'
#' @param collection A character string. Zotero collection containing the
#'   publications to update.
#'
#' @param mailto Optional email address used for the OpenAlex polite pool.
#'
#' @param batch_size Number of DOIs queried per OpenAlex request.
#'
#' @return
#' Invisibly returns `NULL`.

update_publication_pages <- function(
  collection = "Me",
  mailto = "nicolas.casajus@gmail.com",
  batch_size = 20
) {
  refs <- fetch_zotero_reference_metadata(collection = collection)

  dois <- refs$doi[!is.na(refs$doi) & refs$doi != ""]

  metrics <- get_publication_metrics(
    dois = dois,
    mailto = mailto,
    batch_size = batch_size
  )

  for (i in seq_len(nrow(refs))) {
    ref <- refs[i, ]

    key <- ref$citation_key

    openalex_metrics <- NULL

    if (!is.na(ref$doi) && ref$doi != "") {
      doi <- normalize_doi(ref$doi)

      if (doi %in% names(metrics)) {
        openalex_metrics <- metrics[[doi]]
      }
    }

    new_metadata <- build_publication_metadata(
      metadata = ref,
      metrics = openalex_metrics
    )

    old_content <- read_publication_file(key)

    old_metadata <- NULL

    if (!is.null(old_content)) {
      old_metadata <- parse_toml_frontmatter(old_content)
    }

    final_metadata <- merge_publication_metadata(old_metadata, new_metadata)

    toml <- serialize_toml(final_metadata)

    write_publication_md_file(key = key, x = toml)
  }

  invisible(NULL)
}


###
###
###

# #' Write Bibliography Files
# #'
# #' @description
# #' Generates two md files in `publication/_includes/`:
# #'   - list_of_articles.md
# #'   - list_of_books.md
# #'
# #' @keywords internal

# update_bibliography <- function() {
#   all_refs <- get_publications()

#   ## Journal articles

#   ref <- all_refs[all_refs$category == "journalArticle", ]

#   content <- NULL
#   for (i in seq_len(nrow(ref))) {
#     content <- c(content, make_article_citation(ref[i, ]), "")
#   }

#   content <- paste0(content, collapse = "\n")

#   writeLines(
#     content,
#     here::here("publications", "_includes", "list_of_articles.md")
#   )

#   ## Books & Book chapters

#   ref <- all_refs[all_refs$category %in% c("book", "bookSection"), ]

#   content <- NULL
#   for (i in seq_len(nrow(ref))) {
#     content <- c(content, make_book_citation(ref[i, ]), "")
#   }

#   content <- paste0(content, collapse = "\n")

#   writeLines(
#     content,
#     here::here("publications", "_includes", "list_of_books.md")
#   )

#   invisible(NULL)
# }


# #' Generate Bibliography for Journal Articles
# #'
# #' @keywords internal

# make_article_citation <- function(ref) {
#   content <- "1. "
#   content <- c(content, ref[["author"]])
#   content <- c(content, " ")
#   content <- c(content, "(", ref[["year"]], ")")
#   content <- c(content, " ")

#   if (!is.na(ref[["doi"]])) {
#     content <- c(
#       content,
#       "[**",
#       ref[["title"]],
#       "**](https://doi.org/",
#       ref[["doi"]],
#       ")"
#     )
#   } else {
#     content <- c(
#       content,
#       "[**",
#       ref[["title"]],
#       "**](",
#       ref[["url"]],
#       ")"
#     )
#   }

#   content <- c(content, ". ")
#   content <- c(content, "**_", ref[["journal"]], "_**")

#   if (!is.na(ref[["volume"]])) {
#     content <- c(content, ", ")
#     content <- c(content, ref[["volume"]])
#   }

#   content <- c(content, ", ")
#   content <- c(content, ref[["pages"]])
#   content <- c(content, ". ")
#   content <- c(
#     content,
#     paste0("&nbsp;[[PDF](pdf/", create_pdf_filename(ref), ")]{.pdf}")
#   )

#   paste0(content, collapse = "")
# }


# #' Generate PDF Filename
# #'
# #' @return A string of the form: `Year-LastNameOfFirstAuthor-JournalName.pdf`
# #'
# #' @keywords internal

# create_pdf_filename <- function(ref) {
#   paste0(
#     ref[["year"]],
#     "-",
#     gsub("\\s.*", "", ref[["author"]]),
#     "-",
#     gsub("(:)?\\s", "-", ref[["journal"]]),
#     ".pdf"
#   ) |>
#     tolower()
# }


# #' Generate Bibliography for Books & Book Chapters
# #'
# #' @keywords internal

# make_book_citation <- function(ref) {
#   content <- "1. "
#   content <- c(content, ref[["author"]])
#   content <- c(content, " ")
#   content <- c(content, "(", ref[["year"]], ")")
#   content <- c(content, " ")

#   if (!is.na(ref[["url"]])) {
#     content <- c(
#       content,
#       "[**",
#       ref[["title"]],
#       "**](",
#       ref[["url"]],
#       ")"
#     )
#   } else {
#     content <- c(
#       content,
#       "**",
#       ref[["title"]],
#       "**"
#     )
#   }

#   content <- c(content, ". ")

#   if (ref[["category"]] == "bookSection") {
#     content <- c(content, "_In:_ ", ref[["book_title"]])
#     content <- c(content, ". ")
#     content <- c(content, "**", ref[["publisher"]], "**")
#     content <- c(content, ", ")
#     content <- c(content, "pp ", ref[["pages"]])
#     content <- c(content, ".")
#   }

#   if (ref[["category"]] == "book") {
#     content <- c(content, "**", ref[["publisher"]], "**")
#     content <- c(content, ", ")
#     content <- c(content, ref[["place"]])
#     content <- c(content, ", ")
#     content <- c(content, ref[["num_pages"]], " pp")
#     content <- c(content, ".")
#   }

#   paste0(content, collapse = "")
# }


# #' Write Bibliography Summary File
# #'
# #' @description
# #' Generates one md file in `publication/_includes/`:
# #'   - publication-count.md
# #'
# #' Compute the number of journal articles and books (including book chapters).
# #'
# #' @keywords internal

# update_publication_stats <- function() {
#   ref <- get_publications()

#   content <- ""
#   content <- c(
#     content,
#     nrow(ref[ref$category == "journalArticle", ]),
#     " articles"
#   )

#   content <- c(content, "{{< iconify mdi:dot >}}")
#   content <- c(
#     content,
#     nrow(ref[ref$category %in% c("book", "bookSection"), ]),
#     " books and book chapters"
#   )

#   content <- paste0(content, collapse = "")

#   writeLines(
#     content,
#     here::here("publications", "_includes", "publication-count.md")
#   )

#   invisible(NULL)
# }


#' Shorten full names (Doe, John becomes Doe, J)
#'
#' @description
#' This function shortens fullnames by cutting given names. Different output
#' formats are available.
#'
#' @param data A strings vector of fullnames to shorten.
#' @param upper_case A boolean. If TRUE, returns strings in upper case.
#' @param lower_case A boolean. If TRUE, returns strings in lower case.
#' @param ending Suffix added to first name initials.
#' @param separator Character to separator last name from first names initials.
#' @param given_first A boolean. If TRUE, name starts with the first name
#'   initials (except for the first author).
#'
#' @details Last name comes first and given name should be preceded with a coma.
#'
#' @keywords internal
#'
#' @examples
#' author <- "Casajus, Nicolas Jean-Guy"
#'
#' shorten_authors(author)
#' ## [1] "Casajus NJ-G"
#'
#' shorten_authors(author, upper_case = TRUE, ending = ".")
#' ## [1] "CASAJUS N.J.-G."
#'
#' shorten_authors(author, lower_case = TRUE, ending = ".")
#' ## [1] "casajus n.j.-g."
#'
#' shorten_authors(author, ending = ". ")
#' ## [1] "Casajus N. J.-G."
#'
#' shorten_authors(author, ending = ".", separator = ", ")
#' ## [1] "Casajus, N. J.-G."
#'
#' authors <- c("Casajus, Nicolas Jean-Guy", "Mouquet, Nicolas")
#'
#' shorten_authors(authors, ending = ".", given_first = TRUE)
#' ## [1] "Casajus N.J.-G." "N. Mouquet"

shorten_authors <- function(
  data,
  upper_case = FALSE,
  lower_case = FALSE,
  ending = "",
  separator = " ",
  given_first = FALSE
) {
  if (missing(data)) {
    stop("Argument 'data' is required.")
  }

  if (is.null(data)) {
    stop("Argument 'data' is required.")
  }

  if (!is.character(data)) {
    stop("Argument 'data' must be a character.")
  }

  if (!is.logical(upper_case)) {
    stop("Argument 'upper_case' must be a boolean.")
  }

  if (!is.logical(lower_case)) {
    stop("Argument 'lower_case' must be a boolean.")
  }

  if (upper_case && lower_case) {
    stop("You have to choose between 'upper_case' and 'lower_case'.")
  }

  data <- unlist(strsplit(data, " ; "))

  authors <- vector(mode = "character", length = length(data))

  for (i in 1:length(data)) {
    author <- strsplit(data[i], ', ')[[1]]

    if (length(author) != 2) {
      stop("Author name doesn't follow the format: 'Fullname, Firstname(s)'.")
    }

    last_name <- author[1]

    first_names <- strsplit(author[2], "-")[[1]]

    hyphenated <- NULL

    for (j in 1:length(first_names)) {
      burst <- strsplit(first_names[j], " ")[[1]]

      initials <- NULL

      for (k in 1:length(burst)) {
        initials <- c(
          initials,
          paste0(toupper(substr(burst[k], 1, 1)), ending)
        )
      }

      initials <- paste0(initials, collapse = "")
      hyphenated <- c(hyphenated, initials)
    }

    first_names <- paste0(hyphenated, collapse = "-")
    first_names <- gsub(" -| -", "-", first_names)
    first_names <- gsub("^\\s{1,}|\\s{1,}$", "", first_names)

    if (given_first) {
      if (i > 1) {
        author <- paste(first_names, last_name, sep = separator)
      } else {
        author <- paste(last_name, first_names, sep = separator)
      }
    } else {
      author <- paste(last_name, first_names, sep = separator)
    }

    authors[i] <- author
  }

  if (upper_case) {
    authors <- toupper(authors)
  }

  if (lower_case) {
    authors <- tolower(authors)
  }

  return(authors)
}
