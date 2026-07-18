#' Get the path to the software directory
#'
#' Get the path to the directory containing software Markdown files.
#'
#' @return
#' A length-one character vector giving the absolute path to the
#' `content/software` directory.

path_software <- function() {
  here::here("content", "software")
}


#' List software Markdown files
#'
#' List Markdown files containing software metadata from the software
#' directory (`content/software`).
#'
#' Only files matching the naming convention `^[a-z0-9]+\\.md$` are returned
#' (the `_index.md` is ignored).
#'
#' @return
#' A character vector containing the filenames of software Markdown files.

list_software_md_files <- function() {
  list.files(
    path = path_software(),
    pattern = "^[a-z0-9]+\\.md$",
    full.names = FALSE,
    recursive = FALSE
  )
}


#' Read a software Markdown file
#'
#' Read the content of a Markdown file from the software directory.
#'
#' @param filename A character string. The name of the Markdown file to read.
#'
#' @return
#' A character vector containing the lines of the Markdown file.

read_md_file <- function(filename) {
  stopifnot(
    length(filename) == 1,
    is.character(filename)
  )

  filename <- file.path(path_software(), filename)

  readLines(filename, encoding = "UTF-8")
}


#' Parse TOML front matter from a Markdown file
#'
#' Extract and parse the TOML front matter from the content of a Markdown file.
#' The front matter is expected to be delimited by lines equal to `+++`.
#'
#' @param x A character vector containing the lines of a Markdown file.
#'
#' @return
#' A named list containing the parameters defined in the TOML front matter.

parse_toml_frontmatter <- function(x) {
  stopifnot(is.character(x))

  pos <- which(trimws(x) == "+++")

  if (length(pos) != 2) {
    stop("The TOML front matter is malformed", call. = FALSE)
  }

  x <- x[(pos[1] + 1):(pos[2] - 1)]

  toml::parse_toml(x)
}


#' Read software metadata entries
#'
#' Read and parse all software metadata files from the software directory.
#' Each Markdown file is expected to contain TOML front matter describing
#' a software entry.
#'
#' @return
#' A named list of software entries. Each entry contains the filename and
#' the parsed TOML metadata.

read_software_entries <- function() {
  files <- list_software_md_files()

  lapply(files, function(file) {
    list(
      filename = file,
      frontmatter = read_md_file(file) |>
        parse_toml_frontmatter()
    )
  })
}


#' Extract a software repository from metadata
#'
#' Extract the repository owner and name from the repository URL stored in
#' the software metadata.
#'
#' The repository URL is expected to be stored in `extra.links.source` and
#' to match the format `https://github.com/owner/repo`.
#'
#' @param entry A software entry returned by [read_software_entries()].
#'
#' @return
#' A data frame with one row and the following columns:
#'   - `filename`: The Markdown file associated with the software entry.
#'   - `platform`: The repository hosting platform.
#'   - `owner`: The repository owner.
#'   - `repo`: The repository name.

extract_software_repository <- function(entry) {
  stopifnot(
    is.list(entry),
    all(c("filename", "frontmatter") %in% names(entry))
  )

  links <- entry$frontmatter$extra$links

  if (is.null(links) || !"source" %in% names(links)) {
    stop(
      "The field 'extra.links.source' is missing",
      call. = FALSE
    )
  }

  url <- links$source

  if (!is.character(url) || length(url) != 1) {
    stop(
      "The repository URL should be a single character string",
      call. = FALSE
    )
  }

  if (!grepl("^https://github\\.com/[^/]+/[^/]+/?$", url)) {
    stop(
      "The repository URL should match 'https://github.com/owner/repo'",
      call. = FALSE
    )
  }

  fragments <- strsplit(url, "/+")[[1]]

  data.frame(
    filename = entry$filename,
    platform = "github",
    owner = fragments[3],
    repo = fragments[4]
  )
}


#' Extract software repositories
#'
#' Extract GitHub repository information from a collection of software
#' metadata entries.
#'
#' Each entry is expected to contain repository information in
#' `extra.links.source`. The extraction of a single repository is delegated
#' to [extract_software_repository()].
#'
#' @param entries A list of software entries returned by
#'   [read_software_entries()].
#'
#' @return
#' A data frame containing one row per software repository with the following
#' columns:
#'   - `filename`: The Markdown file associated with the software entry.
#'   - `platform`: The repository hosting platform.
#'   - `owner`: The repository owner.
#'   - `repo`: The repository name.

extract_software_repositories <- function(entries) {
  stopifnot(is.list(entries))

  do.call(
    rbind,
    lapply(entries, extract_software_repository)
  )
}


#' Validate a repository metadata table
#'
#' Validate that a data frame contains the required columns and values needed
#' to query GitHub repository metadata.
#'
#' @param repos A data frame containing repository identifiers. It must
#'   contain the columns `filename`, `owner`, and `repo`.
#'
#' @return
#' No return value.

validate_repository_table <- function(repos) {
  stopifnot(
    is.data.frame(repos),
    nrow(repos) > 0
  )

  required <- c(
    "filename",
    "owner",
    "repo"
  )

  missing <- setdiff(required, names(repos))

  if (length(missing) > 0) {
    stop(
      sprintf(
        "Missing required columns: %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!all(vapply(repos[required], is.character, logical(1)))) {
    stop(
      "Repository metadata columns should be character vectors",
      call. = FALSE
    )
  }

  if (anyNA(repos[, required])) {
    stop(
      "Repository metadata should not contain missing values",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Split repositories into batches
#'
#' Split a repository metadata table into smaller batches.
#' This is used to limit the number of repositories included in a single
#' API request.
#'
#' @param repos A data frame containing repository metadata.
#'
#' @param batch_size An integer giving the maximum number of repositories per
#'   batch. Defaults to 25.
#'
#' @return
#' A list of data frames, each containing at most `batch_size` repositories.

split_repo_batches <- function(repos, batch_size = 25) {
  split(
    repos,
    ceiling(seq_len(nrow(repos)) / batch_size)
  )
}


#' Build a GitHub GraphQL repository query
#'
#' Build a GraphQL query to retrieve metadata for a set of GitHub repositories.
#'
#' @param repos A data frame containing repository identifiers. It must contain
#'   the columns `owner` and `repo`.
#'
#' @return
#' A character string containing a GitHub GraphQL query.

build_github_graphql_query <- function(repos) {
  queries <- vapply(
    seq_len(nrow(repos)),
    function(i) {
      sprintf(
        '
        repo%d: repository(owner: "%s", name: "%s") {
          stargazerCount
          forkCount
          licenseInfo {
            spdxId
          }
          pullRequests(states: OPEN) {
            totalCount
          }
          issues(states: OPEN) {
            totalCount
          }
          defaultBranchRef {
            target {
              ... on Commit {
                committedDate
              }
            }
          }
        }
        ',
        i,
        repos$owner[i],
        repos$repo[i]
      )
    },
    character(1)
  )

  paste0(
    "{\n",
    paste(queries, collapse = "\n"),
    "\n}"
  )
}


#' Parse GitHub GraphQL repository metadata
#'
#' Parse the response returned by the GitHub GraphQL API and convert it into
#' a repository metadata table.
#'
#' @param response
#' A response object returned by [gh::gh_gql()].
#'
#' @param repos
#' A data frame containing the repositories included in the GraphQL request.
#' It must contain the columns `filename`, `owner`, and `repo`.
#'
#' @return
#' A data frame containing one row per repository with GitHub metadata:
#' - `filename`: The Markdown filename associated with the repository.
#' - `owner`: The repository owner.
#' - `repo`: The repository name.
#' - `license`: The SPDX license identifier.
#' - `stars`: The number of stars.
#' - `forks`: The number of forks.
#' - `open_issues`: The number of open issues.
#' - `pull_requests`: The number of open pull requests.
#' - `last_commit`: The date of the latest commit on the default branch.
#'
#' @keywords internal

parse_github_repo_response <- function(response, repos) {
  stopifnot(
    is.list(response),
    "data" %in% names(response)
  )

  do.call(
    rbind,
    lapply(seq_len(nrow(repos)), function(i) {
      repo_data <- response$data[[paste0("repo", i)]]

      if (is.null(repo_data)) {
        return(
          data.frame(
            filename = repos$filename[i],
            owner = repos$owner[i],
            repo = repos$repo[i],
            license = NA_character_,
            stars = NA_integer_,
            forks = NA_integer_,
            open_issues = NA_integer_,
            pull_requests = NA_integer_,
            last_commit = NA_character_
          )
        )
      }

      data.frame(
        filename = repos$filename[i],
        owner = repos$owner[i],
        repo = repos$repo[i],
        license = if (is.null(repo_data$licenseInfo)) {
          NA_character_
        } else {
          repo_data$licenseInfo$spdxId
        },
        stars = repo_data$stargazerCount,
        forks = repo_data$forkCount,
        open_issues = repo_data$issues$totalCount,
        pull_requests = repo_data$pullRequests$totalCount,
        last_commit = if (!is.null(repo_data$defaultBranchRef)) {
          as.character(
            as.Date(repo_data$defaultBranchRef$target$committedDate)
          )
        } else {
          NA_character_
        }
      )
    })
  )
}


#' Fetch GitHub metadata for a repository batch
#'
#' Retrieve GitHub repository metadata for a batch of repositories using the
#' GitHub GraphQL API.
#'
#' This function builds a GraphQL query using
#' [build_github_graphql_query()], sends the request through [gh::gh_gql()],
#' and parses the response using [parse_github_repo_response()].
#'
#' @param repos A data frame containing repository identifiers. It must contain
#'   the columns `filename`, `owner`, and `repo`.
#'
#' @return
#' A data frame containing one row per repository with GitHub metadata.

fetch_github_repo_batch <- function(repos) {
  query <- build_github_graphql_query(repos)

  response <- gh::gh_gql(query)

  parse_github_repo_response(
    response,
    repos
  )
}


#' Normalize GitHub repository metadata
#'
#' Normalize repository metadata returned by the GitHub API so that it matches
#' the conventions used by the website.
#'
#' @param meta A data frame containing GitHub repository metadata.
#'
#' @return
#' A data frame containing normalized GitHub repository metadata.

normalize_github_metadata <- function(meta) {
  stopifnot(is.data.frame(meta))

  meta$license <- ifelse(
    meta$license == "NOASSERTION",
    "MIT",
    meta$license
  )

  meta
}


#' Fetch GitHub repository metadata
#'
#' Retrieve metadata for GitHub repositories using the GitHub GraphQL API.
#' Repository information is requested in batches to limit the size of API
#' requests.
#'
#' @param repos A data frame containing repository identifiers. It must contain
#'   the columns `filename`, `owner`, and `repo`.
#'
#' @param batch_size An integer giving the maximum number of repositories per
#'   API request. Defaults to `25`.
#'
#' @return
#' A data frame containing one row per repository with GitHub metadata.

fetch_github_repo_metadata <- function(repos, batch_size = 25) {
  validate_repository_table(repos)

  stopifnot(
    length(batch_size) == 1,
    is.numeric(batch_size),
    batch_size > 0
  )

  batches <- split_repo_batches(
    repos,
    batch_size
  )

  meta <- do.call(
    rbind,
    lapply(batches, fetch_github_repo_batch)
  )

  rownames(meta) <- NULL

  normalize_github_metadata(meta)
}


#' Update GitHub metadata section
#'
#' Update the `extra.github` section of software metadata with new GitHub
#' repository information.
#'
#' If the section does not exist, it is created. Existing sections of
#' `extra` are preserved unchanged.
#'
#' @param extra A named list containing software metadata sections.
#'
#' @param github A named list or data frame row containing normalized GitHub
#'   repository metadata.
#'
#' @return
#' A named list containing the updated software metadata sections.

update_github_section <- function(extra, github) {
  stopifnot(
    is.list(extra),
    is.list(github) || is.data.frame(github)
  )

  github <- as.list(github)

  github_fields <- c(
    "license",
    "stars",
    "forks",
    "open_issues",
    "pull_requests",
    "last_commit"
  )

  extra$github <- github[github_fields]

  extra
}


#' Update GitHub metadata of a software entry
#'
#' Update the GitHub metadata section of a software entry using metadata
#' retrieved from the GitHub API.
#'
#' The function updates only the `extra.github` section of the entry and
#' leaves other metadata sections unchanged.
#'
#' @param entry
#' A software entry returned by [read_software_entries()].
#'
#' @param github_metadata
#' A data frame containing GitHub repository metadata. It must contain a
#' `filename` column matching the software entry filename.
#'
#' @return
#' A software entry with updated GitHub metadata.

update_software_entry_github <- function(entry, github_metadata) {
  stopifnot(
    is.list(entry),
    all(c("filename", "frontmatter") %in% names(entry)),
    is.data.frame(github_metadata),
    "filename" %in% names(github_metadata)
  )

  github <- github_metadata[
    github_metadata$filename == entry$filename,
    ,
    drop = FALSE
  ]

  if (nrow(github) != 1) {
    stop(
      sprintf(
        "GitHub metadata not found for '%s'",
        entry$filename
      ),
      call. = FALSE
    )
  }

  entry$frontmatter$extra <- update_github_section(
    entry$frontmatter$extra,
    github[1, ]
  )

  entry
}


#' Update GitHub metadata of software entries
#'
#' Update the GitHub metadata section of all software entries using metadata
#' retrieved from the GitHub API.
#'
#' Each entry is matched to GitHub metadata using its `filename`.
#'
#' @param entries
#' A list of software entries returned by [read_software_entries()].
#'
#' @param github_metadata
#' A data frame containing normalized GitHub repository metadata. It must
#' contain a `filename` column matching software entries.
#'
#' @return
#' A list of software entries with updated GitHub metadata.

update_software_entries_github <- function(entries, github_metadata) {
  stopifnot(
    is.list(entries),
    is.data.frame(github_metadata)
  )

  lapply(
    entries,
    update_software_entry_github,
    github_metadata = github_metadata
  )
}


#' Extract software metrics
#'
#' Extract metrics used to compute software ranking weights from updated
#' software metadata entries.
#'
#' This function expects entries enriched with GitHub metadata by
#' [update_software_entries_github()]. Metrics are extracted from the parsed
#' TOML front matter.
#'
#' @param entries A list of software entries returned by
#'   [update_software_entries_github()].
#'
#' @return
#' A data frame containing one row per software entry with the following
#' columns:
#'   - `filename`: The Markdown filename associated with the software entry.
#'   - `status`: The software status.
#'   - `stars`: The number of GitHub stars.

extract_software_metrics <- function(entries) {
  stopifnot(is.list(entries))

  do.call(
    rbind,
    lapply(entries, function(entry) {
      stopifnot(
        is.list(entry),
        all(c("filename", "frontmatter") %in% names(entry))
      )

      extra <- entry$frontmatter$extra

      status <- if ("status" %in% names(extra)) {
        extra$status
      } else {
        NA_character_
      }

      stars <- if (
        "github" %in% names(extra) && "stars" %in% names(extra$github)
      ) {
        extra$github$stars
      } else {
        NA_integer_
      }

      data.frame(
        filename = entry$filename,
        status = status,
        stars = as.integer(stars),
        stringsAsFactors = FALSE
      )
    })
  )
}


#' Compute software weights
#'
#' Compute and assign Zola weights to software entries based on their status
#' and popularity metrics.
#'
#' The weight is assigned as a top-level front matter field and is not stored
#' in the `extra` metadata section.
#'
#' @param entries A list of software entries returned by
#' [update_software_entries_github()].
#'
#' @return
#' A list of software entries with updated Zola weights.

compute_software_weights <- function(entries) {
  metrics <- extract_software_metrics(entries)

  metrics$status <- factor(
    metrics$status,
    levels = c(
      "stable",
      "experimental",
      "deprecated"
    ),
    ordered = TRUE
  )

  metrics <- metrics[
    order(
      metrics$status,
      -metrics$stars,
      metrics$filename
    ),
  ]

  weights <- seq_len(nrow(metrics))

  lapply(entries, function(entry) {
    index <- match(
      entry$filename,
      metrics$filename
    )

    if (!is.na(index)) {
      entry$frontmatter$weight <- weights[index]
    }

    entry
  })
}


#' Check whether software metadata has changed
#'
#' Compare two software metadata objects after TOML serialization.
#'
#' This function is used to avoid rewriting Markdown files when the generated
#' metadata is identical to the existing one.
#'
#' @param old A named list containing the original TOML front matter.
#'
#' @param new A named list containing the updated TOML front matter.
#'
#' @return
#' A logical value indicating whether the metadata has changed.

has_metadata_changed <- function(old, new) {
  stopifnot(
    is.list(old),
    is.list(new)
  )

  old_toml <- serialize_toml(old)
  new_toml <- serialize_toml(new)

  !identical(old_toml, new_toml)
}


#' Serialize TOML metadata
#'
#' Convert a TOML metadata object into its textual representation.
#'
#' @param x A named list containing TOML-compatible metadata.
#'
#' @return
#' A character vector containing the serialized TOML expression.

serialize_toml <- function(x) {
  stopifnot(
    is.list(x)
  )

  toml::write_toml(x)
}


#' Write a software Markdown file
#'
#' Write the TOML front matter of a software entry to a Markdown file.
#'
#' The generated file is wrapped with TOML front matter delimiters (`+++`).
#'
#' @param filename A character string. The name of the Markdown file to write.
#'
#' @param x A TOML expression containing the software metadata.
#'
#' @return
#' No return value.

write_software_md_file <- function(filename, x) {
  stopifnot(
    length(filename) == 1,
    is.character(filename)
  )

  x <- c(
    "+++",
    as.character(x),
    "+++"
  )

  filename <- file.path(path_software(), filename)

  writeLines(
    x,
    filename,
    useBytes = TRUE
  )

  invisible(NULL)
}


#' Write software metadata entries
#'
#' Serialize and write modified software metadata entries to Markdown files.
#'
#' Only entries whose metadata changed are written to disk.
#'
#' @param entries
#' A list of software entries returned by
#' [update_software_entries_github()].
#'
#' @param original_entries
#' A list of software entries before metadata updates.
#'
#' @return
#' No return value.

write_software_entries <- function(entries, original_entries) {
  stopifnot(
    is.list(entries),
    is.list(original_entries),
    length(entries) == length(original_entries)
  )

  mapply(
    function(entry, original) {
      if (
        !has_metadata_changed(
          original$frontmatter,
          entry$frontmatter
        )
      ) {
        return(invisible(NULL))
      }

      toml <- serialize_toml(entry$frontmatter)

      write_software_md_file(
        entry$filename,
        toml
      )

      invisible(NULL)
    },
    entries,
    original_entries
  )

  invisible(NULL)
}


#' Update software metadata
#'
#' Update software Markdown files with metadata retrieved from external
#' sources.
#'
#' The update pipeline reads existing software entries, extracts repository
#' information, fetches GitHub metadata, updates entries, computes weights,
#' and writes modified files back to disk.
#'
#' @return
#' No return value.

update_software_metadata <- function() {
  entries <- read_software_entries()

  repositories <- extract_software_repositories(entries)

  github_metadata <- fetch_github_repo_metadata(
    repositories
  )

  updated_entries <- update_software_entries_github(
    entries,
    github_metadata
  )

  updated_entries <- compute_software_weights(
    updated_entries
  )

  write_software_entries(
    updated_entries,
    entries
  )

  invisible(NULL)
}
