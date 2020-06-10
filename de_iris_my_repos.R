local({
  requires_pkg <- function(...) {
    pkgs <- c(...)
    lapply(pkgs, function(pkg) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop("`", pkg, "` is required: install.packages('", pkg, "')", call. = FALSE)
      }
    })
    invisible(TRUE)
  }

  if (requires_pkg("magrittr")) {
    `%>%` <- magrittr::`%>%`
  }

  gh_search_for_iris <- function(user = gh::gh_whoami()$login, extensions = c("r", "rmd", "md")) {
    extensions <- if (!is.null(extensions)) {
      paste0("+", paste0("extension:", extensions, collapse = "+"))
    } else ""
    q_search <- glue::glue(
      "https://api.github.com/search/code",
      "?q=iris+user:{user}{extensions}"
    )
    if (!length(q_search)) {
      stop(
        "Unable to create query string, you may need to manually provide your GitHub `user` name.",
        call. = FALSE
      )
    }
    iris <- gh::gh(q_search)

    if (!length(iris$items)) {
      return(dplyr::tibble())
    }

    iris$items %>%
      purrr::map(`[`, c("name", "path", "sha", "url", "html_url", "repository")) %>%
      purrr::map_dfr(function(item) {
        item$owner <- item$repository$owner$login
        item$repo <- item$repository$name
        item$repo_is_fork <- item$repository$fork
        item$repo_url <- item$repository$html_url
        item$issues_url <- item$repository$issues_url
        item$repository <- NULL
        item
      }) %>%
      dplyr::filter(!repo_is_fork) %>%
      dplyr::rename(file_sha = sha) %>%
      dplyr::mutate(file = purrr::pmap(., gh_get_master_file)) %>%
      dplyr::mutate(matching_lines = purrr::map(file, lines_with_matches)) %>%
      dplyr::filter(purrr::map_lgl(matching_lines, ~ length(.x) > 0))
  }

  gh_decode_content <- function(x) {
    if (is.list(x)) x <- x$content
    stopifnot(is.character(x))
    base64enc::base64decode(x) %>%
      rawToChar() %>%
      strsplit("\n") %>%
      .[[1]]
  }

  gh_get_matched_file <- function(owner, repo, file_sha, ...) {
    gh::gh(glue::glue("/repos/{owner}/{repo}/git/blobs/{file_sha}")) %>%
      gh_decode_content()
  }

  gh_get_master_file <- function(owner, repo, path, ...) {
    gh::gh(glue::glue("/repos/{owner}/{repo}/contents/{path}")) %>%
      gh_decode_content()
  }

  lines_with_matches <- function(file) {
    which(grepl("iris", file, fixed = TRUE))
  }

  print_match <- function(path, file, matching_lines, ...) {
    cli::cli_h2(path)
    for (ml in matching_lines) {
      if (ml != matching_lines[1]) cli::cat_line()
      for (line in seq(max(1, ml - 1), min(length(file), ml + 1))) {
        text <- file[line]
        text <- gsub("[{]", "\u2774", text)
        text <- gsub("[}]", "\u2775", text)
        text <- gsub("iris", "{strong iris}", text)
        if (line == ml) {
          cliapp::cli_alert_danger("{{envvar {line}}} {text}")
        } else {
          cliapp::cli_alert_info("{{envvar {line}}} {text}")
        }
      }
    }
  }

  show_repo_matches <- function(owner, repo, files, ..., ask = TRUE) {
    if (isTRUE(ask)) {
      cli::cli_h1("{owner}/{repo}")
      n_files <- nrow(files)
      n_refs <- sum(purrr::map_int(files$matching_lines, length))
      cli::cli_text("{.code iris} is referenced on {.strong {n_refs} line{?s}} in {.strong {n_files} file{?s}} in {owner}/{repo}")
      cli::cli_ul(files$path)
    }
    gh_create_repo_issue(owner, repo, files, ask = ask)
  }

  gh_issues <- function(owner, repo) {
    issues <- gh::gh(glue::glue("/repos/{owner}/{repo}/issues"))
    if (!length(issues)) return(FALSE)
    issue_numbers <- as.numeric(purrr::map_chr(issues, "number"))
    names(issue_numbers) <- purrr::map_chr(issues, "title")
    issue_numbers
  }

  gh_create_repo_issue <- function(owner, repo, files, ..., ask = TRUE) {
    collapse <- function(x) glue::glue_collapse(x, sep = "\n")

    title <- glue::glue("Remove references to iris")
    issues <- gh_issues(owner, repo)
    if (length(issues) && title %in% names(issues)) {
      issue_number <- unname(issues[title])
      cli::cli_alert_warning("An open issue to remove iris already exists at {owner}/{repo}#{issue_number}")
      return(issue_number)
    }

    body_files <- glue::glue("- [ ] [{files$path}]({files$path})") %>% collapse()
    body_lines <- purrr::map2_chr(
      files$html_url,
      files$matching_lines,
      function(url, lines) {
        glue::glue("{url}#L{lines}") %>% collapse()
      }) %>%
      collapse()
    body <- glue::glue(
      "It's time to [move on from the `iris` dataset](https://armchairecology.blog/iris-dataset/). ",
      "Try some of the other datasets in `data()`, ",
      "or data in other packages such as `ggplot2::mpg`, `dplyr::starwars`, ",
      "[<code>fivethirtyeight::bechdel</code>](https://fivethirtyeight-r.netlify.app/articles/bechdel.html), ",
      "or this dataset about [penguins](https://github.com/allisonhorst/penguins).",
      "\n\nReference(s) to `iris` occur in the following files.\n\n{body_files}",
      "\n\n### References\n\n{body_lines}"
    )

    do <- if (ask) {
      cliapp::cli_text("Create issue in {{strong {owner}/{repo}}}: {{emph {title}}}?")
      yesno::yesno("")
    } else TRUE

    if (!do) return(NA_real_)

    endpoint <- glue::glue("POST /repos/{owner}/{repo}/issues")
    res <- gh::gh(endpoint, title = title, body = body)
    status <- gsub("(\\d+).*", "\\1", attributes(res)$response$status)
    if (grepl("20\\d", status)) {
      cliapp::cli_alert_success("Created issue #{{strong {res$number}}} in {owner}/{repo}")
    } else {
      httr::stop_for_status(as.numeric(status))
    }
    as.numeric(res$number)
  }

  gh_create_file_issue <- function(owner, repo, paths, html_urls, matching_lines, ..., ask = TRUE) {
    body <- glue::glue("{html_url}#L{matching_lines}")
    body <- glue::glue_collapse(body, sep = "\n")
    body <- glue::glue("Remove {length(matching_lines)} reference(s) to `iris` from {path}.\n\n{body}")
    title <- glue::glue("Remove iris from {path}")
    do <- if (ask) {
      cliapp::cli_text("Create issue in {{strong {owner}/{repo}}}: {{emph {title}}}?")
      yesno::yesno2("", yes = "Yes, open an issue", no = "No")
    } else TRUE
    if (!do) return(NA_real_)
    endpoint <- glue::glue("POST /repos/{owner}/{repo}/issues")
    res <- gh::gh(endpoint, title = title, body = body)
    status <- gsub("(\\d+).*", "\\1", attributes(res)$response$status)
    if (grepl("20\\d", status)) {
      cliapp::cli_alert_success("Created issue #{{strong {res$number}}} in {owner}/{repo}")
    } else {
      httr::stop_for_status(as.numeric(status))
    }
    as.numeric(res$number)
  }

  de_iris_my_repos <<- function(user = NULL, extensions = c("r", "rmd", "md"), dry_run = NULL, ask = NULL) {
    requires_pkg("gh", "cliapp", "cli", "purrr", "httr", "dplyr", "base64enc", "glue", "yesno", "magrittr", "tidyr")

    if (is.null(user)) user <- gh::gh_whoami()$login

    cliapp::start_app()
    cliapp::cli_alert_info(
      "Searching for references to {code iris} in repositories by {{strong {user}}"
    )
    iris_results <- gh_search_for_iris(user, extensions)
    if (!nrow(iris_results)) {
      cliapp::cli_alert_success(
        "Awesome! No references to {code iris} were found in the default branch of {user}'s repositories."
      )
      return()
    } else {
      n_matches <- sum(purrr::map_dbl(iris_results$matching_lines, length))
      n_files <- nrow(iris_results)
      n_repos <- length(unique(iris_results$repo))
      cli::cli_alert_danger(
        "{n_matches} reference{?s} found in {n_files} file{?s} in {n_repos} repo{?s}"
      )
    }

    if (is.null(dry_run)) {
      dry_run <- !yesno::yesno2(
        "Do you want to review results and open issues?",
        yes = "Yes, review and open",
        no = "No, just return results as a tibble"
      )
    }

    if (isTRUE(dry_run)) {
      return(iris_results)
    }

    if (is.null(ask)) {
      cli::cli_alert(
        "Automatically open issues in {n_repos} repositor{?y/ies}? (Choose a negative option to decide individually.)"
      )
      ask <- !yesno::yesno("")
    }

    iris_results <- iris_results %>%
      tidyr::nest(files = c(name, path, file_sha, url, html_url, file, matching_lines))

    iris_results$issue_number <- purrr::pmap_dbl(
      iris_results, show_repo_matches, ask = ask
    )

    invisible(iris_results)
  }
})
