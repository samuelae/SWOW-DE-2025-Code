# Setup
token <- Sys.getenv("fp-ollama")
base_url <- "https://fp-ollama.psycho.unibas.ch"

# Functions
get_models <- function() {
  # https://docs.openwebui.com/getting-started/api-endpoints#-retrieve-all-models
  req <- httr2::request(paste0(base_url, "/api/models")) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    )
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  models <- c()
  for (i in 1:length(resp$data)) {
    models[i] <- resp$data[[i]]$name
  }
  models
}
chat_completion <- function(
  user,
  system = NULL,
  model = get_models()[1],
  temperature = NULL,
  max_tokens = NULL,
  display = FALSE,
  raw = FALSE
) {
  # https://docs.openwebui.com/getting-started/api-endpoints#-chat-completions

  # prepare request body
  body <- list(model = model)
  if (!is.null(system)) {
    body$messages <- list(
      list(role = "system", content = system),
      list(role = "user", content = user)
    )
  } else {
    body$messages <- list(list(role = "user", content = user))
  }
  if (!is.null(temperature)) {
    body$temperature = temperature
  }
  if (!is.null(temperature)) {
    body$max_tokens = max_tokens
  }
  body$stream = FALSE

  # post request
  req <- httr2::request(paste0(base_url, "/api/chat/completions")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body)
  resp <- req |> httr2::req_perform()
  resp_json <- resp |> httr2::resp_body_json()
  content <- resp_json$choices[[1]]$message$content
  if (display) {
    markdown_with_header <- paste0(
      "
---
title: '",
      model,
      " Response'
---
",
      content
    )
    tmp <- tempfile(fileext = ".md")
    writeLines(markdown_with_header, tmp)
    rmarkdown::render(tmp, output_format = "html_document", quiet = TRUE)
    rstudioapi::viewer(gsub("\\.md$", ".html", tmp))
    return(dplyr::if_else(raw, content, stringr::str_squish(content)))
  } else {
    return(dplyr::if_else(raw, content, stringr::str_squish(content)))
  }
}
