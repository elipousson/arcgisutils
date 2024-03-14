#' Vector of valid ESRI service types
#'
#' Length 9 character vector
#' @noRd
esri_service_types <- c(
  "MapServer", "FeatureServer", "ImageServer", "GeoDataServer",
  "GeocodeServer", "GeometryServer", "GPServer", "WFSServer", "WFCServer"
)

#' Vector of valid ESRI layer types
#'
#' Length 3 character vector
#' @noRd
esri_layer_types <- c(
  "FeatureLayer", "Table", "GroupLayer"
)

#' Named vector of ESRI Item URL patterns
#'
#' Length 9 character vector of item or content URL patterns.
#' @noRd
esri_item_url_patterns <- c(
  "content" = "/home/content\\.html\\?view=",
  "search" = "/home/search\\.html",
  "item" = "/home/item\\.html\\?id=",
  "group" = "/home/group\\.html\\?id=",
  "user" = "/home/user\\.html\\?user=",
  "scene" = "/home/webscene/viewer\\.html\\?webscene=",
  "app" = "/index\\.html\\?appid=",
  "notebook" = "/notebook/notebook\\.html\\?rid=",
  "experience" = "/experience/"
)


#' List ESRI url types and patterns
#'
#' @name list_esri_url
#' @keywords internal
NULL

#' - [list_esri_service_type_patterns()] returns a vector of ESRI service types
#'
#' @name list_esri_service_type_patterns
#' @rdname list_esri_url
list_esri_service_type_patterns <- function() {
  setNames(
    paste0("(?<=/rest/services).+/", esri_service_types, "/"),
    esri_service_types
  )
}

#' - [list_esri_service_url_patterns()] returns a vector of ESRI URL patterns for data services and items
#' @rdname list_esri_url
list_esri_service_url_patterns <- function() {
  c(
    "root" = "/rest/services/?$",
    "service" = paste0("(?<=/rest/services)",
                       "(", paste0(esri_service_types, collapse = "|"), ")/?$"),
    "layer" = paste0("(?<=/rest/services)",
                     "(", paste0(esri_service_types, collapse = "|"), ")",
                     "/[[:digit:]]+/?$"),
    list_esri_service_type_patterns()
  )
}

#' - [list_esri_url_messages()] returns a vector of messages for what defines a valid URL of the specified type
#' @name list_esri_url_messages
#' @rdname list_esri_url
list_esri_url_messages <- function() {
  c(
    "root" = "A {.val {type}} URL must end in {.str /rest/services}.",
    "folder" = "A {.val {type}} URL must be a 'Folder' endpoint",
    "service" = "A {.val {type}} URL must end in a supported service type: {.or {esri_service_types}}",
    "feature" = "A {.val {type}} URL must end in a feature ID.",
    "content" = "A {.val {type}} URL must include the text {.str /content/}.",
    "search" = "A {.val {type}} URL must include the text {.str /home/search.html?q=}.",
    "item" = "An {.val {type}} URL must include the text {.str /home/item.html?id=}.",
    "group" = "A {.val {type}} URL must include the text {.str /home/group.html?id=}.",
    "user" = "A {.val {type}} URL must include the text {.str /home/user.html?user=}.",
    "scene" = "A {.val {type}} URL must include the text {.str /home/webscene/viewer.html?webscene=}.",
    "app" = "An {.val {type}} URL must include the text {.str /index.html?appid=}.",
    "notebook" = "An {.val {type}} URL must include the text {.str /notebook/notebook.html?rid=}.",
    "experience" = "An {.val {type}} URL must include the text {.str /experience/}.",
    setNames(
      paste0("A {.val {type}} URL must include the text {.str /", esri_service_types, "/} after {.str /rest/services/}"),
      esri_service_types
    )
  )
}
#' - [list_esri_url_messages()] returns a vector of all URL types or a specified url type
#' @name list_esri_url_types
#' @rdname list_esri_url
list_esri_url_types <- function(type = NULL) {
  types <- c(
    esri_service_url_patterns,
    esri_item_url_patterns,
    list_esri_service_url_patterns()
  )

  if (is.null(type)) {
    return(types)
  }

  types[[type]]
}

#' Is x an ESRI service or item URL?
#'
#' @description
#' A family of functions for testing objects to determine if they are a URL for
#' an ESRI service or item. These internal functions are used by
#' [check_esri_url()]. Checking if a URL is a folder URL requires the item
#' metadata and can't be determined based on the URL pattern alone.
#'
#' - [is_esri_services_url()]: Does x match the pattern of a ESRI Service URL?
#' - [is_esri_folder_url()]: Is x a folder URL?
#' - [is_esri_item_url()]: Does x match the pattern of a ESRI Item URL?
#'
#' @name is_esri_url
#' @keywords internal
NULL

#' @name is_esri_services_url
#' @rdname is_esri_url
#' @export
is_esri_services_url <- function(x, type = NULL) {
  is_url(
    x,
    pattern = set_url_pattern(list_esri_service_url_patterns(), nm = type),
    perl = TRUE
  )
}

#' @name is_esri_item_url
#' @rdname is_esri_url
#' @export
is_esri_item_url <- function(x, type = NULL) {
  is_url(
    x,
    pattern = set_url_pattern(esri_item_url_patterns, nm = type),
    perl = TRUE
  )
}

#' @name is_esri_folder_url
#' @rdname is_esri_url
#' @export
is_esri_folder_url <- function(x, metadata = NULL, ..., call = caller_env()) {
  if (is.null(metadata)) {
    check_string(x, call = call)
    metadata <- fetch_layer_metadata(httr2::request(x), ..., error_call = call)
  }

  # FIXME: This is carried over from esri2sf but needs to be double-checked
  is_url(x, pattern = "/rest/services") & ("folders" %in% names(metadata))
}


#' Is x an ESRI url of the specified type?
#'
#' @inheritParams check_url
#' @param type Default "service". `type` must be a string matching a service URL
#'   type ("root", "folder", "service", "feature") or a content url type ("content",
#'   "search", "item", "group", "user", "scene", "app", "notebook", "experience"). `type` can
#'   also be a service type: "MapServer", "FeatureServer", "ImageServer",
#'   "GeoDataServer", "GeocodeServer", "GeometryServer", "GPServer",
#'   "WFSServer", or "WFCServer".
#' @keywords internal
check_esri_url <- function(x,
                           type = "service",
                           values = NULL,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  check_url(x, arg = arg, call = call)

  esri_service_url_patterns <- list_esri_service_url_patterns()
  esri_service_type_patterns <- list_esri_service_type_patterns()

  if (is.null(values)) {
    values <- names(
      c(esri_item_url_patterns,
        esri_service_url_patterns,
        esri_service_type_patterns)
    )
  }

  type <- arg_match(type, values = values)

  if (type %in% names(esri_service_url_patterns)) {
    pattern <- esri_service_url_patterns
  } else if (type %in% names(esri_item_url_patterns)) {
    pattern <- esri_item_url_patterns
  } else if (type %in% names(esri_service_type_patterns)) {
    pattern <- esri_service_type_patterns
  }

  pattern <- set_url_pattern(pattern, type)

  if (is_url(x, pattern = pattern, perl = TRUE)) {
    return(invisible(NULL))
  }

  info_message <- valid_esri_url_messages[type]

  info_message <- set_names(
    info_message,
    rep_len("i", length(info_message))
  )

  cli::cli_abort(
    c("{.arg {arg}} must be a valid {.val {type}} url.",
      validation_messages),
    call = call
  )
}


#' Helper for setting URL pattern
#' @noRd
set_url_pattern <- function(pattern = NULL, nm = NULL, collapse = "|") {
  if (is.null(pattern)) {
    return(pattern)
  }

  if (!is.null(nm)) {
    nm <- match.arg(nm, names(pattern))
    pattern <- pattern[nm]
  }

  paste0(pattern, collapse = collapse)
}

#' Does x match the pattern of a URL?
#' @noRd
is_url <- function(
    x,
    pattern = NULL,
    ...) {
  if (!is_vector(x) || is_empty(x)) {
    return(FALSE)
  }

  url_pattern <-
    "http[s]?://(?:[[:alnum:]]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

  if (is.null(pattern)) {
    return(grepl(url_pattern, x, ...))
  }

  pattern <- paste0(pattern, collapse = "|")

  grepl(url_pattern, x, ...) & grepl(pattern, x, ...)
}


#' Check if x is a valid URL
#'
#' @inheritParams cli::cli_abort
#' @inheritParams check_string
#' @keywords internal
check_url <- function(
    x,
    pattern = NULL,
    ...,
    allow_null = FALSE,
    message = NULL,
    arg = caller_arg(url),
    call = caller_env()) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  if (is_url(x, pattern = pattern, ...)) {
    return(invisible(NULL))
  }

  check_string(
    x,
    allow_empty = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  if (is.null(message)) {
    message <- "{.arg {arg}} must be a valid url,
    not {.obj_type_friendly {x}}."

    if (!is.null(pattern)) {
      message <- "{.arg {arg}} must be a valid url
    matching the pattern {.str {pattern}}"
    }
  }

  cli::cli_abort(
    message,
    call = call
  )
}

#' Check if x is a string
#'
#' @param allow_null Default `FALSE`. If `FALSE`, error if input object is
#'   `NULL`. If `TRUE` and input is `NULL`, invisibly return `NULL`.
#' @param allow_empty Default `TRUE`. If `FALSE`, error if input object is an
#'   empty string, `""`. If `TRUE` and input is `""`, invisibly return `NULL`.
#' @inheritParams rlang::args_error_context
#' @keywords internal
check_string <- function(
    x,
    allow_empty = TRUE,
    allow_null = FALSE,
    arg = caller_arg(x),
    call = caller_env()
) {

  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  message <- "{.arg {arg}} must be a scalar character vector."

  if (is_scalar_character(x)) {
    if (allow_empty || x != "") {
      return(invisible(NULL))
    }

    message <- '{.arg {arg}} must be a non-empty string.'
  }

  cli::cli_abort(
    message,
    call = call
  )
}
