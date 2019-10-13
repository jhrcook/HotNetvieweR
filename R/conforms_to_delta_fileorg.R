#' Checks if a directory for a delta value conforms to the expected output
#'
#' Returns \code{TRUE} or \{FALSE} for whether a directory conforms to the
#' expected organization of the delta directory output by HotNet2.
#'
#' @param delta_dir The delta directory.
#'
#' @return A boolean value for wether the directory conforms to the expected file organization.
#'
#' @export conforms_to_delta_fileorg
conforms_to_delta_fileorg <- function(delta_dir) {

    components_txt <- file.path(delta_dir, "components.txt")
    if (!check_for_file(components_txt)) return(FALSE)

    results_json <- file.path(delta_dir, "results.json")
    if (!check_for_file(results_json)) return(FALSE)

    significance_txt <- file.path(delta_dir, "significance.txt")
    if (!check_for_file(significance_txt)) return(FALSE)

    return(TRUE)
}


check_for_file <- function(fname) {
    if (!file.exists(fname)) {
        message(glue::glue("Expected file at {fname} is not available."))
        return(FALSE)
    }
    return(TRUE)
}
