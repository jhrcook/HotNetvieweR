#' Read in the "k" values for a delta directory
#'
#' @param delta_dir The direcotry for a delta value.
#'
#' @return A tibble with columns "size", "expected", "actual", and "pval".
get_k_for_delta <- function(delta_dir) {
    sig_tib <- readr::read_tsv(
        paste0(delta_dir, "/significance.txt"),
        progress = FALSE, col_types = readr::cols())
    colnames(sig_tib) <- c("size", "expected", "actual", "pval")
    return(sig_tib)
}
