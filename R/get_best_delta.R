#' Finds the best delta for a the HotNet2 results
#'
#' Briefly, ... (explain how the best delta is chosen). More is explained in the
#' vignette "...".
#'
#' @param delta_tib A data frame (or tibble) with the columns ... for all of the delta values.
#' @param pval_cutoff The maximum p-value to use for statistical significance.
#'
#' @return The best delta value to use or \code{NA} if none can be used.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang !!
get_best_delta <- function(delta_df, pval_cutoff = 0.05) {
    delta_best <- delta_df %>%
        dplyr::group_by(delta) %>%
        dplyr::summarise(sig_ks = sum(pval < !!pval_cutoff)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(sig_ks > 0) %>%
        dplyr::arrange(dplyr::desc(sig_ks), delta) %>%
        dplyr::slice(1) %>%
        dplyr::pull(delta)
    if (length(delta_best) > 0) {
        return(delta_best)
    } else {
        # if no significant deltas
        return(NA)
    }
}

utils::globalVariables(
    c("delta", "sig_ks", "pval"),
    add = TRUE
)
