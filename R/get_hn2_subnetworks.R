#' Read in HotNet2 results
#'
#' Point this function at the output directory from HotNet2 and it will retrieve
#' the results from that analysis.
#'
#' @param output_dir The output directory from HotNet2.
#'
#' @return An S3 object with the parsed results from HotNet2.
#'
#' @export get_hn2_subnetworks
get_hn2_subnetworks <- function(output_dir) {

    # check if the output directory exists
    if (!dir.exists(output_dir)) {
        stop(glue::glue("The directory {output_dir} is not accessible or does not exist."))
    }







}
