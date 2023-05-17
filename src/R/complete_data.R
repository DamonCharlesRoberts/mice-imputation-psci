#' clean the miceRanger output
#'
'.__module__.'
#' @param data_frame
#' @return list of data.frames
#' @export
complete_data <- function (
    data_frame
) {
    mr_list <- base::lapply(
        data_frame
        ,miceRanger::completeData
    )
    mr_df_list <- lapply(
        mr_list
        ,dplyr::bind_rows
        ,.id="dataset"
    )
}