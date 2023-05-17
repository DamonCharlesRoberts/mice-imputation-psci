'.__module__.'
#' execute query to database and organize data as needed for ridgeline
#' @param datasets number of samples
#' @param type type of data to access
#' @param connection to database
organize_data <- function (
    datasets=1000
    ,type="original"
    ,con=NULL
) {
    # execute query
        #* define table names
    queryList <- base::lapply(
        1:datasets-1
        ,function (x) {
            base::paste0(type,"_",x)
        }
    )
        #* execute the query
    originalList <- base::lapply(
        queryList
        , function (x) {
            data.table::as.data.table(
                DBI::dbGetQuery(
                    con
                    ,base::paste0(
                        "SELECT X,Z,Y FROM "
                        ,x 
                        ,sep = ""
                    )
                )
            )
        }
    )
    # bind the datasets together but provide an id column
    bindedList <- data.table::rbindlist(
        originalList
        ,idcol="dataset"
    )
    # Close connection to database
    DBI::dbDisconnect(con)
    # return result
    return(bindedList)
}
