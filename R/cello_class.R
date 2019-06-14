
#' Cello class for storing dimension reduction info
#' @export
Cello <- setClass("Cello",
                  slots = c(
                      name = "character", # The name of cvis
                      idx = "numeric", # The index of global cds object
                      proj = "list", # The projections as a list of data frame
                      pmeta = "data.frame", # The local meta data
                      notes = "character" # Other information to display to the user
                  )
)

