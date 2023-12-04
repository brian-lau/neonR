#' NeonR: Class definition
#'
#' @return
#' @export
#'
#' @examples
NeonR <- function(
){
  obj <- list(
    info         = tibble::tibble(),
    scene_camera = tibble::tibble(),
    sections     = tibble::tibble(),
    has_trials   = FALSE,
    data         = tibble::tibble()
  )

  class(obj) <- "neonr"
#  class(obj) <- append(class(obj), "neonr")

  return(obj)
}

#' #' @export
#' print <- function(x, ...){
#'   UseMethod("print", x)
#' }

#' print.neonr:
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
print.neonr <- function(x){
  print(x[["data"]])
  #return(obj$data$gaze)
}