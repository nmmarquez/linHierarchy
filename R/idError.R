#' Calls error for class non-conformity
#' 
#' Error if object not of specified class
#' @param obj object to test class of.
#' @param class the class for which the object is tested
#' @export

idError <- function (obj, class = 'interData'){
    if (class (obj) != class){
        stop (deparse (substitute (obj)), ' argument must be of the class ', 
              class)
    }
}