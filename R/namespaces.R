cache <- new.env(hash = TRUE, parent = emptyenv())
cache$namespaces <- list()
cache$hooks <- new.env(hash = TRUE, parent = emptyenv())
cache$handlers <- new.env(hash = TRUE, parent = emptyenv())
cache$enabled <- new.env(hash = TRUE, parent = emptyenv())

#' @title Get the list of namespaces in the order in which they will be
#'        processed.
#'
#' @return A character vector
#' @examples
#' get_namespaces()
#' @export
get_namespaces <- function(disabled = FALSE) {
  cache$namespaces
}

#' @title Set the list of namespaces in the order in which they will be
#'        processed. The namespaces should already exist.
#'
#' @param namespaces A chacter vector
#' @return The function is only executed for its side-effect.
#' @examples
#' set_namespace(list("namespace-1", "namespace-2"))
#' @export
set_namespaces <- function(namespaces) {
  assign("namespaces", namespaces, cache)
}

#' @title Check is a namespace exists.
#'
#' @param namespaces A chacter vector
#' @return TRUE if the namespace exists, FALSE otherwise.
#' @examples
#' exists_namespace("namespace-1")
namespace_exists <- function(namespace) {
  namespaces <- get_namespaces()
  is.element(namespace, namespaces)
}

#' @title Initializes processor namespace if it does not exist.
#'
#' @description This function checks if the namespace already exists. If it does
#'              not, then the namespace is set as the next namespace in the
#'              sequence of namespaces and the hooks and handlers of this
#'              namespace are set to NULL.
#' @param namespace A character
#' @return The function is only executed for its side-effect.
initialize_namespace <- function(namespace) {
  if(namespace_exists(namespace)) return(NULL)
  namespaces <- get_namespaces()
  set_namespaces(append(namespaces, list(namespace)))
  assign(namespace,
         list(environment_begin = NULL,
              environment_finish = NULL,
              function_begin = NULL,
              function_finish = NULL),
         cache$hooks)
  assign(namespace,
         list(environment = NULL,
              function_header = NULL,
              function_formals = NULL,
              function_body = NULL),
         cache$handlers)
  assign(namespace, TRUE, cache$enabled)

}

enable_namespace <- function(namespace) {
  assign(namespace, TRUE, cache$enabled)
}

disable_namespace <- function(namespace) {
  assign(namespace, FALSE, cache$enabled)
}

namespace_position <- function() {
}

`namespace_position<-` <- function(namespace, position) {

}

## register_processor <- function(processor_name, ) {

## }

## deregister_processor <- function(processor_name) {

## }

check_namespace_exists <- function(namespace) {
  if(!namespace_exists(namespace))
    stop("namespace '", namespace, "' does not exist")
}

