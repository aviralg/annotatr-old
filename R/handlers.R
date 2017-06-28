handler_types <- c("environment", "function_header",
                   "function_formals", "function_body")

check_handler_type <- function(type)
  if (!is.element(type, handler_types))
    stop("unknown handler type '", type,
         "'; should be one of ", paste(handler_types, collapse = ", "))

annotation_handlers <- function(namespace, type) {
  check_handler_type(type)
  check_namespace_exists(namespace)
  cache$handlers[[namespace]][[type]]
}

## `annotation_handlers<-` <- function(namespace, type, handlers) {
##   check_handler_type(type)
##   initialize_namespace(namespace)
##   all_handlers <- cache$handlers[[namespace]]
##   all_handlers[[type]] <- handlers
##   assign(namespace, all_handlers, cache$handlers)
## }

register_annotation_handler <- function(namespace, type, handler) {
  register_handlers(namespace, type, list(handler))
}  

deregister_annotation_handler <- function(namespace, type, handler) {
  deregister_handlers(namespace, type, list(handler))
}

register_annotation_handlers <- function(namespace, type, handlers) {
  register_handlers(namespace, type, handlers)
}  

deregister_annotation_handlers <- function(namespace, type, handlers) {
  deregister_handlers(namespace, type, handlers)
}

register_handlers <- function(namespace, type, handlers) {
  check_handler_type(type)
  initialize_namespace(namespace)
  all_handlers <- cache$handlers[[namespace]]
  all_handlers[[type]] <- append(all_handlers[[type]], handlers)
  assign(namespace, all_handlers, cache$handlers)
}

deregister_handlers <- function(namespace, type, handlers) {
  check_handler_type(type)
  check_namespace_exists(namespace)
  all_handlers <- cache$handlers[[namespace]]
  all_handlers[[type]] <- setdiff(all_handlers[[type]], handlers)
  assign(namespace, all_handlers, cache$handlers)
}

handler_modes <- c("individual", "digest", "once")

check_handler_mode <- function(mode)
  if (!is.element(mode, handler_modes))
    stop("unknown handler mode type '", mode,
         "'; should be one of ", paste(handler_modes, collapse = ", "))

create_handler <- function(name, matcher, action,
                           mode = "individual",
                           remove = FALSE) {
  check_handler_mode(mode)
  handler <- list(name = name, matcher = matcher,
                  action = action, mode = mode,
                  remove = remove)
  class(handler) <- c("annotation.handler")
  handler
}

print.annotation.handler <- function(handler) {
  cat(paste("Annotation Handler ::", "\n",
            "Name    :: ", handler$name, "\n",
            "Matcher :: ", handler$matcher, "\n",
            "Action  :: ", handler$action, "\n",
            "Mode    :: ", handler$mode, "\n",
            "Remove  :: ", handler$remove, "\n",
            sep = ""))
}
