event_types <- c("environment_begin", "environment_end",
                 "function_begin", "function_end")

check_event_type <- function(type)
  if (!is.element(type, event_types))
    stop("unknown event type '", type,
         "'; should be one of ", paste(event_types, collapse = ", "))


event_hook <- function(namespace, type) {
  check_event_type(type)
  check_namespace_exists(namespace)
  cache$hooks[[namespace]][[type]]
}

register_event_hook <- function(namespace, type, hook) {
  check_event_type(type)
  initialize_namespace(namespace)
  all_hooks <- cache$hooks[[namespace]]
  all_hooks[[type]] <- hook
  assign(namespace, all_hooks, cache$hooks)
}

deregister_event_hook <- function(namespace, type) {
  check_event_type(type)
  initialize_namespace(namespace)
  all_hooks <- cache$hooks[[namespace]]
  all_hooks[[type]] <- NULL
  assign(namespace, all_hooks, cache$hooks)
}

