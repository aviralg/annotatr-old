process_header_handler <- function(handler, annotations, name, fun) {
  matcher <- handler$matcher
  action <- handler$action
  mode <- handler$mode
  remove <- handler$remove
  matches <- NULL
  remaining <- list()
  for (annotation in annotations) {
    match <- matcher(annotation)
    if(identical(match, FALSE)) {
        remaining <- append(remaining, list(annotation))
    } else {
      if (!remove) remaining <- append(remaining, list(annotation))
      if  (mode != "digest") fun <- action(match, name, fun)
      else matches <- append(matches, list(match))
      if (mode == "once") break
    } 
  }
  if (!is.null(matches))
    fun <- action(matches, name, fun)
  list(fun, remaining)
}

process_header_handlers <- function(handlers, headanns, name, fun) {
  for (handler in handlers) {
    fun_el <- process_header_handler(handler, headanns, name, fun)
    fun <- fun_el[[1]]
    headanns <- fun_el[[2]]
  }
  list(fun, headanns)
}

process_formal_handler <- function(handler, annotations, name, fun, formal) {
  matcher <- handler$matcher
  action <- handler$action
  mode <- handler$mode
  remove <- handler$remove
  matches <- NULL
  remaining <- list()
  for (annotation in annotations) {
    match <- matcher(annotation)
    if(identical(match, FALSE)) {
      remaining <- append(remaining, list(annotation))
    } else {
      if (!remove) remaining <- append(remaining, list(annotation))
      if (mode != "digest") fun <- action(match, name, fun, formal)
      else matches <- append(matches, list(match))
      if (mode == "once") break
    } 
  }
  if (!is.null(matches))
    fun <- action(matches, name, fun)
  list(fun, remaining)
}

process_formals_handlers <- function(handlers, foranns, name, fun) {
  for (handler in handlers) {
    for (formal in names(formals(fun))) {
      annotations <- foranns[[formal]]
      fun_el <- process_formal_handler(handler, annotations, name, fun, as.symbol(formal))
      fun <- fun_el[[1]]
      annotations <- fun_el[[2]]
      foranns[[formal]] <- annotations
    }
  }
  list(fun, foranns)
}

process_body_handlers <- process_header_handlers

process_fun <- function(name, fun, env, hooks, handlers) {
  begin_hook <- hooks$function_begin
  finish_hook <- hooks$function_finish
  if (is.function(begin_hook)) fun <- begin_hook(name, fun)
  headanns <- annotations(fun, "header")
  formanns <- annotations(fun, "formals")
  bodyanns <- annotations(fun, "body")
  fun_headanns <- process_header_handlers(handlers$function_header,
                                          headanns, name, fun)
  fun_formanns <- process_formals_handlers(handlers$function_formals,
                                           formanns, name, fun_headanns[[1]])
  fun_bodyanns <- process_body_handlers(handlers$function_body,
                                        bodyanns, name, fun_formanns[[1]])
  fun <- fun_bodyanns[[1]]
  annotations(fun, "header") <- fun_headanns[[2]]
  annotations(fun, "formals") <- fun_formanns[[2]]
  annotations(fun, "body") <- fun_bodyanns[[2]]
  if (is.function(finish_hook)) fun <- finish_hook(name, fun)
  fun
}

process_annotations <- function(env = parent.frame()) {
  for (namespace in get_namespaces()) {
    if(cache$enabled[[namespace]])
      process_namespace(namespace, env)
  }
}

process_namespace <- function(namespace, env) {
  hooks <- cache$hooks[[namespace]]
  handlers <- cache$handlers[[namespace]]
  begin_hook <- hooks$environment_begin
  finish_hook <- hooks$environment_finish
  if (is.function(begin_hook)) begin_hook(env)
  # process_environment_handlers(handlers$environment, annotations(env), env)
  for (name in ls(env)) {
    object <- env[[name]]
    if (is.function(object)) {
      assign(name, process_fun(name, object, env, hooks, handlers), env)
    }
  }
  if (is.function(finish_hook)) finish_hook(env)
}
