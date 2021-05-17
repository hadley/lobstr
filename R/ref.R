#' Display tree of references
#'
#' This tree display focusses on the distinction between names and values.
#' For each reference-type object (lists, environments, and optional character
#' vectors), it displays the location of each component. The display
#' shows the connection between shared references using a locally unique id.
#'
#' @param ... One or more objects
#' @param character If `TRUE`, show references from character vector in to
#'   global string pool
#' @export
#' @family object inspectors
#' @examples
#' x <- 1:100
#' ref(x)
#'
#' y <- list(x, x, x)
#' ref(y)
#' ref(x, y)
#'
#' e <- new.env()
#' e$e <- e
#' e$x <- x
#' e$y <- list(x, e)
#' ref(e)
#'
#' # Can also show references to global string pool if requested
#' ref(c("x", "x", "y"))
#' ref(c("x", "x", "y"), character = TRUE)
ref <- function(..., character = FALSE) {
  x <- list(...)
  seen <- child_env(emptyenv(), `__next_id` = 1)

  out <- lapply(x, ref_tree, character = character, seen = seen)

  n <- length(x)
  if (n > 1) {
    out[-n] <- lapply(out[-n], function(x) c(x, ""))
  }
  new_raw(unlist(out))
}

ref_tree <- function(x, character = FALSE, seen = child_env(emptyenv()), layout = box_chars()) {

  addr <- obj_addr(x)
  has_seen <- env_has(seen, addr)
  id <- obj_id(seen, addr)

  desc <- obj_desc(addr, type_sum(x), has_seen, id)

  # Not recursive or already seen
  if (!has_references(x, character) || has_seen) {
    return(desc)
  }

  # Remove classes to avoid custom methods (note that environments cannot be unclasse()ed)
  attr(x, "class") <- NULL

  # recursive cases
  if (is.list(x)) {
    subtrees <- lapply(x, ref_tree, layout = layout, seen = seen, character = character)
  } else if (is.environment(x)) {
    subtrees <- lapply(as.list(x, all.names = TRUE), ref_tree, layout = layout, seen = seen, character = character)
  } else if (is.character(x)) {
    subtrees <- ref_tree_chr(x, layout = layout, seen = seen)
  }
  subtrees <- name_subtree(subtrees)

  self <- str_indent(desc, paste0(layout$n, " "), paste0(layout$v, " "))

  n <- length(subtrees)
  if (n == 0) {
    return(self)
  }
  c(
    self,
    unlist(lapply(subtrees[-n],
      str_indent,
      paste0(layout$j, layout$h),
      paste0(layout$v,  " ")
    )),
    str_indent(subtrees[[n]],
      paste0(layout$l, layout$h),
      "  "
    )
  )
}

type_sum <- function(x) {
  if (is_installed("pillar")) {
    pillar::type_sum(x)
  } else {
    typeof(x)
  }
}

obj_desc <- function(addr, type, has_seen, id) {
  if (has_seen) {
    paste0("[", grey(paste0(id, ":", addr)), "]")
  } else {
    paste0("[", crayon::bold(id), ":", addr, "] ", "<", type, ">")
  }
}

has_references <- function(x, character = FALSE) {
  is_list(x) || is.environment(x) || (character && is_character(x))
}

ref_tree_chr <- function(x, layout = box_chars(), seen = child_env(emptyenv())) {
  addrs <- obj_addrs(x)

  has_seen <- logical(length(x))
  ids <- integer(length(x))
  for (i in seq_along(addrs)) {
    has_seen[[i]] <- env_has(seen, addrs[[i]])
    ids[[i]] <- obj_id(seen, addrs[[i]])
  }

  type <- paste0('string: "', str_truncate(x, 10), '"')

  out <- Map(obj_desc, addrs, type, has_seen, ids)
  names(out) <- names(x)
  out
}

obj_id <- function(env, ref) {
  if (env_has(env, ref)) {
    env_get(env, ref)
  } else {
    id <- env_get(env, "__next_id")
    env_poke(env, "__next_id", id + 1)
    env_poke(env, ref, id)

    id
  }
}
