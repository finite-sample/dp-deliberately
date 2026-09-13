#' Define an event-specific group contrast
#' @param id Unique contrast name.
#' @param focal,reference Functions accepting the people table and returning a
#'   logical vector. Missing membership remains unknown, not FALSE.
#' @param rationale Substantive explanation of the comparison.
#' @param membership Optional data frame with event_id, person_id, focal,
#'   reference. Supply either this table or both functions.
#' @param event_id Optional event IDs to which the definition applies.
#' @param reference_advantaged Whether the reference is explicitly designated
#'   advantaged for the D measure. Default FALSE.
#' @return A contrast definition; [audit_dp()] stores evaluated memberships.
#' @examples
#' define_contrast("education", function(p) p$education == "lower",
#'   function(p) p$education == "higher", rationale = "Study-specific education contrast")
#' @export
define_contrast <- function(
  id,
  focal = NULL,
  reference = NULL,
  rationale,
  membership = NULL,
  event_id = NULL,
  reference_advantaged = FALSE
) {
  if (
    !is.logical(reference_advantaged) ||
      length(reference_advantaged) != 1L ||
      is.na(reference_advantaged)
  ) {
    stop("reference_advantaged must be TRUE or FALSE.")
  }
  if (
    length(id) != 1L ||
      is.na(id) ||
      !nzchar(id) ||
      length(rationale) != 1L ||
      is.na(rationale) ||
      !nzchar(rationale)
  ) {
    stop("A contrast needs a nonempty ID and rationale.")
  }
  if (is.null(membership)) {
    if (!is.function(focal) || !is.function(reference)) {
      stop("Supply two predicates or a membership table.")
    }
  } else if (!is.null(focal) || !is.null(reference)) {
    stop("Supply predicates or a membership table, not both.")
  }
  structure(
    list(
      id = id,
      focal = focal,
      reference = reference,
      rationale = rationale,
      membership = membership,
      event_id = event_id,
      reference_advantaged = reference_advantaged
    ),
    class = "deliberation_contrast"
  )
}

resolve_contrasts <- function(x, contrasts) {
  if (inherits(contrasts, "deliberation_contrast")) {
    contrasts <- list(contrasts)
  }
  if (!length(contrasts)) {
    return(list())
  }
  ids <- vapply(contrasts, function(c) c$id, "")
  if (anyDuplicated(ids)) {
    stop("Contrast IDs must be unique.")
  }
  people <- table_data(x, "people")
  if (is.null(people)) {
    stop("Contrasts require people.")
  }
  lapply(contrasts, function(c) {
    p <- people[people$role == "participant", , drop = FALSE]
    if (!is.null(c$event_id)) {
      p <- p[p$event_id %in% c$event_id, , drop = FALSE]
    }
    m <- c$membership
    if (is.null(m)) {
      m <- p[c("event_id", "person_id")]
      f <- c$focal(p)
      r <- c$reference(p)
      if (length(f) != nrow(p) || length(r) != nrow(p)) {
        stop("Predicates must return one logical value per participant.")
      }
      m$focal <- f
      m$reference <- r
    }
    if (
      !is.data.frame(m) ||
        !all(c("event_id", "person_id", "focal", "reference") %in% names(m))
    ) {
      stop("Membership needs event_id, person_id, focal, reference.")
    }
    key <- c("event_id", "person_id")
    if (
      anyNA(m[key]) ||
        anyDuplicated(row_key(m, key)) ||
        !all(row_key(m, key) %in% row_key(p, key))
    ) {
      stop("Invalid membership identifiers.")
    }
    if (
      !is.logical(m$focal) ||
        !is.logical(m$reference) ||
        any(m$focal & m$reference, na.rm = TRUE)
    ) {
      stop("Membership must be logical and disjoint.")
    }
    c$membership <- m
    c$focal <- c$reference <- NULL
    c
  })
}

contrast_side <- function(d, contrast) {
  m <- contrast$membership
  idx <- match(
    row_key(d, c("event_id", "person_id")),
    row_key(m, c("event_id", "person_id"))
  )
  ifelse(
    is.na(m$focal[idx]) | is.na(m$reference[idx]),
    NA_character_,
    ifelse(
      m$focal[idx],
      "focal",
      ifelse(m$reference[idx], "reference", "outside")
    )
  )
}
