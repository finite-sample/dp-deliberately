inventory_arguments <- function(arguments) {
  if (!is.null(arguments) && "node_type" %in% names(arguments)) {
    arguments <- arguments[
      !is.na(arguments$node_type) & arguments$node_type == "argument",
      ,
      drop = FALSE
    ]
  }
  arguments
}

session_recorded <- function(x, session) {
  coverage <- table_data(x, "coverage")
  if (
    is.null(coverage) || !is.finite(session$start) || !is.finite(session$end)
  ) {
    return(FALSE)
  }
  coverage <- coverage[
    coverage$event_id == session$event_id &
      coverage$session_id == session$session_id,
    ,
    drop = FALSE
  ]
  observed <- union_intervals(coverage$start, coverage$end)
  intersection_length(
    data.frame(start = session$start, end = session$end),
    observed
  ) >=
    session$end - session$start - 1e-10
}
