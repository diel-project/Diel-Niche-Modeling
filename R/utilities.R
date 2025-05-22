#' @keywords internal

get.diel.vars <- function(x){
    all_expr <- paste(
      x,
      collapse = " "
    )
    all_vars <- unlist(
      regmatches(
        all_expr,
        gregexpr("\\b[a-zA-Z]\\w*\\b", all_expr)
      )
    )
  datetime_cols <- setdiff(
    all_vars,
    c("x", "TRUE", "FALSE")
  )
  return(datetime_cols)
}

