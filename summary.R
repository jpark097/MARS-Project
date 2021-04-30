library(tidyr)
summary.mars <- function(object) {
  splits <- object$splits
  names <- object$x_names
  B <- object$B
  coefficients <- object$coefficients
  positions <- splits %>%
    purrr::map(`[`, 2) %>%
    purrr::map(~ dplyr::as_tibble(.) %>%
                 .[nrow(.), ] %>%
                 dplyr::pull()) %>%
    unlist()
  at_split <- splits %>%
    purrr::map(`[`, 4) %>%
    purrr::map(~ dplyr::as_tibble(.) %>%
                 .[nrow(.), ] %>%
                 dplyr::pull()) %>%
    unlist() %>%
    {ifelse(is.na(.), "", paste("split at value:", .))}
  
  names <- names %>%
    purrr::set_names(0, 2:length(.))
  names_rev <- as.integer(names(names)) %>%
    purrr::set_names(names)
  name_basis_fn <- c()
  for (i in seq_along(positions)) {
    name_basis_fn[[i]] <- names(which(positions[i] == names_rev))
  }
  name_basis_fn <- unlist(name_basis_fn)
  b_names <- names(dplyr::as_tibble(B)) %>%
    paste0(":")
  comp <- c("Intercept", rep("Component 1: ", length(b_names) - 1))
  sign <- ifelse(coefficients < 0, -1, 1) %>%
    {c("", paste0("sign ", .[2:length(.)], ";"))}
  coefficients <- c(rep(list(""), length(coefficients) - 1), list(coefficients))
  
  purrr::pmap(
    list(at_split, name_basis_fn, comp, sign, b_names, coefficients),
    function(a, b, c, d, e, f) {
      cat(e, "\n", c, b, d, a, "\n", "\n", f)
    }
  ) %>% .[[1]]
  
}


