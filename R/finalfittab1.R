#' Make final fit table 1 with autonaming
#'
#' @param dataset The dataset
#' @param dependent set to call byvar (single dependent variable)
#' @param explanatory set to call vars list
#' @param p
#' @param p_cat
#' @param total_col
#' @param na_to_prop
#' @param cont
#' @param add_row_totals
#' @param ...
#'
#' @returns table 1 in flextable form
#' @export
#'
#' @examples
#'
#'vars = c( "sex", "age_diag", "age_65",  "smoke_status", "smoke_pack_years", "t_stage_group", "ecog_group", "ecog_diag_group",  "pet_scan_stag", "brain_imag_stag")
#'byvar = "overall_therapy1l_grouped"
#'finalfittab1(dat %>% filter(is.na(pop_stagei)), p = F)



finalfittab1 <- function(dataset, dependent = byvar, explanatory = vars,
                         p = TRUE, p_cat = "chisq", total_col = TRUE,
                         na_to_prop = FALSE, cont = "median", add_row_totals = FALSE, ...){

  # Guard: dependent shouldn't also appear in explanatory
  if (!is.null(dependent) && dependent %in% explanatory) {
    stop(paste0("'", dependent, "' is in both `dependent` and `explanatory` — remove it from one."))
  }

  summary_factorlist(dataset %>% ungroup() |>  mutate_if(is.character, funs(factor(.))),
                     dependent,
                     explanatory,
                     na_include = TRUE,
                     na_to_prop = na_to_prop,
                     p = p,
                     na_to_p = FALSE,
                     p_cat = p_cat,
                     cont = cont,
                     total_col = total_col,
                     add_col_totals = TRUE,
                     add_row_totals = add_row_totals,
                     col_totals_prefix = "N = ",
                     digits = c(1,1,2,0,0), ...) -> t1

  if (add_row_totals == TRUE){
    t1 <- t1 %>% select(-"Total N")
  }

  ## Row Name Specifications
  for(i in 1:(count(t1[1])[1,1])){
    if (t1[i,1] != "" & t1[i,1] %in% recode$var){
      matched_labels <- recode$label[recode$var == t1[i,1]]
      if (length(matched_labels) > 1) {
        warning(paste0("Multiple recode matches for '", t1[i,1], "' — using first match. Check recode table for duplicates."))
      }
      t1[i,1] <- matched_labels[1]
    }
  }

  ## Table style Specifications
  t1 %>% flextable::flextable() # %>%
  # bold(bold = TRUE, part = "header") %>%
  # bold(bold = TRUE, i = 1) %>%
  # align(align = "center", part = "all") %>%
  # align(align = "left", j = 1) %>%
  # align(align = "right", j = 2) # %>%
  #  labelizor(labels = mapping, part = "all")

}
