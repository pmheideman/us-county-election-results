# Shared helpers for the Data_creation R port.
# These exist because the same two Stata idioms recur ~10x across the do-files:
#   (1) zero-padding a numeric geography code before concatenating it into a join key
#   (2) `collapse (mean) ... (sum) ... [pweight=w], by(...)`

pad <- function(x, width) formatC(as.integer(x), width = width, flag = "0")

# Mirrors Stata's `collapse (mean) mean_vars (sum) <prefixes> [pweight=weight], by(group_vars)`.
# sum_prefixes are matched with startsWith() against the columns actually present in df,
# exactly like a Stata wildcard (e.g. "imm*") expands only over the variables that exist
# in the dataset being collapsed -- deliberately NOT a fixed variable list, because the
# source do-file collapses the same-looking prefix list against differently-shaped frames
# in different blocks.
weighted_collapse <- function(df, group_vars, weight, sum_prefixes = character(0), mean_vars = character(0)) {
  sum_vars <- df %>%
    select(-all_of(group_vars)) %>%
    select(where(is.numeric)) %>%
    names() %>%
    Filter(function(v) any(startsWith(v, sum_prefixes)) && !(v %in% mean_vars) && v != weight, .)

  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      across(all_of(mean_vars), ~ weighted.mean(.x, w = .data[[weight]], na.rm = TRUE)),
      across(all_of(sum_vars), ~ sum(.x * .data[[weight]], na.rm = TRUE)),
      .groups = "drop"
    )
}

# Mirrors the recurring Stata idiom in the CPS shift-share instrument construction:
#   gen t  = value if flag==1
#   gen nt = value if flag==0
#   bys id_vars: egen in_group     = total(weight * t)   (or max(t), when value is already
#   bys id_vars: egen not_in_group = total(weight * nt)   a pre-aggregated per-id total)
# i.e. split a weighted sum by a 0/1 flag into two id-level totals, side by side. Used for the
# rich/poor, Mexican/non-Mexican and Latino/non-Latino splits of the immigration instrument.
shift_share_split <- function(df, id_vars, flag, weight, value) {
  totals <- df %>%
    group_by(across(all_of(c(id_vars, flag)))) %>%
    summarise(total = sum(.data[[weight]] * .data[[value]], na.rm = TRUE), .groups = "drop")
  in_group <- totals %>% filter(.data[[flag]] == 1) %>% select(all_of(id_vars), in_group = total)
  out_group <- totals %>% filter(.data[[flag]] == 0) %>% select(all_of(id_vars), out_group = total)
  full_join(in_group, out_group, by = id_vars)
}
