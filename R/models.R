pneumo_model <- function(x,
                         ari,
                         iso3,
                         year,
                         year_range) {
  x <- x / 100
  ari <- log(remove_below_0(ari))
  year_subset <- year %in% year_range
  large_states <- !whoville::is_small_member_state(iso3)
  ari <- ifelse(year_subset,
                min_impute(ari, ari[large_states]),
                ari)
  ari_sq <- ari ^ 2
  x <- log(x / (1.01 - x))
  df <- data.frame(x = x,
                   ari = ari,
                   ari_sq = ari_sq)
  mdl <- stats::lm(x ~ ari + ari_sq, df)
  stats::predict(mdl, df, subset = (year_subset & large_states))
}
