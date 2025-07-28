# Calculate summary statistics for Bland-Altman means and differences

# Create appropriate df for mean/difference analysis
make_md_df <- function(smpl = NULL, exp1, exp2, pot = TRUE) {
  if (is.null(smpl)) {
    smpl <- seq_along(exp1)
  }

  df <- data.frame(
    Sample = as.character(smpl),
    Exp1 = exp1[smpl],
    Exp2 = exp2[smpl]
  )

  df$Difference <- df$Exp1 - df$Exp2
  df$Mean <- (df$Exp1 + df$Exp2) / 2

  if (pot) {
    df$Pot <- (df$Exp1 + df$Exp2) / 2
  } else {
    df$Pot <- NA
  }

  return(df)
}

repexp.stats <- function(df) {
  rSpearman <- cor(x = df[["Exp1"]], y = df[["Exp2"]], method = "spearman")

  summarise(df,
    n = n(),
    t = qt(0.975, n - 1),
    MeanDiff = mean(Difference),
    StdDev = sd(Difference),
    MSD = 2 * StdDev,
    UDL = MeanDiff + (t * StdDev / sqrt(n)),
    LDL = MeanDiff - (t * StdDev / sqrt(n)),
    ULSA = MeanDiff + (2 * StdDev),
    LLSA = MeanDiff - (2 * StdDev)
  ) %>%
    mutate(
      r = rSpearman,
      r2 = r^2
    ) %>%
    select(-t, -StdDev)
}
