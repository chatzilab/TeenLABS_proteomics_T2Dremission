fit_lmm <- function(x, y, covars, df)
{
  model <- as.formula(
    paste0(
      y, "~", x, " + ", paste(covars, collapse = " + "), " + (1|key)"
    )
  )
  fit <- lmer(
    model,
    data = df
  )
  
  output <- summary(fit)$coefficients[2,] |> 
    t() |> as.data.frame()
  colnames(output) <- c("estimate", "se", "df", "statistic", "pvalue")
  
  output$outcome <- y
  output$assay <- x
  
  conf_interval <- confint(fit)[4,] |>
    t() |> as.data.frame()
  colnames(conf_interval) <- c("conf_low", "conf_high")
  
  output <- bind_cols(output, conf_interval) %>% 
    dplyr::select(outcome, assay, everything())
  
  return(output)
}

fit_glmm <- function(x, y, covars, df)
{
  model <- as.formula(
    paste0(
      y, "~", x, " + ", paste(covars, collapse = " + "), " + (1|key)"
    )
  )
  fit <- glmer(
    model,
    data = df,
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
  )
  
  output <- summary(fit)$coefficients[2,] |> 
    t() |> as.data.frame()
  colnames(output) <- c("estimate", "se", "statistic", "pvalue")
  
  output$outcome <- y
  output$assay <- x
  
  conf_interval <- confint(fit, method = "Wald")[3,] |>
    t() |> as.data.frame()
  colnames(conf_interval) <- c("conf_low", "conf_high")
  
  output <- bind_cols(output, conf_interval) %>% 
    dplyr::select(outcome, assay, everything())
  
  return(output)
}

# Fit cox model
fit_cox <- function(x, y, covars, df)
{
  model <- as.formula(
    paste0(
      "Surv(time_start, time_end, ", y, ") ~", x, " + ", paste(covars, collapse = " + "), " + cluster(key)"
    )
  )
  fit <- coxph(
    model,
    data = df
  )
  
  model_sum <- summary(fit)
  
  p.value <- model_sum$coefficients[1,6] |> as.numeric()
  statistic <- model_sum$coefficients[1,5] |> as.numeric()
  beta <- model_sum$coefficients[1,1] |> as.numeric() # coeficient beta
  HR <- round(exp(beta), 2)
  HR.confint.lower <- round(model_sum$conf.int[1,3], 2)
  HR.confint.upper <- round(model_sum$conf.int[1,4], 2)
  
  output <- c(beta, HR, HR.confint.lower, HR.confint.upper, wald.test, p.value)
  names(output)<-c("estimate", "HR", "conf_low", "conf_high", "statistic", "pvalue")
  output <- t(output) |> as.data.frame()
  output$outcome <- y
  output$assay <- x
  output <- output %>%
    dplyr::select(outcome, assay, everything())
  
  return(output)
}

# Create volcano plots
make_volcano <- function(y, model_summary)
{
  mycolors <- c("red", "grey")
  names(mycolors) <- c("q value < 0.05", "Null")
  
  x_label <- if(y == "fpg"){
    "Effect estimates for fasting glucose (mg/dL)"
  }else if(y == "hba1c"){
    "Effect estimates for HbA1c (%)"
  }else if(y == "homa"){
    "Effect estimates for HOMA-IR"
  }else if(y == "ins"){
    "Effect estimates for fasting insulin (mg/dL)"
  }else{
    "Log(hazard ratios) for diabetes remission"
  }
  
  plot_input <- model_summary %>% 
    dplyr::filter(outcome == y)
  
  max_abs_val <- max(abs(plot_input$estimate))
  x = plot_input$estimate
  
  volcano_plot <- ggplot(
    data = plot_input,
    aes(x = x, y = -log10(pvalue),
        col = significance,
        label = label
    )) + 
    geom_point() +
    geom_hline(yintercept = -log10(0.05/dim(plot_input)[1]),
               color = "red", lty = "dashed") +
    geom_hline(yintercept = -log10(0.05),
               color = "blue", lty = "dashed") +
    xlim(-max_abs_val, max_abs_val) +
    ylab(expression(-log[10]~"(p)")) +
    xlab(x_label) +
    theme_classic() +
    geom_text_repel(max.overlaps = 100, size = 2.5)  +
    scale_colour_manual(values = mycolors) +
    theme(plot.caption = element_text(hjust = 0),
          legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size = 11, family = "sans"),
          axis.text = element_text(size = 11),
          axis.title = element_text(
            size = 11,
            margin = ggplot2::margin(0,-10,0,0)))
  
  return(volcano_plot)
}

# make labels for interaction() used in ggplot2
make_labels <- function(labels) {
  result <- str_split(labels, "\\.")
  unlist(lapply(result, function(x) x[1]))
}

# decimal ceiling
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

# Custom function to exclude missing values in table1
render_ignore_na <- function(x) {
  y <- na.omit(x)  # Remove NAs
  c(
    "N" = length(y),  # Count of non-missing values
    "Proportion" = mean(y, na.rm = TRUE)  # Compute proportion ignoring NAs
  )
}

# Create p value column in table1
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- fisher.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# fuzzy match protein names
fuzzy_match <- function(x, candidates, threshold) {
  dists <- stringdist(x, candidates, method = "jw")
  min_dist <- min(dists)
  if (min_dist <= threshold) {
    return(candidates[which.min(dists)])
  } else {
    return(NA)
  }
}
