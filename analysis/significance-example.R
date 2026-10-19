# Analysis behind the opening classroom discussion. Run from the repo root:
# Rscript analysis/significance-example.R
# Sourcing this file defines the function without writing files.

run_significance_example <- function(data_path = "home_goods_customers.csv") {
  customers <- read.csv(data_path, stringsAsFactors = FALSE)
  baseline <- c("Age", "HouseholdIncome", "HouseholdSize", "ChildrenAtHome",
                "Homeowner", "Region", "Education", "HomeSizeSqFt",
                "SiteVisits30d", "ProductViews30d", "DaysSinceLastVisit",
                "EmailEngagement90d", "PreviousSpend")
  required <- c("CampaignSpend", "Treatment", baseline)
  d <- customers[complete.cases(customers[, required]), ]
  d$IncomeThousands <- d$HouseholdIncome / 1000
  d$HomeSizeHundreds <- d$HomeSizeSqFt / 100
  stopifnot(nrow(d) == 9603, all(d$Treatment %in% 0:1),
            all(d$Homeowner %in% 0:1))

  model_a <- lm(CampaignSpend ~ Treatment + Homeowner + IncomeThousands +
                  HouseholdSize + HomeSizeHundreds + Age, data = d)
  model_b <- update(model_a, . ~ . + PreviousSpend)

  # Conventional lm inference is displayed; HC3 is a sensitivity check because
  # CampaignSpend has many zeroes and unequal residual variance is plausible.
  focal_result <- function(m, label, term = "Homeowner") {
    ordinary <- coef(summary(m))[term, ]
    ci <- confint(m, term)
    x <- model.matrix(m)
    bread <- solve(crossprod(x))
    weighted_x <- x * as.numeric(residuals(m) / (1 - hatvalues(m)))
    hc3 <- bread %*% crossprod(weighted_x) %*% bread
    robust_se <- sqrt(diag(hc3))[term]
    critical <- qt(.975, df.residual(m))
    data.frame(model = label, term = term, n = nobs(m),
               estimate = unname(ordinary[1]),
               conf_low = ci[1], conf_high = ci[2],
               p_value = unname(ordinary[4]),
               hc3_se = unname(robust_se),
               hc3_conf_low = unname(ordinary[1] - critical * robust_se),
               hc3_conf_high = unname(ordinary[1] + critical * robust_se),
               hc3_p_value = unname(2 * pt(-abs(ordinary[1] / robust_se),
                                         df.residual(m))),
               row.names = NULL)
  }
  selected <- rbind(focal_result(model_a, "A: household profile"),
                    focal_result(model_b, "B: profile + previous spend"))
  stopifnot(identical(rownames(model.frame(model_a)),
                      rownames(model.frame(model_b))),
            selected$p_value[1] < .05, selected$p_value[2] > .05,
            selected$hc3_p_value[1] < .05, selected$hc3_p_value[2] > .05)

  # Search audit: these candidates were inspected to find a clear classroom
  # contrast, not as a confirmatory test of a preregistered homeowner hypothesis.
  # All use the same rows. No transformations of the outcome or selective
  # exclusions were considered. CustomerID and post-assignment Conversion
  # are excluded from every model.
  predictors <- c("Age", "IncomeThousands", "HouseholdSize", "ChildrenAtHome",
                  "Homeowner", "Region", "Education", "HomeSizeHundreds",
                  "SiteVisits30d", "ProductViews30d", "DaysSinceLastVisit",
                  "EmailEngagement90d", "PreviousSpend")
  full <- lm(reformulate(c("Treatment", predictors), "CampaignSpend"), data = d)
  numeric_predictors <- setdiff(predictors, c("Region", "Education"))
  audit <- do.call(rbind, lapply(numeric_predictors, function(v) {
    simple <- lm(reformulate(c("Treatment", v), "CampaignSpend"), data = d)
    data.frame(candidate = v, simple_estimate = coef(simple)[v],
               simple_p = coef(summary(simple))[v, 4],
               full_estimate = coef(full)[v],
               full_p = coef(summary(full))[v, 4],
               correlation_with_previous_spend = cor(d[[v]], d$PreviousSpend),
               n = nrow(d), row.names = NULL)
  }))
  variants <- list(
    "Treatment + Homeowner" = lm(CampaignSpend ~ Treatment + Homeowner, d),
    "Add PreviousSpend only" = lm(CampaignSpend ~ Treatment + Homeowner +
                                   PreviousSpend, d),
    "Household profile (A)" = model_a,
    "Profile + PreviousSpend (B)" = model_b,
    "Profile + ChildrenAtHome" = update(model_a, . ~ . + ChildrenAtHome),
    "All baseline variables" = full
  )
  variant_results <- do.call(rbind, lapply(names(variants), function(label) {
    focal_result(variants[[label]], label)
  }))
  list(data = d, model_a = model_a, model_b = model_b, selected = selected,
       search = audit, homeowner_variants = variant_results,
       previous_spend_by_homeowner = aggregate(PreviousSpend ~ Homeowner, d, mean))
}

if (sys.nframe() == 0L) {
  result <- run_significance_example()
  dir.create("analysis", showWarnings = FALSE)
  write.csv(result$selected, "analysis/significance-selected-results.csv", row.names = FALSE)
  write.csv(result$search, "analysis/significance-model-search.csv", row.names = FALSE)
  write.csv(result$homeowner_variants, "analysis/significance-homeowner-variants.csv", row.names = FALSE)
  write.csv(result$previous_spend_by_homeowner,
            "analysis/significance-previous-spend.csv", row.names = FALSE)
  print(result$selected, digits = 8)
  print(result$previous_spend_by_homeowner)
}
