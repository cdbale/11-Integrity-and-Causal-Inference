# Run from the lecture repository: Rscript analysis/causal-inference.R
# Creates analytical results and figures only; does not render the slide deck.
user_library <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library",
                          paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = "."))
if (dir.exists(user_library)) .libPaths(c(user_library, .libPaths()))
library(ggplot2)

dir.create("analysis/results", recursive = TRUE, showWarnings = FALSE)
dir.create("Figures", showWarnings = FALSE)
customers <- read.csv("home_goods_customers.csv", stringsAsFactors = FALSE)

# Exhaustive universe chosen by substantive baseline groups, not fitted results.
# Categorical factors enter as a whole; no cherry-picking of their indicators.
baseline_groups <- list(
  Demographics = c("Age", "Education"),
  Income = "HouseholdIncome",
  HouseholdComposition = c("HouseholdSize", "ChildrenAtHome"),
  Housing = c("Homeowner", "HomeSizeSqFt"),
  Region = "Region",
  WebsiteActivity = c("SiteVisits30d", "ProductViews30d"),
  Recency = "DaysSinceLastVisit",
  PriorEmailEngagement = "EmailEngagement90d",
  HistoricalSpending = "PreviousSpend"
)
baseline_variables <- unlist(baseline_groups, use.names = FALSE)
required_variables <- c("CampaignSpend", "Treatment", baseline_variables)
analysis_data <- customers[complete.cases(customers[required_variables]), required_variables]
analysis_data$Region <- factor(analysis_data$Region,
                              levels = c("Midwest", "Northeast", "South", "West"))
analysis_data$Education <- factor(analysis_data$Education,
  levels = c("High school or less", "Some college/associate", "Bachelor's", "Graduate"))
stopifnot(all(analysis_data$Treatment %in% 0:1), nrow(analysis_data) > 0)

choices <- expand.grid(rep(list(c(FALSE, TRUE)), length(baseline_groups)))
names(choices) <- names(baseline_groups)
fits <- lapply(seq_len(nrow(choices)), function(i) {
  selected <- as.logical(choices[i, ])
  controls <- unlist(baseline_groups[selected], use.names = FALSE)
  formula <- reformulate(c("Treatment", controls), response = "CampaignSpend")
  fit <- lm(formula, data = analysis_data)
  stopifnot(nobs(fit) == nrow(analysis_data), !anyNA(coef(fit)))
  data.frame(specification = i, n = nobs(fit), baseline_groups = sum(selected),
             treatment_estimate = unname(coef(fit)["Treatment"]),
             formula = paste(deparse(formula, width.cutoff = 500), collapse = " "))
})
specifications <- cbind(do.call(rbind, fits), choices)
write.csv(specifications, "analysis/results/causal-specifications.csv", row.names = FALSE)

unadjusted <- specifications$treatment_estimate[specifications$baseline_groups == 0]
fully_adjusted <- specifications$treatment_estimate[specifications$baseline_groups == length(baseline_groups)]
difference_in_means <- with(analysis_data, mean(CampaignSpend[Treatment == 1]) -
                            mean(CampaignSpend[Treatment == 0]))
stopifnot(isTRUE(all.equal(unname(unadjusted), unname(difference_in_means), tolerance = 1e-10)))

# A full-rank matrix uses three indicators per four-level categorical variable.
# Region reference = Midwest; Education reference = High school or less.
matrix_formula <- reformulate(c("Treatment", baseline_variables))
design <- model.matrix(matrix_formula, analysis_data)[, -1, drop = FALSE]
correlations <- cor(design)
write.csv(correlations, "analysis/results/causal-correlation-matrix.csv", row.names = TRUE)
label_map <- c(Treatment = "Treatment", Age = "Age",
  EducationSome.college.associate = "Some college", EducationBachelor.s = "Bachelor's",
  EducationGraduate = "Graduate", HouseholdIncome = "Income", HouseholdSize = "Household size",
  ChildrenAtHome = "Children", Homeowner = "Homeowner", HomeSizeSqFt = "Home size",
  RegionNortheast = "Northeast", RegionSouth = "South", RegionWest = "West",
  SiteVisits30d = "Site visits", ProductViews30d = "Product views",
  DaysSinceLastVisit = "Days since visit", EmailEngagement90d = "Prior email clicks",
  PreviousSpend = "Previous spend")
labels <- unname(label_map[make.names(colnames(design))])
stopifnot(!anyNA(labels))

corr_long <- expand.grid(row = seq_len(ncol(design)), column = seq_len(ncol(design)))
corr_long$r <- as.vector(correlations)
corr_long$TreatmentPair <- corr_long$row == 1 | corr_long$column == 1
corr_long$label <- ifelse(corr_long$TreatmentPair, sprintf("%.2f", corr_long$r), "")
matrix_plot <- ggplot(corr_long, aes(column, row, fill = r)) +
  geom_tile(color = "white", linewidth = .25) +
  geom_tile(data = subset(corr_long, TreatmentPair), fill = NA, color = "#16324F", linewidth = .65) +
  geom_text(aes(label = label), size = 2.6) +
  scale_fill_gradient2(low = "#3377A5", mid = "white", high = "#C75B39",
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  scale_x_continuous(breaks = seq_along(labels), labels = labels, expand = c(0, 0)) +
  scale_y_reverse(breaks = seq_along(labels), labels = labels, expand = c(0, 0)) +
  coord_equal() +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right", plot.margin = margin(8, 8, 8, 8))
ggsave("Figures/causal-correlation-matrix.png", matrix_plot, width = 11.8, height = 7.2, dpi = 180)

treatment_correlations <- data.frame(variable = colnames(design)[-1],
                                    label = labels[-1], correlation = correlations[1, -1])
write.csv(treatment_correlations, "analysis/results/causal-treatment-correlations.csv", row.names = FALSE)
# A readable display of the Treatment column, split into two blocks for slides.
correlation_display <- treatment_correlations
correlation_display$block <- rep(c("a", "b"), c(9, 8))
correlation_display$position <- c(9:1, 9:2)
correlation_display$value <- sprintf("%+.3f", correlation_display$correlation)
treatment_matrix_plot <- ggplot(correlation_display) +
  geom_text(aes(x = 0, y = position, label = label), hjust = 0, size = 5.5) +
  geom_tile(aes(x = 2.2, y = position, fill = correlation), width = .7, height = .88) +
  geom_text(aes(x = 2.2, y = position, label = value), size = 5.5) +
  scale_fill_gradient2(low = "#3377A5", mid = "white", high = "#C75B39",
                       midpoint = 0, limits = c(-1, 1), guide = "none") +
  scale_x_continuous(limits = c(-.1, 2.6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(.5, 9.5), expand = c(0, 0)) +
  facet_wrap(~block, nrow = 1) + theme_void() +
  theme(strip.text = element_blank(), panel.spacing.x = grid::unit(1.3, "cm"),
        plot.margin = margin(8, 8, 8, 8))
ggsave("Figures/causal-treatment-correlations.png", treatment_matrix_plot,
       width = 10.8, height = 4.3, dpi = 180, bg = "white")
summary_results <- data.frame(
  n = nrow(analysis_data), omitted_missing = nrow(customers) - nrow(analysis_data),
  treatment_n = sum(analysis_data$Treatment == 1), control_n = sum(analysis_data$Treatment == 0),
  specifications = nrow(specifications),
  unadjusted = unadjusted, fully_adjusted = fully_adjusted,
  minimum = min(specifications$treatment_estimate),
  median = median(specifications$treatment_estimate),
  maximum = max(specifications$treatment_estimate),
  maximum_absolute_treatment_correlation = max(abs(treatment_correlations$correlation)))
write.csv(summary_results, "analysis/results/causal-summary.csv", row.names = FALSE)

coefficient_plot <- ggplot(specifications, aes(treatment_estimate)) +
  geom_histogram(binwidth = .05, boundary = 0, fill = "#36789B", color = "white", linewidth = .3) +
  geom_vline(xintercept = unadjusted, linetype = "dashed", linewidth = .85, color = "#A54222") +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5), expand = c(0, 0),
                     labels = function(x) paste0("$", format(x, trim = TRUE))) +
  labs(x = "Treatment coefficient: dollars per customer", y = "Number of specifications",
       subtitle = sprintf("512 models | Same %s customers | Dashed line: unadjusted difference = $%.2f",
                          format(nrow(analysis_data), big.mark = ","), unadjusted)) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(), plot.margin = margin(12, 16, 12, 12),
        plot.subtitle = element_text(size = 13, margin = margin(b = 16)))
ggsave("Figures/causal-treatment-specifications.png", coefficient_plot, width = 10.8, height = 4.8, dpi = 180)
print(summary_results)
print(treatment_correlations)
