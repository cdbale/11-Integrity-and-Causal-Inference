# Run from the lecture repository. Produces figures only; does not knit slides.
# Values transcribed from Trust_Badge_Verification (4).pdf, dated 2026-10-19.
# These are published-in-the-draft summaries, not re-estimates from raw data.
.libPaths(c("C:/Users/Cam/AppData/Local/R/win-library/4.6", .libPaths()))
library(ggplot2)
navy <- "#003B71"
teal <- "#008B8B"
theme_set(theme_minimal(base_size = 19) + theme(
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold", size = 22),
  plot.subtitle = element_text(size = 16),
  plot.caption = element_text(size = 12, hjust = 0),
  plot.margin = margin(15, 25, 15, 15),
  plot.title.position = "plot", plot.caption.position = "plot",
  legend.position = "top", legend.title = element_blank()))
save_plot <- function(p, name, height = 5.4) {
  ggsave(file.path("Figures", paste0("research-", name, ".png")), p,
         width = 11, height = height, dpi = 180, bg = "white")
}

# Table 1, manuscript p. 8 (PDF p. 9). Reported 95% intervals.
s1 <- data.frame(outcome = c("Purchase intentions", "Review helpfulness",
  "Trust in review", "Trust in reviewer", "Trust in platform"),
  estimate = c(.43, .40, .70, .80, .41),
  lo = c(.08, .12, .44, .53, .15), hi = c(.77, .67, .96, 1.07, .68))
s1$outcome <- factor(s1$outcome, levels = rev(s1$outcome))
p <- ggplot(s1, aes(estimate, outcome)) +
  geom_vline(xintercept = 0, color = "grey55", linetype = 2) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = .18,
                linewidth = 1.4, color = navy) +
  geom_point(size = 4, color = navy) +
  geom_text(aes(label = sprintf("+%.2f", estimate)), nudge_y = .34, size = 5.4) +
  scale_x_continuous(limits = c(-.04, 1.12), breaks = seq(0, 1, .2)) +
  labs(x = "Badge effect (points on a 1-7 scale)", y = NULL,
    title = "The badge increased trust and intentions",
    subtitle = "Randomized badge vs. no badge | 249 participants",
    caption = "Dots: treatment minus control. Lines: reported 95% confidence intervals.\nSource: Beck, Bale, Bale & Smith (2026 draft), Table 1.")
save_plot(p, "study1-effects")

# Table 3, manuscript p. 11 (PDF p. 12): purchase-intentions model.
s2 <- data.frame(attribute = c("Identity verification badge", "1-star review vs. 5-star",
  "3-star review vs. 5-star", "Helpfulness button", "Recent review", "Identity disclosure"),
  estimate = c(.196, -3.170, -.891, -.078, .169, .209),
  se = c(.071, .080, .073, .069, .064, .066))
s2$attribute <- factor(s2$attribute, levels = rev(s2$attribute))
p <- ggplot(s2, aes(estimate, attribute)) +
  geom_vline(xintercept = 0, color = "grey55", linetype = 2) +
  geom_errorbar(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se),
                orientation = "y", width = .18, linewidth = 1.3, color = navy) +
  geom_point(size = 4, color = navy) +
  geom_text(aes(label = sprintf("%+.2f", estimate)), nudge_y = .34, size = 5) +
  scale_x_continuous(limits = c(-3.6, .65)) +
  labs(x = "Effect on purchase intentions (1-7 scale)", y = NULL,
    title = "Effects on Purchase Intentions",
    subtitle = "Randomized conjoint | 480 students | 1,380 responses",
    caption = "Lines: approximate 95% confidence intervals. Source: Table 3.")
save_plot(p, "study2-main-effects")

# Section 3.2.1, manuscript pp. 11-12 (PDF pp. 12-13).
# These are within-badge contrasts against five stars, NOT predicted levels.
interaction <- data.frame(stars = rep(c(1, 3, 5), 2),
  badge = rep(c("No verification badge", "Verification badge"), each = 3),
  contrast = c(-2.764, -.491, 0, -3.585, -1.138, 0))
p <- ggplot(interaction, aes(stars, contrast, color = badge, group = badge)) +
  geom_hline(yintercept = 0, color = "grey65", linetype = 2) +
  geom_line(linewidth = 1.4) + geom_point(size = 4) +
  geom_text(data = subset(interaction, stars < 5 & badge == "No verification badge"),
    aes(label = sprintf("%.1f", contrast)), nudge_y = .25, size = 5.3, show.legend = FALSE) +
  geom_text(data = subset(interaction, stars < 5 & badge == "Verification badge"),
    aes(label = sprintf("%.1f", contrast)), nudge_y = -.27, size = 5.3, show.legend = FALSE) +
  scale_color_manual(values = c("No verification badge" = "#666666", "Verification badge" = teal)) +
  scale_x_continuous(breaks = c(1, 3, 5), labels = c("1 star", "3 stars", "5 stars")) +
  scale_y_continuous(limits = c(-4, .4), breaks = -4:0) +
  labs(x = NULL, y = "Purchase-intention difference\nrelative to 5 stars", title = "Verification strengthens the rating's influence",
    caption = "Five stars = 0 within each group. Source: Section 3.2.1.")
save_plot(p, "study2-interaction")

# Predicted levels: Trust_Badge_Verification (5).pdf, Appendix Table 3, p. 5.
# Use the purchase-intention interaction model (not the task/carryover model).
# Average equally over the 8 combinations of the three other randomized binary
# features. Predictions are population means (respondent random effect = 0).
# Rounded published coefficients support approximate levels. Prediction CIs need
# the joint coefficient covariance matrix, which the appendix does not provide.
prediction_profiles <- expand.grid(stars = c(1, 3, 5), verified = 0:1,
  helpful = 0:1, recent = 0:1, disclosed = 0:1)
prediction_profiles$predicted <- with(prediction_profiles,
  5.461 + .383 * verified - 2.764 * (stars == 1) - .491 * (stars == 3) -
  .154 * helpful - .044 * recent + .090 * disclosed -
  .821 * verified * (stars == 1) - .647 * verified * (stars == 3) +
  .095 * verified * helpful + .288 * verified * recent + .197 * verified * disclosed)
predicted_levels <- aggregate(predicted ~ stars + verified,
                              data = prediction_profiles, FUN = mean)
predicted_levels <- predicted_levels[order(predicted_levels$verified, predicted_levels$stars), ]
stopifnot(isTRUE(all.equal(predicted_levels$predicted,
                          c(2.643, 4.916, 5.407, 2.495, 4.942, 6.080), tolerance = 1e-10)))
predicted_levels$badge <- ifelse(predicted_levels$verified == 1,
                                "Verified reviewer", "Unverified reviewer")
predicted_levels$label_y <- predicted_levels$predicted + c(.28, -.28, -.28, -.28, .28, .28)
dir.create("analysis/results", showWarnings = FALSE, recursive = TRUE)
write.csv(predicted_levels[c("stars", "verified", "predicted")],
          "analysis/results/research-study2-predicted-levels.csv", row.names = FALSE)
p <- ggplot(predicted_levels, aes(stars, predicted, color = badge, group = badge)) +
  geom_line(linewidth = 1.4) + geom_point(aes(shape = badge), size = 4) +
  geom_text(aes(y = label_y, label = sprintf("%.2f", predicted)), size = 5.3,
            show.legend = FALSE) +
  scale_color_manual(values = c("Unverified reviewer" = "#666666", "Verified reviewer" = teal)) +
  scale_shape_manual(values = c("Unverified reviewer" = 16, "Verified reviewer" = 17)) +
  scale_x_continuous(breaks = c(1, 3, 5), labels = c("1 star", "3 stars", "5 stars")) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(x = NULL, y = "Predicted purchase intentions\n(1-7 scale)",
    title = "Predicted Purchase Intentions",
    caption = "Other review features averaged evenly across their randomized levels.\nSource: Appendix Table 3; predictions from reported coefficients.")
save_plot(p, "study2-predicted-levels")

# Table 4, manuscript p. 15 (PDF p. 16). Rates computed from counts.
s3 <- data.frame(outcome = rep(c("All clicks", "Outbound clicks"), each = 2),
  creative = rep(c("Customer Review", "Identity Verified"), 2),
  clicks = c(305, 372, 254, 305), impressions = c(21077, 21306, 21077, 21306))
s3$rate <- 100 * s3$clicks / s3$impressions
s3$creative <- factor(s3$creative, levels = c("Customer Review", "Identity Verified"))
p <- ggplot(s3, aes(outcome, rate, fill = creative)) +
  geom_col(position = position_dodge(.72), width = .62) +
  geom_text(aes(label = sprintf("%.2f%%", rate)), position = position_dodge(.72),
    vjust = -.5, size = 6) +
  scale_fill_manual(values = c("#999999", navy)) +
  scale_y_continuous(limits = c(0, 2.05), breaks = seq(0, 2, .5)) +
  labs(x = NULL, y = "Clicks / impressions (%)", title = "The verified creative had higher click rates",
    subtitle = "All clicks: +0.30 percentage points | Outbound: +0.23 percentage points",
    caption = "All clicks: clicks anywhere on the ad. Outbound: clicks to the website. Source: Table 4.")
save_plot(p, "study3-results")
