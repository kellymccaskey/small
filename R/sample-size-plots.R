library(ggplot2)
library(scales)
library(quantreg)
library(gridExtra)
library(xtable)
library(doParallel)


df <- readRDS("R/simulations/sample-size-simulations.RData")
df <- df[df$sep == 0, ]

base_breaks <- function(n = 3, include = NULL) {
  function(x) {
    sort(c(axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, nint = n), include))
  }
}

rq90_coef <- rq(log10(mse_infl) ~ log10(info), tau = 0.9, data = subset(df, par_type == "Slope Coefficient"))
new_df <- data.frame(info = c(1:20*10, 3:10*100))
new_df$mse_infl90_coef <- round(10^predict(rq90_coef, newdata = new_df), 1)

rq90_int <- rq(log10(mse_infl) ~ log10(info), tau = 0.9, data = subset(df, par_type == "Intercept"))
new_df$mse_infl90_int <- round(10^predict(rq90_int, newdata = new_df), 1)
new_df


gg <- ggplot(subset(df, par_type == "Slope Coefficient"), aes(x = info, y = mse_infl/100)) + 
  geom_point(aes(size = n, color = true_value), shape = 1, alpha = 1.0, solid = FALSE) +
  geom_quantile(quantiles = c(0.9), color = "black") +
  scale_color_gradient2(high = "#7570b3",
                        mid = "grey70",
                        low = "#d95f02",
                        midpoint = 0) + 
  
  scale_x_log10(minor_breaks = NULL, breaks = base_breaks(), labels = prettyNum) +
  scale_y_log10(minor_breaks = NULL, breaks = base_breaks(include = c(.02, .03)), labels = percent) +
  facet_wrap(~ par_type) + 
  #geom_smooth(se = FALSE, color = "grey50") + 
  #geom_hline(yintercept = 3, linetype = "dotted") + 
  #annotate(geom = "text", x = -Inf, y = 3, label = "3% MSE-Inflation",
  #         hjust = -0.2, vjust = -0.2) + 
  labs(x = expression(xi),
       y = "MSE-Inflation",
       color = "True Coefficient",
       size = "Sample Size") + 
  theme_bw()

# 3% cutoff
cutoff_df <- data.frame(info = 3:1500)
cutoff_df$mse_infl90_coef <- 10^predict(rq90_coef, newdata = cutoff_df)
cutoff_df <- cutoff_df[cutoff_df$mse_infl90_coef < 3, ]
cutoff_coef <- which(cutoff_df$mse_infl90_coef == max(cutoff_df$mse_infl90_coef))
coef_y_3 <- cutoff_df$mse_infl90_coef[cutoff_coef]/100
coef_x_3 <- cutoff_df$info[cutoff_coef]

# 10% cutoff
cutoff_df <- data.frame(info = 3:1500)
cutoff_df$mse_infl90_coef <- 10^predict(rq90_coef, newdata = cutoff_df)
cutoff_df <- cutoff_df[cutoff_df$mse_infl90_coef < 10, ]
cutoff_coef <- which(cutoff_df$mse_infl90_coef == max(cutoff_df$mse_infl90_coef))
coef_y_10 <- cutoff_df$mse_infl90_coef[cutoff_coef]/100
coef_x_10 <- cutoff_df$info[cutoff_coef]

df$n_cat <- cut(df$n, breaks = c(-Inf, 1:3*500, Inf), 
                labels = c("Sample Size Between 200 and 5000",
                           "Sample Size Between 501 and 1,000",
                           "Sample Size Between 1,001 and 1,500",
                           "Sample Size Between 1,501 and 2,000"))
gg1 <- gg %+% subset(df, par_type == "Slope Coefficient") + 
  geom_segment(aes(x = coef_x_3, xend = coef_x_3, y = 0, yend = coef_y_3), linetype = "dashed") + 
  geom_segment(aes(x = 0, xend = coef_x_3, y = coef_y_3, yend = coef_y_3), linetype = "dashed") + 
  annotate(geom = "text", x = 2, y = coef_y_3, label = paste("xi == ", coef_x_3), parse = TRUE,
           vjust = -0.1, hjust = 0.1, size = 4) +
  geom_segment(aes(x = coef_x_10, xend = coef_x_10, y = 0, yend = coef_y_10), linetype = "dashed") + 
  geom_segment(aes(x = 0, xend = coef_x_10, y = coef_y_10, yend = coef_y_10), linetype = "dashed") + 
  annotate(geom = "text", x = 2, y = coef_y_10, label = paste("xi == ", coef_x_10), parse = TRUE,
           vjust = -0.1, hjust = 0.1, size = 4) +  annotate(geom = "text", x = 3, y = 10^(sum(coef(rq90_coef)*c(1, log10(3))))/100, 
                                                            label = "90th percentile fit", hjust = -0.1, size = 4) + 
  labs(title = "MSE-Inflation of ML Relative to PML for the\nSlope Coefficients as the Information Increases")
print(gg1)

gg1 + facet_wrap(~n_cat)
ggsave("manuscript/figs/mse-infl-larger-samples-4cat-slope.pdf")


# 3% cutoff
cutoff_df <- data.frame(info = 3:1500)
cutoff_df$mse_infl90_int <- 10^predict(rq90_int, newdata = cutoff_df)
cutoff_df <- cutoff_df[cutoff_df$mse_infl90_int < 3, ]
cutoff_int <- which(cutoff_df$mse_infl90_int == max(cutoff_df$mse_infl90_int))
int_y_3 <- cutoff_df$mse_infl90_int[cutoff_int]/100
int_x_3 <- cutoff_df$info[cutoff_int]

# 10% cutoff
cutoff_df <- data.frame(info = 3:1500)
cutoff_df$mse_infl90_int <- 10^predict(rq90_int, newdata = cutoff_df)
cutoff_df <- cutoff_df[cutoff_df$mse_infl90_int < 10, ]
cutoff_int <- which(cutoff_df$mse_infl90_int == max(cutoff_df$mse_infl90_int))
int_y_10 <- cutoff_df$mse_infl90_int[cutoff_int]/100
int_x_10 <- cutoff_df$info[cutoff_int]

gg2 <- gg %+% subset(df, par_type == "Intercept") + 
  geom_segment(aes(x = int_x_3, xend = int_x_3, y = 0, yend = int_y_3), linetype = "dashed") + 
  geom_segment(aes(x = 0, xend = int_x_3, y = int_y_3, yend = int_y_3), linetype = "dashed") + 
  annotate(geom = "text", x = 2, y = int_y_3, label = paste("xi == ", int_x_3), parse = TRUE,
           vjust = -0.1, hjust = 0.1, size = 3) +
  geom_segment(aes(x = int_x_10, xend = int_x_10, y = 0, yend = int_y_10), linetype = "dashed") + 
  geom_segment(aes(x = 0, xend = int_x_10, y = int_y_10, yend = int_y_10), linetype = "dashed") + 
  annotate(geom = "text", x = 2, y = int_y_10, label = paste("xi == ", int_x_10), parse = TRUE,
           vjust = -0.1, hjust = 0.1, size = 3) +  annotate(geom = "text", x = 3, y = 10^(sum(coef(rq90_int)*c(1, log10(3))))/100, 
                                                            label = "90th percentile fit", hjust = -0.1, size = 4) + 
  labs(title = "MSE-Inflation of ML Relative to PML for\nthe Intercept as the Information Increases")
print(gg2)

gg2 + facet_wrap(~n_cat)
ggsave("manuscript/figs/mse-infl-larger-samples-4cat-intercept.pdf")

pdf("manuscript/figs/mse-infl-larger-samples.pdf", height = 5, width = 13)
grid.arrange(gg1, gg2, nrow = 1)
dev.off()

rq90_coef <- rq(log10(mse_infl) ~ log10(info), tau = 0.9, data = subset(df, par_type == "Slope Coefficient"))
new_df <- data.frame(info = c(1:20*10, 3:10*100))
new_df$mse_infl90_coef <- round(10^predict(rq90_coef, newdata = new_df), 1)

rq90_int <- rq(log10(mse_infl) ~ log10(info), tau = 0.9, data = subset(df, par_type == "Intercept"))
new_df$mse_infl90_int <- round(10^predict(rq90_int, newdata = new_df), 1)
new_df

new_mat <- as.matrix(new_df)
rownames(new_mat) <- NULL
colnames(new_mat) <- c("$xi$", 
                    "MSE-Inflation for Slope Coefficients",
                    "MSE-Inflation for Intercept")
xtab <- xtable(new_mat, digits = c(0, 0, 1, 1))
print(xtab, include.rownames=FALSE)
