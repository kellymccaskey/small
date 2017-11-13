
library(ggplot2)
library(ggrepel)

x <- seq(.1, 1, length.out = 1000)
score_fn <- function(x) {
  1/x - 5
}
score_fn1 <- function(x) {
  1/(x) - 5 + 1
}
score_fn2 <- function(x) {
  1/(x) - 5 - 1
}
root <- rootSolve::uniroot.all(score_fn, c(.1, 1))
root1 <- rootSolve::uniroot.all(score_fn1, c(.1, 1))
root2 <- rootSolve::uniroot.all(score_fn2, c(.1, 1))

points_to_label <- data.frame(x = c(root, root, root1, root2),
                              y = c(-1, 1, 0, 0), 
                              label = c("score function misses low",
                                        "score function misses high",
                                        "estimate pushed even higher\nby curvature",
                                        "estimate trapped close\nby curvature"))

df <- data.frame(beta = x, score = score_fn(x)) 



ggplot(df, aes(x = beta, y = score)) +
  #geom_vline(xintercept = root, color = "grey") + 
  scale_x_continuous(limits = c(.1, 1), breaks = root, labels = expression(beta^{true})) + 
  scale_y_continuous(breaks = c(0), 
                     minor_breaks = NULL) + 
  
  geom_line() + #geom_point() + 
  geom_line(aes(y = score + 1), linetype = 2) + 
  geom_line(aes(y = score - 1), linetype = 2) + 
  labs(x = expression(beta), 
       y = "Score Function",
       title = "Illustrating the Source of the Bias") + 
  theme_bw() #+ 
  #geom_text_repel(data = points_to_label, aes(x = x, y = y, label = label))
ggsave("manuscript/figs/illustrate-bias.pdf", height = 6, width = 8)
  


  
