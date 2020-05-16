#!/usr/bin/env Rscript

require(ggplot2)

# Load our data.
mydata <- read.csv("data.csv")

# ------------------------------------------------------------------------------

# Plot LR(1) construction time as a function of the number of LR(1) states.

myplot <-
  ggplot(
    subset(mydata, lr1time >= 0.05),
    aes(x=lr1states, y=lr1time)
  ) +
  geom_point(size=2) +
#  scale_x_log10() +
#  scale_y_log10() +
  xlab("# LR(1) states") +
  ylab("LR(1) construction time (seconds)")

ggsave("lr1states-lr1time.pdf", myplot, width=12, height=8, units="cm")

# ------------------------------------------------------------------------------

# Plot LR(1) construction time as a function of the number of terminal symbols.

myplot <-
  ggplot(
    subset(mydata, lr1time >= 0.05),
    aes(x=terminals, y=lr1time)
  ) +
  geom_point(size=2) +
#  scale_x_log10() +
#  scale_y_log10() +
  xlab("# terminals") +
  ylab("LR(1) construction time (seconds)")

ggsave("terminals-lr1time.pdf", myplot, width=12, height=8, units="cm")

# ------------------------------------------------------------------------------

# Plot the numbers of terminal and nonterminal symbols.

myplot <-
  ggplot(
    mydata,
    aes(x=terminals, y=nonterminals)
  ) +
  geom_point(size=2) +
  xlab("# terminals") +
  ylab("# nonterminals")

ggsave("terminals-nonterminals.pdf", myplot, width=12, height=8, units="cm")
