library(ggplot2)
library(tidyverse)

amostra <- c(0.5,	1.2,	1.2,	1.5,	1.6,	2.0, 2.2,	2.6,	3.0,	3.7,	4.1,	5.3)
print(amostra)

tab <- data.frame(media = mean(amostra), desvpad = sd(amostra))
print(tab)

hist(amostra)
boxplot(amostra)