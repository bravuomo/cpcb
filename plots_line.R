library(polynom)
library(ggplot2)

set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
df <- data.frame("x"=dsmall$carat, "y"=dsmall$price)

my.formula <- y ~ poly(x, 3, raw = TRUE)
p <- ggplot(df, aes(x, y)) 
p <- p + geom_point(alpha=2/10, shape=21, fill="blue", colour="black", size=5)
p <- p + geom_smooth(method = "lm", se = FALSE, 
                     formula = my.formula, 
                     colour = "red")

m <- lm(my.formula, df)
my.eq <- as.character(signif(as.polynomial(coef(m)), 3))
label.text <- paste(gsub("x", "~italic(x)", my.eq, fixed = TRUE),
              paste("italic(R)^2",  
                    format(summary(m)$r.squared, digits = 2), 
                    sep = "~`=`~"),
		paste("italic(p)",  
                    format(summary(m)$p.value, digits = 3), 
                    sep = "~`=`~"),
                    sep = "~~~~")


p + annotate(geom = "text", x = 0.2, y = 15000, label = label.text, 
             family = "serif", hjust = 0, parse = TRUE, size = 4)


p <- ggplot(my.data, aes(x, y2, color = group)) +
  geom_point() +
  stat_smooth(aes(fill = group, color = group), method = "lm", formula = formula) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  theme_bw()
ggpar(p, palette = "jco")
