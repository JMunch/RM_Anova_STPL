### Plotting functions with ggplot

# useful links:
# http://t-redactyl.io/blog/2016/03/creating-plots-in-r-using-ggplot2-part-9-function-plots.html
# https://kohske.wordpress.com/2010/12/25/draw-function-without-data-in-ggplot2/



### Example:

library(ggplot2)

cubeFun <- function(x) {
    x^8 * 0.5
}

p9 <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) +
    stat_function(fun = cubeFun)
p9
