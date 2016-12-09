coef <- list(c(47, 2, 0, 0), 
             c(7, 42, -8, 0),
             c(78, -71, 43, -7))


coef <- list(c(47, 2, 0), 
             c(7, 42, -8))

my_data <- data.frame("x" = rep(1:4, times = 20))

cols <- c("1", "2", "3")

library(ggplot2)    
g1 <- ggplot(data = my_data, aes(x = x)) 

for(i in 1:3){
    my_fun <- function(x){
        sum(as.vector(outer(x, 0:3, FUN="^")) * coef[[i]])
    }
    my_fun <- Vectorize(my_fun)
    g1 <- (g1 + stat_function(fun = my_fun, aes(col = cols[i]), lwd = 1.2))
    
    print(g1)
}


k <- 4


library(tidyverse)
poly_curve_data <- data.frame(x = seq(1, k, length.out = 100), 
           tcrossprod(outer(seq(1, k, length.out = 100), 0:(k-1), `^`), do.call(rbind, poly_coef))) %>% 
    gather(var, y, -x)

g1 <- ggplot(data = rma_data_long, aes(x = condition, y = value)) + 
    geom_point() + 
    labs(col = "Order of \npolynomial", x = "Condition", y = "Value", title = "Orthogonal polynomial contrasts") + 
    geom_path(data = poly_curve_data, aes(x, y, color = var), lwd = 12)


