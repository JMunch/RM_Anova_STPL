### Plotting orthogonal polynomial contrasts ###



# ggplot approach ---------------------------------------------------------


poly_fun1 <- function(x) {
    poly.fit.1$coefficients[1] + poly.fit.1$coefficients[2]*x
}

poly_fun2 <- function(x) {
    poly.fit.2$coefficients[1] + poly.fit.2$coefficients[2]*x + 
        poly.fit.2$coefficients[3]*(x^2)
}


poly_fun3 <- function(x) {
    poly.fit.3$coefficients[1] + poly.fit.3$coefficients[2]*x + 
        poly.fit.3$coefficients[3]*(x^2) + poly.fit.3$coefficients[4]*(x^3)
}


poly.fit.4 <- poly.fit.max

poly_fun4 <- function(x) {
    poly.fit.4$coefficients[1] + poly.fit.4$coefficients[2]*x + 
        poly.fit.4$coefficients[3]*(x^2) + poly.fit.4$coefficients[4]*(x^3) +
        poly.fit.4$coefficients[5]*(x^4)
}


ggplot(data = rma_data_long, aes(x = condition, y = value)) + geom_point() + 
    labs(col = "Order of \npolynomial", x = "Condition", y = "Value", title = "Orthogonal polynomial contrasts") +
    stat_function(fun = poly_fun1, aes(col = "1"), lwd = 1.2) + 
    stat_function(fun = poly_fun2, aes(col = "2"), lwd = 1.2) + 
    stat_function(fun = poly_fun3, aes(col = "3"), lwd = 1.2) + 
    stat_function(fun = poly_fun4)
