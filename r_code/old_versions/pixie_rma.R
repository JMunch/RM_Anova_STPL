dust(rma_results) %>% 
    sprinkle(cols = "p-value", fn = quote(pvalString(value))) %>% 
    sprinkle(cols = c("Sum of squares", "Mean squares", "F-value"),
             round = 3) 

