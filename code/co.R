# Function for number prints
co <- function(data, x = NULL, nsmall = 2, nround = 2, ger = T) {
  if ((is.data.frame(data) | is_tibble(data)) & !is.null(x) ){
    x <- data %>% pull(x)  
  } else {
    x <- data
  }
  if (ger == T) {
    out <- x %>% format(digits = nround, nsmall = nsmall, decimal.mark = ",")
  } else {
    out <- x %>% format(digits = nround, nsmall = nsmall)
  }
  
  return(out)
}