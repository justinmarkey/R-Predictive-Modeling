x <- c(1,3,5,4,8,9,50,12,-3.4)


q3 = with (x, quantile(x, .75) )

q1 = with (x, quantile(x, .25) )

low = q1 - 1.5*(q3-q1)
upper = q3 + 1.5*(q3-q1)


winsor <- with (x,
  ifelse(x > upper, upper),
  ifelse(x < lower, lower, x)
  )


mean(winsor)
