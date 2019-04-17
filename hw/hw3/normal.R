a = seq(-5,5,0.1)

normal = function(x, mean, sd){
  nr = (1/sqrt(2*pi*sd^2)) * exp(1)^(-(x-mean)^2/(2*sd^2))
  return (nr)
}
plot(a, normal(a, 0, 1), type='l' )

x = seq(-5, 25, 0.1)
u = c(5, 10 ,15)
sd = 2
weights = c(1/3,1/3,1/3)
y = 1/3*normal(x, 5, sd) + 1/3*normal(x, 10, sd) + 1/3*normal(x, 15, sd)
plot(x, y, type='l' )
