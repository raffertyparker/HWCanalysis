# This generates plots to how the effect of increasing rho has on the single-var
# Copied from youtube comments, interesting but not otherwise relevent to my work
z = rnorm(1000) 
gen = function(rho) {
  x = numeric(length(z))
  x[1] = z[1] 
  for (i in 2:length(z)){ 
    x[i] = rho*x[i-1] + z[i]
  } 
  x } 
display = function(rho) {
  x = gen(rho)
  plot(x, main=as.character(rho)) 
  lines(x) }
for (it in 1:100) {
  display(it/100) 
  Sys.sleep(0.5) }