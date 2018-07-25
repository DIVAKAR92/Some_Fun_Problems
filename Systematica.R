#' Function to simulate 2 random points on the circumference of a circle of radius r
#' Inputs is the radius of the circle (r), the default value is 1.
#' Output are the vector of co-ordinates of 2 points in the order (x1, y1, x2, y2)
#' 
generate_points_circle = function(r = 1){
  if(r <= 0) warning("Radius of circle can not be negative!")
  theta = runif(1)*2*pi; phi = runif(1)*2*pi;
  result = c(r*cos(theta), r*sin(theta), r*cos(phi), r*sin(phi))
  return(result)
}

#' Function to calculate the probability that the chord joining two random points drawn on
#' the circumference of the circle is larger than the side of an equilateral traingle 
#' inscribed within it. 
#' Inputs are nsim = number of simulations to be done and r =  radius of circle
calculate_probability = function(nsim = 10000, r = 1){
  side_length = sqrt(3)*r
  chord_length = rep(0, nsim)
  for(i in 1:nsim){
    tmp = generate_points_circle(r)
    chord_length[i] = sqrt((tmp[1] - tmp[3])^2 + (tmp[2] - tmp[4])^2)
  }
  prob = sum(chord_length > side_length)/nsim
  return(prob)
}
print("The required probability is ")
calculate_probability(1000000,1)

#'========================= Question 4.b. ==================================
#'Function to 

p = 0.2
mat = matrix(c(1-p,p,0,1-p,0,p,0,1-p,p), nrow = 3, byrow = TRUE)
ev = eigen(mat)$vectors
sqrt(p*(1-p))
