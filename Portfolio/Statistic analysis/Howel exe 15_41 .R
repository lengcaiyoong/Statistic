#mediator

#Exercise 15.41
#   When the authors used both the Just-World Prime and Global Warming Skepticism as
#       predictors, the standardized regression coefficients between Just-World Prime and Willingness
#       to Reduce Carbon Footprint dropped to 20.16, with a standard error of 0.13

Wdat <- cbind(BetaWeight = c(0.33,-0.6,-0.34),
SE = c(0.15,0.025,0.15))



mediator = function(b1,s1,b2,s2){
  result <- (b1*b2)/(sqrt(b1^2*s2^2+b2^2*s1^2-s1^2*s2^2))
  return(result)
}



mediator(0.33,0.15,-0.6,0.025)
pnorm(mediator(0.33,0.15,-0.6,0.025),0,1)
        #thus, strong evidence showing mediating effect


#chapter 15 example
    mediator(0.403,0.096,0.321,0.106)
    pnorm(2.5,0,1,lower.tail=F)



