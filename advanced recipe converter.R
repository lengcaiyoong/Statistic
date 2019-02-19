print(noquote("100g : 600g = 2tbsp : x "))
a <- as.numeric(readline(noquote("Enter first term: ")))
b <- as.numeric(readline(noquote("Enter 2nd term: ")))
c <- as.numeric(readline(noquote("Enter 3rd term: ")))

result <- as.numeric((b*c)/a)

print(noquote(paste("x", "=", result)))




