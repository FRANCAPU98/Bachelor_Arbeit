# Gini Coefficien Grafic

library(ineq)

str(AirPassengers)
data(Ilocos)

LC <- Lc(Ilocos$income)

plot(Lc(Ilocos$income))
plot(Lc(Ilocos$income),col="red",lwd=4, xlab = "Cumulative share of people
     from lower to highest incomes", ylab = "Cumulative share of income earned", 
     main = "", xlim=c(0,1), ylim=c(0,1))

polygon(LC$p, LC$L, col = "#65BFFF")
polygon(seq(0,1,by =((0.1)/(633-1)), LC$L, col = "#cyan2")
text(0.6, 0.5, "A", col = "Black", cex = 2)
text(0.8, 0.2, "B", col = "Black", cex = 2)
legend("topleft", 
       legend = c("Line of Equality (45° Degree)", "Lorenz Curze"), 
       col = 1:2, pch = 19, bty = "n")

?legend
?axis


length(LC$L)
