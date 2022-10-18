#### 686-Modulo76 ####

TrimDicEneFeb20 <- read_sav(file.choose())
View(TrimDicEneFeb20)
names(TrimDicEneFeb20)


#Ingreso total monetario en la ocupación principaö por trb. dependiente #p211a y #p211b
#p211a
table(is.na(TrimDicEneFeb20$p211a))
summary(TrimDicEneFeb20$p211a)

Gini_Gen <- ineq(TrimDicEneFeb20$p211a, type = "Gini")
plot(Lc(TrimDicEneFeb20$p211a))


#p211b
table(is.na(TrimDicEneFeb20$p211b))
summary(TrimDicEneFeb20$p211b)

#Ingreso total monetario en la ocupación principaö por trb. independiente #p212a y #p212b
#p212a
summary(TrimDicEneFeb20$p212a)

#p212b
summary(TrimDicEneFeb20$p212b)

#Ingreso monetarioen en el mes anterior por actividad secundaria  #p212c y #p212d
#p212c 
summary(TrimDicEneFeb20$p212c)

#p212d
summary(TrimDicEneFeb20$p212d)


survey_i_RMLC20 <- TrimDicEneFeb20[which(TrimDicEneFeb20$dominio == 8 ),]
View(survey_i_RMLC20)
