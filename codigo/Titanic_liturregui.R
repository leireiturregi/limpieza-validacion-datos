Titanic <- readXL("C:/Users/usuario/Documents/Practica 2 Titanic/Titanic.xlsx", rownames=FALSE, header=TRUE, 
  na="", sheet="Titanic_Research_v6", stringsAsFactors=TRUE)

names(Titanic)
dim(Titanic)

# Resumen estadístico
summary(Titanic)

# Borramos los campos innecesarios
Titanic$name <- NULL
Titanic$ticket <- NULL
Titanic$cabin <- NULL
Titanic$embarked <- NULL
Titanic$boat <- NULL 
Titanic$body <- NULL
Titanic$home.dest <- NULL

names(Titanic)
summary(Titanic)

# Sustituimos por la media los elementos vacios del atributo AGE
Titanic$age[which(is.na(Titanic$age))] <- mean(Titanic$age,na.rm = TRUE)

# Sustituimos por la media los elementos vacios del atributo SIBSP
Titanic$sibsp[which(is.na(Titanic$sibsp))] <- mean(Titanic$sibsp,na.rm = TRUE)

# Sustituimos por la media los elementos vacios del atributo FARE
Titanic$fare <- as.integer(Titanic$fare)
Titanic$fare[which(is.na(Titanic$fare))] <- mean(Titanic$fare,na.rm = TRUE)

# Normalizamos los campos PCLASS, SURVIVED y AGE
library(dplyr)
Titanic <- mutate(Titanic, clase = forcats::fct_recode(as.factor(pclass), "PRIMERA" = "1", "SEGUNDA" = "2", "TERCERA" = "3"))
Titanic <- mutate(Titanic, superviviente = forcats::fct_recode(as.factor(survived), "SI" = "1", "NO" = "0"))
Titanic <- mutate(Titanic, edad = forcats::fct_recode(as.factor(ifelse(age>=18, "ADULTO", "MENOR"))))
Titanic <- mutate(Titanic, precio_ticket = forcats::fct_recode(as.factor(ifelse(fare>=100, "ALTO", "BAJO"))))

#Creamos el nuevo conjunto de datos
New_Titanic<-Titanic[,c(8,9,3,10,5,6,11)]

# Exportamos a CSV
write.csv(New_Titanic, "Titanic_clean_data.csv")

# Varianza
shapiro.test(Titanic$pclass)
shapiro.test(Titanic$age)
shapiro.test(Titanic$sibsp)
shapiro.test(Titanic$parch)
shapiro.test(Titanic$fare)

library(C50)

# Pasamos el campo superviviente a factor
New_Titanic$superviviente<-as.factor(New_Titanic$superviviente)

X <- New_Titanic[,c(1,3,4,5,6,7)] 
y <- New_Titanic[,2]

#Creamos el arbol de decisión
modelo_arbol <- C50::C5.0(X, y)
summary(modelo_arbol)
plot(modelo_arbol)

# Barras edad y clase
library(ggplot2)
ggplot(New_Titanic) + geom_bar(aes(edad, fill = superviviente), position = "fill") + facet_wrap(~ clase) + labs(y = "Sobrevive / Muere", title = "Supervivientes por clase y edad")

#Barras sexo y clase
ggplot(New_Titanic) + geom_bar(aes(sex, fill = superviviente), position = "fill") + facet_wrap(~ clase) + labs(y = "Sobrevive / Muere", title = "Supervivientes por clase y sexo")


