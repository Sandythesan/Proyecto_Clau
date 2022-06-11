#############
# Paquetería
####
library(RCurl)
library("scatterplot3d")
library(MASS)
library("forcats")

############
#    Importación de los datos
####

x <- getURL("https://raw.githubusercontent.com/EduardoSelimMM/ProyectosRegresion/main/datasets/facebook_cosmetic_data.csv")
fb <- read.csv(text = x)

##########
#  Cambio para que estas variables sean factores
####

fb$Type<-as.factor(fb$Type)
fb$Category<-as.factor(fb$Category)
fb$Paid<-as.factor(fb$Paid)
fb$Post.Month<-as.factor(fb$Post.Month)
fb$Post.Weekday<-as.factor(fb$Post.Weekday)
fb$Post.Hour<-as.factor(fb$Post.Hour)

#########
#    Aquí se puede hacer cualquier manejo de los missing data
#           Yo lo que hice fue eliminarlos, eran pocos y por facilidad
#                    más que nada para que lo demás corriera bien
####
as.data.frame(sapply(fb,function(x) sum(is.na(x))))
fb$Paid[is.na(fb$Paid)]<-0
as.data.frame(sapply(fb,function(x) sum(is.na(x))))

###########################
##  Colapsando las variables categóricas de Mes, día de la semana y Hora
##########

library("forcats")
fb$Post.Weekday.col = fct_collapse(fb$Post.Weekday,inicio_sem = c("2","3","4","5"),fin_sem = c("1","6","7"))
table(fb$Post.Weekday.col)

fb$Post.Month.col = fct_collapse(fb$Post.Month,cuatri_1 = c("1","2","3","4"),cuatri_2 = c("5","6","7","8"),cuatri_3 = c("9","10","11","12"))
table(fb$Post.Month.col)

fb$Post.Hour.col = fct_collapse(fb$Post.Hour,maniana = c("5","6","7","8","9","10","11"),tarde = c("12","13","14","15","16","17","18","19"),noche = c("20","22","23","1","2","3","4"))
table(fb$Post.Hour.col)

##########
#  Se dan los nombres y los summary de los datos
####

names(fb)
summary(fb)

###############
#    Las siguientes son gráficas para ver las correlaciones entre las var. de respuesta
####
pairs(~Lifetime.Post.Total.Reach+ Lifetime.Post.Total.Impressions+
        Lifetime.Engaged.Users+Lifetime.Post.Consumers+
        Lifetime.Post.Consumptions+Lifetime.Post.Impressions.by.people.who.have.liked.your.Page +
        Lifetime.Post.reach.by.people.who.like.your.Page+
        Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post+
        comment+like+share+Total.Interactions, data = fb)


df<-fb[8:19]
# Grupos
x<-7
names(fb[x])
groups <- fb[,x]
# Número de grupos
l <- length(unique(groups))
pairs(df, col = hcl.colors(l, "Temps")[groups])

###############
#    Las siguientes son gráficas para ver las correlaciones entre las var. de entrada
####

pairs(~log(Lifetime.Post.Total.Reach)+Page.total.likes+Post.Month+Post.Weekday+Post.Hour,
      col = factor(fb$Type), pch = 19, data=fb, main="Type")

pairs(~Lifetime.Post.Total.Reach+Page.total.likes+Post.Month+Post.Weekday+Post.Hour,
      col = factor(fb$Category), pch = 19, data=fb,main="Category")

pairs(~Lifetime.Post.Total.Reach+Page.total.likes+Post.Month+Post.Weekday+Post.Hour,
      col = factor(fb$Paid), pch = 19, data=fb, main="Paid")

"
Sin embargo, no se observa nada muy claro.
Si es de notar que la variable Total de likes tiene relación importante con el Mes
   Eso es lógico pues los datos fueron del 2014, del 1 de enero al 31 de diciembre
       es de esperarse que la página tenga más suscriptores con el tiempo.
"

###############
#    Las siguientes son gráficas para ver las correlaciones dos variables continuas y la de salida
#               Sigue siendo un análisis exploratorio para ver si existe correlación
####

scatterplot3d(fb$Post.Month,fb$Post.Weekday,
              fb$Lifetime.Post.Total.Reach,
              type = "h", color = "blue", pch = 16,
              angle=150)$plane3d(model1)

scatterplot3d(fb$Post.Hour,fb$Post.Weekday,
              fb$Lifetime.Post.Total.Reach,
              type = "h", color = factor(fb$Category), pch = 16,
              angle=40)$plane3d(model2)

scatterplot3d(fb$Post.Hour,fb$Post.Month,
              fb$Lifetime.Post.Total.Reach,
              type = "h", color = factor(fb$Category), pch = 16,
              angle=30)$plane3d(model3)

"
No se vé que el modelo utilizando esas variables vaya a explicar mucho
"

"
Para la parte que sigue se van a hacer varias cosas.

1) Se hizo un modelo sólo utilizando las variables:
Mes, día, hora, tipo, clase, pago

Pensando que estas son variables que se pueden controlar.
A este le llamamos el modelo pred, de predictivo

2) Se hizo un modelo sólo utilizando las variables:
Mes, día, hora, tipo, clase, pago

3) Se realizó un modelo utilizando las variables de
   Mes, Día de la semana y Hora, agrupadas en
   Mes: Cuatrimestres:
   Día de la semana: Entre semana: Lunes-Jueves; Fin de semana Viernes-Domingo
   Y hora: Mañana, tarde y noche
___
4) Se realizó el comando step para poder obtener 
el mejor modelo de los casos anteriores.

"
#############
#    1) Modelo predictivo con su optimización
####

fb_pred<-lm(Lifetime.Post.Total.Reach~Post.Hour+Post.Weekday+
              Post.Month+Type+Category+Paid+Page.total.likes, fb)
step.model_pred<-stepAIC(fb_pred, direction = "both",
                         trace = TRUE)
summary(fb_pred)
summary(step.model_pred)

par(mfrow=c(2,2))
plot(fb_pred)

#############
#    2) Se realizó de nuevo el modelo predictivo, pero utilizando el log del alcance
####

fb_pred_log<-lm(log(Lifetime.Post.Total.Reach)~Post.Hour+Post.Weekday+
                  Post.Month+Type+Category+Paid+Page.total.likes, fb)
step.model_pred_log<-stepAIC(fb_pred_log, direction = "both",
                             trace = TRUE)
summary(fb_pred_log)
summary(step.model_pred_log) 

par(mfrow=c(2,2))
plot(fb_pred_log)



#############
#    3) Se realizó de nuevo el modelo predictivo,
#              pero utilizando el log del alcance y las variables agrupadas
"
   Mes, Día de la semana y Hora, agrupadas en
   Mes: Cuatrimestres:
   Día de la semana: Entre semana: Lunes-Jueves; Fin de semana Viernes-Domingo
   Y hora: Mañana, tarde y noche
"
####

fb_pred_log2<-lm(log(Lifetime.Post.Total.Reach)~Post.Hour.col+
                   Post.Weekday.col+Post.Month.col+Type+Category+
                   Paid+Page.total.likes, fb)
step.model_pred_log2<-stepAIC(fb_pred_log2, direction = "both",
                              trace = TRUE)
summary(step.model_pred_log2)
plot(step.model_pred_log2)


################
#     Predicción de valores
###

modelo.final<-step.model_pred_log

hora<-3
mes<-12
tipo<-"Photo"
categoria<-2
pagado<-0

datos.nuevos<-data.frame(Post.Hour=hora,
                         Post.Month=mes,
                         Type=tipo,
                         Category=categoria,
                         Paid=pagado)
exp(predict(modelo.final, newdata = datos.nuevos))



###############
#    Los siguientes boxplots son para ver el comportamiento de las variables explicativas y las variables de respuesta
####

###############
#    Boxplots
####
boxedit<-function(df, col){
  vectorname<-names(fb)[col]
  boxplot(df[vectorname], col = "palegreen2",
          notch = TRUE, # Add notch if TRUE,
          ylab=vectorname,lty = 1)
  title(vectorname)
}


par(mfrow=c(1,2))
vect<-c(1,8:19)
for (i in vect) {
  boxedit(fb, i)
}
#

###############
#    Barplots, frecuencia de publicaciones por categoría de las variables Categóricas
####
dev.off()
baredit<-function(df, col){
  vectorname<-names(fb)[col]
  barplot(table(fb[vectorname]), col="lightpink")
  title(vectorname, cex=2)
}

par(mfrow=c(3,2))
for (i in 2:7) {
  baredit(fb,i)
}

###############
#    Número de Outliers en las categorías cuantitativas
####
outliers<-function(df,col){
  #df<-fb
  #col<-1
  vectorname<-names(df)[col]
  out<-length(boxplot.stats(fb[,col])$out)
  outlier<-data.frame(out)
  names(outlier)<-vectorname
  return(t(outlier))
}

tmp<-outliers(fb,1)
out<-rbind(out,tmp)

for (i in c(8:19)) {
  tmp<-outliers(fb,i)
  out<-rbind(out,tmp)
}
out
