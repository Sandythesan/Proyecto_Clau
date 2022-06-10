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
#  Se cambió el NA de la variable PAID, por un 0. Los otros NA se dejaron ya que no estaban en variables de interés
as.data.frame(sapply(fb,function(x) sum(is.na(x))))
fb$Paid[is.na(fb$Paid)]<-0
as.data.frame(sapply(fb2,function(x) sum(is.na(x))))

###########
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

"
# De lo anterior es de notar que hay relaciones medio lineales entre:

 Like y Total interactions
 Life engaged users y Life post consumption
"
###############
#    Las siguientes son gráficas para ver las correlaciones entre las var. de entrada
##     Pero sólo se usaron las variables continuas, y se pintó de acuerdo a las var categóricas
####


pairs(~log(Lifetime.Post.Total.Reach)+Page.total.likes+Post.Month+Post.Weekday+Post.Hour,
      col = factor(fb$Type), pch = 19, data=fb, main="Type")

pairs(~Lifetime.Post.Total.Reach+Page.total.likes+Post.Month+Post.Weekday+Post.Hour,
      col = factor(fb$Category), pch = 19, data=fb,main="Category")

pairs(~Lifetime.Post.Total.Reach+Page.total.likes+Post.Month+Post.Weekday+Post.Hour,
      col = factor(fb$Paid), pch = 19, data=fb, main="Paid")

"
Sin embargo, al menos yo, no observo nada muy claro.
Si es de notar que la variable Total de likes tiene relación importante con el Mes
   Eso es lógico pues los datos fueron del 2014, del 1 de enero al 31 de diciembre
       es de esperarse que la página tenga más suscriptores con el tiempo.
"

###############
#    Las siguientes son gráficas para ver las correlaciones dos variables continuas y la de salida
#               Sigue siendo un análisis exploratorio para ver si existe correlación
####

model1<-lm(fb$Lifetime.Post.Total.Reach~fb$Post.Month+fb$Post.Weekday)
scatterplot3d(fb$Post.Month,fb$Post.Weekday,
              fb$Lifetime.Post.Total.Reach,
              type = "h", color = "blue", pch = 16,
              angle=150)$plane3d(model1)

model2<-lm(fb$Lifetime.Post.Total.Reach~fb$Post.Hour+fb$Post.Weekday)
scatterplot3d(fb$Post.Hour,fb$Post.Weekday,
              fb$Lifetime.Post.Total.Reach,
              type = "h", color = factor(fb$Category), pch = 16,
              angle=40)$plane3d(model2)

model3<-lm(fb$Lifetime.Post.Total.Reach~fb$Post.Hour+fb$Post.Month)
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

--
2) Se hizo un modelo con todas las variables. Se le llamó el modelo saturado

___
3) Se realizó el comando step para poder obtener el mejor modelo de los dos casos anteriores.

"
#############
#    1) Modelo predictivo con su optimización
####

fb_pred<-lm(fb$Lifetime.Post.Total.Reach~fb$Post.Hour+fb$Post.Weekday+fb$Post.Month+fb$Type+fb$Category+fb$Paid)
step.model_pred<-stepAIC(fb_pred, direction = "both",
                         trace = TRUE)
summary(step.model_pred)
plot(step.model_pred)


#############
#    2) Modelo total, saturado, con todas las variables; y su optimización
####


fb_sat<-lm(log(Lifetime.Post.Total.Reach)~., fb)
step.model_sat <- stepAIC(fb_sat, direction = "both", 
                      trace = TRUE)
summary(step.model_sat)
plot(step.model_sat)


#############
#    4) Se realizó de nuevo el modelo predictivo, pero utilizando el log del alcance
####

fb_pred_log<-lm(log(fb$Lifetime.Post.Total.Reach)~fb$Post.Hour+fb$Post.Weekday+fb$Post.Month+fb$Type+fb$Category+fb$Paid)
step.model_pred_log<-stepAIC(fb_pred_log, direction = "both",
                             trace = TRUE)
summary(step.model_pred_log) 
plot(step.model_pred_log)
                     
#############
#    5) Se realizó de nuevo el modelo predictivo, pero utilizando el log del alcance y con las nuevas variables agrupadas
####                     
fb_pred_log2<-lm(log(fb$Lifetime.Post.Total.Reach)~fb$Post.Hour.col+fb$Post.Weekday.col+fb$Post.Month.col+fb$Type+fb$Category+fb$Paid+fb$Page.total.likes)
step.model_pred_log2<-stepAIC(fb_pred_log2, direction = "both",
                             trace = TRUE)
summary(step.model_pred_log2)
plot(step.model_pred_log2)
                 
