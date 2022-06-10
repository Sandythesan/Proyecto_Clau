#
#   Boxplots


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
as.data.frame(sapply(fb,function(x) sum(is.na(x))))

###########
library("forcats")
fb$Post.Weekday.col = fct_collapse(fb$Post.Weekday,inicio_sem = c("2","3","4","5"),fin_sem = c("1","6","7"))
table(fb$Post.Weekday.col)

fb$Post.Month.col = fct_collapse(fb$Post.Month,cuatri_1 = c("1","2","3","4"),cuatri_2 = c("5","6","7","8"),cuatri_3 = c("9","10","11","12"))
table(fb$Post.Month.col)

fb$Post.Hour.col = fct_collapse(fb$Post.Hour,maniana = c("5","6","7","8","9","10","11"),tarde = c("12","13","14","15","16","17","18","19"),noche = c("20","22","23","1","2","3","4"))
table(fb$Post.Hour.col)          

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

