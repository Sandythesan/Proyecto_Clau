
# Aquí pueden editar el archivpo
boxedit<-function(df, col){
  vectorname<-names(fb)[col]
  boxplot(df[vectorname], col = "palegreen2",
          notch = TRUE, # Add notch if TRUE,
          ylab= vectorname,lty = 1)
  title(vectorname)
}

# Ejemplo
boxedit(fb, 1)
