#Copyright 2021 Daniel Martín-Moreno Romero
#
#Licensed under the Apache License, Version 2.0 (the "License");
#you may not use this file except in compliance with the License.
#You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#Unless required by applicable law or agreed to in writing, software
#distributed under the License is distributed on an "AS IS" BASIS,
#WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#See the License for the specific language governing permissions and
#limitations under the License.

library(shiny)
library(ggplot2)
library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(e1071)


shinyServer(function(input, output, session) {
    dataset <- reactive ({ 
        file <-  input$file1
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Por favor seleccione un fichero"))
        entradas.df <- read.csv2(file$datapath, header = TRUE)
        
        entradas.df$importe_x_kg=entradas.df$importe/entradas.df$neto
        entradas.df$importe_x_kg_pts=entradas.df$importe_x_kg*166.386
        

        kilopuntosDesdeFila <- function(fila){
            puntosGrado=0
            puntosAcidez=0
            puntosGluconico=0
            puntosPotasio=0
            
            pesoGrado = as.numeric(input$grado)
            pesoAcidez = as.numeric(input$acidez)
            pesoPotasio = as.numeric(input$potasio)
            pesoGluconico = as.numeric(input$gluconico)
             
            
            if (fila[1]<9)
                puntosGrado=0
            else if (fila[1]>13)
                puntosGrado=pesoGrado
            else
                puntosGrado = pesoGrado-((pesoGrado/41)*(13-fila[1])*10)
            
            if (fila[2]<3.01 | fila[2]>8.49)
                puntosAcidez=0
            else if (fila[2]<7.01 & fila[2]>4.49)
                puntosAcidez=pesoAcidez
            else if (fila[2]>=3.01 & fila[2]<=4.49)
                puntosAcidez=(fila[2] - 3)*100*pesoAcidez/149
            else
                puntosAcidez=(8.5 - fila[2])*100*pesoAcidez/149
            
            if (fila[3]<=1500)
                puntosPotasio=pesoPotasio
            else if (fila[3]>=3000)
                puntosPotasio=0
            else
                puntosPotasio=pesoPotasio - (fila[3]-1500)*pesoPotasio/1500
            
            if (fila[4]<=0)
                puntosGluconico=pesoGluconico
            else if (fila[4]>2.5)
                puntosGluconico=0
            else
                puntosGluconico=(2.51 - fila[4])*100*pesoGluconico/251
            
            puntosGrado+puntosAcidez+puntosGluconico+puntosPotasio
        }
        
        entradasUvaBASoloParametros.df=entradas.df[,c(5:8)]

        #función a ejecutar para el dataframe
        kilopuntosDesdeDF <- function(df) {	apply(df, MARGIN=1, FUN=kilopuntosDesdeFila)}
        
        #añadimos una nueva columna al dataframe original
        entradas.df$kilopuntos=kilopuntosDesdeDF(entradasUvaBASoloParametros.df)
        entradas.df$kilopuntos=round(entradas.df$kilopuntos,digits=2)

        ########################################################################

        entradas.df
    })
    output$output_filename=renderText({
        req(input$file1)
        paste("Para ",input$file1[1,1]," con ",nrow(dataset())," entradas y un importe de ",round(sum(dataset()[,9]),2),"€")
    })
    output$output_kilopuntos_suma=renderText({
        paste("Para un total de: ",as.numeric(input$grado)+as.numeric(input$acidez)+as.numeric(input$potasio)+as.numeric(input$gluconico)," kilopuntos")
    })
    output$output_summary <- renderPrint({
        summary(dataset()[,c(2:9)])
    })
    
    output$output_summary_kilopuntos <- renderPrint({
        summary(dataset()[,c(12)])
    })
    
    #Pestaña Visualizaciones
    output$output_filename_g=renderText({
        req(input$file1)
        paste("Para ",input$file1[1,1]," con ",nrow(dataset())," entradas")
    })
    
    #Grafico fechas
    output$output_plot_hist_fechas <- renderPlot({
        fechas.table=table(dataset()$fecha)
        fechas.df=as.data.frame(fechas.table)
    
        plot = ggplot(data=fechas.df, aes(x=Var1, y=Freq))+
            geom_bar(stat="identity", fill="steelblue", col="black", width = 1)+
            labs(x = "Fecha") + labs(y = "Num. entradas")+
            ggtitle("Entradas por fecha ")+
            theme(axis.text.x = element_text(size = 8.5,
                                             angle = 45,
                                             color = "black",hjust = 1))+
            theme(plot.title = element_text(hjust = 0.5))
        plot
        })
    
    #Grafico grado
    output$output_plot_hist_grado <- renderPlot({
        grado.table=table(dataset()$grado)
        grado.df=as.data.frame(grado.table)
        
        plot = ggplot(data=grado.df, aes(x=Var1, y=Freq))+
            geom_bar(stat="identity", fill="steelblue", col="black", width = 1)+
            labs(x = "Grado") + labs(y = "Num. entradas")+
            ggtitle("Distribución de Grado ")+
            theme(axis.text.x = element_text(size = 8.5,
                                             angle = 45,
                                             color = "black",hjust = 1))+
            theme(plot.title = element_text(hjust = 0.5))
        plot
    })
    
    #Grafico acidez
    output$output_plot_hist_acidez <- renderPlot({
        plot <- ggplot(dataset(), aes(x=acidez))+
            geom_histogram(fill="steelblue", colour = "black")+
            labs(x = "Acidez") + labs(y = "Num. entradas")+
            ggtitle("Distribución de Acidez ")+
            theme(plot.title = element_text(hjust = 0.5))
        plot
    })
    
    #Grafico potasio
    output$output_plot_hist_potasio <- renderPlot({
        plot <- ggplot(dataset(), aes(x=potasio))+
            geom_histogram(fill="steelblue", colour = "black")+
            labs(x = "Potasio") + labs(y = "Num. entradas")+
            ggtitle("Distribución de Potasio")+
            theme(plot.title = element_text(hjust = 0.5))
        plot
    })
    
    #Grafico gluconico
    output$output_plot_hist_gluconico <- renderPlot({
        plot <- ggplot(dataset(), aes(x=gluconico))+
            geom_histogram(fill="steelblue", colour = "black")+
            labs(x = "Glucónico") + labs(y = "Num. entradas")+
            ggtitle("Distribución de Glucónico")+
            theme(plot.title = element_text(hjust = 0.5))
        plot
    })
    
    #Grafico kilopuntos
    output$output_plot_hist_kilopuntos <- renderPlot({
        plot <- ggplot(dataset(), aes(x=kilopuntos))+
            geom_histogram(fill="steelblue", colour = "black")+
            labs(x = "Kilopuntos") + labs(y = "Num. entradas")+
            ggtitle("Distribución de Kilopuntos")+
            theme(plot.title = element_text(hjust = 0.5))
        plot
    })
    
   
    ### EVENTO CALCULAR
    observeEvent(input$file1, {
        output$output_comparacion_estadisticos_precios <- NULL
        output$output_plot_valor_kp <- NULL
        output$output_plot_hist_importe_real <- NULL
        output$output_plot_hist_importe_simulado <- NULL
        
        
        output$output_table_corr_importe_simulado <- NULL
        output$output_plot_corr_importe_simulado <- NULL
        output$output_table_corr_importe_real <- NULL
        output$output_plot_corr_importe_real <- NULL
        
        output$output_plot_impacto_importe_real <-NULL
        output$output_plot_impacto_importe_simulado <- NULL
        output$output_plot_impacto_importe_diferencias <- NULL
        output$output_text_impacto_importe_diferencias_dist <- NULL
        output$output_table_impacto_importe_diferencias_dist_1 <- NULL
        output$output_table_impacto_importe_diferencias_dist_2 <- NULL
        
        output$output_plot_arbol <- NULL
        output$output_text_arbol_reglas <- NULL
        output$output_plot_clases<- NULL

        output$output_text_arbol_reglas  <- NULL
        output$output_text_matriz_confusion <- NULL
        output$output_table_entradas_dif_mas_pos <- NULL
        output$output_table_entradas_dif_mas_neg <- NULL
        output$output_table_entradas_mas_kp <- NULL
        output$output_table_entradas_menos_kp <- NULL
        
    })
    
     observeEvent(input$button_simular, {
        req(input$file1)
        valor=input$valor
        
        pesoGrado = as.numeric(input$grado)
        pesoAcidez = as.numeric(input$acidez)
        pesoPotasio = as.numeric(input$potasio)
        pesoGluconico = as.numeric(input$gluconico)
        
        entradas.df = dataset()
         
        ##########################################################################
        min_kilopuntos=min(entradas.df$kilopuntos)
        entradas.df$kilopuntosDesdeMinimo=entradas.df$kilopuntos-min_kilopuntos
        entradas.df$kp_desde_min_por_neto=entradas.df$neto*entradas.df$kilopuntosDesdeMinimo
        
        
        entradas.df$importe_sim_kp_fijo={
            if (input$criterio=="Porcentaje fijo sobre el total")
                (sum(entradas.df$importe)*as.numeric(input$valor))*entradas.df$neto/sum(entradas.df$neto)
            else 
                as.numeric(input$valor)*entradas.df$neto
        }
        
        dif_a_repartir=sum(entradas.df$importe) - sum(entradas.df$importe_sim_kp_fijo)
        
        entradas.df$importe_sim_kp_var=(entradas.df$kp_desde_min_por_neto / sum(entradas.df$kp_desde_min_por_neto)) * dif_a_repartir

        entradas.df$importe_sim_kp=entradas.df$importe_sim_kp_fijo+entradas.df$importe_sim_kp_var
        entradas.df$importe_sim_kp_x_kg=entradas.df$importe_sim_kp/entradas.df$neto
        entradas.df$importe_sim_kp_x_kg_pts=entradas.df$importe_sim_kp_x_kg*166.386
        
        output$output_comparacion_estadisticos_precios <- renderPrint({
            cols <- c("importe_x_kg","importe_sim_kp_x_kg")
            summary(entradas.df[cols])
        })
        output$output_valor <- renderText({paste(input$criterio," Valor: ",valor," | Pesos (Grado: ",pesoGrado,", Acidez: ",pesoAcidez,", Potasio: ",pesoPotasio,", Glucónico: ",pesoGluconico,")", sep="")})
        
        #vemos el valor del kp
        entradas.df$valor_kp=entradas.df$importe_sim_kp/entradas.df$neto
        entradas.df$valor_kp_pts=entradas.df$valor_kp*166.386
        
        output$output_plot_valor_kp <- renderPlot({
            plot <- ggplot(entradas.df, aes(importe_sim_kp_x_kg, kilopuntos))+
                geom_line()+
                geom_line(color = "steelblue")+
                labs(x = "Importe") + labs(y = "Kilopuntos")+
                ggtitle("Valor Simulado del kilopunto")+
                theme(plot.title = element_text(hjust = 0.5))
            plot
        })
        
        #sacamos los calculos de que numero de socios para los histogramas está mas alto
        #dividimos por 30 pues es el numero que por defecto saca de barras en geom_histogram
        tablaImportesR.df=as.data.frame(table(cut(entradas.df$importe_x_kg, seq(min(entradas.df$importe_x_kg),max(entradas.df$importe_x_kg),(max(entradas.df$importe_x_kg)-min(entradas.df$importe_x_kg))/30))))
        tablaImportesS.df=as.data.frame(table(cut(entradas.df$importe_sim_kp_x_kg, seq(min(entradas.df$importe_sim_kp_x_kg),max(entradas.df$importe_sim_kp_x_kg),(max(entradas.df$importe_sim_kp_x_kg)-min(entradas.df$importe_sim_kp_x_kg))/30))))
        
        #para jugar en los graficos comparativa con la misma escala en las Y
        maxYLim=max(max(tablaImportesR.df$Freq),max(tablaImportesS.df$Freq))
        
        #Grafico precio real
        output$output_plot_hist_importe_real <- renderPlot({
            plot <- ggplot(entradas.df, aes(x=importe_x_kg))+
                geom_histogram(fill="steelblue", colour = "black")+
                labs(x = "Importe por Kg(€)") + labs(y = "Num. entradas")+
                ggtitle("Distribución de Importe por Kg")+
                coord_cartesian(ylim = c(0, maxYLim))+
                theme(plot.title = element_text(hjust = 0.5))
            plot
        })
        
        output$output_plot_hist_importe_simulado <- renderPlot({
            plot <- ggplot(entradas.df, aes(x=importe_sim_kp_x_kg))+
                #geom_histogram(fill="steelblue", colour = "black",breaks = seq(min(entradas.df$importe_sim_kp_x_kg), max(entradas.df$importe_sim_kp_x_kg), by = bwS))+
                geom_histogram(fill="steelblue", colour = "black")+
                labs(x = "Importe simulado por Kg (€)") + labs(y = "Num. entradas")+
                ggtitle("Distribución de Importe simulado por Kg")+
                coord_cartesian(ylim = c(0, maxYLim))+
                theme(plot.title = element_text(hjust = 0.5))
            plot
        })
        
        #metemos en el estudio la fecha numerica
        entradas.df$fecha_num=as.numeric(as.Date(entradas.df$fecha)-min(as.Date(entradas.df$fecha))+1)

        entradas_cor_importe_sim <- cor(entradas.df[, c(4:8,22,18)]) #metemos neto y fecha solo para ver que no guarda relaciÃ³n
        output$output_table_corr_importe_simulado <- renderTable({
            data.frame(entradas_cor_importe_sim)
        },rownames=TRUE,striped = TRUE)
        
        output$output_plot_corr_importe_simulado <- renderPlot({
            #defino una paleta de colores. TODO: depurar comentarios
            col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
                                      "#77AADD", "#4477AA"))
            plot <-corrplot(entradas_cor_importe_sim, method = "square", #method tambien puede ser shade, o circle, o eclipse, o pie, color es el estandar
                     tl.col = "black",
                     tl.srt = 45, col = col(200), #expandemos col. TODO: poner valor menor
                     addCoef.col = "black", 
                     order = "hclust", 
                     type = "upper",
                     diag = F,
                     addshade = "all",
                     cex.axis = 0.3) 
            
            plot
        })
        
        entradas_cor_importe <- cor(entradas.df[, c(4:8,22,10)])
        output$output_table_corr_importe_real <- renderTable({
            data.frame(entradas_cor_importe)
        },rownames=TRUE,striped = TRUE)
        
        output$output_plot_corr_importe_real <- renderPlot({
            #defino una paleta de colores. 
            col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
                                      "#77AADD", "#4477AA"))
            plot <-corrplot(entradas_cor_importe, method = "square", 
                            tl.col = "black",
                            tl.srt = 45, col = col(100), 
                            addCoef.col = "black",
                            order = "hclust",
                            type = "upper",
                            diag = F,
                            addshade = "all",
                            cex.axis = 0.3) 
            
            plot
        })
            
        #vamos a hacer ahora agrupaciones por socio de importe y importeCalc
        importesPorSocio <- tapply(entradas.df$importe, factor(entradas.df$idSocio), sum, na.rm=TRUE)
        
        importesCalcPorSocio <- tapply(entradas.df$importe_sim_kp, factor(entradas.df$idSocio), sum, na.rm=TRUE)
        
        
        socio_importes.df=data.frame(idSocio=names(importesPorSocio),importe=importesPorSocio)
        socio_importesCalc.df=data.frame(idSocio=names(importesCalcPorSocio),importeCalc=importesCalcPorSocio)
        
        socios.df=merge(socio_importes.df,socio_importesCalc.df,by="idSocio")
        socios.df$importeCalc=as.numeric(format(socios.df$importeCalc, scientific = FALSE))
        
        socios.df$dif=(socios.df$importeCalc-socios.df$importe)/socios.df$importe

        #Pestaña Impacto en socios
        output$output_valor_imp <- renderText({paste(input$criterio," Valor: ",valor," | Pesos (Grado: ",pesoGrado,", Acidez: ",pesoAcidez,", Potasio: ",pesoPotasio,", Glucónico: ",pesoGluconico,")", sep="")})
        
        #calculos necesarios para tener la misma escala en las y
        tablaSociosR.df=as.data.frame(table(cut(socios.df$importe, seq(min(socios.df$importe),max(socios.df$importe),(max(socios.df$importe)-min(socios.df$importe))/30))))
        tablaSociosS.df=as.data.frame(table(cut(socios.df$importeCalc, seq(min(socios.df$importeCalc),max(socios.df$importeCalc),(max(socios.df$importeCalc)-min(socios.df$importeCalc))/30))))
        
        #para jugar en los graficos comparativa con la misma escala en las Y
        maxYLimSocios=max(max(tablaSociosR.df$Freq),max(tablaSociosS.df$Freq))
        
        
        output$output_plot_impacto_importe_real <- renderPlot({
            plot <- ggplot(socios.df, aes(x=importe))+
                geom_histogram(fill="steelblue", colour = "black")+
                labs(x = "Importe") + labs(y = "Num. socios")+
                ggtitle("Distribución de Importes por Socio")+
                coord_cartesian(ylim = c(0, maxYLimSocios))+
                theme(plot.title = element_text(hjust = 0.5))
            plot
        })
        output$output_plot_impacto_importe_simulado <- renderPlot({
            plot <- ggplot(socios.df, aes(x=importeCalc))+
                geom_histogram(fill="steelblue", colour = "black")+
                labs(x = "Importe Simulado") + labs(y = "Num. socios")+
                ggtitle("Distribución de Importes Simulados por Socio")+
                coord_cartesian(ylim = c(0, maxYLimSocios))+
                theme(plot.title = element_text(hjust = 0.5))
            plot
        })
        
        output$output_plot_impacto_importe_diferencias <- renderPlot({
            plot <- ggplot(socios.df, aes(x=dif))+
                geom_histogram(fill="steelblue", colour = "black")+
                labs(x = "Diferencia (%)") + labs(y = "Num. socios")+
                ggtitle("Distribución de diferencias de importes por socio")+
                theme(plot.title = element_text(hjust = 0.5))
            plot
        })
        table_freq_1.df=as.data.frame(table(cut(socios.df$dif, seq.int(-1, 0, 0.1))))
        names(table_freq_1.df)[1]="Rango"
        names(table_freq_1.df)[2]="Frecuencia"
        output$output_table_impacto_importe_diferencias_dist_1 <- renderTable(table_freq_1.df)
        table_freq_2.df=as.data.frame(table(cut(socios.df$dif, seq.int(0, 1, 0.1))))
        names(table_freq_2.df)[1]="Rango"
        names(table_freq_2.df)[2]="Frecuencia"
        output$output_table_impacto_importe_diferencias_dist_2 <- renderTable(table_freq_2.df)
        
        
        shinyjs::alert("¡Simulación realizada!")
        
        ##########################################################################
        
        output$output_valor_clas <- renderText({paste(input$criterio," Valor: ",valor," | Pesos (Grado: ",pesoGrado,", Acidez: ",pesoAcidez,", Potasio: ",pesoPotasio,", Glucónico: ",pesoGluconico,")", sep="")})
        
        
        entradas.df$dif = (entradas.df$importe_sim_kp-entradas.df$importe)/entradas.df$importe
        breakpoints=quantile(entradas.df$dif, c(.2, .4, .6, .8)) 
        breakpoints=c(-Inf, breakpoints, Inf)
        categoriasAfectados=c("MAN","AN","NA","AP","MAP")
        entradas.df$clase=cut(entradas.df$dif,breaks=breakpoints,labels=categoriasAfectados)
        entradas.df$clase = ordered(entradas.df$clase, levels=c("MAN","AN","NA","AP","MAP"))
        names(entradas.df)
        
        #me quedo solo con las columnas pertinentes
        entradasUvaBAParaClasificacion.df=entradas.df[,c(4:8,22,24)]
        str(entradasUvaBAParaClasificacion.df)
        
        #https://rpubs.com/jboscomendoza/arboles_decision_clasificacion
        set.seed(1000)
        entrenamiento.ids <- createDataPartition(entradasUvaBAParaClasificacion.df$clase, p = 0.7, list = F)
        #aleatoriamente selecciona índices de files del array suministrado como primer argumento. De forma inteligente
        entradasUvaBAEntrentamiento.df = entradasUvaBAParaClasificacion.df[entrenamiento.ids,]
        
        #conformamos el dataset de prueba
        entradasUvaBDAPrueba.df <- setdiff(entradasUvaBAParaClasificacion.df, entradasUvaBAEntrentamiento.df)
        
        arbol_model <- rpart(formula = clase ~ ., data = entradasUvaBAEntrentamiento.df,
                             method = "class",
                             control = rpart.control(minsplit = 20, cp = 0.01))
        
        
        prediccion <- predict(arbol_model, newdata = entradasUvaBDAPrueba.df, type = "class")
        
        entradas_prueba_clase_factor=as.factor(entradasUvaBDAPrueba.df[["clase"]])
        
        #calculamos la matriz de confusión
        matriz_confusion <- confusionMatrix(prediccion, entradas_prueba_clase_factor)
        
        paletaColores <- list("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")
        
        output$output_plot_arbol <- renderPlot(prp(arbol_model, type=2, extra=104, nn = TRUE, fallen.leaves = TRUE, 
                                                   faclen=4, varlen=8, shadow.col= "gray", box.palette = paletaColores ))
        
        output$output_text_arbol_exactitud <- renderText({paste("Exactitud clasificación: ",round(as.numeric(matriz_confusion$overall[[1]])*100,2), "%" ) })
        
        output$output_plot_clases <- renderPlot({
            plot <- ggplot(entradas.df, aes(x=clase, y=dif, fill = as.factor(clase)))+
                geom_boxplot() +
                scale_fill_manual(values=c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")) +
                ggtitle("Distribución de las diferencias de precios entre las entradas por clase")+
                xlab("Clase") +
                ylab("Diferencia") +
                labs(fill = "Clase") +
                theme(legend.position = "bottom")+
                theme(plot.title = element_text(hjust = 0.5))
            plot
        })
        
        ##########################################################################
        output$output_valor_clas_plus <- renderText({paste(input$criterio," Valor: ",valor," | Pesos (Grado: ",pesoGrado,", Acidez: ",pesoAcidez,", Potasio: ",pesoPotasio,", Glucónico: ",pesoGluconico,")", sep="")})
        
        output$output_text_arbol_reglas <- renderPrint(arbol_model)
        output$output_text_matriz_confusion <- renderPrint(matriz_confusion)

        
        output$output_table_entradas_dif_mas_pos <- renderTable({
            head(entradas.df[order(entradas.df$dif,decreasing=T),c(2:10,12,18)],3)
        },rownames=FALSE,striped = TRUE,digits = 4)
        
        output$output_table_entradas_dif_mas_neg <- renderTable({
            head(entradas.df[order(entradas.df$dif),c(2:10,12,18)],3)
        },rownames=FALSE,striped = TRUE,digits = 4)
        
        output$output_table_entradas_mas_kp <- renderTable({
            head(entradas.df[order(entradas.df$kilopuntos,decreasing=T),c(2:10,12,18)],3)
        },rownames=FALSE,striped = TRUE,digits = 4)
        
        output$output_table_entradas_menos_kp <- renderTable({
            head(entradas.df[order(entradas.df$kilopuntos),c(2:10,12,18)],3)
        },rownames=FALSE,striped = TRUE,digits = 4)
        
        #########################################################################
        
                
    })
    
    
})
