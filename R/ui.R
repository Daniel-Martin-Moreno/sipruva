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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    tags$head(tags$script(src = "message-handler.js")),
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;} h1 {text-align:center; border-style: solid;  border-color: #726f6f; #output_text_arbol_reglas {font-size:60%}; #output_text_matriz_confusion {font-size: 8px}"))
    ),
    pageWithSidebar(
        headerPanel("SIPRUVA - Cuadro de Mando"),
        sidebarPanel(
            h4("Pesos de parámetros"),
            textInput("grado", "Grado:", "45"),
            textInput("acidez", "Acidez:", "15"),
            textInput("potasio", "Potasio:", "15"), 
            textInput("gluconico", "Glucónico:", "25"),
            p("Seleccionamos el fichero"),
                fileInput("file1", "Elija listado de entradas", accept = ".csv"),
            hr(),  
            selectInput("criterio", "Seleccione criterio de pago",
                    choices = c("Precio mínimo", "Porcentaje fijo sobre el total")),
            textInput("valor", "Valor:", ""),
            actionButton("button_simular", "Simular"),
            width = 3
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Resumen", id="tabs",
                  p("Resumen de estadísticos:"), 
                  textOutput("output_filename"),
                  p(""),
                  verbatimTextOutput("output_summary"), #verbatim significa pintar con la forma clÃ¡sica
                  tags$head(tags$style("#output_summary{font-size:80%;}")),
                  p(strong("Estadísticos de kilopuntos")),
                  textOutput("output_kilopuntos_suma"),
                  verbatimTextOutput("output_summary_kilopuntos")
                ),
                tabPanel("Visualizaciones", 
                         p("Gráficos de distribución de fecha, parámetros de uva, y de kilopuntos en base a los pesos presentados"), 
                         textOutput("output_filename_g"),
                         p(""),
                         plotOutput("output_plot_hist_fechas"),
                         fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("output_plot_hist_potasio"), plotOutput("output_plot_hist_gluconico"))
                         ),
                         fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("output_plot_hist_grado"), plotOutput("output_plot_hist_acidez"))
                         ),
                         fluidRow(
                             splitLayout(cellWidths = c("25%", "50%", "25%"), textOutput(""), plotOutput("output_plot_hist_kilopuntos"), textOutput(""))
                         )
                ),
                tabPanel("Precio simulado", 
                         textOutput("output_valor"),
                         p(""),
                         p("Comparamos el importe real y el precio simulado a partir de los parámetros pasados"), 
                         fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), div(verbatimTextOutput("output_comparacion_estadisticos_precios"), style="padding: 25px"), plotOutput("output_plot_valor_kp"))
                         ),
                         fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("output_plot_hist_importe_real"), plotOutput("output_plot_hist_importe_simulado"))
                         ),
                         p(strong("Relaciones entre parámetros con el precio simulado:")),
                         #fluidRow(splitLayout(cellWidths = c("35%", "65%"), plotOutput("output_plot_corr_importe_simulado"),div(tableOutput("output_table_corr_importe_simulado"), style = "font-size:80%; padding: 40px 0;"))                         ),
                         div(tableOutput("output_table_corr_importe_simulado"), style = "font-size:90%"),
                         p("Matriz de correlación:"),
                         plotOutput("output_plot_corr_importe_simulado"),
                         p(strong("Relaciones de parámetros con el precio real:")),
                         div(tableOutput("output_table_corr_importe_real"), style = "font-size:90%"),
                         p("Matriz de correlación:"),
                         plotOutput("output_plot_corr_importe_real")
                ),
                tabPanel("Impacto en socios", 
                         textOutput("output_valor_imp"),
                         p(""),
                         fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("output_plot_impacto_importe_real"), plotOutput("output_plot_impacto_importe_simulado"))
                         ),
                         p(strong("Distribución de las diferencias de importe real e importe simulado entre los socios:")),
                         fluidRow(
                             splitLayout(cellWidths = c("50%", "25%", "25%"), plotOutput("output_plot_impacto_importe_diferencias"), tableOutput("output_table_impacto_importe_diferencias_dist_1"), tableOutput("output_table_impacto_importe_diferencias_dist_2"))
                         )
                ),
                tabPanel("Árbol de clasificación", 
                         textOutput("output_valor_clas"),
                         p(""),
                         p(strong("Clasificación de las entradas de uva en base a la diferencia: precio simulado - precio real")),
                         div(p("MAN: Muy Afectado Negativamente - AN: Afectado Negativamente - NA: No Afectado - AP: Afectado Positivamente - MAP: Afectado Positivamente"), style = "font-size:80%"),
                         plotOutput("output_plot_arbol"),
                         textOutput("output_text_arbol_exactitud"),
                         p(""),
                         p(strong("Clases de entradas por rango de la diferencia")),
                         plotOutput("output_plot_clases")
                ),
                tabPanel("En detalle (*)", 
                         textOutput("output_valor_clas_plus"),
                         p(""),
                         p("Detalle de las reglas de clasificación:"),
                         verbatimTextOutput("output_text_arbol_reglas"),
                         p(""),
                         p("Matriz de confusión:"),
                         verbatimTextOutput("output_text_matriz_confusion"),
                         p("Entradas con diferencia de precios más positiva"),
                         div(tableOutput("output_table_entradas_dif_mas_pos"), style = "font-size:80%"),
                         p("Entradas con diferencia de precios más negativa"),
                         div(tableOutput("output_table_entradas_dif_mas_neg"), style = "font-size:80%"),
                         p("Entradas con mas kilopuntos"),
                         div(tableOutput("output_table_entradas_mas_kp"), style = "font-size:80%"),
                         p("Entradas con menos kilopuntos"),
                         div(tableOutput("output_table_entradas_menos_kp"), style = "font-size:80%")
                ),
                tabPanel("Ayuda",
                         p("SIPRUVA es el Trabajo Fin de Máster de Daniel Martín-Moreno Romero"),
                         br(),
                         div(p("Máster de Análisis y Visualización de Datos Masivos"), style="text-align:center"),
                         div(p("UNIR - Universidad Internacional de La Rioja"), style="text-align:center"),
                         div(p("Edición Febrero 2020-Febrero 2021"), style="text-align:center"),
                         br(),
                         br(),
                         p("************************************************************************"),
                         p("Copyright 2021 Daniel Martín-Moreno Romero"),
                         br(),
                         p("Licensed under the Apache License, Version 2.0 (the \"License\") you may not use this file except in compliance with the License."),
                         p("You may obtain a copy of the License at"),
                         p("    http://www.apache.org/licenses/LICENSE-2.0"),
                         p("Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."),
                         p("See the License for the specific language governing permissions and limitations under the License."),
                         p("************************************************************************"),
                         br(),
                         p(strong("Formato del fichero de entrada")),
                         p("Se requiere un fichero de entrada con los siguientes atributos: fecha, idSocio, neto, grado, acidez, potasio, gluconico, importe (más un atributo inicial numérico que dentro de la metodología SIPRUVA viene a significar el número de fila en el Listado CSV inicial resultado de transformar los ficheros Excel)"),
                         br(),
                         p("A continuación explicamos el contenido de las distintas pestañas del Cuadro de Mando."),
                         br(),
                         p(strong("Resumen")),
                         p("Se muestran 3 tipos de datos: estadísticos básicos de las variables pasadas en el fichero de entrada, y estadísticos de kilopuntos."),
                         br(),
                         p(strong("Visualizaciones")),
                         p("Gráficos de distribución de fechas, de parámetros de calidad de la uva y de kilopuntos."),
                         br(),
                         p(strong("Precio simulado")),
                         p("Comparativa de estadísticos básicos de kilopuntos, gráfica del valor simulado del kilopunto, distribuciones de importes simulados y estudio de correlación de variables."),
                         br(),
                         p(strong("Impacto en socios")),
                         p("Información de cómo sería el impacto en el pago a percibir por los socios con respecto a la simulación."),
                         br(),
                         p(strong("Árbol de clasificación")),
                         p("Aplicación del algoritmo CART de clasificación y distribución de las entradas entre las clases:"),
                         br(),
                         p(strong("En detalle (*)")),
                         p("Presentación de información avanzada de tipo más técnico: detalle matriz de confusión, etc.:"),
                         br()
                )
            )
        )
    )
))
