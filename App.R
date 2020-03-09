rm(list=ls())
library(shiny)
r2_file="./Emisiones Base/R2 ENE.csv"
r2_tot="./Emisiones Base/R2 TOT.csv"
r2_sect="./Emisiones Base/R2 SECT.csv"
gr_table="./Emisiones Base/Datos usados en gráfica.xlsx"
comp_file="./Base de datos/Información Adicional.xlsx"
dir.base="./Emisiones Base"
if (!dir.exists(dir.base)){dir.create(dir.base)}


if (file.exists(r2_file))
{Dat=read.csv(r2_file)
yr_min=min(Dat$ANO,na.rm=T)
yr_max=max(Dat$ANO,na.rm=T)} else
{
  yr_min=2010
  yr_max=2020
}

# Definición de Interface gráfica
ui <- shinyUI(
  navbarPage("Emisiones GEI Sector minero Energético",
             tabPanel(
               "Estimación de emisiones",
               sidebarLayout(
                 sidebarPanel(
                   ## Caja para la selección de la base de datos en excel
                   fluidRow(column(12,fileInput("flx", "Selección de Base de datos",buttonLabel = "Buscar",placeholder="No hay ningun archivo seleccionado",width = 800))),
                   actionButton("doCALC", "Calcular Emisiones GEI",width=200,style="color: #fff; background-color: #337ab7; border-color: #2e6da4",icon("play-circle")),
                   plotOutput("logo1"),
                   h6("Herramienta desarrollada por Edison Y. Ortiz, MSc (2019)")),
                 mainPanel(h2("Instrucciones"),
                           h4("General"),
                           p("Este aplicativo esta basado en código R, con interfaz HTML. Este aplicativo permite estimar las emisiones y su respectiva incertidumbre de emisiones
                              GEI para el Sector Minero-Energético del país, y consta de tres secciones, las cuales pueden verse en las pestañas en la parte superior de esta ventana:"),
                           p(strong("Estimación de emisiones (Esta Pestaña):"),"Lugar en el cual se realizan las estimaciones de las emisiones y se presentan las instrucciones de todo el aplicativo."),
                           p(strong("Gráficas:"),"Pestaña en la cual se pueden obtener distintas gráficas dependiendo de las opciones seleccionadas"),
                           p(strong("Tabla:"), "Pestaña en la cual se encontrarán los datos empleados para hacer la gráfica"),
                           p("Para el correcto funcionamiento de este aplicativo, se requiere que este instalado el lenguaje R, RStudio, y las siguientes librerías:"),
                           p(em("triangle, openxlsx, nleqslv, fitdistrplus, ggplot2, ggalluvial, reshape2, shiny, DT, imager"),"y todas las librerias requeridas por éstas"),
                           h4("Pestaña 'Estimación de emisiones'"),
                           p("1. Cargue la base de datos de actividades y factores de emisión con el boton",strong("Buscar"), ", puede ubicar este archivo en la Carpeta",em("Base de datos")),
                           p("2. Oprima el botón",strong("Calcular Emisiones GEI")),
                           p("3. Verifique las notificaciones en la esquina inferior derecha, si aparece ",span("Rojo",style="color:red")," indica que hay una falla en la base de datos, (no se ha cargado la base de datos o hay un error en la configuración de la base de datos. 
                             Si la notificación es",span("gris",style="color:gray"), "el aplicativo empezó a correr y se encontrará estimando las emisiones lo mas desagregadas posibles para el sector"),
                           p("4. Cuando la notificación sea",span("azul",style="color:lightblue"),"el proceso habrá terminado y se debe haber generado los archivos 'R2 ENE.csv', 'R2 SECT.csv' y 'R2 TOT.csv' en la carpeta",em("Emisiones Base")),
                           h4("Pestaña 'Gráfica'"),
                           p("1. Verifique que el archivo 'R2 ENE.csv exista, si no es así, estime las emisiones con la pestaña anterior"),
                           p("2. Seleccione los sectores que se desean graficar según interes"),
                           p("3. Seleccione las Categorías IPCC que desee tener en cuenta para las gráficas según interes"),
                           p("4. Selecciones el tipo de desagregación de las gráficas, puede seleccionar ambas opciones para ver resultados simultáneos"),
                           p("5. Seleccione el tipo de gráfico que desee"),
                           p("6. Seleccione si desea agregar las barras de error, el total de lo seleccionado y la incertidumbre, éstos valores se presentarán solo
                             en lso gráficos de series temporales."),
                           p("7. Seleccione la paleta de colores que desee usar en la gráfica, por defecto se encontrará marcada la paleta ",em("'Conjunto 1'")),
                           p("8. Seleccione el tamaño de texto dentro de la gráfica, por defecto encontrará marcado el tamaño ",em("'20'")),
                           p("9. Seleccione el mínimo valor que tendrá la escala de emisiones, en MtCO2eq, por defecto aparecerá ",em("'0'"),". El valor de '0' implica una escala automática, un valor de '2' implica una división mínima de 2 MtCO2eq en la gráfica, etc."),
                           p("10. Seleccione el rango de años a graficar"),
                           p("11. Oprima el botón ",strong("Graficar"),"para generar la gráfica deseada"),
                           p("12. Una vez generada la gráfica, puede cambiar el color de la gráfica, el tamaño del texto y el valor mínimo de emisiones de MtCO2eq
                             haciendo nuevamente una selección y oprimiendo los botones azules que se encuentran a la derecha de cada item"),
                           h4("Pestaña 'Tabla'"),
                           p("En esta pestaña aparecen los datos usandos para realizar la gráfica, en esta pestaña se puede filtrar la información, y buscar datos específicos. Los datos usados para graficar son guardados en la carpeta",
                             em("Emisiones Base")," y el archivo se llamará 'Datos usados en gráfica.xlsx'"),
                           h4("Pestaña 'Resumen'"),
                           p("En esta pestaña se grafican por defecto las emisiones GEI de la linea base, las emisiones del escenario de mitigación y las emisiones estimadas por el IDEAM en el BUR"),
                           p("1. Verifique que los archivos 'R2 TOT.csv' y 'R2 SECT.csv', si no es así, estime las emisiones en la pestaña ",strong("Emisiones")),
                           p("2. Oprima el botón", strong("Extraer Resultados")," para cargar los resultados obtenidos de estimación de emisiones de esta herramienta y actualizar las gráficas predeterminadas y generar información adicional,
                             entre ellas la variación anual de los porcentajes de participación de emisiones por los distintos sectores del ministerio, la variación anual de los porcentajes de participación de cada uno de los GEI estimados
                              y una tabla que muestra la evolución de las emisiones percápita del sector minero-energètico del país"))
               )
              ),
             tabPanel("Gráficas",
                      sidebarLayout(
                      sidebarPanel(
                      fluidRow(column(12, 
                                      checkboxGroupInput("checkSectors", 
                                                         "Sectores", 
                                                         choices = list("Sistema Interconectado Nacional" = "Sistema Interconectado Nacional", 
                                                                        "Zonas No Interconectadas" = "Zonas No Interconectadas", 
                                                                        "Petróleo y Gas" = "Petróleo y Gas",
                                                                        "Minería de Carbón y Producción de Coque"="Minería de Carbón y Producción de Coque"
                                                                        ),
                                                         selected = c("Sistema Interconectado Nacional","Zonas No Interconectadas","Petróleo y Gas",
                                                                      "Minería de Carbón y Producción de Coque")))),
                      fluidRow(column(12, 
                                      checkboxGroupInput("checkipcc", 
                                                         "Categorías IPCC", 
                                                         choices = list("1A1a  Producción de electricidad y calor como actividad principal" = "1A1a  Producción de electricidad y calor como actividad principal ", 
                                                                        "1A1b  Refinación de petróleo"="1A1b  Refinación de petróleo", 
                                                                        "1A1c Fabricación de combustibles sólidos y otras industrias energéticas" = "1A1c Fabricación de combustibles sólidos y otras industrias energéticas",
                                                                        "1A3e Otro transporte"="1A3e Otro transporte",
                                                                        "1B1a  Minería carbonífera y manejo del carbón"="1B1a  Minería carbonífera y manejo del carbón",
                                                                        "1B2a  Petróleo"="1B2a  Petróleo",
                                                                        "1B2b  Gas Natural"="1B2b  Gas Natural",
                                                                        "2B8b Etileno"="2B8b Etileno",
                                                                        "3B4a Humedales que permanecen como tales"="3B4a Humedales que permanecen como tales"
                                                         ),
                                                         selected = c("1A1a  Producción de electricidad y calor como actividad principal ",
                                                                      "1A1b  Refinación de petróleo",
                                                                      "1A1c Fabricación de combustibles sólidos y otras industrias energéticas",
                                                                      "1A3e Otro transporte",
                                                                      "1B1a  Minería carbonífera y manejo del carbón",
                                                                      "1B2a  Petróleo",
                                                                      "1B2b  Gas Natural",
                                                                      "2B8b Etileno",
                                                                      "2B8c Dicloruro de etileno y monómero cloruro de vinilo",
                                                                      "3B4a Humedales que permanecen como tales")))),
                      fluidRow(column(4, 
                                      checkboxGroupInput("checkclass", 
                                                         "Desagregación", 
                                                         choices = list("Sector" = "Sector", 
                                                                        "Cat. IPCC" = "IPCC"
                                                         ),
                                                         selected = "Sector")),
                                    column(4, 
                                      
                                      selectInput("checkgraph", 
                                                   "Tipo de Gráfico", 
                                                   choices = list("Serie Temporal" = "Ts", 
                                                                  "Barras" = "bar",
                                                                  "Diagrama Sankey" = "Sankey"
                                                   ),
                                                   selected = "bar"))),
                      fluidRow(column(4, 
                                      checkboxInput("checkunc", "Agregar Barra de Error", value = TRUE)),
                               column(6,
                                      selectInput("checkpalette", 
                                                  "Paleta de Colores", 
                                                  choices = list("Conjunto 1" = "Set1",
                                                                 "Conjunto 2" = "Set2",
                                                                 "Conjunto 3" = "Set3",
                                                                 "Pasteles 1" = "Pastel1",
                                                                 "Pasteles 2" = "Pastel2",
                                                                 "Parejas de Colores"="Paired",
                                                                 "Tonos oscuros"="Dark2",
                                                                 "Tonos acentuados"="Accent",
                                                                 "Tonos espectrales"="Spectral",
                                                                 "Amarillo-Naranja-Rojo" = "YlOrRd", 
                                                                 "Amarillo-Naranja-Café" = "YlOrBr",
                                                                 "Amarillo-Verde-Azul" = "YlGnBu",
                                                                 "Amarillo-Verde"="YlGn",
                                                                 "Rojos"="Reds",
                                                                 "Rojo-Púrpura"="RdPu",
                                                                 "Rojo-Amarillo-Verde"="RdYlGn",
                                                                 "Rojo-Amarillo-Azul"="RdYlBu",
                                                                 "Rojo-Blanco-Gris"="RdGy",
                                                                 "Rojo-Blanco-Azul"="RdBu",
                                                                 "Púrpuras"="Purples",
                                                                 "Púrpura-Rojo"="PuRd",
                                                                 "Púrpura-Azul-Verde" = "PuBuGn",
                                                                 "Púrpura-Azul"="PuBu",
                                                                 "Púrpura-Blanco-Verde"="PRGn",
                                                                 "Rosa-Blanco-Verde"="PiYG",
                                                                 "Café-Blanco-Verde"="BrBG",
                                                                 "Naranja-Rojo"="OrRd",
                                                                 "Naranja-Blanco-Púrpura"="PuOr",
                                                                 "Naranjas"="Oranges",
                                                                 "Grises" ="Greys",
                                                                 "Verdes" ="Greens",
                                                                 "Verde-Azul"="GnBu",
                                                                 "Azul-Púrpura"="BuPu",
                                                                 "Azul-Verde"="BuGn",
                                                                 "Azules"="Blues"
                                                                 
                                                  ),
                                                  selected = "Set1")),
                               column(2,
                                      actionButton("chcol","",style="color: #fff; background-color: #337ab7; border-color: #2e6da4",icon("fill-drip")))),
                      fluidRow(column(4,
                                      checkboxInput("checkTot", "Agregar Total", value = TRUE)),
                               column(6,
                                      numericInput("checktxtsz", "Tamaño de Fuente", value = 20)),
                               column(2,
                                      actionButton("chtxt","",style="color: #fff; background-color: #337ab7; border-color: #2e6da4",icon("text-height")))),
                      fluidRow(column(4, 
                                      checkboxInput("checkbub", "Agregar Incertidumbre", value = TRUE)),
                               column(6,
                                      numericInput("checkmntck", "Mínima marca de emisión", value = 0)),
                               column(2,
                                      actionButton("chtck","",style="color: #fff; background-color: #337ab7; border-color: #2e6da4",icon("ruler")))),
                      sliderInput("sl_year", "Años",step=1,
                                  min = yr_min, max = yr_max, value = c(yr_min, yr_max)),
                      actionButton("doGRAPH", "Graficar",width=200,style="color: #fff; background-color: #337ab7; border-color: #2e6da4",icon("chart-bar"))),
                      mainPanel(plotOutput("gp",height  = "800"),
                                verbatimTextOutput("info1")
                                ))
                      ),
             tabPanel("Tabla",mainPanel(h3("Datos usados en la gráfica"),DT::dataTableOutput("table"))),
             tabPanel("Resumen",mainPanel(h3("Resumen general de emisiones"),
                                          actionButton("doRES","Extraer Resultados",width=200,style="color: #fff; background-color: #337ab7; border-color: #2e6da4",icon("chart-bar")),
                                          textOutput("ResInf")),
                      fluidRow(column(6,plotOutput("res"),plotOutput("resS")),
                               column(6,fluidRow(column(6,plotOutput("per")),
                                                 column(6,plotOutput("per2"))),DT::dataTableOutput("tabler"))))
  )
)

# Definición de la Lógica del servidor
server <- function(input, output) {
  require(triangle)
  require(ggplot2)
  require(DT)
  require(openxlsx)
  require(reshape2)
  require(scales)
  require(imager)
  require(ggalluvial)
  
   
  #Cargando logos
  logo1=load.image("./Scripts/pigccme.png")
  output$logo1 <- renderPlot({ 
    plot(logo1,xaxt = "n",yaxt="n")
    axis(side = 1, col.ticks = "white",col="white")
    axis(side=2,col.ticks = "white",col="white")
    mtext("X",2,at=seq(-9999,9999,1),line=1,col = "white")
    mtext("X",1,at=seq(-9999,9999,1),line=1,col = "white")
    
  })
  if (file.exists("./Scripts/graph.Rdata"))
  {
  file.remove("./Scripts/graph.Rdata")
  }
    observeEvent(input$doCALC,{
    id=showNotification("Estimando Emisiones...",type="default",duration=NULL)
    dir.s="./Scripts" ## Directorio de Scripts
    source("./Scripts/UNC_ENE.R",encoding = "UTF-8")
    #librerias necesarias llamadas por cada función
    #Variables modificables
    flx=input$flx$datapath
    
    if (length(flx)==0){showNotification("Archivo no encontrado",type="error",duration=10);removeNotification(id)}else
    {
      Dat=try(ENE_CALC(file=flx,dir.s=dir.s),silent=T)
      if (class(Dat)=="try-error")
      {showNotification("La base de datos no se encuentra configurada correctamente",type="error",duration=15);removeNotification(id)}else
      {showNotification("Estimación Finalizado",type="message",duration=20);removeNotification(id)
      file.copy("R2 ENE.csv",r2_file,overwrite = T)
      file.remove("R2 ENE.csv")}
      ## Generando archivo de emisiones totales del sector
      DAT=read.csv(r2_file)
      GWP=read.csv(paste0("./Scripts/GWP.csv"))
      
      gwp=GWP[c(4,3)]
      
      Dat1=merge(DAT,gwp,by.x="GAS",by.y="Conv.2",all.x=T)
      Dat1=Dat1[-c(10:13)]
      Dat1$EM=Dat1$EM*Dat1$GWP/1000 ### Paso a Mt
      Dat1=SelDist(Dat1,"EM","UN_EM","UP_EM","_EM")
      Tot=SUMCALC(Dat1,agg=NULL,it=100)
      Tot=Tot$MC
      Tot$SN=Tot$EM*(1-Tot$UN)
      Tot$SP=Tot$EM*(1+Tot$UP)
      Tot2=Dat1[Dat1$Method=="MC",]
      Tot2=aggregate(Tot2["EM"],Tot2[c("ANO","GAS","Sector")],sum,na.rm=T)
      write.csv(Tot,r2_tot,row.names = F)
      write.csv(Tot2,r2_sect,row.names = F)
    }
    
  })
  
  ### Gráfica
  observeEvent(input$doGRAPH,{
    sink("sink-code.txt",type = c("output", "message"))
    id1=showNotification("Agregando Emisiones y Graficando...",type="default",duration=NULL)
    dir.s="./Scripts" ## Directorio de Scripts
    source("./Scripts/plot.emis.r",encoding = "UTF-8")
    DAT=read.csv(r2_file)
    grtype=input$checkgraph
    grcl=input$checkclass
    if(length(grcl)==2){grcl="all"}
    
    if (grtype=="Sankey")
    {grcl="Diagram"}
    
    chS=c(input$checkSectors)
    chC=c(input$checkipcc)
    
    
    
    gr=paste(grtype,grcl)
    UN=input$checkunc
    TOT=input$checkTot
    YR=input$sl_year
    AB=input$checkbub
    pt=input$checkpalette
    txs=input$checktxtsz
    mts=input$checkmntck
    
    # Impresion de variables
    
    print(paste0("Sectores:", chS))
    print(paste0("Categorìas: ", chC))
    print(paste0("Gráfica: ", gr))
    print(paste0("Barras de Error?: ", UN))
    print(paste0("Total?: ", TOT))
    print(paste0("Años: ", YR))
    print(paste0("Incenrtidumbre?: ", AB))
    print(paste0("Tamaño Texto: ", pt))
    print(paste0("Separación Emisiones: ", txs))

    print("Empezando la gráfica")
    A=plot.emis(DAT,
               year = YR,
               SelectGraph=gr,
               addU = UN,
               showT = TOT,
               dir.s = dir.s,
               checkSectors = chS,
               checkipcc = chC,
               addBubble = AB,
               palette=pt,
               txt.size=txs,
               mn.tck=mts)
    
    print("Extrayendo tabla")
    #A=NULL
    #output$test<-renderText(paste0("Seleccion: ",chS,"ss",class(chS)))
    if (is.null(nrow(A))&(class(A)=="NULL"))
    {
      showNotification("Los sectores seleccionados no contienen las categorías IPCC señaladas",type="error",duration=15)
      removeNotification(id1)
    }else if (length(A)==1&class(A)=="logical")
    {
      showNotification("Debe seleccionar al menos un sector y una categoría IPCC",type="error",duration=15)
      removeNotification(id1)
    } else
    {
      tabla=A$data
      if (grtype!="Sankey")
      {
      tabla$UM=(tabla$UN+tabla$UP)/2
      tabla$UN=paste0(round(100*tabla$UN,0),"%")
      tabla$UP=paste0(round(100*tabla$UP,0),"%")
      tabla$EM=round(tabla$EM,2)
      names(tabla)[1:2]=c("Metodo","Año")
      h=which(names(tabla)=="EM")
      names(tabla)[h]="Emisión [Mt CO2eq]"
      h=which(names(tabla)=="NCAT.IPCC")
      names(tabla)[h]="Cat. IPCC"
      h=which(names(tabla)=="UN")
      names(tabla)[h]="Incertidumbre [-%]"
      h=which(names(tabla)=="UP")
      names(tabla)[h]="Incertidumbre [+%]"
      h=which(names(tabla)=="SN")
      if (length(h)!=0)
      {tabla$SN=round(tabla$SN,2)}
      names(tabla)[h]="Límite Inferior [Mt CO2eq]"
      h=which(names(tabla)=="SP")
      if (length(h)!=0)
      {tabla$SP=round(tabla$SP,2)}
      names(tabla)[h]="Límite Superior [Mt CO2eq]"
      h=which(names(tabla)=="UM")
      if (length(h)!=0)
      {tabla$UM=paste0(round(100*tabla$UM,0),"%")}
      names(tabla)[h]="Incertidumbre promedio [%]"
  
      h=which(names(tabla)==c("PFD"))
      tabla=tabla[-h]
      h=which(names(tabla)==c("par_1"))
      tabla=tabla[-h]
      h=which(names(tabla)==c("par_2"))
      tabla=tabla[-h]
      h=which(names(tabla)==c("par_3"))
      tabla=tabla[-h]
      h=which(names(tabla)==c("brksz"))
      tabla=tabla[-h]
      tabla=tabla[-1]
      }else
      {
        tabla$EM=round(tabla$EM,2)
        names(tabla)=c("Categoría IPCC","Sector","Emisión [Mt CO2eq]")
      }
      
      rownames(tabla)=NULL
      write.xlsx(tabla,gr_table)
      tablaDT=datatable(tabla,
                        options=list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                     pageLength = 100,
                                     searchHighlight = FALSE,
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#0071BE', 'color': '#fff'});",
                                       "$(this.api().table().header()).css({'font-size': '100%'});",
                                       "}")),
                        filter = list(position = 'top', clear = FALSE)
                        )
      
      output$gp <- renderPlot({ 
        plot(A)
      },height="auto")
      print("Tabla Finalizada")
      output$table=DT::renderDataTable(tablaDT)
      showNotification("Gráfica Finalizada",type="message",duration=20);removeNotification(id1)
      showNotification("Tabla Finalizada",type="message",duration=20)
      save(list = c("A"), file = "./Scripts/graph.Rdata")
    }
  })
  
  
  
#  txs=input$checktxtsz
#  mts=input$checkmntck
  
  ## Cambiar color de la gráfica
  observeEvent(input$chcol,
               {
                 pt=input$checkpalette
                if (file.exists("./Scripts/graph.Rdata"))
                {
                 load("./Scripts/graph.Rdata")
                 ### Cambiar Color de gráfica
                 A=A+scale_fill_brewer(palette = pt)+scale_color_brewer(palette = pt)
                 save(list = c("A"), file = "./Scripts/graph.Rdata")
                 output$gp <- renderPlot({ 
                   plot(A)
                 },height="auto")
                } else
                {showNotification("Para Volver a Colorear, primero realice la gráfica deseada",type="error",duration=10)}
                 
               })
  
  ## Cambiar Tamaño de letra
  observeEvent(input$chtxt,
               {
                 txs=input$checktxtsz
                 if (file.exists("./Scripts/graph.Rdata"))
                 {
                   load("./Scripts/graph.Rdata")
                   ### Cambiar Color de gráfica
                   A=A+theme(text = element_text(size=txs))
                   save(list = c("A"), file = "./Scripts/graph.Rdata")
                   output$gp <- renderPlot({ 
                     plot(A)
                   },height="auto")
                 } else
                 {showNotification("Para cambiar el tamaño de la fuente, primero realice la gráfica deseada",type="error",duration=10)}
                 
               })
  
  ## Cambiar Tamaño de letra
  observeEvent(input$chtck,
               {
                 mts=input$checkmntck
                 if (file.exists("./Scripts/graph.Rdata"))
                 {
                   load("./Scripts/graph.Rdata")
                   if (mts==0)
                   {A=A+scale_y_continuous(breaks=waiver())}else if (mts>0)
                   {A=A+scale_y_continuous(breaks=seq(0,1e5,mts))}
                   save(list = c("A"), file = "./Scripts/graph.Rdata")
                   output$gp <- renderPlot({ 
                     plot(A)
                   },height="auto")
                 } else
                 {showNotification("Para cambiar las divisiones de emisión, primero realice la gráfica deseada",type="error",duration=10)}
                 
               })  
  
    ## Grafica Base
  Dt2=loadWorkbook(comp_file)
  Pop=readWorkbook(Dt2,sheet=1,cols=c(1:2)) ## Población
  Emis=readWorkbook(Dt2,sheet=2,startRow = 2)
  names(Emis)[1]=c("Año")
  Emis.melt=melt(Emis,id.vars="Año",value.name = "Emision")
  Emis.melt$variable=gsub("."," ",Emis.melt$variable,fixed=T)
  
  
  g=ggplot(Emis.melt,aes(Año,Emision))+geom_line(aes(color=variable),size=1)+
    geom_point(aes(color=variable),size=2)+theme_bw()+
    ylab(expression("Mt"~CO[2]*"eq."))+
    scale_x_continuous(breaks=seq(2010,2030,2),minor_breaks = 2010:2030)+scale_y_continuous(breaks = seq(10,100,5))+
    theme(legend.position = "bottom",legend.title = element_blank(),
          text=element_text(size=20),legend.direction = "vertical")+
    scale_color_brewer(palette = "Set1")
  
  ### Emisiones sectoriales
  EmisS=readWorkbook(Dt2,sheet=3,startRow = 2,colNames = T)
  EmisS.melt=melt(EmisS,id.vars="Año",value.name = "Emision")
  EmisS.melt$variable=gsub("."," ",EmisS.melt$variable,fixed=T)
  EmisS.melt$variable=factor(EmisS.melt$variable)
  
  
    
    cols=t(readWorkbook(Dt2,sheet=3,rows = 1:2,colNames = F))
    cols=data.frame(cols[cols[,2]!="Año",])
    
    cols$X1=as.character(cols$X1)
 
    cols$X2=factor(cols$X2,levels = levels(EmisS.melt$variable))
    
    cols=cols[order(cols$X2),]
    
    
    gs=ggplot(EmisS.melt,aes(Año,Emision))+geom_line(aes(color=variable),size=1)+
      geom_point(aes(color=variable),size=2)+theme_bw()+
      ylab(expression("Mt"~CO[2]*"eq."))+scale_color_manual(name="Sector",breaks=cols$X2,values = cols$X1)+
      theme(legend.position = "bottom",legend.direction = "vertical",text = element_text(size=20))
    

    
    
    
    
  observeEvent(input$doRES,{
    if (file.exists(r2_tot))
    {
      
      Tot=read.csv(r2_tot)
      
      Tot$Año=Tot$ANO
      Tot$Emision=Tot$EM
      Tot$Method="Reporte MinEnergía"
      g=ggplot(Emis.melt,aes(Año,Emision))+geom_ribbon(data=Tot,aes(ymin = SN, ymax = SP),fill="gray50",alpha=0.2)+
        geom_line(data = Tot,aes(ANO,EM,color=Method),size=1)+
        geom_line(aes(color=variable),size=1)+
        geom_point(aes(color=variable),size=2)+theme_bw()+
        ylab(expression("Mt"~CO[2]*"eq."))+
        scale_x_continuous(breaks=seq(2010,2030,2),minor_breaks = 2010:2030)+scale_y_continuous(breaks = seq(10,100,5))+
        theme(legend.position = "bottom",legend.title = element_blank(),legend.direction = "vertical",
              text=element_text(size=20),plot.title = element_text(size=20))+
        scale_color_brewer(palette = "Set1")+
        ggtitle("Comparación de emisiones del sector\ncon respecto a los reportes IDEAM")
      
      Tot2=read.csv(r2_sect)
      Tot2=Tot2[Tot2$Sector!="Producción de Ferroniquel",]
      g1=ggplot(Tot2,aes(ANO,EM))+geom_bar(aes(fill=Sector),position="fill",stat="identity")+
        theme_bw()+scale_fill_brewer(palette = "YlOrRd")+
        scale_y_continuous("Porcentaje",breaks=seq(0,1,0.1),labels = scales::percent)+
        scale_x_continuous("Año",breaks = seq(yr_min,yr_max,1))+
        theme(legend.position = "bottom",legend.direction = "vertical",text=element_text(size=20),
              axis.text.x = element_text(angle = 90),plot.title = element_text(size=15))+
        ggtitle("Participación de los subsectores en las\nemisiones GEI del sector minero-energético")
      
      g2=ggplot(Tot2,aes(ANO,EM))+geom_bar(aes(fill=GAS),position="fill",stat="identity")+
        theme_bw()+scale_fill_brewer("GEI",palette = "Greens")+
        scale_y_continuous("Porcentaje",breaks=seq(0,1,0.1),labels = scales::percent)+
        scale_x_continuous("Año",breaks = seq(yr_min,yr_max,1))+
        theme(legend.position = "bottom",legend.direction = "vertical",text=element_text(size=20),
              axis.text.x = element_text(angle = 90),plot.title = element_text(size=15))+
        ggtitle("Distribución de los GEI en las emisiones\ndel sector minero-energético")
      Tot3=merge(Tot,Pop,by.x="ANO",by.y="Año",all.x=T)
      Tot3$EMpc=(Tot3$EM/Tot3$Poblacion)*1e6
      Tot3=Tot3[c("ANO","EM","Poblacion","EMpc")]
      
      
      output$per <- renderPlot({ 
        plot(g1)
      },height="auto")
      output$per2 <- renderPlot({ 
        plot(g2)
      },height="auto")
      
      tablar=Tot3
      tablar$EM=round(tablar$EM,3)
      tablar$Poblacion=round(tablar$Poblacion,0)
      tablar$EMpc=round(tablar$EMpc,3)
      
      names(tablar)=c("Año","Emisión [Mt CO2eq]","Población Nacional","Emisión per cápita [t CO2eq/hab]")
      rownames(tablar)=NULL
      tablaDTR=datatable(tablar,
                         options=list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                      pageLength = 100,
                                      searchHighlight = FALSE,
                                      initComplete = JS(
                                        "function(settings, json) {",
                                        "$(this.api().table().header()).css({'background-color': '#0071BE', 'color': '#fff'});",
                                        "$(this.api().table().header()).css({'font-size': '100%'});",
                                        "}")),
                         filter = list(position = 'top', clear = FALSE)
      )
      
      output$tabler=DT::renderDataTable(tablaDTR)
      output$res <- renderPlot({ 
        plot(g)
      },height="auto")
      

      
      ### Agregando a Grafica Sectorial
      
      cols.a=data.frame(X1="black",X2="Reporte MinEnergía")
      cols=rbind(cols,cols.a)
      
      Tots=Tot[c("ANO","Method","EM")]
      names(Tots)=names(EmisS.melt)
      Emis2=rbind(EmisS.melt,Tots)
      
      gs=ggplot(Emis2,aes(Año,Emision))+geom_line(aes(color=variable),size=1)+
        geom_point(aes(color=variable),size=2)+theme_bw()+
        ylab(expression("Mt"~CO[2]*"eq."))+scale_color_manual(name="Sector",breaks=cols$X2,values = cols$X1)+
        theme(legend.position = "bottom",legend.direction = "vertical",text = element_text(size=20))+
        geom_ribbon(data=Tot,aes(ymin = SN, ymax = SP),fill="gray50",alpha=0.2)

      output$resS <- renderPlot({ 
        plot(gs)
      },height=700)

    } 
  })
  output$res <- renderPlot({ 
    plot(g)
  },height="auto")

  output$resS <- renderPlot({ 
    plot(gs)
  },height=700)
  
  
}

shinyApp(ui = ui, server = server)