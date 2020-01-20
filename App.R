rm(list=ls())
library(shiny)
r2_file="./Emisiones Base/R2 ENE.csv"
r2_tot="./Emisiones Base/R2 TOT.csv"
r2_sect="./Emisiones Base/R2 SECT.csv"
comp_file="./Base de Datos/Información Adicional.xlsx"
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
                   actionButton("doCALC", "Calcular Emisiones GEI",width=200,style="color: #fff; background-color: #337ab7; border-color: #2e6da4",icon("play-circle"))
                 ),
                 mainPanel(h2("Instrucciones"),
                           h4("General"),
                           p("Este aplicativo esta basado en código R, con interfaz HTML. Este aplicativo permite estimar las emisiones y su respectiva incertidumbre de emisiones
                              GEI para el Sector Minero-Energético del país, y consta de tres secciones, las cuales pueden verse en las pestañas en la parte superior de esta ventana:"),
                           p(strong("Estimación de emisiones (Esta Pestaña):"),"Lugar en el cual se realizan las estimaciones de las emisiones y se presentan las instrucciones de todo el aplicativo."),
                           p(strong("Gráficas:"),"Pestaña en la cual se pueden obtener distintas gráficas dependiendo de las opciones seleccionadas"),
                           p(strong("Tabla:"), "Pestaña en la cual se encontrarán los datos empleados para hacer la gráfica"),
                           p("Para el correcto funcionamiento de este aplicativo, se requiere que este instalado el lenguaje R, RStudio, y las siguientes librerías:"),
                           p(em("triangle, openxlsx, nleqslv, fitdistrplus, ggplot2, reshape2, shiny"),"y todas las librerias requeridas por éstas"),
                           h4("Pestaña 'Estimación de emisiones'"),
                           p("1. Cargue la base de datos de actividades y factores de emisión con el boton",strong("Buscar"), ", puede ubicar este archivo en la Carpeta",em("Base de datos")),
                           p("2. Oprima el botón",strong("Calcular Emisiones GEI")),
                           p("3. Verifique las notificaciones en la esquina inferior derecha, si aparece ",span("Rojo",style="color:red")," indica que hay una falla en la base de datos, (no se ha cargado la base de datos o hay un error en la configuración de la base de datos. 
                             Si la notificación es",span("gris",style="color:gray"), "el aplicativo empezó a correr y se encontrará estimando las emisiones lo mas desagregadas posibles para el sector"),
                           p("4. Cuando la notificación sea",span("azul",style="color:lightblue"),"el proceso habrá terminado y se debe haber generado el archivo 'R2 ENE.csv' en la carpeta",em("Emisiones Base")),
                           h4("Pestaña 'Gráfica'"),
                           p("1. Verifique que el archivo 'R2 ENE.csv exista, si no es así, estime las emisiones con la pestaña anterior"),
                           p("2. Seleccione los sectores que se desean graficar según interes"),
                           p("3. Seleccione las Categorías IPCC que desee tener en cuenta para las gráficas según interes"),
                           p("4. Selecciones el tipo de desagregación de las gráficas, puede seleccionar ambas opciones para ver resultados simultáneos"),
                           p("5. Seleccione el tipo de gráfico que desee"),
                           p("6. Seleccione si desea agregar las barras de error, el total de lo seleccionado y la incertidumbre"),
                           p("7. Seleccione el rango de años a graficar"),
                           p("8. Oprima el botón ",strong("Graficar"),"para generar la gráfica deseada"),
                           h4("Pestaña 'Tabla'"),
                           p("En esta pestaña aparecen los datos usandos para realizar la gráfica, en esta pestaña se puede filtrar la información, y buscar datos específicos"))
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
                      fluidRow(column(6, 
                                      checkboxGroupInput("checkclass", 
                                                         "Desagregación", 
                                                         choices = list("Sector" = "Sector", 
                                                                        "Cat. IPCC" = "IPCC"
                                                         ),
                                                         selected = "Sector")),
                                    column(6, 
                                      
                                      radioButtons("checkgraph", 
                                                   "Tipo de Gráfico", 
                                                   choices = list("Serie Temporal" = "Ts", 
                                                                  "Barras" = "bar"
                                                   ),
                                                   selected = "bar"))),
                      fluidRow(column(6, 
                                      checkboxInput("checkunc", "Agregar Barra de Error", value = TRUE)),
                               column(6,checkboxInput("checkTot", "Agregar Total", value = TRUE))),
                      fluidRow(column(6, 
                                      checkboxInput("checkbub", "Agregar Incertidumbre", value = TRUE))),
                      sliderInput("sl_year", "Años",step=1,
                                  min = yr_min, max = yr_max, value = c(yr_min, yr_max)),
                      actionButton("doGRAPH", "Graficar",width=200,style="color: #fff; background-color: #337ab7; border-color: #2e6da4",icon("chart-bar"))),
                      mainPanel(plotOutput("gp",height  = "800"),
                                verbatimTextOutput("info1")))
                      ),
             tabPanel("Tabla",mainPanel(h3("Datos usados en la gráfica"),dataTableOutput("table"))),
             tabPanel("Resumen",mainPanel(h3("Resumen general de emisiones"),textOutput("ResInf")),
                      fluidRow(column(6,plotOutput("res"),fluidRow(column(6,plotOutput("per")),
                                                                   column(6,plotOutput("per2")))),
                               column(6,dataTableOutput("tabler"))))
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
    id1=showNotification("Agregando Emisiones y Graficando...",type="default",duration=NULL)
    dir.s="./Scripts" ## Directorio de Scripts
    source("./Scripts/plot.emis.r",encoding = "UTF-8")
    DAT=read.csv(r2_file)
    grtype=input$checkgraph
    grcl=input$checkclass
    if(length(grcl)==2){grcl="all"}
    
    chS=c(input$checkSectors)
    chC=c(input$checkipcc)
    
    gr=paste(grtype,grcl)
    UN=input$checkunc
    TOT=input$checkTot
    YR=input$sl_year
    AB=input$checkbub
    A=plot.emis(DAT,
                year = YR,
                SelectGraph=gr,
                addU = UN,
                showT = TOT,
                dir.s = dir.s,
                checkSectors = chS,
                checkipcc = chC,
                addBubble = AB)
    
    tabla=A$data
    tabla$UN=paste0(round(100*tabla$UN,0),"%")
    tabla$UP=paste0(round(100*tabla$UP,0),"%")
    tabla$EM=round(tabla$EM,3)
    
    
    
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
    {tabla$SN=round(tabla$SN,3)}
    names(tabla)[h]="Límite Inferior [Mt CO2eq]"
    h=which(names(tabla)=="SP")
    if (length(h)!=0)
    {tabla$SP=round(tabla$SP,3)}
    names(tabla)[h]="Límite Superior [Mt CO2eq]"
    h=which(names(tabla)=="UM")
    if (length(h)!=0)
    {tabla$UM=paste0(round(tabla$UM,0),"%")}
    names(tabla)[h]="Incertidumbre promedio [%]"

    
    
    tabla=tabla[-1]
    rownames(tabla)=NULL
    tablaDT=datatable(tabla,
                      options=list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                   pageLength = 100,
                                   searchHighlight = FALSE,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#0071BE', 'color': '#fff'});",
                                     "$(this.api().table().header()).css({'font-size': '150%'});",
                                     "}")),
                      filter = list(position = 'top', clear = FALSE)
                      )
    
    output$gp <- renderPlot({ 
      plot(A)
    },height="auto")
    
    showNotification("Gráfica Finalizada",type="message",duration=20);removeNotification(id1)
    output$table=DT::renderDataTable(tablaDT)
    
  })
  
  ### Resumen de emisiones
  if (file.exists(r2_tot))
  {
    output$ResInf=renderText("El archivo de Emisiones R2 Existe")
  } else
  {
    output$ResInf=renderText('El archivo de Emisiones R2 no existe, cree dicho archivo en la pestaña "Emisiones"')
  }
  
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
          text=element_text(size=20))+
    scale_color_brewer(palette = "Set1")
  
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
            text=element_text(size=20))+
      scale_color_brewer(palette = "Set1")
  
  Tot2=read.csv(r2_sect)
  Tot2=Tot2[Tot2$Sector!="Producción de Ferroniquel",]
  g1=ggplot(Tot2,aes(ANO,EM))+geom_bar(aes(fill=Sector),position="fill",stat="identity")+
     theme_bw()+scale_fill_brewer(palette = "YlOrRd")+
     scale_y_continuous("Porcentaje",breaks=seq(0,1,0.1),labels = scales::percent)+
     scale_x_continuous("Año",breaks = seq(yr_min,yr_max,1))+theme(legend.position = "bottom",legend.direction = "vertical")
  
  g2=ggplot(Tot2,aes(ANO,EM))+geom_bar(aes(fill=GAS),position="fill",stat="identity")+
    theme_bw()+scale_fill_brewer(palette = "Greens")+
    scale_y_continuous("Porcentaje",breaks=seq(0,1,0.1),labels = scales::percent)+
    scale_x_continuous("Año",breaks = seq(yr_min,yr_max,1))+theme(legend.position = "bottom",legend.direction = "vertical")
    
  }
  
  Tot3=merge(Tot,Pop,by.x="ANO",by.y="Año",all.x=T)
  Tot3$EMpc=(Tot3$EM/Tot3$Poblacion)*1e6
  Tot3=Tot3[c("ANO","EM","Poblacion","EMpc")]
  
  output$res <- renderPlot({ 
    plot(g)
  },height="auto")
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
  
  
  
  
}

shinyApp(ui = ui, server = server)