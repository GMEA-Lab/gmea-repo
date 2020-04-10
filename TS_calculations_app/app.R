library(shiny)
ui<-fluidPage(
  
  fluidRow(
    column(12,tags$p("This is a contribution to the",tags$a(href="http://www.mare-incognitum.no/index.php/arcticabc","ArcticABC", target="_blank"),"research project."))
    ),

  fluidRow(
    column(12,tags$h3("Forward method for TS calculations: enter the values based on your net samples to obtain the theoretical mean Target Strength"))
  ),
  
  fluidRow(
    column(1,offset=1,tags$h5("Copepods")),
    column(1,tags$h5("Meganyctiphanes norvegica")),
    column(1,tags$h5("Thysanoessa raschii")),
    column(1,tags$h5("Thysanoessa inermis")),
    column(1,tags$h5("Amphipods")),
    column(1,tags$h5("Decapods")),
    column(1,tags$h5("Chaetognaths")),
    column(1,tags$h5("Polychaetes")),
    column(1,tags$h5("Limacina spp.")),
    column(1,tags$h5("Clione limacina")),
    column(1,tags$h5("Medusae"))
  ),
  
  
  fluidRow(
    column(1,tags$h5("Relative abundance, from 0 to 1")),
    column(1,numericInput(inputId="cop_abun",value=0,step=0.01, min=0, max=1, label="")),
    column(1,numericInput(inputId="Mnorvegica_abun",value=0,step=0.01, min=0, max=1, label="")),
    column(1,numericInput(inputId="Traschii_abun",value=0,step=0.01, min=0, max=1, label="")),
    column(1,numericInput(inputId="Tinermis_abun",value=0,step=0.01,min=0, max=1, label="")),
    column(1,numericInput(inputId="amphipods_abun",value=0,step=0.01,min=0, max=1, label="")),
    column(1,numericInput(inputId="decapods_abun",value=0,step=0.01,min=0, max=1, label="")),
    column(1,numericInput(inputId="chae_abun",value=0,step=0.01,min=0, max=1, label="")),
    column(1,numericInput(inputId="poly_abun", value=0,step=0.01,min=0, max=1,label="")),
    column(1,numericInput(inputId="Limacina_abun",value=0,step=0.01,min=0, max=1, label="")),
    column(1,numericInput(inputId="clione_abun",value=0,step=0.01,min=0, max=1, label="")),
    column(1,numericInput(inputId="jelly_abun",value=0,step=0.01,min=0, max=1, label=""))
  ),
  
  fluidRow(
    column(1,tags$h5("Average length (mm)")),
    column(1,numericInput(inputId="cop_length",value=1,step=0.1,min=0,max=500,label="")),
    column(1,numericInput(inputId="Mnorvegica_length",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="Traschii_length",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="Tinermis_length",value=1,step=0.1, min=0,max=500,label="")),
    column(1,numericInput(inputId="amphipods_length",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="decapods_length",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="chae_length", value=1,step=0.1,min=0,max=500,label="")),
    column(1,numericInput(inputId="poly_length",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="Limacina_length",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="clione_length",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="jelly_length",value=1,step=0.1,min=0,max=500, label=""))
  ),
  
  
  fluidRow(
    column(1,tags$h5("Standard deviation in length (mm)")),
    column(1,numericInput(inputId="cop_SD",value=0,step=0.01,min=0,max=500, label="")),
    column(1,numericInput(inputId="Mnorvegica_SD",value=0,step=0.01,min=0,max=500, label="")),
    column(1,numericInput(inputId="Traschii_SD",value=0,step=0.01,min=0,max=500, label="")),
    column(1,numericInput(inputId="Tinermis_SD",value=0,step=0.01,min=0,max=500, label="")),
    column(1,numericInput(inputId="amphipods_SD",value=0,step=0.01,min=0,max=500, label="")),
    column(1,numericInput(inputId="decapods_SD",value=0,step=0.01,min=0,max=500, label="")),
    column(1,numericInput(inputId="chae_SD",value=0,step=0.01, min=0,max=500,label="")),
    column(1,numericInput(inputId="poly_SD",value=0,step=0.01,min=0,max=500,label="")),
    column(1,numericInput(inputId="Limacina_SD",value=0,step=0.01,min=0,max=500,label="")),
    column(1,numericInput(inputId="clione_SD",value=0,step=0.01,min=0,max=500, label="")),
    column(1,numericInput(inputId="jelly_SD",value=0,step=0.01,min=0,max=500, label=""))
  ),
  
  fluidRow(
    column(1,tags$h5("Average width (mm)")),
    column(1,numericInput(inputId="cop_width", value=1,step=0.1,min=0,max=500,label="")),
    column(1,numericInput(inputId="Mnorvegica_width",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="Traschii_width",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="Tinermis_width", value=1,step=0.1,min=0,max=500,label="")),
    column(1,numericInput(inputId="amphipods_width",value=1,step=0.1, min=0,max=500,label="")),
    column(1,numericInput(inputId="decapods_width",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="chae_width",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="poly_width",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="Limacina_width",value=1,step=0.1,min=0,max=500, label="")),
    column(1,numericInput(inputId="clione_width",value=1,step=0.1,min=0,max=500,label="")),
    column(1,numericInput(inputId="jelly_width",value=1, step=0.1,min=0,max=500,label=""))
  ),
  
  fluidRow(
    column(1,offset=1,numericInput(inputId="freq",value=200, min=0, max=1500, label="Frequency (kHz)")),
    column(1,numericInput(inputId="soundspeed", value=1500,label="Sound speed (m s^-1)"))
  ),
  
  fluidRow(
    column(1,offset=1,actionButton(inputId="run",label="Run")),
    column(1,downloadButton('downloadPlot',label="Download plot"))
  ),
  
  fluidRow(
    column(8,offset=2,plotOutput("charts")),
    column(2,textOutput("averageTS"))
  )
)


server<-function(input, output) {
  
  piechart<- eventReactive(input$run,{
    
    cop_abun<-as.numeric(input$cop_abun)
    cop_length<-as.numeric(input$cop_length)/1000
    cop_SD<-as.numeric(input$cop_SD)/1000
    cop_width<-as.numeric(input$cop_width)/1000
    Mnorvegica_abun<-as.numeric(input$Mnorvegica_abun)
    Mnorvegica_length<-as.numeric(input$Mnorvegica_length)/1000
    Mnorvegica_SD<-as.numeric(input$Mnorvegica_SD)/1000
    Mnorvegica_width<-as.numeric(input$Mnorvegica_width)/1000
    Traschii_abun<-as.numeric(input$Traschii_abun)
    Traschii_length<-as.numeric(input$Traschii_length)/1000
    Traschii_SD<-as.numeric(input$Traschii_SD)/1000
    Traschii_width<-as.numeric(input$Traschii_width)/1000
    Tinermis_abun<-as.numeric(input$Tinermis_abun)
    Tinermis_length<-as.numeric(input$Tinermis_length)/1000
    Tinermis_SD<-as.numeric(input$Tinermis_SD)/1000
    Tinermis_width<-as.numeric(input$Tinermis_width)/1000
    amphipods_abun<-as.numeric(input$amphipods_abun)
    amphipods_length<-as.numeric(input$amphipods_length)/1000
    amphipods_SD<-as.numeric(input$amphipods_SD)/1000
    amphipods_width<-as.numeric(input$amphipods_width)/1000
    decapods_abun<-as.numeric(input$decapods_abun)
    decapods_length<-as.numeric(input$decapods_length)/1000
    decapods_SD<-as.numeric(input$decapods_SD)/1000
    decapods_width<-as.numeric(input$decapods_width)/1000
    chae_abun<-as.numeric(input$chae_abun)
    chae_length<-as.numeric(input$chae_length)/1000
    chae_SD<-as.numeric(input$chae_SD)/1000
    chae_width<-as.numeric(input$chae_width)/1000
    poly_abun<-as.numeric(input$poly_abun)
    poly_length<-as.numeric(input$poly_length)/1000
    poly_SD<-as.numeric(input$poly_SD)/1000
    poly_width<-as.numeric(input$poly_width)/1000
    Limacina_abun<-as.numeric(input$Limacina_abun)
    Limacina_length<-as.numeric(input$Limacina_length)/1000
    Limacina_SD<-as.numeric(input$Limacina_SD)/1000
    Limacina_width<-as.numeric(input$Limacina_width)/1000
    clione_abun<-as.numeric(input$clione_abun)
    clione_length<-as.numeric(input$clione_length)/1000
    clione_SD<-as.numeric(input$clione_SD)/1000
    clione_width<-as.numeric(input$clione_width)/1000
    jelly_abun<-as.numeric(input$jelly_abun)
    jelly_length<-as.numeric(input$jelly_length)/1000
    jelly_SD<-as.numeric(input$jelly_SD)/1000
    jelly_width<-as.numeric(input$jelly_width)/1000
    freq<-as.numeric(input$freq)*1000
    soundspeed<-as.numeric(input$soundspeed)
    
    if (cop_abun>0) {
      R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=15/1000 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=cop_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=cop_SD # Standard deviation in length
      TS_cop<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_cop<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (Mnorvegica_abun>0) {
      R=0.047 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=Mnorvegica_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=Mnorvegica_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=Mnorvegica_SD # Standard deviation in length
      TS_Mnorvegica<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.047 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_Mnorvegica<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (Traschii_abun>0) {
      R=0.044 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=Traschii_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=Traschii_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=Traschii_SD # Standard deviation in length
      TS_Traschii<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.044 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_Traschii<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (Tinermis_abun>0) {
      R=0.041 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=Tinermis_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=Tinermis_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=Tinermis_SD # Standard deviation in length
      TS_Tinermis<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.041 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_Tinermis<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (amphipods_abun>0) {
      R=0.056 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=amphipods_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=amphipods_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=amphipods_SD # Standard deviation in length
      TS_amphipods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.056 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_amphipods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (decapods_abun>0) {
      R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=decapods_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=decapods_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=decapods_SD # Standard deviation in length
      TS_decapods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_decapods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (chae_abun>0) {
      R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=chae_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=chae_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=chae_SD # Standard deviation in length
      TS_chae<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_chae<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (poly_abun>0) {
      R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=poly_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=poly_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=poly_SD # Standard deviation in length
      TS_poly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_poly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (Limacina_abun>0) {
      R=0.50 # Reflection coefficient. See TS_variables_description.xlsx for details
      f=freq # Frequency in Hz
      D=Limacina_width # Mean body width from net samples (m)
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      TS_Limacina<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
    }
    else {
      R=0.50 # Reflection coefficient. See TS_variables_description.xlsx for details
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      TS_Limacina<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
    }
    
    if (clione_abun>0) {
      R=0.012 # Reflection coefficient. See TS_variables_description.xlsx for details
      f=freq # Frequency in Hz
      D=clione_width # Mean body width from net samples (m)
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      TS_clione<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
    }
    else {
      R=0.012 # Reflection coefficient. See TS_variables_description.xlsx for details
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      TS_clione<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
    }
    
    if (jelly_abun>0) {
      R=0.02 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=jelly_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=jelly_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=jelly_SD # Standard deviation in length
      TS_jelly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.02 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_jelly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    meanTS<-10*log10((cop_abun*10^(TS_cop/10))+(Mnorvegica_abun*10^(TS_Mnorvegica/10))+(Traschii_abun*10^(TS_Traschii/10))+(Tinermis_abun*10^(TS_Tinermis/10))+(amphipods_abun*10^(TS_amphipods/10))+(decapods_abun*10^(TS_decapods/10))+(chae_abun*10^(TS_chae/10))+(poly_abun*10^(TS_poly/10))+(Limacina_abun*10^(TS_Limacina/10))+(clione_abun*10^(TS_clione/10))+(jelly_abun*10^(TS_jelly/10)))
    
    # Pie Chart with Percentages
    slices <- c((cop_abun*TS_cop)/meanTS,(Mnorvegica_abun*TS_Mnorvegica)/meanTS,(Traschii_abun*TS_Traschii)/meanTS,(Tinermis_abun*TS_Tinermis)/meanTS,(amphipods_abun*TS_amphipods)/meanTS,(decapods_abun*TS_decapods)/meanTS,(chae_abun*TS_chae)/meanTS,(poly_abun*TS_poly)/meanTS,(Limacina_abun*TS_Limacina)/meanTS,(clione_abun*TS_clione)/meanTS,(jelly_abun*TS_jelly)/meanTS)
    lbls <- c("copepods","Meganyctiphanes norvegica","Thysanoessa raschii","Thysanoessa inermis","Amphipods","Decapods","Chaetognaths","Polychaetes","Limacina spp.","Clione limacina","Medusae")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    pie(slices,labels=NA,  col=c("blue","chartreuse","cyan","burlywood4","white","darkgrey","magenta","orange","red","black","pink1"),main="Contribution of each zooplankton group to the mean Target Strength")
    legend("topright",lbls, cex=1, fill=c("blue","chartreuse","cyan","burlywood4","white","darkgrey","magenta","orange","red","black","pink1"))
    
  })
  
  TS<- eventReactive(input$run,{
    
    cop_abun<-as.numeric(input$cop_abun)
    cop_length<-as.numeric(input$cop_length)/1000
    cop_SD<-as.numeric(input$cop_SD)/1000
    cop_width<-as.numeric(input$cop_width)/1000
    Mnorvegica_abun<-as.numeric(input$Mnorvegica_abun)
    Mnorvegica_length<-as.numeric(input$Mnorvegica_length)/1000
    Mnorvegica_SD<-as.numeric(input$Mnorvegica_SD)/1000
    Mnorvegica_width<-as.numeric(input$Mnorvegica_width)/1000
    Traschii_abun<-as.numeric(input$Traschii_abun)
    Traschii_length<-as.numeric(input$Traschii_length)/1000
    Traschii_SD<-as.numeric(input$Traschii_SD)/1000
    Traschii_width<-as.numeric(input$Traschii_width)/1000
    Tinermis_abun<-as.numeric(input$Tinermis_abun)
    Tinermis_length<-as.numeric(input$Tinermis_length)/1000
    Tinermis_SD<-as.numeric(input$Tinermis_SD)/1000
    Tinermis_width<-as.numeric(input$Tinermis_width)/1000
    amphipods_abun<-as.numeric(input$amphipods_abun)
    amphipods_length<-as.numeric(input$amphipods_length)/1000
    amphipods_SD<-as.numeric(input$amphipods_SD)/1000
    amphipods_width<-as.numeric(input$amphipods_width)/1000
    decapods_abun<-as.numeric(input$decapods_abun)
    decapods_length<-as.numeric(input$decapods_length)/1000
    decapods_SD<-as.numeric(input$decapods_SD)/1000
    decapods_width<-as.numeric(input$decapods_width)/1000
    chae_abun<-as.numeric(input$chae_abun)
    chae_length<-as.numeric(input$chae_length)/1000
    chae_SD<-as.numeric(input$chae_SD)/1000
    chae_width<-as.numeric(input$chae_width)/1000
    poly_abun<-as.numeric(input$poly_abun)
    poly_length<-as.numeric(input$poly_length)/1000
    poly_SD<-as.numeric(input$poly_SD)/1000
    poly_width<-as.numeric(input$poly_width)/1000
    Limacina_abun<-as.numeric(input$Limacina_abun)
    Limacina_length<-as.numeric(input$Limacina_length)/1000
    Limacina_SD<-as.numeric(input$Limacina_SD)/1000
    Limacina_width<-as.numeric(input$Limacina_width)/1000
    clione_abun<-as.numeric(input$clione_abun)
    clione_length<-as.numeric(input$clione_length)/1000
    clione_SD<-as.numeric(input$clione_SD)/1000
    clione_width<-as.numeric(input$clione_width)/1000
    jelly_abun<-as.numeric(input$jelly_abun)
    jelly_length<-as.numeric(input$jelly_length)/1000
    jelly_SD<-as.numeric(input$jelly_SD)/1000
    jelly_width<-as.numeric(input$jelly_width)/1000
    freq<-as.numeric(input$freq)*1000
    soundspeed<-as.numeric(input$soundspeed)
    
    if (cop_abun>0) {
      R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=cop_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=cop_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=cop_SD # Standard deviation in length
      TS_cop<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_cop<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (Mnorvegica_abun>0) {
      R=0.047 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=Mnorvegica_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=Mnorvegica_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=Mnorvegica_SD # Standard deviation in length
      TS_Mnorvegica<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.047 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_Mnorvegica<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (Traschii_abun>0) {
      R=0.044 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=Traschii_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=Traschii_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=Traschii_SD # Standard deviation in length
      TS_Traschii<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.044 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_Traschii<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (Tinermis_abun>0) {
      R=0.041 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=Tinermis_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=Tinermis_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=Tinermis_SD # Standard deviation in length
      TS_Tinermis<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.041 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_Tinermis<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (amphipods_abun>0) {
      R=0.056 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=amphipods_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=amphipods_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=amphipods_SD # Standard deviation in length
      TS_amphipods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.056 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_amphipods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (decapods_abun>0) {
      R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=decapods_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=decapods_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=decapods_SD # Standard deviation in length
      TS_decapods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_decapods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (chae_abun>0) {
      R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=chae_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=chae_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=chae_SD # Standard deviation in length
      TS_chae<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_chae<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (poly_abun>0) {
      R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=poly_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=poly_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=poly_SD # Standard deviation in length
      TS_poly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_poly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    if (Limacina_abun>0) {
      R=0.50 # Reflection coefficient. See TS_variables_description.xlsx for details
      f=freq # Frequency in Hz
      D=Limacina_width # Mean body width from net samples (m)
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      TS_Limacina<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
    }
    else {
      R=0.50 # Reflection coefficient. See TS_variables_description.xlsx for details
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      TS_Limacina<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
    }
    
    if (clione_abun>0) {
      R=0.012 # Reflection coefficient. See TS_variables_description.xlsx for details
      f=freq # Frequency in Hz
      D=clione_width # Mean body width from net samples (m)
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      TS_clione<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
    }
    else {
      R=0.012 # Reflection coefficient. See TS_variables_description.xlsx for details
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      TS_clione<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
    }
    
    if (jelly_abun>0) {
      R=0.02 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=jelly_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=jelly_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=jelly_SD # Standard deviation in length
      TS_jelly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.02 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_jelly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    
    meanTS<-10*log10((cop_abun*10^(TS_cop/10))+(Mnorvegica_abun*10^(TS_Mnorvegica/10))+(Traschii_abun*10^(TS_Traschii/10))+(Tinermis_abun*10^(TS_Tinermis/10))+(amphipods_abun*10^(TS_amphipods/10))+(decapods_abun*10^(TS_decapods/10))+(chae_abun*10^(TS_chae/10))+(poly_abun*10^(TS_poly/10))+(Limacina_abun*10^(TS_Limacina/10))+(clione_abun*10^(TS_clione/10))+(jelly_abun*10^(TS_jelly/10)))
    cat("Mean Target Strength = ", meanTS)

  })
  
  output$charts<-renderPlot({
    
    piechart()
    
    
  }) 
  
  output$averageTS<-renderPrint({
    
    TS()
    
  })
  
  output$downloadPlot<- downloadHandler(
    filename = function() {
      paste("TS_chart","png",sep=".") },
    content <- function(file) {
      png(file)
      cop_abun<-as.numeric(input$cop_abun)
      cop_length<-as.numeric(input$cop_length)/1000
      cop_SD<-as.numeric(input$cop_SD)/1000
      cop_width<-as.numeric(input$cop_width)/1000
      Mnorvegica_abun<-as.numeric(input$Mnorvegica_abun)
      Mnorvegica_length<-as.numeric(input$Mnorvegica_length)/1000
      Mnorvegica_SD<-as.numeric(input$Mnorvegica_SD)/1000
      Mnorvegica_width<-as.numeric(input$Mnorvegica_width)/1000
      Traschii_abun<-as.numeric(input$Traschii_abun)
      Traschii_length<-as.numeric(input$Traschii_length)/1000
      Traschii_SD<-as.numeric(input$Traschii_SD)/1000
      Traschii_width<-as.numeric(input$Traschii_width)/1000
      Tinermis_abun<-as.numeric(input$Tinermis_abun)
      Tinermis_length<-as.numeric(input$Tinermis_length)/1000
      Tinermis_SD<-as.numeric(input$Tinermis_SD)/1000
      Tinermis_width<-as.numeric(input$Tinermis_width)/1000
      amphipods_abun<-as.numeric(input$amphipods_abun)
      amphipods_length<-as.numeric(input$amphipods_length)/1000
      amphipods_SD<-as.numeric(input$amphipods_SD)/1000
      amphipods_width<-as.numeric(input$amphipods_width)/1000
      decapods_abun<-as.numeric(input$decapods_abun)
      decapods_length<-as.numeric(input$decapods_length)/1000
      decapods_SD<-as.numeric(input$decapods_SD)/1000
      decapods_width<-as.numeric(input$decapods_width)/1000
      chae_abun<-as.numeric(input$chae_abun)
      chae_length<-as.numeric(input$chae_length)/1000
      chae_SD<-as.numeric(input$chae_SD)/1000
      chae_width<-as.numeric(input$chae_width)/1000
      poly_abun<-as.numeric(input$poly_abun)
      poly_length<-as.numeric(input$poly_length)/1000
      poly_SD<-as.numeric(input$poly_SD)/1000
      poly_width<-as.numeric(input$poly_width)/1000
      Limacina_abun<-as.numeric(input$Limacina_abun)
      Limacina_length<-as.numeric(input$Limacina_length)/1000
      Limacina_SD<-as.numeric(input$Limacina_SD)/1000
      Limacina_width<-as.numeric(input$Limacina_width)/1000
      clione_abun<-as.numeric(input$clione_abun)
      clione_length<-as.numeric(input$clione_length)/1000
      clione_SD<-as.numeric(input$clione_SD)/1000
      clione_width<-as.numeric(input$clione_width)/1000
      jelly_abun<-as.numeric(input$jelly_abun)
      jelly_length<-as.numeric(input$jelly_length)/1000
      jelly_SD<-as.numeric(input$jelly_SD)/1000
      jelly_width<-as.numeric(input$jelly_width)/1000
      freq<-as.numeric(input$freq)*1000
      soundspeed<-as.numeric(input$soundspeed)
      
      if (cop_abun>0) {
        R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=15/1000 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=cop_width # Mean body width from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=cop_SD # Standard deviation in length
        TS_cop<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      else {
        R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=0.1 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=0.1 # Standard deviation in length
        TS_cop<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      
      if (Mnorvegica_abun>0) {
        R=0.047 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=Mnorvegica_length # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=Mnorvegica_width # Mean body width from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=Mnorvegica_SD # Standard deviation in length
        TS_Mnorvegica<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      else {
        R=0.047 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=0.1 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=0.1 # Standard deviation in length
        TS_Mnorvegica<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      
      if (Traschii_abun>0) {
        R=0.044 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=Traschii_length # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=Traschii_width # Mean body width from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=Traschii_SD # Standard deviation in length
        TS_Traschii<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      else {
        R=0.044 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=0.1 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=0.1 # Standard deviation in length
        TS_Traschii<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      
      if (Tinermis_abun>0) {
        R=0.041 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=Tinermis_length # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=Tinermis_width # Mean body width from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=Tinermis_SD # Standard deviation in length
        TS_Tinermis<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      else {
        R=0.041 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=0.1 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=0.1 # Standard deviation in length
        TS_Tinermis<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      
      if (amphipods_abun>0) {
        R=0.056 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=amphipods_length # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=amphipods_width # Mean body width from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=amphipods_SD # Standard deviation in length
        TS_amphipods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      else {
        R=0.056 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=0.1 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=0.1 # Standard deviation in length
        TS_amphipods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      
      if (decapods_abun>0) {
        R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=decapods_length # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=decapods_width # Mean body width from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=decapods_SD # Standard deviation in length
        TS_decapods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      else {
        R=0.038 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=0.1 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=0.1 # Standard deviation in length
        TS_decapods<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      
      if (chae_abun>0) {
        R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=chae_length # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=chae_width # Mean body width from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=chae_SD # Standard deviation in length
        TS_chae<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      else {
        R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=0.1 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=0.1 # Standard deviation in length
        TS_chae<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      
      if (poly_abun>0) {
        R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=poly_length # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=poly_width # Mean body width from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=poly_SD # Standard deviation in length
        TS_poly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      else {
        R=0.030 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=0.1 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=0.1 # Standard deviation in length
        TS_poly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      
      if (Limacina_abun>0) {
        R=0.50 # Reflection coefficient. See TS_variables_description.xlsx for details
        f=freq # Frequency in Hz
        D=Limacina_width # Mean body width from net samples (m)
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        TS_Limacina<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
      }
      else {
        R=0.50 # Reflection coefficient. See TS_variables_description.xlsx for details
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        TS_Limacina<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
      }
      
      if (clione_abun>0) {
        R=0.012 # Reflection coefficient. See TS_variables_description.xlsx for details
        f=freq # Frequency in Hz
        D=clione_width # Mean body width from net samples (m)
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        TS_clione<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
      }
      else {
        R=0.012 # Reflection coefficient. See TS_variables_description.xlsx for details
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        TS_clione<-10*log10(25/144*pi^4*D^6*f^4*R^2*c^(-4)*(1+25/9*pi^4*f^4*D^4*c^(-4))^-1)
      }
      
      if (jelly_abun>0) {
        R=0.02 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=jelly_length # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=jelly_width # Mean body width from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=jelly_SD # Standard deviation in length
        TS_jelly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      else {
        R=0.02 # Reflection coefficient. See TS_variables_description.xlsx for details
        L=0.1 # Average length from net samples (m)
        f=freq # Frequency in Hz
        D=0.1 # Mean body widt from net samples (m)
        Beta=L/D # Length over body width
        c=soundspeed # Sound speed in m s-1, based on CTD profiles
        s=0.1 # Standard deviation in length
        TS_jelly<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
      }
      
      meanTS<-10*log10((cop_abun*10^(TS_cop/10))+(Mnorvegica_abun*10^(TS_Mnorvegica/10))+(Traschii_abun*10^(TS_Traschii/10))+(Tinermis_abun*10^(TS_Tinermis/10))+(amphipods_abun*10^(TS_amphipods/10))+(decapods_abun*10^(TS_decapods/10))+(chae_abun*10^(TS_chae/10))+(poly_abun*10^(TS_poly/10))+(Limacina_abun*10^(TS_Limacina/10))+(clione_abun*10^(TS_clione/10))+(jelly_abun*10^(TS_jelly/10)))
      
      # Pie Chart with Percentages
      slices <- c((cop_abun*TS_cop)/meanTS,(Mnorvegica_abun*TS_Mnorvegica)/meanTS,(Traschii_abun*TS_Traschii)/meanTS,(Tinermis_abun*TS_Tinermis)/meanTS,(amphipods_abun*TS_amphipods)/meanTS,(decapods_abun*TS_decapods)/meanTS,(chae_abun*TS_chae)/meanTS,(poly_abun*TS_poly)/meanTS,(Limacina_abun*TS_Limacina)/meanTS,(clione_abun*TS_clione)/meanTS,(jelly_abun*TS_jelly)/meanTS)
      lbls <- c("copepods","Meganyctiphanes norvegica","Thysanoessa raschii","Thysanoessa inermis","Amphipods","Decapods","Chaetognaths","Polychaetes","Limacina spp.","Clione limacina","Medusae")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels=NA, col=c("blue","chartreuse","cyan","burlywood4","white","darkgrey","magenta","orange","red","black","pink1"),main="Contribution of each zooplankton group to the mean Target Strength")
      legend("topright", lbls, cex=1, fill=c("blue","chartreuse","cyan","burlywood4","white","darkgrey","magenta","orange","red","black","pink1"))
      dev.off()}
  )
  
}
shinyApp(ui=ui,server=server)