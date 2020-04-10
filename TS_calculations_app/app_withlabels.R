library(shiny)
ui<-fluidPage(
  
fluidRow(
  column(1,offset=1,tags$h5("Calanus spp.")),
  column(1,tags$h5("Meganycthiphanes norvegica")),
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
  column(1,numericInput(inputId="cal_abun",value=0.1, min=0, max=1, label="")),
  column(1,numericInput(inputId="Mnorvegica_abun",value=0.1, min=0, max=1, label="")),
  column(1,numericInput(inputId="Traschii_abun",value=0.1, min=0, max=1, label="")),
  column(1,numericInput(inputId="Tinermis_abun",value=0.1,min=0, max=1, label="")),
  column(1,numericInput(inputId="amphipods_abun",value=0.1,min=0, max=1, label="")),
  column(1,numericInput(inputId="decapods_abun",value=0.1,min=0, max=1, label="")),
  column(1,numericInput(inputId="chae_abun",value=0.1,min=0, max=1, label="")),
  column(1,numericInput(inputId="poly_abun", value=0.1,min=0, max=1,label="")),
  column(1,numericInput(inputId="Limacina_abun",value=0.1,min=0, max=1, label="")),
  column(1,numericInput(inputId="clione_abun",value=0.1,min=0, max=1, label="")),
  column(1,numericInput(inputId="jelly_abun",value=0.1,min=0, max=1, label=""))
),
  
fluidRow(
  column(1,tags$h5("Average length (mm)")),
  column(1,numericInput(inputId="cal_length",value=0.1,min=0,max=500,label="")),
  column(1,numericInput(inputId="Mnorvegica_length",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="Traschii_length",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="Tinermis_length",value=0.1, min=0,max=500,label="")),
  column(1,numericInput(inputId="amphipods_length",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="decapods_length",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="chae_length", value=0.1,min=0,max=500,label="")),
  column(1,numericInput(inputId="poly_length",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="Limacina_length",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="clione_length",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="jelly_length",value=0.1,min=0,max=500, label=""))
),


fluidRow(
  column(1,tags$h5("Standard deviation in length (mm)")),
  column(1,numericInput(inputId="cal_SD",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="Mnorvegica_SD",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="Traschii_SD",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="Tinermis_SD",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="amphipods_SD",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="decapods_SD",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="chae_SD",value=0.1, min=0,max=500,label="")),
  column(1,numericInput(inputId="poly_SD",value=0.1,min=0,max=500,label="")),
  column(1,numericInput(inputId="Limacina_SD",value=0.1,min=0,max=500,label="")),
  column(1,numericInput(inputId="clione_SD",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="jelly_SD",value=0.1,min=0,max=500, label=""))
),

fluidRow(
  column(1,tags$h5("Average width (mm)")),
  column(1,numericInput(inputId="cal_width", value=0.1,min=0,max=500,label="")),
  column(1,numericInput(inputId="Mnorvegica_width",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="Traschii_width",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="Tinermis_width", value=0.1,min=0,max=500,label="")),
  column(1,numericInput(inputId="amphipods_width",value=0.1, min=0,max=500,label="")),
  column(1,numericInput(inputId="decapods_width",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="chae_width",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="poly_width",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="Limacina_width",value=0.1,min=0,max=500, label="")),
  column(1,numericInput(inputId="clione_width",value=0.1,min=0,max=500,label="")),
  column(1,numericInput(inputId="jelly_width",value=0.1, min=0,max=500,label=""))
),

fluidRow(
  column(1,offset=1,numericInput(inputId="freq",value=200, min=0, max=1500, label="Frequency in kHz")),
  column(1,numericInput(inputId="soundspeed", value=1500,label="Sound speed (m^s-1)"))
),

fluidRow(
  column(1,offset=1,actionButton(inputId="run",label="Run")),
  column(1,actionButton(inputId="save",label="Save image"))
),

fluidRow(
  column(6,offset=3,plotOutput("charts")),
  column(2,textOutput("averageTS"))
) 

)

server<-function(input, output) {

  piechart<- eventReactive(input$run,{
    
    cal_abun<-as.numeric(input$cal_abun)
    cal_length<-as.numeric(input$cal_length)/1000
    cal_SD<-as.numeric(input$cal_SD)/1000
    cal_width<-as.numeric(input$cal_width)/1000
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
    freq<-as.numeric(input$freq)/1000
    soundspeed<-as.numeric(input$soundspeed)
    
    if (cal_abun>0) {
      R=0.026 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=cal_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=cal_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=cal_SD # Standard deviation in length
      TS_cal<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.026 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_cal<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
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
    
    meanTS<-(cal_abun*TS_cal)+(Mnorvegica_abun*TS_Mnorvegica)+(Traschii_abun*TS_Traschii)+(Tinermis_abun*TS_Tinermis)+(amphipods_abun*TS_amphipods)+(decapods_abun*TS_decapods)+(chae_abun*TS_chae)+(poly_abun*TS_poly)+(Limacina_abun*TS_Limacina)+(clione_abun*TS_clione)+(jelly_abun*TS_jelly)
    
    # Pie Chart with Percentages
    slices <- c((cal_abun*TS_cal)/meanTS,(Mnorvegica_abun*TS_Mnorvegica)/meanTS,(Traschii_abun*TS_Traschii)/meanTS,(Tinermis_abun*TS_Tinermis)/meanTS,(amphipods_abun*TS_amphipods)/meanTS,(decapods_abun*TS_decapods)/meanTS,(chae_abun*TS_chae)/meanTS,(poly_abun*TS_poly)/meanTS,(Limacina_abun*TS_Limacina)/meanTS,(clione_abun*TS_clione)/meanTS,(jelly_abun*TS_jelly)/meanTS)
    lbls <- c("Calanus","Meganyctiphanes norvegica","Thysanoessa raschii","Thysanoessa inermis","Amphipods","Decapods","Chaetognaths","Polychaetes","Limacina spp.","Clione limacina","Medusae")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    pie(slices, labels=lbls, col=rainbow(length(lbls)),main="Contribution of each group to the mean Target Strength")
    #legend("topright", c("Calanus","Meganyctiphanes norvegica","Thysanoessa raschii","Thysanoessa inermis","Amphipods","Decapods","Chaetognaths","Polychaetes","Limacina spp.","Clione limacina","Medusae"), cex=0.8, fill=rainbow(length(lbls)))
    
    })

  TS<- eventReactive(input$run,{
    
    cal_abun<-as.numeric(input$cal_abun)
    cal_length<-as.numeric(input$cal_length)/1000
    cal_SD<-as.numeric(input$cal_SD)/1000
    cal_width<-as.numeric(input$cal_width)/1000
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
    freq<-as.numeric(input$freq)/1000
    soundspeed<-as.numeric(input$soundspeed)
   
    if (cal_abun>0) {
      R=0.026 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=cal_length # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=cal_width # Mean body width from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=cal_SD # Standard deviation in length
      TS_cal<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
    }
    else {
      R=0.026 # Reflection coefficient. See TS_variables_description.xlsx for details
      L=0.1 # Average length from net samples (m)
      f=freq # Frequency in Hz
      D=0.1 # Mean body widt from net samples (m)
      Beta=L/D # Length over body width
      c=soundspeed # Sound speed in m s-1, based on CTD profiles
      s=0.1 # Standard deviation in length
      TS_cal<-10*log10(0.08*(R^2)*(L^2)*(Beta^-1)*(1-exp(-8*(pi^2)*(f^2)*(D^2)*(s^2)*(c^-2))*cos(pi*f*D*(c^-1)*(4-0.5*pi*(pi*f*D*(c^-1)+0.4)^-1))))
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
    
    meanTS<-(cal_abun*TS_cal)+(Mnorvegica_abun*TS_Mnorvegica)+(Traschii_abun*TS_Traschii)+(Tinermis_abun*TS_Tinermis)+(amphipods_abun*TS_amphipods)+(decapods_abun*TS_decapods)+(chae_abun*TS_chae)+(poly_abun*TS_poly)+(Limacina_abun*TS_Limacina)+(clione_abun*TS_clione)+(jelly_abun*TS_jelly)
    cat("Mean TS = ", meanTS)
  })
  
  output$charts<-renderPlot({
   
    piechart()
    
 
  }) 
  
  output$averageTS<-renderPrint({

TS()
    
  })
}

shinyApp(ui=ui,server=server)


cat("TS calanus =", TS_cal)
cat("TS m norvegica =", TS_Mnorvegica)
cat("TS T raschii =", TS_Traschii)
cat("TS T inermis =", TS_Tinermis)
cat("TS amphipods =", TS_amphipods)
cat("TS decapods =", TS_decapods)
cat("TS chae =", TS_chae)
cat("TS poly =", TS_poly)
cat("TS T Limacina =", TS_Limacina)
cat("TS T clione =", TS_clione)
cat("TS jelly =", TS_jelly)