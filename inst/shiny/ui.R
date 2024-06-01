# Define UI for application that draws a histogram
fluidPage(
  theme = shinytheme("sandstone"),
  tags$head(
    tags$style(HTML("
            .title {
                font-family: 'montserrat';
                font-size: 24px;
                color: #000080;
                text-align: center;
                font-weight: bold;
            }
            .sub-title{
                font-family: 'montserrat';
                font-size: 16px;
                color: #000080;
                text-align: left;
                font-weight: bold;
            }
            .sub-sub-title{
              font-family : 'montserrat';
              font-size: 14px;
              color: #000000;
              text-align: left;
              font-weight: bold;
            }
            #summarydist {
               border-collapse: collapse;
               width: 100%;
               font-size: 14px;
            }
            #summarydist th {
            color : #000080;
            }
            #summarydist td, #summarydist th {
               border: 1px solid #ddd;
               padding: 8px;
            }
            #summarydist tr:nth-child(even) {
               background-color: #308fc2;
            }
            #summarydist th {
               padding-top: 12px;
               padding-bottom: 12px;
               text-align: left;
               background-color: #3279a8;
               color: white;
            }
        "))
  ),
  
  # Application title
  titlePanel(div(textOutput("titleDash"), class = "title")),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width=3,
      selectInput("menu", "Select an option:", choices = c("Probability" = "prob", "Characteristics" = "char")),
      
      # Data probasi
      conditionalPanel(
        condition = "input.menu=='prob'",
        radioButtons("dist", "Choose distribution:", choices = c("MSNBurr" = "msnburr", "MSNBurr-IIa" = "msnburr2a", "GMSNBurr" = "gmsnburr", "Jones-Faddy's Skew-t" = "jfst"), selected = "msnburr"),
        numericInput("num_samples", "Number of samples:", value = 100),
        
        conditionalPanel(
          condition = "input.dist=='msnburr'",
          sliderInput("bmu", "mu (μ)", min = -10, max = 10, step = 0.01, value = 0),
          sliderInput("bsigma", "sigma (σ)", min = 0.01, max = 10, step = 0.01, value = 1),
          sliderInput("balpha", "alpha (α)", min = 0.01, max = 10, step = 0.01, value = 1)
        ),
        
        conditionalPanel(
          condition = "input.dist=='msnburr2a'",
          sliderInput("b2mu", "mu (μ)", min = -10, max = 10, step = 0.01, value = 0),
          sliderInput("b2sigma", "sigma (σ)", min = 0.01, max = 10, step = 0.01, value = 1),
          sliderInput("b2alpha", "alpha (α)", min = 0.01, max = 10, step = 0.01, value = 1)
        ),
        
        conditionalPanel(
          condition = "input.dist=='gmsnburr'",
          sliderInput("gmu", "mu (μ)", min = -10, max = 10, step = 0.01, value = 0),
          sliderInput("gsigma", "sigma (σ)", min = 0.01, max = 10, step = 0.01, value = 1),
          sliderInput("galpha", "alpha (α)", min = 0.01, max = 10, step = 0.01, value = 1),
          sliderInput("gbeta", "beta (β)", min = 0.01, max = 10, step = 0.01, value = 1)
        ),
        
        conditionalPanel(
          condition = "input.dist=='jfst'",
          sliderInput("jmu", "mu (μ)", min = -10, max = 10, step = 0.01, value = 0),
          sliderInput("jsigma", "sigma (σ)", min = 0.01, max = 10, step = 0.01, value = 1),
          sliderInput("jalpha", "alpha (α)", min = -1, max = 1, step = 0.01, value = 0),
          sliderInput("jkappa", "kappa (κ)", min = 0.01, max = 0.5, step = 0.01, value = 0.2)
        )
      ),
      
      # charistik distribusi
      conditionalPanel(
        condition = "input.menu=='char'",
        radioButtons("kdist", "Choose distribution:", choices = c("MSNBurr" = "msnburr", "MSNBurr-IIa" = "msnburr2a", "GMSNBurr" = "gmsnburr", "Jones-Faddy's Skew-t" = "jfst"), selected = "msnburr"),
        
        conditionalPanel(
          condition = "input.kdist=='msnburr'",
          sliderInput("kbmu", "μ", min = -10, max = 10, step = 0.01, value = 0),
          sliderInput("kbsigma", "σ", min = 0.01, max = 10, step = 0.01, value = 1),
          sliderInput("kbalpha", "α", min = 0.01, max = 30, step = 0.01, value = 1)
        ),
        
        conditionalPanel(
          condition = "input.kdist=='msnburr2a'",
          sliderInput("kb2mu", "μ", min = -10, max = 10, step = 0.01, value = 0),
          sliderInput("kb2sigma", "σ", min = 0.01, max = 10, step = 0.01, value = 1),
          sliderInput("kb2alpha", "α", min = 0.01, max = 30, step = 0.01, value = 1)
        ),
        
        conditionalPanel(
          condition = "input.kdist=='gmsnburr'",
          sliderInput("kgmu", "μ", min = -10, max = 10, step = 0.01, value = 0),
          sliderInput("kgsigma", "σ", min = 0.01, max = 10, step = 0.01, value = 1),
          sliderInput("kgalpha", "α", min = 0.01, max = 30, step = 0.01, value = 1),
          sliderInput("kgbeta", "β", min = 0.01, max = 30, step = 0.01, value = 1)
        ),
        
        conditionalPanel(
          condition = "input.kdist=='jfst'",
          sliderInput("kjmu", "μ", min = -10, max = 10, step = 0.01, value = 0),
          sliderInput("kjsigma", "σ", min = 0.01, max = 10, step = 0.01, value = 1),
          sliderInput("kjalpha", "α", min = -0.188, max = 0.224, step = 0.01, value = 0),
          sliderInput("kjkappa", "κ", min = 0.02, max = 0.2, step = 0.001, value = 0.2)
        )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition = "input.menu=='prob'",
      #  titlePanel(div(textOutput("describe"), class = "sub-title")),
        fluidRow(
          column(6, plotOutput("pdfPlot")), # Baris ketiga, 2 kolom
          column(6, plotOutput("cdfPlot")),
          align="center"
        ),
        fluidRow(
        column(12,plotOutput("densityneo")),
        align="center"
        )
      ),
      conditionalPanel(
        condition = "input.menu=='char'",
       # titlePanel(div("Characteristics of the Neo-normal Distribution.", class = "sub-title")),
        fluidRow(
          titlePanel(div(textOutput("tableText"), class = "sub-sub-title")),
          tableOutput("summarydist")
        ),
        fluidRow(
          column(6, plotlyOutput("skewPlot")), # Baris kedua, 2 kolom
          column(6, plotlyOutput("kurtoPlot"))
        )
      )
    )
  )
)
