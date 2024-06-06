#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define Function
# Define Function
msnburr_skewness <- function(alpha){
  neodistr::summary_dist(family = "msnburr", par = c(mu = 0, sigma = 1, alpha = alpha))$Skewness
}
msnburr2a_skewness <- function(alpha){
  neodistr::summary_dist(family = "msnburr2a", par = c(mu = 0, sigma = 1, alpha = alpha))$Skewness
}
gmsnburr_skewness <- function(alpha, beta) {
  neodistr::summary_dist(family = "gmsnburr", par = c(mu = 0, sigma = 1, alpha = alpha, beta = beta))$Skewness
}
jfst_skewness <- function(alpha,beta){
  neodistr::summary_dist(family = "jfst", par = c(mu = 0, sigma = 1, alpha = alpha, beta=beta))$Skewness
}

msnburr_kurtosis <- function(alpha){
  neodistr::summary_dist(family = "msnburr", par = c(mu = 0, sigma = 1, alpha = alpha))$`Excess-Kurtosis`
}
msnburr2a_kurtosis <- function(alpha){
  neodistr::summary_dist(family = "msnburr2a", par = c(mu = 0, sigma = 1, alpha = alpha))$`Excess-Kurtosis`
}
gmsnburr_kurtosis <- function(alpha,beta){
  neodistr::summary_dist(family = "gmsnburr", par = c(mu = 0, sigma = 1, alpha = alpha, beta = beta))$`Excess-Kurtosis`
}
jfst_kurtosis <- function(alpha, beta){
  neodistr::summary_dist(family = "jfst", par = c(mu = 0, sigma = 1, alpha = alpha, beta=beta))$`Excess-Kurtosis`
}

jfst_alpha <- function(a,b){
  (a-b) / sqrt(a*b*(a+b))
}
jfst_beta <- function(a,b){
  2/(a+b)
}

data_sequence <- function(min,max,n){
  seq(min, max, length.out = n)
}


# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Membangkitkan angka random berdasarkan distribusi neo-normal yag ditentukan
  data<- reactive({
    if(input$menu=="prob"){
      if(input$dist=="msnburr"){
        rmsnburr(input$num_samples, mu=input$bmu, sigma=input$bsigma, alpha = input$balpha )
      }else if(input$dist=="msnburr2a"){
        rmsnburr2a(input$num_samples, mu=input$b2mu, sigma=input$b2sigma, alpha = input$b2alpha )
      }else if(input$dist=="gmsnburr"){
        rgmsnburr(input$num_samples, mu=input$gmu, sigma=input$gsigma, alpha=input$galpha, beta=input$gbeta )
      }else if(input$dist=="jfst"){
        (rjfst(input$num_samples,mu=input$jmu, sigma=input$jsigma, alpha=input$jalpha, beta=input$jbeta))
      }
    }
  })
  
  # membuat data sequential dari random sample
  dt_seq <- reactive({
    if(input$menu=="prob"){
   #   if(input$dist=="msnburr"){
  #      data_sequence(min(data()), max(data()), input$num_samples)
   #   }else if(input$dist=="msnburr2a"){
  #      data_sequence(min(data()), max(data()),input$num_samples)
  #    }else if(input$dist=="gmsnburr"){
        data_sequence(min(data()), max(data()),input$num_samples)
   #   }else if(input$dist=="jfst"){
    #    data_sequence(min(data()), max(data()),input$num_samples)
     # }
    }
  })
  
  
  
  
  
  xmin <- reactive({
    if(input$menu=="prob"){
      if(input$dist=="msnburr"){
        qmsnburr(0.0001,mu=input$bmu, sigma=input$bsigma,alpha=input$balpha)
      }else if(input$dist=="msnburr2a"){
        qmsnburr2a(0.0001,mu=input$b2mu, sigma=input$b2sigma,alpha=input$b2alpha)
      }else if(input$dist=="gmsnburr"){
        x<-qgmsnburr(0.0001,mu=input$gmu, sigma=input$gsigma,alpha=input$galpha,beta=input$gbeta)
        if(is.infinite(x)){
          ifelse(input$galpha<input$gbeta,qmsnburr(0.0001,mu=input$gmu, sigma=input$gsigma,alpha=input$galpha),qmsnburr2a(0.0001,mu=input$gmu, sigma=input$gsigma,alpha=input$gbeta))
        } else{
          x
        }
        
             }else if(input$dist=="jfst"){
        qjfst(0.0001,mu=input$jmu, sigma=input$jsigma,alpha=input$jalpha,beta=input$jbeta)
      }
    }
  }  )
  
  xmax <- reactive({
    if(input$menu=="prob"){
      if(input$dist=="msnburr"){
        qmsnburr(0.9999,mu=input$bmu, sigma=input$bsigma,alpha=input$balpha)
      }else if(input$dist=="msnburr2a"){
        qmsnburr2a(0.9999,mu=input$b2mu, sigma=input$b2sigma,alpha=input$b2alpha)
      }else if(input$dist=="gmsnburr"){
        x<-qgmsnburr(0.9999,mu=input$gmu, sigma=input$gsigma,alpha=input$galpha,beta=input$gbeta)
        if(is.infinite(x)){
         ifelse(input$galpha<input$gbeta,qmsnburr(0.9999,mu=input$gmu, sigma=input$gsigma,alpha=input$galpha),qmsnburr2a(0.9999,mu=input$gmu, sigma=input$gsigma,alpha=input$gbeta))
        } else{
          x
        }
      }else if(input$dist=="jfst"){
        qjfst(0.9999,mu=input$jmu, sigma=input$jsigma,alpha=input$jalpha,beta=input$jbeta)
      }
    }
  }  )
  
  dtd_seq <- reactive({
    if(input$menu=="prob"){
      data_sequence(xmin(),xmax(), 100)
    #}else if(input$dist=="msnburr2a"){
    #  data_sequence(xmin(),xmax(), 100)
    #}else if(input$dist=="gmsnburr"){
    #  data_sequence(xmin(),xmax(), 100)
    #}else if(input$dist=="jfst"){
    #  data_sequence(xmin(),xmax(), 100)
    }
  })
  densn<- reactive({
    if(input$menu=="prob"){
      if (input$dist=="msnburr"){
        (dmsnburr(dtd_seq(),mu=input$bmu, sigma=input$bsigma, alpha = input$balpha))
      }else if (input$dist=="msnburr2a"){
        (dmsnburr2a(dtd_seq(),mu=input$b2mu, sigma=input$b2sigma, alpha = input$b2alpha))
      }else if (input$dist=="gmsnburr"){
        (dgmsnburr(dtd_seq(),mu=input$gmu, sigma=input$gsigma, alpha=input$galpha, beta=input$gbeta))
      }else if (input$dist=="jfst"){
        (djfst(dtd_seq(),mu=input$jmu, sigma=input$jsigma, alpha=input$jalpha, beta=input$jbeta))
      }
    }
  })
  
  
  cumn<- reactive({
    if(input$menu=="prob"){
      if (input$dist=="msnburr"){
        (pmsnburr(dtd_seq(),mu=input$bmu, sigma=input$bsigma, alpha = input$balpha))
      }else if (input$dist=="msnburr2a"){
        (pmsnburr2a(dtd_seq(),mu=input$b2mu, sigma=input$b2sigma, alpha = input$b2alpha))
      }else if (input$dist=="gmsnburr"){
        (pgmsnburr(dtd_seq(),mu=input$gmu, sigma=input$gsigma, alpha=input$galpha, beta=input$gbeta))
      }else if (input$dist=="jfst"){
        (pjfst(dtd_seq(),mu=input$jmu, sigma=input$jsigma, alpha=input$jalpha, beta=input$jbeta))
      }
    }
  })
  
  
  
  # menggambarkan fungsi densitas peluang dari distribusi normal berdasarkan parameter yang diinputkan pengguna
  densnorm <- reactive({
    if(input$menu=="prob"){
      if(input$dist=="msnburr"){
        dnorm(dtd_seq(),input$bmu, input$bsigma)
      }else if (input$dist=="msnburr2a"){
        dnorm(dtd_seq(),input$b2mu, input$b2sigma)
      }else if (input$dist=="gmsnburr"){
        dnorm(dtd_seq(),input$gmu, input$gsigma)
      }else if (input$dist=="jfst"){
        dnorm(dtd_seq(),input$jmu, input$jsigma)
      }
    }
  })
  
  # fungsi densitas peluang untuk masing-masing distribusi
  dens<- reactive({
    if(input$menu=="prob"){
      if (input$dist=="msnburr"){
        (dmsnburr(dt_seq(),mu=input$bmu, sigma=input$bsigma, alpha = input$balpha))
      }else if (input$dist=="msnburr2a"){
        (dmsnburr2a(dt_seq(),mu=input$b2mu, sigma=input$b2sigma, alpha = input$b2alpha))
      }else if (input$dist=="gmsnburr"){
        (dgmsnburr(dt_seq(),mu=input$gmu, sigma=input$gsigma, alpha=input$galpha, beta=input$gbeta))
      }else if (input$dist=="jfst"){
        (djfst(dt_seq(),mu=input$jmu, sigma=input$jsigma, alpha=input$jalpha, beta=input$jbeta))
      }
    }
  })
  
  
  
  
  # Define summary untuk nilai dari setiap distribusi
  # Data Summary 
  summ <- reactive({
    if(input$menu=="char"){
      if(input$kdist=="msnburr"){
        neodistr::summary_dist(family="msnburr",par=c(mu=input$kbmu, sigma=input$kbsigma, alpha=input$kbalpha))
      }else if (input$kdist=="msnburr2a"){
        neodistr::summary_dist(family="msnburr2a", par=c(mu=input$kb2mu, sigma=input$kb2sigma, alpha=input$kb2alpha))
      }else if (input$kdist=="gmsnburr"){
        neodistr::summary_dist(family="gmsnburr", par=c(mu=input$kgmu, sigma=input$kgsigma, alpha=input$kgalpha, beta=input$kgbeta))
      }else if (input$kdist=="jfst"){
        neodistr::summary_dist(family="jfst", par=c(mu=input$kjmu, sigma=input$kjsigma, alpha=input$kjalpha, beta=input$kjbeta))
      }
    }
  })
  
  # ------------------------------------------------------sec 2 -------------------------------------------------------------------------  
  #pdf dan cdf untuk form charistik
  ## pdf
  # fungsi densitas peluang untuk masing-masing distribusi
  
  # Membangkitkan angka random berdasarkan distribusi neo-normal yag ditentukan
  datak<- reactive({
    if(input$menu=="char"){
      if(input$kdist=="msnburr"){
        rmsnburr(200, mu=input$kbmu, sigma=input$kbsigma, alpha = input$kbalpha )
      }else if(input$kdist=="msnburr2a"){
        rmsnburr2a(200, mu=input$kb2mu, sigma=input$kb2sigma, alpha = input$kb2alpha )
      }else if(input$kdist=="gmsnburr"){
        rgmsnburr(200, mu=input$kgmu, sigma=input$kgsigma, alpha=input$kgalpha, beta=input$kgbeta )
      }else if(input$kdist=="jfst"){
        (rjfst(200,mu=input$kjmu, sigma=input$kjsigma, alpha=input$kjalpha, beta=input$kjbeta))
      }
    }
  })
  
  # Define sequence data
  dt_seqk <- reactive({
    if(input$menu=="char"){
     # if(input$dist=="msnburr"){
        data_sequence(min(datak()), max(datak()), 200)
      #}else if(input$dist=="msnburr2a"){
      #  data_sequence(min(datak()), max(datak()),200)
      #}else if(input$dist=="gmsnburr"){
      #  data_sequence(min(datak()), max(datak()),200)
      #}else if(input$dist=="jfst"){
      #  data_sequence(min(datak()), max(datak()),200)
      #}
    }
  })
  
  
  
  title <- reactive({
    if(input$menu =="prob"){
      if(input$dist == "msnburr"){
        paste ("MSNBurr Distribution")
      }else if (input$dist =="msnburr2a"){
        paste("MSNBurr-IIa Distribution")
      }else if (input$dist =="gmsnburr"){
        paste ("GMSNBurr Distribution")
      }else if (input$dist =="jfst"){
        paste ("Jones-Faddy Skew-t Distribution")
      }
    }else if (input$menu=="char"){
      if(input$kdist == "msnburr"){
        paste ("MSNBurr Distribution")
      }else if (input$kdist =="msnburr2a"){
        paste("MSNBurr-IIa Distribution")
      }else if (input$kdist =="gmsnburr"){
        paste ("GMSNBurr Distribution")
      }else if (input$kdist =="jfst"){
        paste ("Jones-Faddy Skew-t Distribution")
      }
    }
  })
  
  tabletxt <- reactive({
    if(input$menu =="char"){
      paste ("Table of Summary")
      
      
    }
  })
  
  des <- reactive({
    if(input$dist == "msnburr"){
      paste ("MSNBurr distribution")
    }else if (input$dist =="msnburr2a"){
      paste("MSNBurr-IIa distribution")
    }else if (input$dist =="gmsnburr"){
      paste ("GMSNBurr distribution")
    }else if (input$dist =="jfst"){
      paste (" Jones-Faddy Skew-t distribution ")
    }
    
  })
  #-----------------------output----------------------------------------------------------------
  output$titleDash <- renderText({
    title()
  })
  
  output$tableText <- renderText({
    tabletxt()
  })
  
  output$describe <- renderText({
    des()
  })
  output$summarydist <- renderTable({
    summ()
  })
  
  #--------------------------------------Density Plot Halaman Depan----------------------------------

  # Calculate density for the example data
  output$densityneo <- renderPlot({
    ggplot(data.frame(Value = dt_seq()), aes(x = Value,y=dens())) +
      geom_density(aes(x = data(), color="kernel"), fill = "lightblue",inherit.aes = FALSE) +
      geom_line(aes(color="pdf"), linewidth = 2, linetype = "solid") +    #geom_density(fill = "lightblue")+
      #geom_line(aes(y = densk()),color="red", size = 1.25, linetype = "solid") +
      scale_color_manual(values = c("pdf" = "red", "kernel" = "darkgreen")) +
      labs(color = "Density Function") +
      labs(title = "Random Generated Number ", x = "x", y = "density") +
      theme_minimal()+
      theme(
        plot.title = element_text(family = "poppins", face = "bold", size = 15, hjust = 0.3),
        axis.title.x = element_text(family = "roboto", size = 16, face = "bold", color = "#2C3E50"),
        axis.title.y = element_text(family = "roboto", size = 16, face = "bold", color = "#2C3E50"),
      )
    
  })
  
  
  # -------------------------------SKEWNESS PLOT----------------------------------------------------------
  output$skewPlot <- renderPlotly({
    if(input$menu=="char"){
      if(input$kdist =="msnburr"){
      #  alphab <- data_sequence(0.01,30,length.out=100)
        alphab <- seq(0.01, 30,length.out=100 )
        yb <- sapply(alphab, msnburr_skewness)
        df<-data.frame(alphab,yb)
        p<-  plot_ly(data=df, x = ~alphab, y = ~yb, type = 'scatter', mode = 'lines',
                                        line = list(color = '#7ECBE3', width = 5),
                                        hovertemplate = paste('α : %{x:.4f}',
                                                              '<br>Skewness : %{y:.4f}<br>',
                                                              '<extra> </extra>' )
        ) %>%
          #marker = list(color = 'red', size = 6, opacity = 0)) 
          layout(
            title = list(text = "Skewness Plot", font = list(family = "montserrat", size = 16, color = "#2C3E50")),
            xaxis = list(title = list(text = "α", font = list(family = "roboto", size = 24, color = "#2C3E50"))),
            yaxis = list(title = list(text = "Skewness", font = list(family = "roboto", size = 14, color = "#2C3E50")))
          ) %>%
          add_trace(
            x = input$kbalpha,
            y = msnburr_skewness(input$kbalpha),
            type = "scatter",
            mode='lines+markers',
            #hoverinfo = 'text',
            #text = ~paste("α : ", df[,7], "<br>Skewness:", "%{y:$,.4f}"),
            showlegend = FALSE,
            marker = list(color = 'red', size = 10, opacity = 0.4),
            hovertemplate = paste('α : %{x:.4f}',
                                  '<br>Skewness : %{y:.4f}<br>',
                                  '<extra> </extra>' )
          )%>%
          config(displayModeBar = FALSE)# Menghilangkan mode bar plotly
        suppressWarnings(p)
      }else if (input$kdist =="msnburr2a"){
        
        alphab2 <- seq(0.01, 30,length.out=100 )
        yb2 <- sapply(alphab2, msnburr2a_skewness)
        df<-data.frame(alphab2,yb2)
        suppressWarnings(      plot_ly(df, x = ~alphab2, y = ~yb2, type = 'scatter', mode = 'lines',
                                       line = list(color = '#000FFF', width = 5),
                                       hovertemplate = paste('α : %{x:.4f}',
                                                             '<br>Skewness : %{y:.4f}<br>',
                                                             '<extra> </extra>' )
        )%>%
          #marker = list(color = 'red', size = 6, opacity = 0)) 
          layout(
            title = list(text = "Skewness Plot", font = list(family = "montserrat", size = 16, color = "#2C3E50")),
            xaxis = list(title = list(text = "α", font = list(family = "roboto", size = 20, color = "#2C3E50"))),
            yaxis = list(title = list(text = "Skewness", font = list(family = "roboto", size = 14, color = "#2C3E50")))
          ) %>%
          add_trace(
            x = input$kb2alpha,
            y = msnburr2a_skewness(input$kb2alpha),
            #hoverinfo = 'text',
            #text = ~paste("α : ", df[,7], "<br>Skewness:", "%{y:$,.4f}"),
            type = "scatter",
            mode='lines+markers',
            showlegend = FALSE,
            marker = list(color = 'red', size = 10, opacity = 4),
            hovertemplate = paste('α : %{x:.4f}',
                                  '<br>Skewness : %{y:.4f}<br>',
                                  '<extra> </extra>' )
          )%>%
          config(displayModeBar = FALSE) ) # Menghilangkan mode bar plotly
      }else if (input$kdist=="gmsnburr"){
        alphag <- seq(0.01, 30,length.out=100 )
        betag <- seq(0.01, 30,length.out=100 )
        z <- matrix(0, length(alphag), length(betag))
        for (i in 1:length(alphag)) {
          for (j in 1 : length(betag)){
            suppressWarnings(z[j, i] <- gmsnburr_skewness(alphag[i], betag[j]))
          }
        }
        df<-data.frame(alphag,betag,z)
        suppressWarnings( plot_ly(df, x = ~alphag, y = ~betag, z = ~z, type = "surface",
                                  hovertemplate = paste('α : %{x:.4f}',
                                                        '<br> β : %{y:.4f}',
                                                        '<br>Skewness : %{z:.4f}<br>',
                                                        '<extra> </extra>' )
        ) %>%
          layout(
            title = list(text = "Skewness Plot", font = list(family = "montserrat", size = 16, color = "#2C3E50")),
            scene = list(
              xaxis = list(title = 'α'),
              yaxis = list(title = 'β'),
              zaxis = list(title = 'skewness')
            ))%>%
          add_trace(
            x = input$kgalpha,
            y = input$kgbeta,
            z = gmsnburr_skewness(input$kgalpha,input$kgbeta),
            #hoverinfo = 'text',
            #text = ~paste("α : ", df[,7], "<br>β : ", df[,7] ,"<br>Skewness:", "%{y:$,.4f}"),
            showlegend = FALSE,
            type = "scatter3d",
            mode='lines+markers',
            marker = list(color = 'red', size = 5, opacity = 10),
            hovertemplate = paste('α : %{x:.4f}',
                                  '<br> β : %{y:.4f}',
                                  '<br>Skewness : %{z:.4f}<br>',
                                  '<extra> </extra>' )
          )%>%
          config(displayModeBar = FALSE))  # Menghilangkan mode bar plotly
      }else if (input$kdist=="jfst"){
        #alpha <- jfst_alpha(input$kjalpha, input$kjbeta)
        #beta <- jfst_beta (input$kjalpha, input$kjalpha)
        #alphajs <- seq(jfst_alpha(2,2), jfst_alpha(10,input$kjbeta), length.out=50)
        alphajs <- seq(1.6, 15,length.out=50 )
        betajs <- seq(1.6, 15,length.out=50 )
        
        #alphajs <- seq(-0.57, 0.47, length.out=50 )
        #betajs <- rep(beta,length.out=50 )
        #betajs <- rep(input$kjbeta, length.out = 50)
        skew<-jfst_skewness(input$kjalpha,input$kjbeta)
        zj <- matrix(,length(betajs), length(alphajs))
        for (i in 1:length(alphajs)) {
          for (j in 1:length(betajs)) {
            suppressWarnings(zj[j,i] <- jfst_skewness(alphajs[i], betajs[j]))
          }
        }
        df<-data.frame(alphajs,betajs,zj)
        suppressWarnings(   plot_ly(df, x = ~alphajs, y = ~betajs, z = ~zj, type = "surface",
                                    hovertemplate = paste('α : %{x:.4f}',
                                                          '<br> β : %{y:.4f}', 
                                                          '<br>Skewness : %{z:.4f}<br>',
                                                          '<extra> </extra>' )) %>%
                              layout(
                                title = list(text = "Skewness Plot", font = list(family = "montserrat", size = 16, color = "#2C3E50")),
                                scene = list(
                                  xaxis = list(title = 'α'),
                                  yaxis = list(title = ' β'),
                                  zaxis = list(title = 'skewness')
                                ))%>%
                              add_trace(
                                x = input$kjalpha,
                                y = input$kjbeta,
                                z = skew,
                                type = "scatter3d",
                                mode='lines+markers',
                                showlegend = FALSE,
                                marker = list(color = 'red', size = 5, opacity = 5),
                                hovertemplate = paste('α : %{x:.4f}',
                                                      '<br> β : %{y:.4f}',
                                                      '<br>Skewness : %{z:.4f}<br>',
                                                      '<extra> </extra>' )
                              )%>%
                              config(displayModeBar = FALSE) ) # Menghilangkan mode bar plotly
      }
    }
  })
  
  
  # -----------------------------------------KURTOSIS PLOT------------------------------------------  
  
  output$kurtoPlot <- renderPlotly({
    if(input$menu=="char"){
      if(input$kdist =="msnburr"){
        alphab <- seq(0.01, 30,length.out=100 )
        ybk <- sapply(alphab, msnburr_kurtosis)
        df<-data.frame(alphab,ybk)
        suppressWarnings( plot_ly(data=df, x = ~alphab, y = ~ybk, type = 'scatter', mode = 'lines',
                                      line = list(color = '#7ECBE3', width = 5),
                                      hovertemplate = paste('α : %{x:.4f}',
                                                            '<br>Excess-Kurtosis : %{y:.4f}<br>',
                                                            '<extra> </extra>' )
        ) %>%
          layout(
            title = list(text = "Excess-Kurtosis Plot", font = list(family = "montserrat", size = 16, color = "#2C3E50")),
            xaxis = list(title = list(text = "α", font = list(family = "roboto", size = 20, color = "#2C3E50"))),
            yaxis = list(title = list(text = "Excess-Kurtosis", font = list(family = "roboto", size = 14, color = "#2C3E50")))
          ) %>%
          add_trace(
            x = input$kbalpha,
            y = msnburr_kurtosis(input$kbalpha),
            type = "scatter",
            mode='lines+markers',
            showlegend = FALSE,
            marker = list(color = 'red', size = 10, opacity = 5),
            hovertemplate = paste('α : %{x:.4f}',
                                  '<br>Excess-Kurtosis : %{y:.4f}<br>',
                                  '<extra> </extra>' )
          )%>%
          config(displayModeBar = FALSE))  # Menghilangkan mode bar plotly
      }else if (input$kdist =="msnburr2a"){
        alphab2 <- seq(0.01, 30,length.out=100 )
        yb2k <- sapply(alphab2, msnburr2a_kurtosis)
        df<-data.frame(alphab2,yb2k)
        suppressWarnings(   plot_ly(df, x = ~alphab2, y = ~yb2k, type = 'scatter', mode = 'lines',
                                    line = list(color = '#000FFF', width = 5),
                                    hovertemplate = paste('α : %{x:.4f}',
                                                          '<br>Excess-Kurtois : %{y:.4f}<br>',
                                                          '<extra> </extra>' )
        ) %>%
          
          layout(
            title = list(text = "Excess-Kurtosis Plot", font = list(family = "montserrat", size = 16, color = "#2C3E50")),
            xaxis = list(title = list(text = "α", font = list(family = "roboto", size = 20, color = "#2C3E50"))),
            yaxis = list(title = list(text = "Excess-kurtosis", font = list(family = "roboto", size = 14, color = "#2C3E50")))
          ) %>%
          add_trace(
            x = input$kb2alpha,
            y = msnburr2a_kurtosis(input$kb2alpha),
            type = "scatter",
            mode='lines+markers',
            showlegend = FALSE,
            marker = list(color = 'red', size = 10, opacity = 5),
            hovertemplate = paste('α : %{x:.4f}',
                                  '<br>Excess-Kurtosis : %{y:.4f}<br>',
                                  '<extra> </extra>' )
          )%>%
          config(displayModeBar = FALSE) ) # Menghilangkan mode bar plotly
      }else if (input$kdist=="gmsnburr"){
        
        
        alphag <- seq(0.01, 30,length.out=50 )
        betag <- seq(0.01, 30,length.out=50 )
        z <- matrix(0, length(alphag), length(betag))
        for (i in 1:length(alphag)) {
          for (j in 1 : length(betag)){
            suppressWarnings(z[j, i] <- gmsnburr_kurtosis(alphag[i], betag[j]))
          }
        }
        df<-data.frame(alphag,betag,z)
        suppressWarnings( plot_ly(df, x = ~alphag, y = ~betag, z = ~z, type = "surface",
                                  hovertemplate = paste('α : %{x:.4f}',
                                                        '<br> β : %{y:.4f}',
                                                        '<br>Excess-Kurtosis : %{z:.4f}<br>',
                                                        '<extra> </extra>' )
        ) %>%
          layout(
            title = list(text = "Excess-Kurtosis plot", font = list(family = "montserrat", size = 16, color = "#2C3E50")),
            scene = list(
              xaxis = list(title = 'α'),
              yaxis = list(title = 'β'),
              zaxis = list(title = 'kurtosis')
            ))%>%
          add_trace(
            x = input$kgalpha,
            y = input$kgbeta,
            z = gmsnburr_kurtosis (input$kgalpha,input$kgbeta),
            type = "scatter3d",
            mode='lines+markers',
            showlegend = FALSE,
            marker = list(color = 'red', size = 5, opacity = 5),
            hovertemplate = paste('α : %{x:.4f}',
                                  '<br> β : %{y:.4f}',
                                  '<br>Excess-Kurtosis : %{z:.4f}<br>',
                                  '<extra> </extra>' )
          )%>%
          config(displayModeBar = FALSE))  # Menghilangkan mode bar plotly
      }else if (input$kdist=="jfst"){
        
        alphast <- seq(2.1, 30,length.out=50 )
        betast <- seq(2.1, 30,length.out=50 )
        z <- matrix(0, length(alphast), length(betast))
        
        for (i in 1:length(alphast)) {
          for (j in 1 : length(betast)){
            suppressWarnings(z[j, i] <- jfst_kurtosis(alphast[i], betast[j]))
          }
        }
        df<-data.frame(alphast,betast,z)
        suppressWarnings(  plot_ly(df, x = ~alphast, y = ~betast, z = ~z, type = "surface",
                                   hovertemplate = paste('α : %{x:.4f}',
                                                         '<br> κ : %{y:.4f}', 
                                                         '<br>Excess-Kutrosis : %{z:.4f}<br>',
                                                         '<extra> </extra>' )) %>%
                             layout(
                               title = list(text = "Excess-Kurtosis Plot", font = list(family = "montserrat", size = 16, color = "#2C3E50")),
                               scene = list(
                                 xaxis = list(title = 'α'),
                                 yaxis = list(title = ' κ'),
                                 zaxis = list(title = 'Excess-kurtosis')
                               ))%>%
                             add_trace(
                               x = input$kjalpha,
                               y = input$kjbeta,
                               z = jfst_kurtosis (input$kjalpha,input$kjbeta),
                               type = "scatter3d",
                               mode='lines+markers',
                               showlegend = FALSE,
                               marker = list(color = 'red', size = 5, opacity = 5),
                               hovertemplate = paste('α : %{x:.4f}',
                                                     '<br> κ : %{y:.4f}',
                                                     '<br>Kurtosis : %{z:.4f}<br>',
                                                     '<extra> </extra>' )
                             )%>%
                             config(displayModeBar = FALSE))  # Menghilangkan mode bar plotly
      }
    }
  })
  
  #-----------------------------------------------PDF PLOT------------------------------------------- 
  output$pdfPlot <- renderPlot({
    if(input$menu=="prob"){
      if(input$dist=="msnburr"){
        suppressWarnings( ggplot(data.frame(Value = dtd_seq()), aes(x = Value,xmin=xmin(),xmax=xmax())) +
                            geom_line(aes(x = Value,y = densn(),color="MSNburr"), linewidth = 2.25, linetype = "solid",inherit.aes = FALSE) +
                            geom_line(aes(x = Value,y = densnorm(),color="normal"), linewidth = 2, linetype = "dashed",inherit.aes = FALSE) +
                            #coord_cartesian(xlim = c(lb(),ub()))+
                            labs(title = "Probability density Function (PDF)", x = "x", y = "density") +
                            #         scale_color_manual(values = c("MSNburr" = "red", "normal" = "darkgreen")) +
                            labs(color = "Distribution") +
                            theme_minimal()+
                            
                            theme(
                              
                              plot.title = element_text(family = "montserrat",  size = 15, hjust = 0.5),
                              axis.title.x = element_text(family = "roboto", size = 14,  color = "#2C3E50"),
                              axis.title.y = element_text(family = "roboto", size = 14,  color = "#2C3E50"),
                              legend.position="bottom"
                            ))
      } else if (input$dist=="msnburr2a"){
        suppressWarnings(  ggplot(data.frame(Value = dtd_seq()), aes(x = Value,xmin=xmin(),xmax=xmax())) +
                             geom_line(aes(x = Value,y = densn(),color="MSNBurr-IIa"), linewidth = 2.25, linetype = "solid") +
                             geom_line(aes(x = Value,y = densnorm(),color="normal"), linewidth = 2, linetype = "dashed",inherit.aes = FALSE) +
                             labs(title = "Probability density Function (PDF)", x = "x", y = "density") +
                             # scale_color_manual(values = c( "normal" = "orange")) +
                             labs(color = "Distribution") +
                             theme_minimal()+
                             #xlim(lb(),ub())+
                             theme(
                               plot.title = element_text(family = "montserrat",  size = 15, hjust = 0.5),
                               axis.title.x = element_text(family = "roboto", size = 14,  color = "#2C3E50"),
                               axis.title.y = element_text(family = "roboto", size = 14,  color = "#2C3E50"),
                               legend.position="bottom"))
        
      } else if (input$dist =="gmsnburr"){
        suppressWarnings(ggplot(data.frame(Value = dtd_seq()), aes(x = Value,xmin=xmin(),xmax=xmax())) +
                           geom_line(aes(x = Value,y = densn(),color="GMSNBurr"), linewidth = 2.25, linetype = "solid",inherit.aes = FALSE) +
                           geom_line(aes(x = Value,y = densnorm(),color="normal"), linewidth = 2, linetype = "dashed",inherit.aes = FALSE) +
                           labs(title = "Probability density Function (PDF)", x = "x", y = "density") +
                           labs(color = "Distribution") +
                           theme_minimal()+
                           theme(
                             plot.title = element_text(family = "montserrat",  size = 15, hjust = 0.5),
                             axis.title.x = element_text(family = "roboto", size = 14,  color = "#2C3E50"),
                             axis.title.y = element_text(family = "roboto", size = 14,  color = "#2C3E50"),
                             legend.position="bottom"
                           ))
      }else if (input$dist == "jfst"){
        suppressWarnings( ggplot(data.frame(Value = dtd_seq()), aes(x = Value,xmin=xmin(),xmax=xmax())) +
                            geom_line(aes(x = Value,y = densn(),color="Jones-Faddy Skew-t"), size = 2.25, linetype = "solid") +
                            geom_line(aes(x = Value,y = densnorm(),color="Normal "), size = 2, linetype = "dashed") +
                            labs(title = "Probability density Function (PDF)", x = "x", y = "density") +
                            #xlim(lb(),ub())+
                            labs(color = "Distribution") +
                            theme_minimal()+
                            theme(
                              plot.title = element_text(family = "montserrat",  size = 15, hjust = 0.5),
                              axis.title.x = element_text(family = "roboto", size = 14,  color = "#2C3E50"),
                              axis.title.y = element_text(family = "roboto", size = 14,  color = "#2C3E50"),
                              legend.position="bottom"
                            ))
      }
    }
    
    
  })
  #--------------------------------------------------CDF PLOT ---------------------------------------------- 
  output$cdfPlot <- renderPlot({
    if(input$menu=="prob"){
      if(input$dist=="msnburr"){
        ggplot(data.frame(Value = dtd_seq()), aes(x = Value,xmin=xmin(),xmax=xmax())) +
          geom_line(aes(y = cumn()),color="#7ECBE3", linewidth = 2.25, linetype = "solid") +
          labs(title = "Cumulative distribution function (CDF)", x = "x", y = "density") +
          theme_minimal()+
          theme(
            plot.title = element_text(family = "montserrat",  size = 15, hjust = 0.5),
            axis.title.x = element_text(family = "roboto", size = 14, color = "#2C3E50"),
            axis.title.y = element_text(family = "roboto", size = 14, color = "#2C3E50"),
          )
      }else if (input$dist == "msnburr2a"){
        ggplot(data.frame(Value = dtd_seq()), aes(x = Value,xmin=xmin(),xmax=xmax())) +
          geom_line(aes(y = cumn()),color="#000FFF", linewidth = 2.25, linetype = "solid") +
          labs(title = "Cumulative distribution function (CDF)", x = "x", y = "density") +
          theme_minimal()+
          theme(
            plot.title = element_text(family = "montserrat",  size = 15, hjust = 0.5),
            axis.title.x = element_text(family = "roboto", size = 14, color = "#2C3E50"),
            axis.title.y = element_text(family = "roboto", size = 14, color = "#2C3E50"),
          )
      }else if (input$dist=="gmsnburr"){
        ggplot(data.frame(Value = dtd_seq()), aes(x = Value,xmin=xmin(),xmax=xmax())) +
          geom_line(aes(y = cumn()),color="#000C66", linewidth = 2.25, linetype = "solid") +
          labs(title = "Cumulative distribution function (CDF)", x = "x", y = "density") +
          theme_minimal()+
          theme(
            plot.title = element_text(family = "montserrat",  size = 15, hjust = 0.5),
            axis.title.x = element_text(family = "roboto", size = 14, color = "#2C3E50"),
            axis.title.y = element_text(family = "roboto", size = 14, color = "#2C3E50"),
          )
      }else if (input$dist =="jfst"){
        ggplot(data.frame(Value = dtd_seq()), aes(x = Value,xmin=xmin(),xmax=xmax())) +
          geom_line(aes(y = cumn()),color="#050A30", linewidth = 2.25, linetype = "solid") +
          labs(title = "Cumulative distribution function (CDF)", x = "x", y = "density") +
          theme_minimal()+
          theme(
            plot.title = element_text(family = "montserrat",  size = 15, hjust = 0.5),
            axis.title.x = element_text(family = "roboto", size = 14, color = "#2C3E50"),
            axis.title.y = element_text(family = "roboto", size = 14, color = "#2C3E50"),
          )
      }
    }
    
    
  })
  
  
  
  
}


