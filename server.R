#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#=====================
# Machinery for the Schelling Model
#====================

#function to calculate distance
dist <- function(x.i, y.i, v) {
  d.v <- c()
  for (j in 1:nrow(v)) {
    dist <- sqrt((as.numeric(x.i) - as.numeric(v[j, 1]))^2 + 
                   (as.numeric(y.i) - as.numeric(v[j, 2]))^2)
    d.v <- c(d.v, dist)
  }
  return(d.v)
}


#simulate Schelling's Model
schelling <- function(Nr, Ng, Nb = 0, j.r, j.g, j.b = 0, m.r, m.g, m.b = 0, interm = TRUE) {
  #generate individuals of types R, G, B and assign place on matrix
  R <- matrix(c(rep("r", Nr), runif(Nr), runif(Nr)), 
              nrow = Nr, ncol = 3)
  G <- matrix(c(rep("g", Ng), runif(Ng), runif(Ng)), 
              nrow = Ng, ncol = 3)
  B <- matrix(c(rep("b", Nb), runif(Nb), runif(Nb)), 
              nrow = Nb, ncol = 3)
  names <- c("type", "x", "y")
  colnames(R) <- names
  colnames(G) <- names
  colnames(B) <- names
  pop <- rbind(R, G, B)
  
  M <- list(r = m.r, b = m.b, g = m.g)
  J <- list(r = j.r, b = j.b, g = j.g)
  
  
  # print(M)
  # print(J)
  #calculate initial distance matrix
  D <- mapply(dist, pop[,"x"], pop[,"y"], 
              MoreArgs = list(pop[, 2:3]), USE.NAMES = FALSE)
  
  #while loop until no more changes
  cycle <- 0
  change <- TRUE
  alloc <- new.env()
  while(change) {
    
    #print intermediate graphs
    change <- FALSE
    if(cycle%%10 == 0 && interm) {
      assign(paste("cycle", cycle, sep = ""), pop, envir = alloc)
    } else if (cycle == 0) {
      assign(paste("cycle", cycle, sep = ""), pop, envir = alloc)
    }
    cycle <- cycle + 1
    
    #loop each individual
    for (i in 1:nrow(pop)) {
      ind <- pop[i, ]
      type <- ind["type"]
      m <- as.numeric(M[type])
      j <- as.numeric(J[type])
      
      #get the m closest neighbors
      top <- pop[order(D[-i,i])[1:m],]
      
      #move ind if fewer than j closest neighbors of same type
      if (length(which(top == type)) < j) {
        pop[i, 2:3] <- c(runif(1), runif(1))
        newdist <- dist(pop[i, "x"], pop[i, "y"], pop[,2:3])
        D[i,] <- newdist
        D[,i] <- newdist
        change <- TRUE
      }
    }
  }
  
  assign(paste("cycle", cycle, sep = ""), pop, envir = alloc)
  assign("ncycle", cycle, envir = alloc)
  return(alloc)
}



library(ggplot2)

#take the output of schelling and plot
plotalloc <- function(alloc) {
  elem <- ls(alloc)[-which(ls(alloc)=="ncycle")]
  pdata <- data.frame()
  for(i in 1:length(elem)) {
    tmp <- data.frame(plot = elem[i], alloc[[elem[i]]])
    pdata <- rbind(pdata, tmp)
  }
  
  cols <- c('b' = 'dodgerblue', 'g' = 'seagreen', 'r' = 'tomato')
  gg <- ggplot(data = pdata, aes(x = as.numeric(as.character(x)), y = value)) + 
    geom_point(aes( y = as.numeric(as.character(y)), color = factor(type))) +
    scale_color_manual(name = "Type", values = cols) +
    theme(legend.title = element_text(size = 14), 
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          panel.background = element_rect(
            fill = "ivory2")) +
    xlab("x") +
    ylab("y") +
    ggtitle("Schelling Model") +
    facet_wrap(~plot)
    print(gg)
    # print("done")
}

#segregation metrics
metrics <- function(final, m.r, m.g, m.b = 0, onlySim = FALSE, noSim = FALSE) {

  M <- list(r = m.r, b = m.b, g = m.g)
  porp <- c()
  D <- mapply(dist, final[,"x"], final[,"y"],
              MoreArgs = list(final[, 2:3]), USE.NAMES = FALSE)

  #similarity neighbors index
  if(!noSim) {
    for (i in 1:nrow(final)) {
      ind <- final[i,]
      type <- ind["type"]
      m <- as.numeric(M[type])
      top <- final[order(D[-i,i])[1:m],]
      porp <- c(porp,length(which(top[,"type"] == type))/m)
    }
    simIndex <- mean(porp)
  } else {
    simIndex = 0
  }

  #find dissimilarity and gini
  if(!onlySim) {
    div <- seq(0, 1, .2)
    nr <- length(which(final[,"type"] == "r"))
    ng <- length(which(final[,"type"] == "g"))
    types <- c(r = nr, g = ng)
    mintype <- names(types[which.min(types)])
    X <- length(which(final[,"type"] == mintype))
    Total <- nrow(final)
    P <- X/Total

    disIndex <- 0
    giniIndex <- 0

    for (i in 1:(length(div)-1)) {
      block.i <- final[which(final[,"x"] >= div[i] & final[,"x"] < div[i+1] &
                               final[,"y"] >= div[i] & final[,"y"] < div[i+1]),]

      r.i <- length(which(block.i[,"type"] == "r"))
      g.i <- length(which(block.i[,"type"] == "g"))
      x.i <- length(which(block.i[,"type"] == mintype))
      t.i <- nrow(block.i)
      p.i <- x.i/t.i
      disIndex <- disIndex + t.i * abs(p.i - P)
      for(j in 1:(length(div)-1)) {
        block.j <- final[which(final[,"x"] >= div[j] & final[,"x"] < div[j+1] &
                                 final[,"y"] >= div[j] & final[,"y"] < div[j+1]),]

        r.j <- length(which(block.j[,"type"] == "r"))
        g.j <- length(which(block.j[,"type"] == "g"))
        x.j <- length(which(block.j[,"type"] == mintype))
        t.j <- nrow(block.j)
        p.j <- x.j/t.j
        add <- t.i * t.j * abs(p.i - p.j)
        giniIndex <- giniIndex + add
      }
    }

    giniIndex <- giniIndex/(2*Total^2*P*(1-P))
    disIndex <- disIndex/(2*Total*P*(1-P))
  } else {
    disIndex = 0
    giniIndex = 0
  }

  return(list(sim = simIndex, dis = disIndex, gini = giniIndex))
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  m.r <- reactive({
    input$m.r/100 * input$Nr})
  m.g <- reactive({
    input$m.g/100 * input$Ng})
  m.b <- reactive({
    input$m.b/100 * input$Nb})
  
  j.r <- reactive({
    j.r <- (input$j.r/100 * m.r())})
  j.g <- reactive({
    input$j.g/100 * m.g()})
  j.b <- reactive({
    input$j.b/100 * m.b()})
  
  s <- reactive({
    s<- schelling(input$Nr, input$Ng, input$Nb, 
                 j.r(), j.g(), j.b(), m.r(), m.g(), m.b(), input$interm)
    })
  
  # p <- reactive({plotalloc(s())})
  # 
  output$schelling <- renderPlot({
      # s <- s()
      plotalloc(s())

  })
  # output$test <- renderUI(
  #   s()
  # )
# 
   output$metrics <- renderTable({
     s <- s()
     m <- metrics(get(ls(s)[2], envir = s), m.r(), m.g(), m.b(), onlySim = FALSE, noSim = FALSE)
     as.data.frame(m)
    })
  
})



