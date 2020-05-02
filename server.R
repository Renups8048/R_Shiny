shinyServer(
  function(input, output, session)({
    
    myData <- reactive({
      if (input$ds == 'file'){
        file1 <- input$datafile
        if (is.null(file1)){
          return()
        }
        data = read.csv(file=file$datapath)
        # data
      }
      
    # })
    # 
    # myData2 <- reactive({
      
      else if (input$ds == 'url')
      {
        data == read.csv(file = input$urldat)
        # data
      }
    # })
    # 
    # myData3 <- reactive({
      
      else if (input$ds == 'ib')
      {
        data = data.frame(get(strsplit(input$inbuilt," ")[[1]][1]))
        
        # data
        View(data)
      }
      
     data
    })
    
    
    output$columns <- renderUI({
      View(myData())
      nms<- names(myData())
      View(myData())
      selectInput("cols","Select Variable",
                  choices = nms)
      
    })
    
    myData1 <- reactive({
      file1 <- input$datafile1
      if (is.null(file1)){
        return()
      }
      data1 = read.csv(file=file$datapath)
      data1
    })
    
    output$columns1 <- renderUI({
      nms1<- names(myData1())
      View(myData1())
      selectInput("cols1","Select Variable",
      choices = nms1)
      # updateSelectInput(session, "cols1",
                        # choices = nms1)
    })
    
    output$columns2 <- renderUI({
      #View(myData())
      nms2<- names(myData())
      View(myData())
      selectInput("cols2","Select Variable to check Indepndence",
                  choices = nms2)
      
    })
    
    output$table <- renderTable(
      head(myData(),10))
    
    # output$hh = renderText({
    #   print(input$cols)
    # })
    
    hypo <- reactive({
      
      if (input$type == 't'){
        # xx <- myData() %>% select(as.character(input$cols1))
        d00 <- myData()
        # d01 <- myData1()
        # xx2 <- d01[,input$cols2]
        xx <- d00[,c(input$cols)]
        
          value = ifelse(input$greater,"g","l")
          conf_level = (1 - as.numeric(input$alpha))
          t = t.test(xx, mu = input$mu, alternative = value, conf.level = conf_level)
          
          alpha <- as.numeric(input$alpha)
          p_value <- as.numeric(t$p.value)
          
          
          if(p_value < alpha){
            decision = "Reject H_0"}
          else{
            decision = "Accept H_0"
          }
          
          print(paste0(summary(t),paste(conf_level, 'The decision is :', decision), sep = '\n'))
        
      }else if(input$type == 'tt'){
        # xx <- myData() %>% select(as.character(input$cols1))
        d00 <- myData()
        xx <- d00[,c(input$cols)]
        d01 <- myData1()
        xx2 <- d01[,input$cols1]
        # if (is.null(xx))
        # {
          value = ifelse(input$greater,"g","l")
          conf_level = (1 - as.numeric(input$alpha))
          t = t.test(mean(xx),mean(xx2),paired = T, alternative = value, conf.level = conf_level)
          
          alpha <- as.numeric(input$alpha)
          p_value <- as.numeric(t$p.value)
          
          
          if(p_value < alpha){
            decision = "Reject H_0"}
          else{
            decision = "Accept H_0"
          }
          
          print(paste0(summary(t),paste(conf_level, 'The decision is :', decision), sep = '\n'))
        # }
      }else if(input$type == 'gt'){
        # xx <- myData() %>% select(as.character(input$cols1))
        d00 <- myData()
        xx <- d00[,input$cols]
        a <- chisq.test(p1$counts, p=null.probs, rescale.p=TRUE, simulate.p.value=TRUE)
        alpha <- as.numeric(input$alpha)
        p_value <- as.numeric(a$p.value)
        if(p_value < alpha){
          decision = "Reject H_0"}
        else{
          decision = "Accept H_0"
        }
        
        print(paste0(summary(t),paste(conf_level, 'The decision is :', decision), sep = '\n'))
      }else if(input$type == 'it'){
        # xx <- myData() %>% select(as.character(input$cols1))
        d00 <- myData()
        xx <- d00[,input$cols]
        d01 <- myData1()
        xx2 <- d01[,input$cols1]
        tbl <- table(xx,xx2)
        a <- chisq.test(tbl)
        alpha <- as.numeric(input$alpha)
        p_value <- as.numeric(a$p.value)
        if(p_value < alpha){
          decision = "Reject H_0"}
        else{
          decision = "Accept H_0"
        }
        
        print(paste0(summary(t),paste(conf_level, 'The decision is :', decision), sep = '\n'))
      }
    })
    
    output$summ <- renderPrint(
      hypo()
    )
    
  })
)