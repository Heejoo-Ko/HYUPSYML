library(shiny);library(shinycustomloader);library(ggpubr);library(survival);library(jsmodule);library(DT);library(shinyWidgets)
source("global.R");library(caret);library(randomForest);library(MLeval);library(MLmetrics)

# library(generics)
# install.packages("generics")
# install.packages("generics",dependencies = TRUE, repos = "http://cran.us.r-project.org")

nfactor.limit <- 10
#library(showtext)
#showtext.opts(dpi = 100)                                ## same to rmd chunk
#font_add("NanumGothic", "/usr/share/fonts/truetype/nanum/NanumGothic.ttf")
#showtext_auto()

# test에 대해서 metric, roc를 그리기
#Lasso coeff 테이블로, 탭 보기좋게 디자인

predictiveValues <- function (data, lev = NULL, model = NULL){
  PPVobj <- posPredValue(data[, "pred"], data[, "obs"])
  NPVobj <- negPredValue(data[, "pred"], data[, "obs"])
  out <- c(PPVobj, NPVobj)
  names(out) <- c("PPV", "NPV")
  out}
MySummary  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  d1 <- predictiveValues(data, lev, model)
  out <- c(a1, b1, c1, d1)
  out}
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}
normalize <- function(x) {
  if(class(x)=="numeric"){
    return ((x-min(x)) / (max(x) - min(x)))
  } else{
    return (x)
  }
}


ui <- navbarPage("REAP_AP4-ML",
                 theme = bslib::bs_theme(version = 3),
                 tabPanel("Data", icon = icon("table"),
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("check_subset", "Subset data", F),
                              uiOutput("subset_var"),
                              uiOutput("subset_val")

                              
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                          tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                              )
                              
                            )
                          )
                 ),
                 tabPanel("Table 1", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )
                          
                 ),
                 tabPanel("ML", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("method_ml", "Model", c("LASSO", "Random forest"), "Random forest", inline = T),
                              selectInput("dep_ml", "Dependent variable", varlist$Outcome, varlist$Outcome[1], multiple = F),
                              selectInput("indep_ml", "Independent variables", varlist$Variables, varlist$Variables, multiple = T),
                              actionBttn("action_ml", "Run ML")
                              
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Result",
                                                   h4("SMOTE로 balance 조절, 70% Training" ),
                                                   verbatimTextOutput("res_ml"), h4("최적 Hyperparameter는 10-fold CV 로 구했습니다. 나머지는  R의 caret 패키지 디폴트 옵션")),
                                          tabPanel("Plot", 
                                                   radioButtons("plottype_ml", "Plot", c("ROC", "VarImp"), "ROC", inline = T),
                                                   withLoader(plotOutput("fig1"), type="html", loader="loader6"),
                                                   h3("Download options"),
                                                   wellPanel(
                                                     uiOutput("downloadControls_fig1"),
                                                     downloadButton("downloadButton_fig1", label = "Download the plot")
                                                   )),
                                          tabPanel("Table",
                                                   radioButtons("tabletype_ml","Table",c("LASSO coeff","Metrics"),"Metrics",inline=T),
                                                   tableOutput("ml_table"))
                                          
                                          
                              )
                            )
                          )
                          
                 )
)



server <- function(input, output, session) {
  
  
  observeEvent(input$check_subset, {
    output$subset_var <- renderUI({
      req(input$check_subset == T)
      #factor_subset <- c(data.list$factor_original, input$factor_vname)
      
      #validate(
      #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
      #)
      
      tagList(
        selectInput("var_subset", "Subset variables",
                    choices = names(out), multiple = T,
                    selected = "age")
      )
    })
    
    output$subset_val <- renderUI({
      req(input$check_subset == T)
      req(length(input$var_subset) > 0)
      var.factor <- vars.factor
      
      outUI <- tagList()
      
      for (v in seq_along(input$var_subset)){
        if (input$var_subset[[v]] %in% vars.factor){
          varlevel <- levels(as.factor(out[[input$var_subset[[v]]]]))
          outUI[[v]] <- selectInput(paste0("val_subset", v), paste0("Subset value: ", input$var_subset[[v]]),
                                    choices = varlevel, multiple = T,
                                    selected = varlevel[length(varlevel) - 1])
        } else{
          val <- stats::quantile(out[[input$var_subset[[v]]]], na.rm = T)
          outUI[[v]] <- sliderInput(paste0("val_subset", v), paste0("Subset range: ", input$var_subset[[v]]),
                                    min = val[1], max = val[5],
                                    value = c(val[2], val[4]))
        }
        
      }
      outUI
    })
  })
  
  
  data.info <- reactive({
    out <- copy(out)
    out.label <- copy(out.label)
    #out.label[, var_label := ref[out.label$variable, name.old]]
    
    if (!is.null(input$check_subset)){
      if (input$check_subset){
        validate(
          need(length(input$var_subset) > 0 , "No variables for subsetting"),
          need(all(sapply(1:length(input$var_subset), function(x){length(input[[paste0("val_subset", x)]])})), "No value for subsetting")
        )
        var.factor <- vars.factor
        #var.conti <- setdiff(data()$conti_original, input$factor_vname)
        
        for (v in seq_along(input$var_subset)){
          if (input$var_subset[[v]] %in% vars.factor){
            out <- out[get(input$var_subset[[v]]) %in% input[[paste0("val_subset", v)]]]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out[, (vars.factor) := lapply(.SD, factor), .SDcols = vars.factor]
            out.label2 <- mk.lev(out)[, c("variable", "level")]
            data.table::setkey(out.label, "variable", "level")
            data.table::setkey(out.label2, "variable", "level")
            out.label <- out.label[out.label2]
          } else{
            out <- out[get(input$var_subset[[v]]) >= input[[paste0("val_subset", v)]][1] & get(input$var_subset[[v]]) <= input[[paste0("val_subset", v)]][2]]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out[, (vars.factor) := lapply(.SD, factor), .SDcols = vars.factor]
            out.label2 <- mk.lev(out)[, c("variable", "level")]
            data.table::setkey(out.label, "variable", "level")
            data.table::setkey(out.label2, "variable", "level")
            out.label <- out.label[out.label2]
          }
        }
        
      }
    }
    
    return(list(data = out, label = out.label))
  })
  
  data <- reactive(data.info()$data)
  data.label <- reactive(data.info()$label)
  
  output$data <- renderDT({
    datatable(data(), rownames=F, editable = F, extensions= "Buttons", caption = "Data",
              options = c(jstable::opt.data("data"), list(scrollX = TRUE))
    )
  })
  
  
  output$data_label <- renderDT({
    datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
              options = c(jstable::opt.data("label"), list(scrollX = TRUE))
    )
  })
  
  
  
  
  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit, showAllLevels = T)
  
  output$table1 <- renderDT({
    tb <- out_tb1()$table
    cap <- out_tb1()$caption
    out.tb1 <- datatable(tb, rownames = T, extensions = "Buttons", caption = cap,
                         options = c(jstable::opt.tb1("tb1"),
                                     list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                     ),
                                     list(scrollX = TRUE)
                         )
    )
    if ("sig" %in% colnames(tb)){
      out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    }
    return(out.tb1)
  })
  
  obj.ml <- eventReactive(input$action_ml, {
    withProgress(message = 'Run in progress',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:100) {
                     incProgress(1/100)
                     Sys.sleep(0.01)
                   }
                   
                   data <- data()
                   if(input$method_ml=="LASSO"){
                     for (v in setdiff(names(data), vars.factor)){
                       data[[v]] <- (data[[v]] - mean(data[[v]], na.rm = T))/sd(data[[v]], na.rm = T)
                     }
                   }
                   
                   set.seed(1)
                   rn.train <- sample(1:nrow(data), size = round(0.7 *nrow(data)), replace = F)
                   data.train <- DMwR::SMOTE(as.formula(paste(input$dep_ml, "~ .")), data = data[rn.train])
                   data.test <- data[-rn.train]
                   
                   fitControl <- trainControl(method = "cv", number = 10,
                                              summaryFunction = MySummary, savePredictions=TRUE, classProbs=TRUE)
                   
                   method <- ifelse(input$method_ml == "LASSO", "glmnet", "rf")
                   tunegrid <- NULL
                   if (input$method_ml == "LASSO"){
                     tunegrid <- expand.grid(alpha = 1, lambda = 10^seq(-5, 3, length = 200))
                   }
                   
                   
                   rf1 <- train(as.formula(paste0(input$dep_ml, " ~", paste(input$indep_ml, collapse = "+"))), data = data.train, method = method,
                                trControl = fitControl, tuneGrid=tunegrid)
                   pred <- data.frame("Obs" = data.test[[input$dep_ml]], "Pred" = predict(rf1, data.test))
                   
                   
                   return(list(obj = rf1, pred = pred, cmat = confusionMatrix(pred$Obs, pred$Pred)))
                   
                 })

    
  })
  
  output$res_ml <- renderPrint({
    list("Metrics" = obj.ml()$cmat, "Result" = obj.ml()$obj)
  })
  
  obj.fig1 <- reactive({
    if (input$plottype_ml == "ROC"){
      obj.roc <- pROC::roc(Obs ~ as.numeric(Pred), data = obj.ml()$pred)
      p <- pROC::ggroc(obj.roc) + see::theme_modern() + geom_abline(slope = 1, intercept = 1, lty = 2) +
        xlab("Specificity") + ylab("Sensitivity") + ggtitle(paste("AUC =", round(obj.roc$auc, 3)))
      return(p)
    } else{
      ggplot(varImp(obj.ml()$obj, scale = F), width = 0.05) + ggpubr::theme_classic2()
      #plot(varImp(obj.ml()$obj, scale = F))
    }
  })
  
  output$ml_table <- renderTable({
    if(input$tabletype_ml=="Metrics"){
      # get_best_result(obj.ml()$pred)
      cm<-obj.ml()$cmat
      
      Matt_Coef <- function (conf_matrix)
      {
        TP <- conf_matrix$table[1,1]
        TN <- conf_matrix$table[2,2]
        FP <- conf_matrix$table[1,2]
        FN <- conf_matrix$table[2,1]
        
        mcc_num <- (TP*TN - FP*FN)
        mcc_den <- 
          as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))
        
        mcc_final <- mcc_num/sqrt(mcc_den)
        return(mcc_final)
      }
      
      cmtable<-data.frame(cbind(t(cm$overall),t(cm$byClass)))[,c("Sensitivity","Specificity","Accuracy","Precision","Pos.Pred.Value","Neg.Pred.Value")]
      MCCresult<-data.frame(Matt_Coef(cm))
      colnames(MCCresult)<-"MCC"
      cmtable<-data.frame(cmtable,MCCresult)
      cmtable
      
    } else{
      if(input$method_ml=="LASSO"){
        lc<-coef(obj.ml()$obj$finalModel, obj.ml()$obj$bestTune$lambda)[-1,]
        data.frame(Variable = names(lc), Coefficient = lc)
      } else{
        "only for LASSO regression"
      }
    }
  })
  
  output$fig1 <- renderPlot({
    obj.fig1()
  })
  
  
  output$downloadControls_fig1 <- renderUI({
    fluidRow(
      column(4,
             selectizeInput("fig1_file_ext", "File extension (dpi = 300)", 
                            choices = c("jpg","pdf", "tiff", "svg", "pptx"), multiple = F, 
                            selected = "pptx"
             )
      ),
      column(4,
             sliderInput("fig_width_fig1", "Width (in):",
                         min = 5, max = 20, value = 8
             )
      ),
      column(4,
             sliderInput("fig_height_fig1", "Height (in):",
                         min = 5, max = 20, value = 6
             )
      )
    )
  })
  
  output$downloadButton_fig1 <- downloadHandler(
    filename =  function() {
      paste("fig1.", input$fig1_file_ext ,sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     if (input$fig1_file_ext == "pptx"){
                       my_vec_graph <- rvg::dml(ggobj  = obj.fig1())
                       
                       doc <- officer::read_pptx()
                       doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                       doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width_fig1, height = input$fig_height_fig1))
                       print(doc, target = file)
                       
                     } else{
                       ggsave(file, obj.fig1(), dpi = 300, units = "in", width = input$fig_width_fig1, height =input$fig_height_fig1)
                     }
                     
                   })
      
    })
  
}


shinyApp(ui, server)