#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)

# Define UI for application
shinyUI(
  fluidPage(theme = shinytheme("cerulean"),
            shinyjs::useShinyjs(),
            
    # Give the page a title
    # titlePanel("The Machine Learning Dashboard"),
    titlePanel(title=div(img(src="Logo.png", height = 100, width = 175), "The Machine Learning Dashboard")),
    
    #Navbar Menu
    navbarPage("Navbar",
               
               #Navigation Tab 1       
               tabPanel("Documentation", includeMarkdown("documentation.Rmd")),
               
               navbarMenu("Create Model",
                          
                          # Create Model Sub Tab 1 ----      
                          tabPanel("Classification",
                                   
                                   h2("Upload Data"),
                                   
                                   # Sidebar layout with input and output definitions ----
                                   sidebarLayout(
                                     
                                     # Sidebar panel for inputs ----
                                     sidebarPanel(
                                       
                                       # Input: Select a file ----
                                       textInput("cfn_getwd","Set Working Directory",value = 'C:\\Users\\kbhandari\\OneDrive - Epsilon\\Desktop'),
                                       textInput("cfn_file_name","Specify file name",value = "example.csv"),
                                       
                                       #Input: Options
                                       textInput("cfn_upload_start", "Start", 0),
                                       
                                       textInput("cfn_upload_nrows", "N-Rows", Inf),
                                       
                                       textInput("cfn_upload_na_strings","NA Strings", "NA"),
                                       
                                       # Input: Select separator ----
                                       radioButtons("cfn_sep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                Tab = "\t"),
                                                    selected = ","),
                                       
                                       # Action Button
                                       actionButton("cfn_file1", "Read Train Data as CSV File"),
                                       
                                       tags$div(style = "padding:10px"),
                                       
                                       actionButton("cfn_file2", "Read Test Data as CSV File (Optional)"),
                                       
                                       # Horizontal line ----
                                       tags$hr(),
                                       
                                        p('If you want a sample .csv file to upload,',
                                        'you can first download the sample',
                                        a(href = 'https://gist.github.com/seankross/a412dfbd88b3db70b74b', 'mtcars.csv'),
                                        'file, and then try uploading them.')
                                       
                                     ),
                                     
                                     # Main panel for displaying outputs ----
                                     mainPanel(
                                       # Output: Data file ----
                                       withSpinner(tableOutput("raw_output"))
                                     )
                                     
                                   ),
                                   
                                   #Data Preprocess Section
                                   conditionalPanel(
                                     condition="output.raw_output != null",
                                     h2("Data Preprocess"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         
                                         #Missing Value Treatment
                                         p("Selecting this checkbox will treat any missing values by 
                                           replacing them with the median or mode wherever appropriate"),
                                         # Input: Checkbox to treat missing values
                                         checkboxInput("cfn_missing", "Treat Missing Values", FALSE),
                                         tags$hr(),
                                         
                                         #One Hot Encoding
                                         p("Selecting this checkbox will create dummy variables for any character
                                           or factor variables in the dataset. Any ID variables will be dropped"),
                                         # Input: Checkbox to show dependent variable summary stats
                                         checkboxInput("cfn_ohe", "One Hot Encoding", FALSE),
                                         tags$hr(),
                                         
                                         #Select DV
                                         p("The dependent variable must have 2 levels only. If the dependent variable
                                           is factor or character, these will be converted to integer with the less
                                           frequent category considered as the event (1)"),
                                         #Input: Choose dependent variable
                                         uiOutput(outputId = "cfn_select_dv"),
                                         tags$hr(),
                                         
                                         #Confirm Changes
                                         actionButton("data_preprocess_confirm","Confirm Changes"),
                                         
                                         #Refresh View
                                         actionButton("data_preprocess_refresh","Refresh View"),
                                         tags$hr(),
                                         
                                         #Action Button:
                                         actionButton("action_1","Proceed"),
                                         p("Changes made are irreversible once the button is clicked")
                                        ),
                                       # Main panel for displaying outputs ----
                                       mainPanel(
                                         tabsetPanel(
                                           id = "data_preprocess_tabs",
                                           type = "tabs",
                                           tabPanel("Data Head", withSpinner(tableOutput("data_preprocess_output"))),
                                           tabPanel("Data Structure", withSpinner(dataTableOutput("cfn_data_structure_output")))
                                         )
                                        )
                                     )
                                  ),
                                  
                                  #EDA Section
                                  conditionalPanel(
                                    condition="output.data_preprocess_output != null && input.action_1 > 0",
                                    h2("EDA"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                        p("Select the independent variables in this dropdown menu to get the 
                                          distribution and plot of the independent variables"),
                                        #Input: Choose independent variable to plot graph
                                        uiOutput(outputId = "cfn_select_iv"),
                                        tags$hr(),
                                        
                                        p("Select the type of plot for continuous variables"),
                                        #Radio buttons to select variables
                                        radioButtons("cfn_cont_plot_type","Select plot type",
                                                     c("Continuous: Density Plot" = "density",
                                                       "Continuous: Histogram" = "histogram",
                                                       "Continuous: Box Plot" = "boxplot")
                                        ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_cont_plot_type == 'histogram'",
                                          numericInput("bins", "Number of bins for histogram:",min = 10, max = 200, step = 1, value = 30)
                                        ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_cont_plot_type == 'boxplot'",
                                          numericInput("sample", "Percentage of dataset to show on boxplot:",min = 0.01, max = 0.2, step = 0.01, value = 0.1)
                                        ),
                                        tags$hr(),
                                        
                                        p("Select the type of plot for categorical variables"),
                                        #Radio buttons to select variables
                                        radioButtons("cfn_cat_plot_type","Select plot type",
                                                     c("Categorical: Bar Plot" = "bar",
                                                       "Categorical: Stacked Bar Plot" = "stacked_bar",
                                                       "Categorical: Pie Chart" = "pie")
                                        ),
                                        tags$hr(),
                                        
                                        #Action Button:
                                        actionButton("action_2","Proceed")
                                        ),
                                      # Main panel for displaying outputs ----
                                      mainPanel(
                                        tabsetPanel(
                                          id = "eda_tabs",
                                          type = "tabs",
                                          tabPanel("Dependent Variable Information", withSpinner(tableOutput("cfn_eda_dv_info_output")),
                                                                                     withSpinner(plotOutput("cfn_eda_dv_info_plot"))),
                                          tabPanel("Variable Summary",withSpinner(verbatimTextOutput("cfn_eda_iv_summary_output"))),
                                          tabPanel("Variable Distribution", withSpinner(plotOutput("cfn_eda_iv_dist_output"))),
                                          tabPanel("Missing Value Summary", div(style="height:500px;",fluidRow(withSpinner(verbatimTextOutput("cfn_eda_missing_summary"))))),
                                          tags$head(tags$style("#cfn_eda_missing_summary{color:black; font-size:14px; font-style:italic; overflow-y:scroll; max-height: 450px; background: ghostwhite;}"))
                                        )
                                      )
                                        )
                                    ),
                                  
                                  #Feature Engineering
                                  conditionalPanel(
                                    condition="output.cfn_eda_dv_info_output != null && input.action_2 != 0",
                                    h2("Feature Engineering"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        uiOutput(outputId = "cfn_fe_select_iv"),
                                        radioButtons("cfn_fe_radio_button","Choose Radio Buttons to Implement Feature Engineering:",
                                                    c("Missing Value Treatment" = "cfn_fe_missing",
                                                      "Frequency / Event Rate of Categorical Variables" = "cfn_fe_cat_replacement",
                                                      "Bin Continuous Variables" = "cfn_fe_bins",
                                                      "Text Extraction" = "cfn_fe_text_extraction",
                                                      "Transformations" = "cfn_fe_transformations",
                                                      "Remove Variable" = "cfn_fe_remove_var",
                                                      "One Hot Encode on Dataset" = "cfn_fe_ohe")),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_fe_radio_button == 'cfn_fe_missing'",
                                          radioButtons("cfn_fe_missing_radio_button","Method to Impute Missing Values:",
                                                       c("Replace with Median/Mode" = "cfn_fe_central_tendency",
                                                         "Replace with a specific value" = "cfn_fe_missing_value")),
                                          conditionalPanel(
                                            condition="input.cfn_fe_missing_radio_button == 'cfn_fe_missing_value'",
                                            textInput("cfn_fe_missing_value_input", "Replace With",0),
                                            p("A string replacement on a numeric/integer variable will be ignored")
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_fe_radio_button == 'cfn_fe_cat_replacement'",
                                          radioButtons("cfn_fe_cat_replacement_radio_button","Method to Replace Categorical Values:",
                                                       c("Replace with Frequency" = "cfn_fe_cat_freq",
                                                         "Replace with Event Rate" = "cfn_fe_cat_event_rate"))
                                        ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_fe_radio_button == 'cfn_fe_bins'",
                                          radioButtons("cfn_fe_bins_radio_button","Method to Bin Variable:",
                                                       c("Binning Based on Weight of Evidence" = "cfn_fe_bin_woe",
                                                         "Binning Based on Automatic Cuts" = "cfn_fe_bin_auto_cuts",
                                                         "Binning Based on Manual Cuts (Continuous)" = "cfn_fe_bin_manual_cuts")),
                                          conditionalPanel(
                                            condition="input.cfn_fe_bins_radio_button == 'cfn_fe_bin_woe'",
                                            numericInput("cfn_fe_bin_woe_value_input", "Choose Number of Bins:", 10, min = 2, max = 100, step = 1)
                                          ),
                                          conditionalPanel(
                                            condition="input.cfn_fe_bins_radio_button == 'cfn_fe_bin_auto_cuts'",
                                            numericInput("cfn_fe_bin_auto_cuts_value_input", "Choose Number of Bins:", 10, min = 2, max = 100, step = 1)
                                          ),
                                          conditionalPanel(
                                            condition="input.cfn_fe_bins_radio_button == 'cfn_fe_bin_manual_cuts'",
                                            textInput("cfn_fe_bin_manual_cuts_value_input", "Specify Manual Bin Ranges:", "-Inf,0,1,2,3,Inf")
                                          )
                                        ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_fe_radio_button == 'cfn_fe_text_extraction'",
                                          radioButtons("cfn_fe_text_extract_radio_button","Method to Extract Text:",
                                                       c("Text Contains" = "cfn_fe_text_contains",
                                                         "Text Equals" = "cfn_fe_text_equals")),
                                          textInput("cfn_fe_text_extraction_value_input", "Specify Text to Extract (Case Sensitive):", "foo bar")
                                        ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_fe_radio_button == 'cfn_fe_transformations'",
                                          radioButtons("cfn_fe_transformations_radio_button","Choose Transformation Type:",
                                                       c("Log Transformation" = "log",
                                                         "Exp Transformation" = "exp",
                                                         "1/X Transformation" = "inv",
                                                         "X^2 Transformation" = "square"))
                                        ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_fe_radio_button == 'cfn_fe_remove_var'",
                                          radioButtons("cfn_fe_remove_var_radio_button","Method to Remove Variable:",
                                                       c("Based on Dropdown Menu" = "cfn_fe_remove_var_dropdown",
                                                         "By Exact Name" = "cfn_fe_remove_var_name")),
                                          textInput("cfn_fe_remove_var_value_input", "Specify the variable to remove:", "var_123")
                                        ),
                                        
                                        actionButton("cfn_fe_action", "Make Feature Changes"),
                                        tags$hr(),
                                        p("One hot encoding will be implemented on the dataset if not done already
                                          for the modeling phase"),
                                        actionButton("action_3","Proceed To Variable Selection")
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          id = "feature_engineering_tabs",
                                          type = "tabs",
                                          tabPanel("Data Head", withSpinner(tableOutput("cfn_fe_output"))),
                                          tabPanel("QC Status", withSpinner(verbatimTextOutput("cfn_fe_qc")))
                                        )
                                      )
                                    )

                                  ),
                                  
                                  #Variable selection
                                  conditionalPanel(
                                    condition="output.cfn_fe_output != null && input.action_3 != 0 && output.cfn_fe_qc != null",
                                    h2("Variable Selection"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                        #Input: Choose independent variable to view correlation
                                        uiOutput(outputId = "cfn_vs_select_iv_corr"),
                                        actionButton("cfn_vs_compute_corr","Compute Correlation"),
                                        tags$hr(),
                                        
                                        p("Click this button to get the information value scores for the independent variables"),
                                        actionButton("cfn_vs_compute_iv", "Compute Information Value"),
                                        tags$hr(),
                                        
                                        p("Click this button to get the anova/t-test significance of the independent variables"),
                                        actionButton("cfn_vs_compute_ttest_anova", "Compute T-Test/Anova"),
                                        tags$hr(),
                                        
                                        p("Click this button to view the statistical signifiance of the independent
                                          variables as determined by running a logistic model, rotating each independent 
                                          variable one at a time"),
                                        actionButton("cfn_vs_compute_ss", "Compute Statistical Significance"),
                                        # tags$div(style = "padding:10px"),
                                        tags$hr(),
                                        
                                        selectInput("cfn_vs_select_method","Variable Selection Method:",
                                                    c("Select all/some variables manually" = "manual",
                                                      "Use information value to select variables" = "iv",
                                                      "Use T-Test/Anova to select variables" = "ttest_anova",
                                                      "Run logistic regression to select variables" = "logit")),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_vs_select_method == 'iv'",
                                          sliderInput("iv_range", "Select Information Value Range:",min = 0, max = 2, step = 0.01, value = c(0.1,0.5)),
                                          p("Variables within the information value range will be selected
                                            for the model building step"),
                                          tags$hr()
                                        ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_vs_select_method == 'ttest_anova'",
                                          numericInput("cfn_ttest_anova_alpha", "Select Alpha Level:",min = 0.01, max = 0.1, step = 0.01, value = 0.05),
                                          p("Variables which have p-values less than the alpha threshold will be selected
                                            for the model building step"),
                                          tags$hr()
                                          ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_vs_select_method == 'manual'",
                                          uiOutput(outputId = "cfn_vs_select_vars_manual_dropdown"),
                                          p("Variables chosen with the dropdown will be selected for the model
                                            building step"),
                                          tags$hr()
                                        ),
                                        
                                        conditionalPanel(
                                          condition="input.cfn_vs_select_method == 'logit'",
                                          p("Variables which have p-values less than the alpha threshold of 0.01 will be selected for the model
                                            building step"),
                                          tags$hr()
                                          ),
                                        
                                        actionButton("action_4","Filter Variables & Proceed")
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          id = "variable_selection_tabs",
                                          type = "tabs",
                                          tabPanel("Data Head", withSpinner(tableOutput("cfn_vs_output"))),
                                          tabPanel("Variable Correlation",withSpinner(dataTableOutput("cfn_vs_corr_output"))),
                                          tabPanel("Information Value", withSpinner(dataTableOutput("iv_vs_output"))),
                                          tabPanel("T-Test/Anova", withSpinner(dataTableOutput("cfn_ttest_anova_vs_output"))),
                                          tabPanel("Statistical Significance", withSpinner(dataTableOutput("ss_vs_output")))
                                        )
                                      )
                                    )
                                  ),
                                  
                                  #Model Building
                                  conditionalPanel(
                                    condition="output.cfn_vs_output != null && input.action_4 != 0",
                                    h2("XGBoost Model Build"),
                                    
                                    wellPanel(
                                      fluidRow(
                                        column(3,sliderInput("cfn_mb_xgb_train_split", "Select Train Split:",min = 0.5, max = 0.9, step = 0.05, value = 0.8)),                                       
                                        column(3,selectInput("cfn_mb_xgb_eval_metric", "Choose Evaluation Metric:", 
                                                            c("AUC" = "auc",
                                                              "Log Loss" = "logloss"), selected = "AUC")),
                                        column(3,selectInput("cfn_mb_xgb_param_booster", "Choose Booster Type:", 
                                                             c("GBTree" = "gbtree",
                                                               "Dart" = "dart"), selected = "GBTree")),
                                        column(3,selectInput("cfn_mb_xgb_param_tree.method", "Choose Tree Method:", 
                                                             c("Auto" = "auto",
                                                               "Exact" = "exact",
                                                               "Approx" = "approx"), selected = "Auto"))
                                      ),
                                      fluidRow(
                                        column(3,numericInput("cfn_mb_xgb_param_eta", "Choose ETA value:", min = 0.0001, max = 0.2, step = 0.0001, value = 0.01)),
                                        column(3,numericInput("cfn_mb_xgb_param_max.depth", "Choose Max Depth:", min = 2, max = 12, step = 1, value = 6)),
                                        column(3,numericInput("cfn_mb_xgb_param_min.child.weight", "Choose Min Child Weight:", min = 0, max = 10, step = 1, value = 1)),
                                        column(3,numericInput("cfn_mb_xgb_param_max.delta.step", "Choose Max Delta Step:", min = 0, max = 10, step = 1, value = 0))
                                      ),
                                      fluidRow(
                                        column(3,numericInput("cfn_mb_xgb_param_subsample", "Choose Subsample Ratio:", min = 0.1, max = 1, step = 0.05, value = 0.5)),
                                        column(3,numericInput("cfn_mb_xgb_param_colsample.by.tree", "Choose Colsample By Tree Ratio:", min = 0, max = 1, step = 0.05, value = 1)),
                                        column(3,numericInput("cfn_mb_xgb_param_alpha", "Choose Alpha:", min = 0, max = 10, step = 1, value = 0)),
                                        column(3,numericInput("cfn_mb_xgb_param_lambda", "Choose Lambda:", min = 0, max = 10, step = 1, value = 1))
                                      ),
                                      fluidRow(
                                        column(3,numericInput("cfn_mb_xgb_param_gamma", "Choose Gamma:", min = 0, max = 10, step = 1, value = 0)),
                                        column(3,numericInput("cfn_mb_xgb_param_nrounds", "Choose Number of Rounds:", min = 10, max = 2000, step = 1, value = 100)),
                                        column(3,numericInput("cfn_mb_xgb_param_early.stopping.rounds", "Choose Early Stopping Rounds:", min = 1, max = 40, step = 1, value = 10)),
                                        column(3,numericInput("cfn_mb_xgb_param_print.every.n", "Print After Every N Trees:", min = 1, max = 50, step = 1, value = 5))
                                      ),
                                      fluidRow(
                                        column(4,actionButton("cfn_mb_xgb_compute", "Build Model"))
                                      )
                                    ),
                                    
                                    tabsetPanel(
                                      id = "xgb_model_build_tabs",
                                      type = "tabs",
                                      tabPanel("Model Output", div(style="height:500px;",fluidRow(verbatimTextOutput("cfn_mb_xgb_output", placeholder = FALSE)))),
                                      tags$head(tags$style("#cfn_mb_xgb_output{color:black; font-size:14px; font-style:italic; overflow-y:scroll; max-height: 450px; background: ghostwhite;}")),
                                      tabPanel("Train Lift", div(style="height:500px;",fluidRow(tableOutput("cfn_mb_xgb_lift_train")))),
                                      tabPanel("Train Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("cfn_mb_xgb_stats_train", placeholder = FALSE)))),
                                      tabPanel("Validation Lift", div(style="height:500px;",fluidRow(tableOutput("cfn_mb_xgb_lift_valid")))),
                                      tabPanel("Validation Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("cfn_mb_xgb_stats_valid", placeholder = FALSE)))),
                                      tabPanel("Test Lift", div(style="height:500px;",fluidRow(tableOutput("cfn_mb_xgb_lift_test")))),
                                      tabPanel("Test Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("cfn_mb_xgb_stats_test", placeholder = FALSE))))
                                    )
                                  ),
                                    
                                    conditionalPanel(
                                      condition = "output.cfn_mb_xgb_stats_train != null && input.cfn_mb_xgb_compute != 0",
                                      
                                      wellPanel(
                                        fluidRow(
                                          column(3,radioButtons("cfn_mb_xgb_varimp","Choose Variable Importance By:",
                                                                c("Gain" = "gain",
                                                                  "Cover" = "cover"))),
                                          column(3,uiOutput("cfn_mb_xgb_varimp_iv")),
                                          column(3,actionButton("cfn_mb_xgb_varimp_plot","Generate Variable Importance Plot"))
                                        ),
                                        fluidRow(
                                          column(3,sliderInput("cfn_mb_xgb_pdp_train_split", "Select Train Ratio:",min = 0.1, max = 1, step = 0.05, value = 0.3)),
                                          column(2,radioButtons("cfn_pdp_select_method","Choose PDP Variables:",
                                                      c("Manually" = "manual",
                                                        "From model's feature importance" = "importance"))),
                                          conditionalPanel(
                                            condition = "input.cfn_pdp_select_method == 'manual'",
                                            column(3,uiOutput(outputId = "cfn_mb_xgb_pdp_vars_manual"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.cfn_pdp_select_method == 'importance'",
                                            column(3,uiOutput(outputId = "cfn_mb_xgb_pdp_vars"))
                                          ),
                                          column(2,actionButton("cfn_mb_xgb_gen_pdp","Generate PDP")),
                                          column(2,uiOutput("cfn_mb_xgb_pdp_iv"))
                                        ),
                                        fluidRow(
                                          column(3,downloadButton("cfn_mb_xgb_report","Download Model Report")),
                                          column(3,downloadButton("cfn_mb_xgb_score","Score Model in SAS")),
                                          column(3,downloadButton("cfn_mb_xgb_test_prob","Download Predictions for Test Data")),
                                          conditionalPanel(
                                            condition = "input.cfn_mb_xgb_compute > 0",
                                            column(3,downloadButton("cfn_mb_xgb_download","Download Model As RData"))
                                          )
                                        )
                                      ),
                                      
                                      tabsetPanel(
                                        id = "xgb_model_interpretation_tabs",
                                        type = "tabs",
                                        tabPanel("Variable Importance", div(style="height:800px;",fluidRow(plotOutput("cfn_mb_xgb_var_imp")))),
                                        tabPanel("Partial Dependence Plots", div(style="height:800px;",fluidRow(
                                          verbatimTextOutput("cfn_mb_xgb_var_summary"),
                                          tags$div(style = "padding:10px"),
                                          column(6,plotOutput("cfn_mb_xgb_pdp_plot", width = "100%")),
                                          column(6,plotOutput("cfn_mb_xgb_density_plot", width = "100%")))
                                        ))
                                      )
                                    )
                                   
                                  ),
                          
                          #Create Model Sub Tab 2       
                          tabPanel("Regression",
                                   
                                   h2("Upload Data"),
                                   
                                   # Sidebar layout with input and output definitions ----
                                   sidebarLayout(
                                     
                                     # Sidebar panel for inputs ----
                                     sidebarPanel(
                                       
                                       # Input: Select a file ----
                                       textInput("rgn_getwd","Set Working Directory",value = 'C:\\Users\\kbhandari\\OneDrive - Epsilon\\Desktop'),
                                       textInput("rgn_file_name","Specify file name",value = "example.csv"),
                                       
                                       #Input: Options
                                       textInput("rgn_upload_start", "Start", 0),
                                       
                                       textInput("rgn_upload_nrows", "N-Rows", Inf),
                                       
                                       textInput("rgn_upload_na_strings","NA Strings", "NA"),
                                       
                                       # Input: Select separator ----
                                       radioButtons("rgn_sep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                Tab = "\t"),
                                                    selected = ","),
                                       
                                       # Action Button
                                       actionButton("rgn_file1", "Read Train Data as CSV File"),
                                       
                                       tags$div(style = "padding:10px"),
                                       
                                       actionButton("rgn_file2", "Read Test Data as CSV File (Optional)"),
                                       
                                       # Horizontal line ----
                                       tags$hr(),
                                       
                                       p('If you want a sample .csv file to upload,',
                                         'you can first download the sample',
                                         a(href = 'https://gist.github.com/seankross/a412dfbd88b3db70b74b', 'mtcars.csv'),
                                         'file, and then try uploading them.')
                                       
                                     ),
                                     
                                     # Main panel for displaying outputs ----
                                     mainPanel(
                                       # Output: Data file ----
                                       withSpinner(tableOutput("rgn_raw_output"))
                                     )
                                   ),
                                   
                                   #Data Preprocess Section
                                   conditionalPanel(
                                     condition="output.rgn_raw_output != null",
                                     h2("Data Preprocess"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         
                                         #Missing Value Treatment
                                         p("Selecting this checkbox will treat any missing values by 
                                           replacing them with the median or mode wherever appropriate"),
                                         # Input: Checkbox to treat missing values
                                         checkboxInput("rgn_missing", "Treat Missing Values", FALSE),
                                         tags$hr(),
                                         
                                         #One Hot Encoding
                                         p("Selecting this checkbox will create dummy variables for any character
                                           or factor variables in the dataset. Any ID variables will be dropped"),
                                         # Input: Checkbox to show dependent variable summary stats
                                         checkboxInput("rgn_ohe", "One Hot Encoding", FALSE),
                                         tags$hr(),
                                         
                                         #Select DV
                                         p("The dependent variable must be continuous. If the dependent variable is
                                           found to be binary or factor, the program will not proceed."),
                                         #Input: Choose dependent variable
                                         uiOutput(outputId = "rgn_select_dv"),
                                         tags$hr(),
                                         
                                         #Confirm Changes
                                         actionButton("rgn_data_preprocess_confirm","Confirm Changes"),
                                         
                                         #Refresh View
                                         actionButton("rgn_data_preprocess_refresh","Refresh View"),
                                         tags$hr(),
                                         
                                         #Action Button:
                                         actionButton("rgn_action_1","Proceed"),
                                         p("Changes made are irreversible once the button is clicked")
                                         ),
                                       # Main panel for displaying outputs ----
                                       mainPanel(
                                         tabsetPanel(
                                           id = "rgn_data_preprocess_tabs",
                                           type = "tabs",
                                           tabPanel("Data Head", withSpinner(tableOutput("rgn_data_preprocess_output"))),
                                           tabPanel("Data Structure", withSpinner(dataTableOutput("rgn_data_structure_output")))
                                         )
                                       )
                                         )
                                     ),
                                   
                                   #EDA Section
                                   conditionalPanel(
                                     condition="output.rgn_data_preprocess_output != null && input.rgn_action_1 > 0",
                                     h2("EDA"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         
                                         p("Select the independent variables in this dropdown menu to get the 
                                           distribution and plot of the independent variables"),
                                         #Input: Choose independent variable to plot graph
                                         uiOutput(outputId = "rgn_select_iv"),
                                         tags$hr(),
                                         
                                         p("Select the type of plot for continuous variables"),
                                         #Radio buttons to select variables
                                         radioButtons("rgn_cont_plot_type","Select plot type",
                                                      c("Continuous: Density Plot" = "density",
                                                        "Continuous: Histogram" = "histogram",
                                                        "Continuous: Box Plot" = "boxplot")
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_cont_plot_type == 'histogram'",
                                           numericInput("rgn_bins", "Number of bins for histogram:",min = 10, max = 200, step = 1, value = 30)
                                         ),
                                         tags$hr(),
                                         
                                         p("Select the type of plot for categorical variables"),
                                         #Radio buttons to select variables
                                         radioButtons("rgn_cat_plot_type","Select plot type",
                                                      c("Categorical: Bar Plot" = "bar",
                                                        "Categorical: Box Plot" = "cat_boxplot",
                                                        "Categorical: Pie Chart" = "pie")
                                         ),
                                         tags$hr(),
                                         
                                         #Action Button:
                                         actionButton("rgn_action_2","Proceed")
                                         ),
                                       # Main panel for displaying outputs ----
                                       mainPanel(
                                         tabsetPanel(
                                           id = "rgn_eda_tabs",
                                           type = "tabs",
                                           tabPanel("Dependent Variable Information", withSpinner(tableOutput("rgn_eda_dv_info_output")),
                                                    withSpinner(plotOutput("rgn_eda_dv_info_plot"))),
                                           tabPanel("Variable Summary",withSpinner(verbatimTextOutput("rgn_eda_iv_summary_output"))),
                                           tabPanel("Variable Distribution", withSpinner(plotOutput("rgn_eda_iv_dist_output"))),
                                           tabPanel("Missing Value Summary", div(style="height:500px;",fluidRow(withSpinner(verbatimTextOutput("rgn_eda_missing_summary"))))),
                                           tags$head(tags$style("#rgn_eda_missing_summary{color:black; font-size:14px; font-style:italic; overflow-y:scroll; max-height: 450px; background: ghostwhite;}"))
                                         )
                                       )
                                     )
                                   ),
                                   
                                   #Feature Engineering
                                   conditionalPanel(
                                     condition="output.rgn_eda_dv_info_output != null && input.rgn_action_2 != 0",
                                     h2("Feature Engineering"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         uiOutput(outputId = "rgn_fe_select_iv"),
                                         radioButtons("rgn_fe_radio_button","Choose Radio Buttons to Implement Feature Engineering:",
                                                      c("Missing Value Treatment" = "rgn_fe_missing",
                                                        "Frequency / Mean of DV for Categorical Variables" = "rgn_fe_cat_replacement",
                                                        "Bin Continuous Variables" = "rgn_fe_bins",
                                                        "Text Extraction" = "rgn_fe_text_extraction",
                                                        "Transformations" = "rgn_fe_transformations",
                                                        "Remove Variable" = "rgn_fe_remove_var",
                                                        "One Hot Encode on Dataset" = "rgn_fe_ohe")),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_fe_radio_button == 'rgn_fe_missing'",
                                           radioButtons("rgn_fe_missing_radio_button","Method to Impute Missing Values:",
                                                        c("Replace with Median/Mode" = "rgn_fe_central_tendency",
                                                          "Replace with a specific value" = "rgn_fe_missing_value")),
                                           conditionalPanel(
                                             condition="input.rgn_fe_missing_radio_button == 'rgn_fe_missing_value'",
                                             textInput("rgn_fe_missing_value_input", "Replace With",0),
                                             p("A string replacement on a numeric/integer variable will be ignored")
                                           )
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_fe_radio_button == 'rgn_fe_cat_replacement'",
                                           radioButtons("rgn_fe_cat_replacement_radio_button","Method to Replace Categorical Values:",
                                                        c("Replace with Frequency" = "rgn_fe_cat_freq",
                                                          "Replace with Mean of DV" = "rgn_fe_cat_mean"))
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_fe_radio_button == 'rgn_fe_bins'",
                                           radioButtons("rgn_fe_bins_radio_button","Method to Bin Variable:",
                                                        c("Binning Based on Automatic Cuts" = "rgn_fe_bin_auto_cuts",
                                                          "Binning Based on Manual Cuts (Continuous)" = "rgn_fe_bin_manual_cuts")),
                                           conditionalPanel(
                                             condition="input.rgn_fe_bins_radio_button == 'rgn_fe_bin_auto_cuts'",
                                             numericInput("rgn_fe_bin_auto_cuts_value_input", "Choose Number of Bins:", 10, min = 2, max = 100, step = 1)
                                           ),
                                           conditionalPanel(
                                             condition="input.rgn_fe_bins_radio_button == 'rgn_fe_bin_manual_cuts'",
                                             textInput("rgn_fe_bin_manual_cuts_value_input", "Specify Manual Bin Ranges:", "-Inf,0,1,2,3,Inf")
                                           )
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_fe_radio_button == 'rgn_fe_text_extraction'",
                                           radioButtons("rgn_fe_text_extract_radio_button","Method to Extract Text:",
                                                        c("Text Contains" = "rgn_fe_text_contains",
                                                          "Text Equals" = "rgn_fe_text_equals")),
                                           textInput("rgn_fe_text_extraction_value_input", "Specify Text to Extract (Case Sensitive):", "foo bar")
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_fe_radio_button == 'rgn_fe_transformations'",
                                           radioButtons("rgn_fe_transformations_radio_button","Choose Transformation Type:",
                                                        c("Log Transformation" = "log",
                                                          "Exp Transformation" = "exp",
                                                          "1/X Transformation" = "inv",
                                                          "X^2 Transformation" = "square"))
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_fe_radio_button == 'rgn_fe_remove_var'",
                                           radioButtons("rgn_fe_remove_var_radio_button","Method to Remove Variable:",
                                                        c("Based on Dropdown Menu" = "rgn_fe_remove_var_dropdown",
                                                          "By Exact Name" = "rgn_fe_remove_var_name")),
                                           textInput("rgn_fe_remove_var_value_input", "Specify the variable to remove:", "var_123")
                                         ),
                                         
                                         actionButton("rgn_fe_action", "Make Feature Changes"),
                                         tags$hr(),
                                         p("One hot encoding will be implemented on the dataset if not done already
                                           for the modeling phase"),
                                         actionButton("rgn_action_3","Proceed To Variable Selection")
                                         ),
                                       mainPanel(
                                         tabsetPanel(
                                           id = "rgn_feature_engineering_tabs",
                                           type = "tabs",
                                           tabPanel("Data Head", withSpinner(tableOutput("rgn_fe_output")))
                                         )
                                       )
                                     )
                                     ),
                                   
                                   #Variable selection
                                   conditionalPanel(
                                     condition="output.rgn_fe_output != null && input.rgn_action_3 != 0",
                                     h2("Variable Selection"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         
                                         #Input: Choose independent variable to view correlation
                                         uiOutput(outputId = "rgn_vs_select_iv_corr"),
                                         actionButton("rgn_vs_compute_corr","Compute Correlation"),
                                         tags$hr(),
                                         
                                         p("Click this button to get the anova/t-test significance of the independent variables"),
                                         actionButton("rgn_vs_compute_ttest_anova", "Compute T-Test/Anova"),
                                         tags$hr(),
                                         
                                         p("Click this button to view the statistical signifiance of the independent
                                           variables as determined by running a linear regression model, rotating each independent 
                                           variable one at a time"),
                                         
                                         
                                         actionButton("rgn_vs_compute_ss", "Compute Statistical Significance"),
                                         # tags$div(style = "padding:10px"),
                                         tags$hr(),
                                         
                                         selectInput("rgn_vs_select_method","Variable Selection Method:",
                                                     c("Select all/some variables manually" = "manual",
                                                       "Use T-Test/Anova to select variables" = "ttest_anova",
                                                       "Run linear regression to select variables" = "linear")),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_vs_select_method == 'ttest_anova'",
                                           numericInput("ttest_anova_alpha", "Select Alpha Level:",min = 0.01, max = 0.1, step = 0.01, value = 0.05),
                                           p("Variables which have p-values less than the alpha threshold will be selected
                                             for the model building step"),
                                           tags$hr()
                                           ),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_vs_select_method == 'manual'",
                                           uiOutput(outputId = "rgn_vs_select_vars_manual_dropdown"),
                                           p("Variables chosen with the dropdown will be selected for the model
                                             building step"),
                                           tags$hr()
                                           ),
                                         
                                         conditionalPanel(
                                           condition="input.rgn_vs_select_method == 'linear'",
                                           p("Variables which have p-values less than the alpha threshold of 0.01 will be selected for the model
                                             building step"),
                                           tags$hr()
                                           ),
                                         
                                         actionButton("rgn_action_4","Filter Variables & Proceed")
                                         ),
                                       mainPanel(
                                         tabsetPanel(
                                           id = "rgn_variable_selection_tabs",
                                           type = "tabs",
                                           tabPanel("Data Head", withSpinner(tableOutput("rgn_vs_output"))),
                                           tabPanel("Variable Correlation",withSpinner(dataTableOutput("rgn_vs_corr_output"))),
                                           tabPanel("T-Test/Anova", withSpinner(dataTableOutput("ttest_anova_vs_output"))),
                                           tabPanel("Statistical Significance", withSpinner(dataTableOutput("rgn_ss_vs_output")))
                                         )
                                       )
                                         )
                                       ),
                                   
                                   #Model Building
                                   conditionalPanel(
                                     condition="output.rgn_vs_output != null && input.rgn_action_4 != 0",
                                     h2("XGBoost Model Build"),
                                     
                                     wellPanel(
                                       fluidRow(
                                         column(3,sliderInput("rgn_mb_xgb_train_split", "Select Train Split:",min = 0.5, max = 0.9, step = 0.05, value = 0.8)),                                       
                                         column(3,selectInput("rgn_mb_xgb_eval_metric", "Choose Evaluation Metric:", 
                                                              c("RMSE" = "rmse",
                                                                "MAE" = "mae"), selected = "RMSE")),
                                         column(3,selectInput("rgn_mb_xgb_param_booster", "Choose Booster Type:", 
                                                              c("GBLinear" = "gblinear",
                                                                "GBTree" = "gbtree",
                                                                "Dart" = "dart"), selected = "GBLinear")),
                                         column(3,selectInput("rgn_mb_xgb_param_tree.method", "Choose Tree Method:", 
                                                              c("Auto" = "auto",
                                                                "Exact" = "exact",
                                                                "Approx" = "approx"), selected = "Auto"))
                                       ),
                                       fluidRow(
                                         column(3,numericInput("rgn_mb_xgb_param_eta", "Choose ETA value:", min = 0.0001, max = 0.2, step = 0.0001, value = 0.01)),
                                         column(3,numericInput("rgn_mb_xgb_param_max.depth", "Choose Max Depth:", min = 2, max = 12, step = 1, value = 6)),
                                         column(3,numericInput("rgn_mb_xgb_param_min.child.weight", "Choose Min Child Weight:", min = 0, max = 10, step = 1, value = 1)),
                                         column(3,numericInput("rgn_mb_xgb_param_max.delta.step", "Choose Max Delta Step:", min = 0, max = 10, step = 1, value = 0))
                                       ),
                                       fluidRow(
                                         column(3,numericInput("rgn_mb_xgb_param_subsample", "Choose Subsample Ratio:", min = 0.1, max = 1, step = 0.05, value = 0.5)),
                                         column(3,numericInput("rgn_mb_xgb_param_colsample.by.tree", "Choose Colsample By Tree Ratio:", min = 0, max = 1, step = 0.05, value = 1)),
                                         column(3,numericInput("rgn_mb_xgb_param_alpha", "Choose Alpha:", min = 0, max = 10, step = 1, value = 0)),
                                         column(3,numericInput("rgn_mb_xgb_param_lambda", "Choose Lambda:", min = 0, max = 10, step = 1, value = 1))
                                       ),
                                       fluidRow(
                                         column(3,numericInput("rgn_mb_xgb_param_gamma", "Choose Gamma:", min = 0, max = 10, step = 1, value = 0)),
                                         column(3,numericInput("rgn_mb_xgb_param_nrounds", "Choose Number of Rounds:", min = 10, max = 2000, step = 1, value = 100)),
                                         column(3,numericInput("rgn_mb_xgb_param_early.stopping.rounds", "Choose Early Stopping Rounds:", min = 1, max = 40, step = 1, value = 10)),
                                         column(3,numericInput("rgn_mb_xgb_param_print.every.n", "Print After Every N Trees:", min = 1, max = 50, step = 1, value = 5))
                                       ),
                                       fluidRow(
                                         column(4,actionButton("rgn_mb_xgb_compute", "Build Model"))
                                       )
                                     ),
                                     
                                     tabsetPanel(
                                       id = "rgn_xgb_model_build_tabs",
                                       type = "tabs",
                                       tabPanel("Model Output", div(style="height:500px;",fluidRow(verbatimTextOutput("rgn_mb_xgb_output", placeholder = FALSE)))),
                                       tags$head(tags$style("#rgn_mb_xgb_output{color:black; font-size:14px; font-style:italic; overflow-y:scroll; max-height: 450px; background: ghostwhite;}")),
                                       tabPanel("Train Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("rgn_mb_xgb_stats_train", placeholder = FALSE)))),
                                       tabPanel("Validation Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("rgn_mb_xgb_stats_valid", placeholder = FALSE)))),
                                       tabPanel("Test Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("rgn_mb_xgb_stats_test", placeholder = FALSE))))
                                     )
                                   ),
                                   
                                   conditionalPanel(
                                     condition="output.rgn_mb_xgb_stats_train != null && input.rgn_mb_xgb_compute != 0",
                                     #Model Interpretation
                                     wellPanel(
                                       fluidRow(
                                         column(3,uiOutput("rgn_mb_xgb_varimp_iv")),
                                         column(3,actionButton("rgn_mb_xgb_varimp_plot","Generate Variable Importance Plot"))
                                       ),
                                       fluidRow(
                                         column(3,sliderInput("rgn_mb_xgb_pdp_train_split", "Select Train Ratio:",min = 0.1, max = 1, step = 0.05, value = 0.3)),
                                         column(2,radioButtons("rgn_pdp_select_method","Choose PDP Variables:",
                                                               c("Manually" = "manual",
                                                                 "From model's feature importance" = "importance"))),
                                         conditionalPanel(
                                           condition = "input.rgn_pdp_select_method == 'manual'",
                                           column(3,uiOutput(outputId = "rgn_mb_xgb_pdp_vars_manual"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.rgn_pdp_select_method == 'importance'",
                                           column(3,uiOutput(outputId = "rgn_mb_xgb_pdp_vars"))
                                         ),
                                         column(2,actionButton("rgn_mb_xgb_gen_pdp","Generate PDP")),
                                         column(2,uiOutput("rgn_mb_xgb_pdp_iv"))
                                       ),
                                       fluidRow(
                                         column(3,downloadButton("rgn_mb_xgb_report","Download Model Report")),
                                         column(3,downloadButton("rgn_mb_xgb_score","Score Model in SAS")),
                                         column(3,downloadButton("rgn_mb_xgb_test_prob","Download Predictions for Test Data")),
                                         conditionalPanel(
                                           condition = "input.rgn_mb_xgb_compute > 0",
                                           column(3,downloadButton("rgn_mb_xgb_download","Download Model As RData"))
                                         )
                                       )
                                     ),
                                     
                                     tabsetPanel(
                                       id = "rgn_xgb_model_interpretation_tabs",
                                       type = "tabs",
                                       tabPanel("Variable Importance", div(style="height:800px;",fluidRow(plotOutput("rgn_mb_xgb_var_imp")))),
                                       tabPanel("Partial Dependence Plots", div(style="height:800px;",fluidRow(
                                         verbatimTextOutput("rgn_mb_xgb_var_summary"),
                                         tags$div(style = "padding:10px"),
                                         column(6,plotOutput("rgn_mb_xgb_pdp_plot", width = "100%")),
                                         column(6,plotOutput("rgn_mb_xgb_density_plot", width = "100%")))
                                       ))
                                     )
                                   )
                                   
                          )
                         ),
               
               ############################################################################
               #----------------------Navbar Menu: Use Existing Model----------------------
               ############################################################################
               
               navbarMenu("Use Existing Model",
                          
                 #Use Existing Model Sub Tab 1       
                 tabPanel("Classification",
                          
                          h2("Upload Data"),
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              # Input: Select a file ----
                              textInput("use_cfn_getwd","Set Working Directory",value = 'C:\\Users\\kbhandari\\OneDrive - Epsilon\\Desktop'),
                              textInput("use_cfn_file_name","Specify file name",value = "example.csv"),
                              
                              #Input: Options
                              textInput("use_cfn_upload_start", "Start", 0),
                              
                              textInput("use_cfn_upload_nrows", "N-Rows", Inf),
                              
                              textInput("use_cfn_upload_na_strings","NA Strings", "NA"),
                              
                              # Input: Select separator ----
                              radioButtons("use_cfn_sep", "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","),
                              
                              # Action Button
                              actionButton("use_cfn_file1", "Read Train Data as CSV File"),
                              
                              tags$div(style = "padding:10px"),
                              
                              actionButton("use_cfn_file2", "Read Test Data as CSV File (Optional)"),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Need to add XGBoost Model Input File UI
                              fileInput("use_cfn_file3", "Upload XGBoost Model RData File")
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              tabsetPanel(
                                id = "use_cfn_upload_files_tabs",
                                type = "tabs",
                                tabPanel("Data Head", withSpinner(tableOutput("use_cfn_raw_output"))),
                                tabPanel("Model Feature Names",div(style="height:500px;",fluidRow(verbatimTextOutput("use_cfn_xgb_features", placeholder = FALSE)))),
                                tags$head(tags$style("#use_cfn_xgb_features{color:black; font-size:14px; font-style:italic; overflow-y:scroll; max-height: 450px; background: ghostwhite;}"))
                              )
                            )
                            
                          ),
                          
                          #Data Preprocess Section
                          conditionalPanel(
                            condition="output.use_cfn_raw_output != null && output.use_cfn_xgb_features != null",
                            h2("Data Preprocess"),
                            sidebarLayout(
                              sidebarPanel(
                                
                                #Missing Value Treatment
                                p("Selecting this checkbox will treat any missing values by 
                                  replacing them with the median or mode wherever appropriate"),
                                # Input: Checkbox to treat missing values
                                checkboxInput("use_cfn_missing", "Treat Missing Values", FALSE),
                                tags$hr(),
                                
                                #One Hot Encoding
                                p("Selecting this checkbox will create dummy variables for any character
                                  or factor variables in the dataset. Any ID variables will be dropped"),
                                # Input: Checkbox to show dependent variable summary stats
                                checkboxInput("use_cfn_ohe", "One Hot Encoding", FALSE),
                                tags$hr(),
                                
                                #Select DV
                                p("The dependent variable must have 2 levels only. If the dependent variable
                                  is factor or character, these will be converted to integer with the less
                                  frequent category considered as the event (1)"),
                                #Input: Choose dependent variable
                                uiOutput(outputId = "use_cfn_select_dv"),
                                tags$hr(),
                                
                                #Confirm Changes
                                actionButton("use_cfn_data_preprocess_confirm","Confirm Changes"),
                                
                                #Refresh View
                                actionButton("use_cfn_data_preprocess_refresh","Refresh View"),
                                tags$hr(),
                                
                                #Action Button:
                                actionButton("use_cfn_action_1","Proceed"),
                                p("Changes made are irreversible once the button is clicked")
                                ),
                              # Main panel for displaying outputs ----
                              mainPanel(
                                tabsetPanel(
                                  id = "use_cfn_data_preprocess_tabs",
                                  type = "tabs",
                                  tabPanel("Data Head", withSpinner(tableOutput("use_cfn_data_preprocess_output"))),
                                  tabPanel("Data Structure", withSpinner(dataTableOutput("use_cfn_data_structure_output")))
                                )
                              )
                                )
                            ),
                          
                          #Feature Engineering
                          conditionalPanel(
                            condition="output.use_cfn_data_preprocess_output != null && input.use_cfn_action_1 != 0",
                            h2("Feature Engineering"),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput(outputId = "use_cfn_fe_select_iv"),
                                radioButtons("use_cfn_fe_radio_button","Choose Radio Buttons to Implement Feature Engineering:",
                                             c("Missing Value Treatment" = "cfn_fe_missing",
                                               "Frequency / Event Rate of Categorical Variables" = "cfn_fe_cat_replacement",
                                               "Bin Continuous Variables" = "cfn_fe_bins",
                                               "Text Extraction" = "cfn_fe_text_extraction",
                                               "Transformations" = "cfn_fe_transformations",
                                               "Remove Variable" = "cfn_fe_remove_var",
                                               "One Hot Encode on Dataset" = "cfn_fe_ohe")),
                                
                                conditionalPanel(
                                  condition="input.use_cfn_fe_radio_button == 'cfn_fe_missing'",
                                  radioButtons("use_cfn_fe_missing_radio_button","Method to Impute Missing Values:",
                                               c("Replace with Median/Mode" = "cfn_fe_central_tendency",
                                                 "Replace with a specific value" = "cfn_fe_missing_value")),
                                  conditionalPanel(
                                    condition="input.use_cfn_fe_missing_radio_button == 'cfn_fe_missing_value'",
                                    textInput("use_cfn_fe_missing_value_input", "Replace With",0),
                                    p("A string replacement on a numeric/integer variable will be ignored")
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_cfn_fe_radio_button == 'cfn_fe_cat_replacement'",
                                  radioButtons("use_cfn_fe_cat_replacement_radio_button","Method to Replace Categorical Values:",
                                               c("Replace with Frequency" = "cfn_fe_cat_freq",
                                                 "Replace with Event Rate" = "cfn_fe_cat_event_rate"))
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_cfn_fe_radio_button == 'cfn_fe_bins'",
                                  radioButtons("use_cfn_fe_bins_radio_button","Method to Bin Continuous Variable:",
                                               c("Binning Based on Weight of Evidence" = "cfn_fe_bin_woe",
                                                 "Binning Based on Automatic Cuts" = "cfn_fe_bin_auto_cuts",
                                                 "Binning Based on Manual Cuts" = "cfn_fe_bin_manual_cuts")),
                                  conditionalPanel(
                                    condition="input.use_cfn_fe_bins_radio_button == 'cfn_fe_bin_woe'",
                                    numericInput("use_cfn_fe_bin_woe_value_input", "Choose Number of Bins:", 10, min = 2, max = 100, step = 1)
                                  ),
                                  conditionalPanel(
                                    condition="input.use_cfn_fe_bins_radio_button == 'cfn_fe_bin_auto_cuts'",
                                    numericInput("use_cfn_fe_bin_auto_cuts_value_input", "Choose Number of Bins:", 10, min = 2, max = 100, step = 1)
                                  ),
                                  conditionalPanel(
                                    condition="input.use_cfn_fe_bins_radio_button == 'cfn_fe_bin_manual_cuts'",
                                    textInput("use_cfn_fe_bin_manual_cuts_value_input", "Specify Manual Bin Ranges:", "-Inf,0,1,2,3,Inf")
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_cfn_fe_radio_button == 'cfn_fe_text_extraction'",
                                  radioButtons("use_cfn_fe_text_extract_radio_button","Method to Extract Text:",
                                               c("Text Contains" = "cfn_fe_text_contains",
                                                 "Text Equals" = "cfn_fe_text_equals")),
                                  textInput("use_cfn_fe_text_extraction_value_input", "Specify Text to Extract (Case Sensitive):", "foo bar")
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_cfn_fe_radio_button == 'cfn_fe_transformations'",
                                  radioButtons("use_cfn_fe_transformations_radio_button","Choose Transformation Type:",
                                               c("Log Transformation" = "log",
                                                 "Exp Transformation" = "exp",
                                                 "1/X Transformation" = "inv",
                                                 "X^2 Transformation" = "square"))
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_cfn_fe_radio_button == 'cfn_fe_remove_var'",
                                  radioButtons("use_cfn_fe_remove_var_radio_button","Method to Remove Variable:",
                                               c("Based on Dropdown Menu" = "cfn_fe_remove_var_dropdown",
                                                 "By Exact Name" = "cfn_fe_remove_var_name")),
                                  textInput("use_cfn_fe_remove_var_value_input", "Specify the variable to remove:", "var_123")
                                ),
                                
                                actionButton("use_cfn_fe_action", "Make Feature Changes"),
                                tags$hr(),
                                p("One hot encoding will be implemented on the dataset if not done already
                                  for the modeling phase"),
                                actionButton("use_cfn_action_3","Proceed To Model Interpretation")
                                ),
                              mainPanel(
                                tabsetPanel(
                                  id = "use_cfn_feature_engineering_tabs",
                                  type = "tabs",
                                  tabPanel("Data Head", withSpinner(tableOutput("use_cfn_fe_output"))),
                                  tabPanel("Feature Name Check", withSpinner(verbatimTextOutput("use_cfn_fe_output_qc")))
                                )
                              )
                            )
                            
                            ),
                          
                          conditionalPanel(
                            condition = "output.use_cfn_fe_output != null && input.use_cfn_action_3 != 0 && output.use_cfn_fe_output_qc != null",
                            
                            h2("Model Interpretation"),
                            wellPanel(
                              fluidRow(
                                column(3,radioButtons("use_cfn_mb_xgb_varimp","Choose Variable Importance By:",
                                                      c("Gain" = "gain",
                                                        "Cover" = "cover"))),
                                column(3,uiOutput("use_cfn_mb_xgb_varimp_iv")),
                                column(3,actionButton("use_cfn_mb_xgb_varimp_plot","Generate Variable Importance Plot"))
                              ),
                              fluidRow(
                                column(3,sliderInput("use_cfn_mb_xgb_pdp_train_split", "Select Train Ratio:",min = 0.1, max = 1, step = 0.05, value = 0.3)),
                                column(2,radioButtons("use_cfn_pdp_select_method","Choose PDP Variables:",
                                                      c("Manually" = "manual",
                                                        "From model's feature importance" = "importance"))),
                                conditionalPanel(
                                  condition = "input.use_cfn_pdp_select_method == 'manual'",
                                  column(3,uiOutput(outputId = "use_cfn_mb_xgb_pdp_vars_manual"))
                                ),
                                conditionalPanel(
                                  condition = "input.use_cfn_pdp_select_method == 'importance'",
                                  column(3,uiOutput(outputId = "use_cfn_mb_xgb_pdp_vars"))
                                ),
                                column(2,actionButton("use_cfn_mb_xgb_gen_pdp","Generate PDP")),
                                column(2,uiOutput("use_cfn_mb_xgb_pdp_iv"))
                              ),
                              fluidRow(
                                column(4,downloadButton("use_cfn_mb_xgb_score","Score Model in SAS")),
                                column(4,downloadButton("use_cfn_mb_xgb_test_prob","Download Predictions for Test Data")),
                                conditionalPanel(
                                  condition = "input.use_cfn_mb_xgb_gen_pdp !=0",
                                  column(4,downloadButton("use_cfn_mb_xgb_report","Download Model Report"))
                                )
                              )
                            ),
                            
                            tabsetPanel(
                              id = "use_cfn_xgb_model_interpretation_tabs",
                              type = "tabs",
                              tabPanel("Variable Importance", div(style="height:800px;",fluidRow(plotOutput("use_cfn_mb_xgb_var_imp")))),
                              tabPanel("Partial Dependence Plots", div(style="height:800px;",fluidRow(
                                verbatimTextOutput("use_cfn_mb_xgb_var_summary"),
                                tags$div(style = "padding:10px"),
                                column(6,plotOutput("use_cfn_mb_xgb_pdp_plot", width = "100%")),
                                column(6,plotOutput("use_cfn_mb_xgb_density_plot", width = "100%"))))),
                              tabPanel("Train Lift", div(style="height:500px;",fluidRow(tableOutput("use_cfn_mb_xgb_lift_train")))),
                              tabPanel("Train Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("use_cfn_mb_xgb_stats_train", placeholder = FALSE)))),
                              tabPanel("Test Lift", div(style="height:500px;",fluidRow(tableOutput("use_cfn_mb_xgb_lift_test")))),
                              tabPanel("Test Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("use_cfn_mb_xgb_stats_test", placeholder = FALSE))))
                            )
                            
                            )
                          
                          ),
                 
                 tabPanel("Regression",
                          h2("Upload Data"),
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              # Input: Select a file ----
                              textInput("use_rgn_getwd","Set Working Directory",value = 'C:\\Users\\kbhandari\\OneDrive - Epsilon\\Desktop'),
                              textInput("use_rgn_file_name","Specify file name",value = "example.csv"),
                              
                              #Input: Options
                              textInput("use_rgn_upload_start", "Start", 0),
                              
                              textInput("use_rgn_upload_nrows", "N-Rows", Inf),
                              
                              textInput("use_rgn_upload_na_strings","NA Strings", "NA"),
                              
                              # Input: Select separator ----
                              radioButtons("use_rgn_sep", "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","),
                              
                              # Action Button
                              actionButton("use_rgn_file1", "Read Train Data as CSV File"),
                              
                              tags$div(style = "padding:10px"),
                              
                              actionButton("use_rgn_file2", "Read Test Data as CSV File (Optional)"),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Need to add XGBoost Model Input File UI
                              fileInput("use_rgn_file3", "Upload XGBoost Model RData File")
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              tabsetPanel(
                                id = "use_rgn_upload_files_tabs",
                                type = "tabs",
                                tabPanel("Data Head", withSpinner(tableOutput("use_rgn_raw_output"))),
                                tabPanel("Model Feature Names",div(style="height:500px;",fluidRow(verbatimTextOutput("use_rgn_xgb_features", placeholder = FALSE)))),
                                tags$head(tags$style("#use_rgn_xgb_features{color:black; font-size:14px; font-style:italic; overflow-y:scroll; max-height: 450px; background: ghostwhite;}"))
                              )
                            )
                            
                          ),
                          
                          #Data Preprocess Section
                          conditionalPanel(
                            condition="output.use_rgn_raw_output != null && output.use_rgn_xgb_features != null",
                            h2("Data Preprocess"),
                            sidebarLayout(
                              sidebarPanel(
                                
                                #Missing Value Treatment
                                p("Selecting this checkbox will treat any missing values by 
                                  replacing them with the median or mode wherever appropriate"),
                                # Input: Checkbox to treat missing values
                                checkboxInput("use_rgn_missing", "Treat Missing Values", FALSE),
                                tags$hr(),
                                
                                #One Hot Encoding
                                p("Selecting this checkbox will create dummy variables for any character
                                  or factor variables in the dataset. Any ID variables will be dropped"),
                                # Input: Checkbox to show dependent variable summary stats
                                checkboxInput("use_rgn_ohe", "One Hot Encoding", FALSE),
                                tags$hr(),
                                
                                #Select DV
                                p("The dependent variable must have 2 levels only. If the dependent variable
                                  is factor or character, these will be converted to integer with the less
                                  frequent category considered as the event (1)"),
                                #Input: Choose dependent variable
                                uiOutput(outputId = "use_rgn_select_dv"),
                                tags$hr(),
                                
                                #Confirm Changes
                                actionButton("use_rgn_data_preprocess_confirm","Confirm Changes"),
                                
                                #Refresh View
                                actionButton("use_rgn_data_preprocess_refresh","Refresh View"),
                                tags$hr(),
                                
                                #Action Button:
                                actionButton("use_rgn_action_1","Proceed"),
                                p("Changes made are irreversible once the button is clicked")
                                ),
                              # Main panel for displaying outputs ----
                              mainPanel(
                                tabsetPanel(
                                  id = "use_rgn_data_preprocess_tabs",
                                  type = "tabs",
                                  tabPanel("Data Head", withSpinner(tableOutput("use_rgn_data_preprocess_output"))),
                                  tabPanel("Data Structure", withSpinner(dataTableOutput("use_rgn_data_structure_output")))
                                )
                              )
                                )
                            ),
                          
                          #Feature Engineering
                          conditionalPanel(
                            condition="output.use_rgn_data_preprocess_output != null && input.use_rgn_action_1 != 0",
                            h2("Feature Engineering"),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput(outputId = "use_rgn_fe_select_iv"),
                                radioButtons("use_rgn_fe_radio_button","Choose Radio Buttons to Implement Feature Engineering:",
                                             c("Missing Value Treatment" = "use_rgn_fe_missing",
                                               "Frequency / Mean of DV for Categorical Variables" = "use_rgn_fe_cat_replacement",
                                               "Bin Continuous Variables" = "use_rgn_fe_bins",
                                               "Text Extraction" = "use_rgn_fe_text_extraction",
                                               "Transformations" = "use_rgn_fe_transformations",
                                               "Remove Variable" = "use_rgn_fe_remove_var",
                                               "One Hot Encode on Dataset" = "use_rgn_fe_ohe")),
                                
                                conditionalPanel(
                                  condition="input.use_rgn_fe_radio_button == 'use_rgn_fe_missing'",
                                  radioButtons("use_rgn_fe_missing_radio_button","Method to Impute Missing Values:",
                                               c("Replace with Median/Mode" = "use_rgn_fe_central_tendency",
                                                 "Replace with a specific value" = "use_rgn_fe_missing_value")),
                                  conditionalPanel(
                                    condition="input.use_rgn_fe_missing_radio_button == 'use_rgn_fe_missing_value'",
                                    textInput("use_rgn_fe_missing_value_input", "Replace With",0),
                                    p("A string replacement on a numeric/integer variable will be ignored")
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_rgn_fe_radio_button == 'use_rgn_fe_cat_replacement'",
                                  radioButtons("use_rgn_fe_cat_replacement_radio_button","Method to Replace Categorical Values:",
                                               c("Replace with Frequency" = "use_rgn_fe_cat_freq",
                                                 "Replace with Mean of DV" = "use_rgn_fe_cat_mean"))
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_rgn_fe_radio_button == 'use_rgn_fe_bins'",
                                  radioButtons("use_rgn_fe_bins_radio_button","Method to Bin Variable:",
                                               c("Binning Based on Automatic Cuts" = "use_rgn_fe_bin_auto_cuts",
                                                 "Binning Based on Manual Cuts (Continuous)" = "use_rgn_fe_bin_manual_cuts")),
                                  conditionalPanel(
                                    condition="input.use_rgn_fe_bins_radio_button == 'use_rgn_fe_bin_auto_cuts'",
                                    numericInput("use_rgn_fe_bin_auto_cuts_value_input", "Choose Number of Bins:", 10, min = 2, max = 100, step = 1)
                                  ),
                                  conditionalPanel(
                                    condition="input.use_rgn_fe_bins_radio_button == 'use_rgn_fe_bin_manual_cuts'",
                                    textInput("use_rgn_fe_bin_manual_cuts_value_input", "Specify Manual Bin Ranges:", "-Inf,0,1,2,3,Inf")
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_rgn_fe_radio_button == 'use_rgn_fe_text_extraction'",
                                  radioButtons("use_rgn_fe_text_extract_radio_button","Method to Extract Text:",
                                               c("Text Contains" = "use_rgn_fe_text_contains",
                                                 "Text Equals" = "use_rgn_fe_text_equals")),
                                  textInput("use_rgn_fe_text_extraction_value_input", "Specify Text to Extract (Case Sensitive):", "foo bar")
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_rgn_fe_radio_button == 'use_rgn_fe_transformations'",
                                  radioButtons("use_rgn_fe_transformations_radio_button","Choose Transformation Type:",
                                               c("Log Transformation" = "log",
                                                 "Exp Transformation" = "exp",
                                                 "1/X Transformation" = "inv",
                                                 "X^2 Transformation" = "square"))
                                ),
                                
                                conditionalPanel(
                                  condition="input.use_rgn_fe_radio_button == 'use_rgn_fe_remove_var'",
                                  radioButtons("use_rgn_fe_remove_var_radio_button","Method to Remove Variable:",
                                               c("Based on Dropdown Menu" = "use_rgn_fe_remove_var_dropdown",
                                                 "By Exact Name" = "use_rgn_fe_remove_var_name")),
                                  textInput("use_rgn_fe_remove_var_value_input", "Specify the variable to remove:", "var_123")
                                ),
                                
                                actionButton("use_rgn_fe_action", "Make Feature Changes"),
                                tags$hr(),
                                p("One hot encoding will be implemented on the dataset if not done already
                                  for the modeling phase"),
                                actionButton("use_rgn_action_3","Proceed To Model Interpretation")
                                ),
                              mainPanel(
                                tabsetPanel(
                                  id = "use_rgn_feature_engineering_tabs",
                                  type = "tabs",
                                  tabPanel("Data Head", withSpinner(tableOutput("use_rgn_fe_output"))),
                                  tabPanel("Feature Name Check", withSpinner(verbatimTextOutput("use_rgn_fe_output_qc")))
                                )
                              )
                            )
                            ),
                          
                          conditionalPanel(
                            condition = "output.use_rgn_fe_output != null && input.use_rgn_action_3 != 0 && output.use_rgn_fe_output_qc != null",
                            
                            h2("Model Interpretation"),
                            wellPanel(
                              fluidRow(
                                column(3,uiOutput("use_rgn_mb_xgb_varimp_iv")),
                                column(3,actionButton("use_rgn_mb_xgb_varimp_plot","Generate Variable Importance Plot"))
                              ),
                              fluidRow(
                                column(3,sliderInput("use_rgn_mb_xgb_pdp_train_split", "Select Train Ratio:",min = 0.1, max = 1, step = 0.05, value = 0.3)),
                                column(2,radioButtons("use_rgn_pdp_select_method","Choose PDP Variables:",
                                                      c("Manually" = "manual",
                                                        "From model's feature importance" = "importance"))),
                                conditionalPanel(
                                  condition = "input.use_rgn_pdp_select_method == 'manual'",
                                  column(3,uiOutput(outputId = "use_rgn_mb_xgb_pdp_vars_manual"))
                                ),
                                conditionalPanel(
                                  condition = "input.use_rgn_pdp_select_method == 'importance'",
                                  column(3,uiOutput(outputId = "use_rgn_mb_xgb_pdp_vars"))
                                ),
                                column(2,actionButton("use_rgn_mb_xgb_gen_pdp","Generate PDP")),
                                column(2,uiOutput("use_rgn_mb_xgb_pdp_iv"))
                              ),
                              fluidRow(
                                column(4,downloadButton("use_rgn_mb_xgb_score","Score Model in SAS")),
                                column(4,downloadButton("use_rgn_mb_xgb_test_prob","Download Predictions for Test Data")),
                                conditionalPanel(
                                  condition = "input.use_rgn_mb_xgb_gen_pdp !=0",
                                  column(4,downloadButton("use_rgn_mb_xgb_report","Download Model Report"))
                                )
                              )
                            ),
                            
                            tabsetPanel(
                              id = "use_rgn_xgb_model_interpretation_tabs",
                              type = "tabs",
                              tabPanel("Variable Importance", div(style="height:800px;",fluidRow(plotOutput("use_rgn_mb_xgb_var_imp")))),
                              tabPanel("Partial Dependence Plots", div(style="height:800px;",fluidRow(
                                verbatimTextOutput("use_rgn_mb_xgb_var_summary"),
                                tags$div(style = "padding:10px"),
                                column(6,plotOutput("use_rgn_mb_xgb_pdp_plot", width = "100%")),
                                column(6,plotOutput("use_rgn_mb_xgb_density_plot", width = "100%"))))),
                              tabPanel("Train Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("use_rgn_mb_xgb_stats_train", placeholder = FALSE)))),
                              tabPanel("Test Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("use_rgn_mb_xgb_stats_test", placeholder = FALSE))))
                            )
                            
                          )
                      )
               ),
               
               #Navigation Tab 4       
               tabPanel("Get The Code",
                        includeMarkdown("code.Rmd")
                        )
    )
  )
)
