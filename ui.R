#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinycssloaders)

# Define UI for application that draws a histogram
shinyUI(fluidPage(shinyjs::useShinyjs(),

    # Application title
    titlePanel(title=div(img(src="Logo.png", height = 100, width = 175), "The Machine Learning Dashboard")),
    
    #Navbar Menu
    navbarPage("Navbar",
               
               #Navigation Tab 1       
               tabPanel("Documentation"),
               
               # Sub Tab 1
               tabPanel("Build Model",
                        
                        h2("Upload Data"),
                        
                        # Sidebar layout with input and output definitions ----
                        sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                                
                                # Input: Select a file ----
                                textInput("bm_getwd","Set Working Directory",value = 'C:\\Users\\kbhandari\\OneDrive - Epsilon\\Desktop\\server'),
                                textInput("bm_file_name","Specify file name",value = "train.csv"),
                                
                                #Input: Options
                                textInput("bm_upload_start", "Start", 0),
                                
                                textInput("bm_upload_nrows", "N-Rows", Inf),
                                
                                textInput("bm_upload_na_strings","NA Strings", "NA"),
                                
                                # Input: Select separator ----
                                radioButtons("bm_sep", "Separator",
                                             choices = c(Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                             selected = ","),
                                
                                # Action Button
                                actionButton("bm_file1", "Read Train Data as CSV File"),
                                
                                tags$div(style = "padding:10px"),
                                
                                actionButton("bm_file2", "Read Test Data as CSV File (Optional)"),
                                
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
                                tabsetPanel(
                                    id = "bm_raw_data_tabs",
                                    type = "tabs",
                                    tabPanel("Train View", withSpinner(tableOutput("bm_train_raw_output"))),
                                    tabPanel("Test View", withSpinner(tableOutput("bm_test_raw_output")))
                                )
                            )
                            
                        ), # End of upload data section
                        
                        #Data Configuration Section
                        h2("Data Configurations"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                #Classification or Regression
                                p("Will this be a classification or regression problem?"),
                                radioButtons("bm_problem_type","Select problem type",
                                             c("Classification" = "classification",
                                               "Regression" = "regression")
                                ),
                                tags$hr(),
                                
                                p("Choose which models to build:"),
                                checkboxInput("bm_data_config_xgboost", "Build XGBoost Model", TRUE),
                                checkboxInput("bm_data_config_linear_logistic", "Build Linear/Logistic Regression Model", TRUE),
                                conditionalPanel(
                                    condition="input.bm_data_config_xgboost == true && input.bm_data_config_linear_logistic == true",
                                    checkboxInput("bm_data_config_ensemble", "Build Ensemble", TRUE)
                                ),
                                tags$hr(),
                                
                                p("Selecting this checkbox will treat any outliers by capping them according to the percentiles"),
                                # Input: Checkbox to treat missing values
                                checkboxInput("bm_data_config_outlier", "Treat Outliers", FALSE),
                                conditionalPanel(
                                    condition="input.bm_data_config_outlier == true",
                                    uiOutput(outputId = "bm_data_config_outlier_vars_placeholder"),
                                    radioButtons("bm_data_config_outlier_capping_lb", "Separator",
                                                 c("P1" = "pct_1",
                                                   "P5" = "pct_5",
                                                   "None" = "none")),
                                    radioButtons("bm_data_config_outlier_capping_ub", "Separator",
                                                 c("P95" = "pct_95",
                                                   "P99" = "pct_99",
                                                   "None" = "none")),
                                    actionButton("bm_data_config_outlier_confirm", "Cap Outliers"),
                                    tags$hr()
                                ),
                                
                                p("Split data into train and validation"),
                                sliderInput("bm_data_config_data_split", "Select Train Split:",min = 0.5, max = 0.9, step = 0.05, value = 0.8),
                                tags$hr(),
                                
                                #Missing Value Treatment
                                p("Selecting this checkbox will treat any missing values by 
                                           replacing them with the median or mode wherever appropriate"),
                                # Input: Checkbox to treat missing values
                                checkboxInput("bm_data_config_missing", "Treat Missing Values", FALSE),
                                tags$hr(),
                                
                                #One Hot Encoding
                                p("Selecting this checkbox will create dummy variables for any character
                                           or factor variables in the dataset. Any ID variables will be dropped"),
                                # Input: Checkbox to show dependent variable summary stats
                                checkboxInput("bm_data_config_ohe", "One Hot Encoding", TRUE),
                                tags$hr(),
                                
                                #Select DV
                                p("The dependent variable must have 2 levels only. If the dependent variable
                                           is factor or character, these will be converted to integer with the less
                                           frequent category considered as the event (1)"),
                                #Input: Choose dependent variable
                                uiOutput(outputId = "bm_select_dv_placeholder"),
                                tags$hr(),
                                
                                #Confirm Changes
                                actionButton("bm_data_config_confirm","Confirm Changes"),
                                
                                #Refresh View
                                actionButton("bm_data_config_refresh","Refresh View"),
                                tags$hr(),
                                
                                #Proceed Button:
                                actionButton("bm_data_config_proceed","Proceed"),
                                tags$hr(),
                                
                                #Undo Button
                                actionButton("bm_data_config_undo","Undo Changes (Get Raw Data Back)")
                                
                            ),
                            # Main panel for displaying outputs ----
                            mainPanel(
                                tabsetPanel(
                                    id = "bm_data_config_tabs",
                                    type = "tabs",
                                    tabPanel("Train View", withSpinner(tableOutput("bm_data_config_train_output"))),
                                    tabPanel("Validation View", withSpinner(tableOutput("bm_data_config_validation_output"))),
                                    tabPanel("Test View", withSpinner(tableOutput("bm_data_config_test_output"))),
                                    tabPanel("Data Shape", withSpinner(tableOutput("bm_data_config_shape"))),
                                    tabPanel("Data Structure", withSpinner(DT::dataTableOutput("bm_data_config_structure"))),
                                    tabPanel("Outlier Summary", withSpinner(DT::dataTableOutput("bm_data_config_outlier_summary")))
                                )
                            )
                        ),
                        
                        #Variable selection
                        h2("Variable Selection"),
                        sidebarLayout(
                            sidebarPanel(
                                
                                #Input: Choose independent variable to view correlation
                                p("Choose independent variable to view correlation"),
                                uiOutput(outputId = "bm_vs_select_iv_correlation_placeholder"),
                                actionButton("bm_vs_compute_correlation","Compute Correlation"),
                                tags$hr(),
                                
                                conditionalPanel(
                                    condition="input.bm_problem_type == 'classification'",
                                    p("Click this button to get the information value scores for the independent variables"),
                                    actionButton("bm_vs_compute_iv", "Compute Information Value"),
                                    tags$hr()
                                    ),
                                
                                p("Click this button to get the anova/t-test significance of the independent variables"),
                                actionButton("bm_vs_compute_ttest_anova", "Compute T-Test/Anova"),
                                tags$hr(),
                                
                                p("Click this button to view the statistical signifiance of the independent
                                      variables as determined by running a logistic/linear regression model, rotating each independent 
                                      variable one at a time"),
                                sliderInput("bm_vs_ss_data_split", "Select Sample Rate:",min = 0.05, max = 0.95, step = 0.05, value = 0.3),
                                actionButton("bm_vs_compute_ss", "Compute Statistical Significance"),
                                tags$hr(),
                                
                                conditionalPanel(
                                    condition="input.bm_problem_type == 'classification'",
                                    selectInput("bm_vs_method","Variable Selection Method:",
                                                c("Select all/some variables manually" = "manual",
                                                  "Use information value to select variables" = "iv",
                                                  "Use T-Test/Anova to select variables" = "ttest_anova",
                                                  "Run logistic/linear regression to select variables" = "regression"))
                                    ),
                                
                                conditionalPanel(
                                    condition="input.bm_problem_type == 'regression'",
                                    selectInput("bm_vs_method","Variable Selection Method:",
                                                c("Select all/some variables manually" = "manual",
                                                  "Use T-Test/Anova to select variables" = "ttest_anova",
                                                  "Run logistic/linear regression to select variables" = "regression"))
                                ),
                                
                                conditionalPanel(
                                    condition="input.bm_vs_method == 'iv'",
                                    sliderInput("bm_vs_iv_range", "Select Information Value Range:",min = 0, max = 2, step = 0.01, value = c(0.1,0.5)),
                                    p("Variables within the information value range will be selected
                                        for the model building step"),
                                    tags$hr()
                                ),
                                
                                conditionalPanel(
                                    condition="input.bm_vs_method == 'ttest_anova'",
                                    numericInput("bm_vs_ttest_anova_alpha", "Select Alpha Level:",min = 0.01, max = 0.1, step = 0.01, value = 0.05),
                                    p("Variables which have p-values less than the alpha threshold will be selected
                                        for the model building step"),
                                    tags$hr()
                                ),
                                
                                conditionalPanel(
                                    condition="input.bm_vs_method == 'manual'",
                                    uiOutput(outputId = "bm_vs_select_vars_manual_dropdown"),
                                    p("Variables chosen with the dropdown will be selected for the model
                                        building step"),
                                    tags$hr()
                                ),
                                
                                conditionalPanel(
                                    condition="input.bm_vs_method == 'regression'",
                                    numericInput("bm_vs_regression_alpha", "Select Alpha Level:",min = 0.01, max = 0.1, step = 0.01, value = 0.05),
                                    p("Variables which have p-values less than the alpha threshold will be selected for the model
                                        building step"),
                                    tags$hr()
                                ),
                                
                                actionButton("bm_vs_proceed","Filter Variables & Proceed"),
                                tags$hr(),
                                
                                actionButton("bm_vs_undo","Undo Changes (Get Data Config Data Back)")
                            ),
                            mainPanel(
                                tabsetPanel(
                                    id = "bm_vs_tabs",
                                    type = "tabs",
                                    tabPanel("Train View", withSpinner(tableOutput("bm_vs_train_output"))),
                                    tabPanel("Variable Correlation",withSpinner(DT::dataTableOutput("bm_vs_correlation_output"))),
                                    tabPanel("Information Value", withSpinner(DT::dataTableOutput("bm_vs_iv_output"))),
                                    tabPanel("T-Test/Anova", withSpinner(DT::dataTableOutput("bm_vs_ttest_anova_output"))),
                                    tabPanel("Statistical Significance", withSpinner(DT::dataTableOutput("bm_vs_ss_output")))
                                )
                            )
                        ), # End of variable selection
                        
                        #EDA Section
                        h2("Exploratory Data Analysis"),
                    
                        sidebarLayout(
                            sidebarPanel(
                                
                                p("Select the independent variables in this dropdown menu to get the 
                                      distribution and plot of the independent variables"),
                                #Input: Choose independent variable to plot graph
                                uiOutput(outputId = "bm_eda_select_iv_placeholder"),
                                tags$hr(),
                                
                                p("Select the type of plot for continuous variables"),
                                #Radio buttons to select variables
                                radioButtons("bm_eda_cont_plot_type","Select plot type",
                                             c("Continuous: Density Plot" = "density",
                                               "Continuous: Histogram" = "histogram",
                                               "Continuous: Box Plot" = "boxplot")
                                ),
                                
                                conditionalPanel(
                                    condition="input.bm_eda_cont_plot_type == 'histogram'",
                                    numericInput("bm_eda_cont_hist_bins", "Number of bins for histogram:",min = 10, max = 200, step = 1, value = 30)
                                ),
                                
                                conditionalPanel(
                                    condition="input.bm_eda_cont_plot_type == 'boxplot'",
                                    numericInput("bm_eda_cont_boxplot_sample", "Percentage of dataset to show on boxplot:",min = 0.01, max = 0.2, step = 0.01, value = 0.1)
                                ),
                                tags$hr(),
                                
                                p("Select the type of plot for categorical variables"),
                                #Radio buttons to select variables
                                radioButtons("bm_eda_cat_plot_type","Select plot type",
                                             c("Categorical: Bar Plot" = "bar",
                                               "Categorical: Stacked Bar Plot" = "stacked_bar",
                                               "Categorical: Pie Chart" = "pie")
                                ),
                                tags$hr(),
                                
                                #Action Button:
                                actionButton("bm_eda_proceed","Proceed")
                            ),
                            # Main panel for displaying outputs ----
                            mainPanel(
                                tabsetPanel(
                                    id = "bm_eda_tabs",
                                    type = "tabs",
                                    tabPanel("Dependent Variable Information", withSpinner(tableOutput("bm_eda_dv_info_table")),
                                             withSpinner(plotOutput("bm_eda_dv_info_plot"))),
                                    tabPanel("Variable Summary",withSpinner(verbatimTextOutput("bm_eda_iv_summary_output"))),
                                    tabPanel("Variable Distribution", withSpinner(plotOutput("bm_eda_iv_dist_output"))),
                                    tabPanel("Missing Value Summary", div(style="height:500px;",fluidRow(withSpinner(verbatimTextOutput("bm_eda_missing_summary"))))),
                                    tags$head(tags$style("#bm_eda_missing_summary{color:black; font-size:14px; font-style:italic; overflow-y:scroll; max-height: 450px; background: ghostwhite;}"))
                                )
                            )
                        ), #End of EDA
                        
                        #Feature Engineering
                            h2("Feature Engineering"),
                            sidebarLayout(
                                sidebarPanel(
                                    uiOutput(outputId = "bm_fe_select_iv_placeholder"),
                                    radioButtons("bm_fe_radio_button","Choose Radio Buttons to Implement Feature Engineering:",
                                                 c("Missing Value Treatment" = "bm_fe_missing",
                                                   "Frequency / Event Rate of Categorical Variables" = "bm_fe_cat_replacement",
                                                   "Bin Continuous Variables" = "bm_fe_bins",
                                                   "Text Extraction" = "bm_fe_text_extraction",
                                                   "Transformations" = "bm_fe_transformations",
                                                   "Remove Variable" = "bm_fe_remove_var",
                                                   "One Hot Encode on Dataset" = "bm_fe_ohe")),
                                    
                                    conditionalPanel(
                                        condition="input.bm_fe_radio_button == 'bm_fe_missing'",
                                        radioButtons("bm_fe_missing_radio_button","Method to Impute Missing Values:",
                                                     c("Replace with Median/Mode" = "bm_fe_central_tendency",
                                                       "Replace with a specific value" = "bm_fe_missing_value")),
                                        conditionalPanel(
                                            condition="input.bm_fe_missing_radio_button == 'bm_fe_missing_value'",
                                            textInput("bm_fe_missing_value_input", "Replace With",0),
                                            p("A string replacement on a numeric/integer variable will be ignored")
                                        )
                                    ),
                                    
                                    conditionalPanel(
                                        condition="input.bm_fe_radio_button == 'bm_fe_cat_replacement'",
                                        radioButtons("bm_fe_cat_replacement_radio_button","Method to Replace Categorical Values:",
                                                     c("Replace with Frequency" = "bm_fe_cat_freq",
                                                       "Replace with Event Rate" = "bm_fe_cat_event_rate"))
                                    ),
                                    
                                    conditionalPanel(
                                        condition="input.bm_fe_radio_button == 'bm_fe_bins'",
                                        radioButtons("bm_fe_bins_radio_button","Method to Bin Variable:",
                                                     c("Binning Based on Weight of Evidence" = "bm_fe_bin_woe",
                                                       "Binning Based on Automatic Cuts" = "bm_fe_bin_auto_cuts",
                                                       "Binning Based on Manual Cuts (Continuous)" = "bm_fe_bin_manual_cuts")),
                                        conditionalPanel(
                                            condition="input.bm_fe_bins_radio_button == 'bm_fe_bin_woe'",
                                            numericInput("bm_fe_bin_woe_value_input", "Choose Number of Bins:", 10, min = 2, max = 100, step = 1)
                                        ),
                                        conditionalPanel(
                                            condition="input.bm_fe_bins_radio_button == 'bm_fe_bin_auto_cuts'",
                                            numericInput("bm_fe_bin_auto_cuts_value_input", "Choose Number of Bins:", 10, min = 2, max = 100, step = 1)
                                        ),
                                        conditionalPanel(
                                            condition="input.bm_fe_bins_radio_button == 'bm_fe_bin_manual_cuts'",
                                            textInput("bm_fe_bin_manual_cuts_value_input", "Specify Manual Bin Ranges:", "-Inf,0,1,2,3,Inf")
                                        )
                                    ),
                                    
                                    conditionalPanel(
                                        condition="input.bm_fe_radio_button == 'bm_fe_text_extraction'",
                                        radioButtons("bm_fe_text_extract_radio_button","Method to Extract Text:",
                                                     c("Text Contains" = "bm_fe_text_contains",
                                                       "Text Equals" = "bm_fe_text_equals")),
                                        textInput("bm_fe_text_extraction_value_input", "Specify Text to Extract (Case Sensitive):", "foo bar")
                                    ),
                                    
                                    conditionalPanel(
                                        condition="input.bm_fe_radio_button == 'bm_fe_transformations'",
                                        radioButtons("bm_fe_transformations_radio_button","Choose Transformation Type:",
                                                     c("Log Transformation" = "log",
                                                       "Exp Transformation" = "exp",
                                                       "1/X Transformation" = "inv",
                                                       "X^2 Transformation" = "square"))
                                    ),
                                    
                                    conditionalPanel(
                                        condition="input.bm_fe_radio_button == 'bm_fe_remove_var'",
                                        radioButtons("bm_fe_remove_var_radio_button","Method to Remove Variable:",
                                                     c("Based on Dropdown Menu" = "bm_fe_remove_var_dropdown",
                                                       "By Exact Name" = "bm_fe_remove_var_name")),
                                        textInput("bm_fe_remove_var_value_input", "Specify the variable to remove:", "var_123")
                                    ),
                                    
                                    actionButton("bm_fe_action", "Make Feature Changes"),
                                    tags$hr(),
                                    
                                    p("One hot encoding will be implemented on the dataset if not done already
                                          for the modeling phase"),
                                    actionButton("bm_fe_proceed","Proceed To Variable Selection"),
                                    tags$hr(),
                                    
                                    actionButton("bm_fe_undo","Undo Changes (Get Data Config Data Back)")
                                ),
                                mainPanel(
                                    tabsetPanel(
                                        id = "bm_fe_tabs",
                                        type = "tabs",
                                        tabPanel("Train Head", withSpinner(tableOutput("bm_fe_train_output"))),
                                        tabPanel("Validation Head", withSpinner(tableOutput("bm_fe_validation_output"))),
                                        tabPanel("Test Head", withSpinner(tableOutput("bm_fe_test_output"))),
                                        tabPanel("QC Status", withSpinner(verbatimTextOutput("bm_fe_qc")))
                                    )
                                )
                            ), #End of feature engineering
                        
                        #XGBoost Model Building
                        h2("XGBoost Model Build"),
                        
                        wellPanel(
                            fluidRow(
                                conditionalPanel(
                                    condition="input.bm_problem_type == 'classification'",
                                    column(4,selectInput("bm_xgb_param_eval_metric", "Choose Evaluation Metric:", 
                                                         c("AUC" = "auc",
                                                           "Log Loss" = "logloss"), selected = "AUC"))
                                ),
                                conditionalPanel(
                                    condition="input.bm_problem_type == 'regression'",
                                    column(4,selectInput("bm_xgb_param_eval_metric", "Choose Evaluation Metric:", 
                                                         c("RMSE" = "rmse",
                                                           "MAE" = "mae"), selected = "RMSE"))
                                ),
                                column(4,selectInput("bm_xgb_param_booster", "Choose Booster Type:", 
                                                     c("GBTree" = "gbtree",
                                                       "Dart" = "dart"), selected = "GBTree")),
                                column(4,selectInput("bm_xgb_param_tree_method", "Choose Tree Method:", 
                                                     c("Auto" = "auto",
                                                       "Exact" = "exact",
                                                       "Approx" = "approx"), selected = "Auto"))
                            ),
                            fluidRow(
                                column(3,numericInput("bm_xgb_param_eta", "Choose ETA value:", min = 0.0001, max = 0.2, step = 0.0001, value = 0.01)),
                                column(3,numericInput("bm_xgb_param_max.depth", "Choose Max Depth:", min = 2, max = 12, step = 1, value = 6)),
                                column(3,numericInput("bm_xgb_param_min.child.weight", "Choose Min Child Weight:", min = 0, max = 10, step = 1, value = 1)),
                                column(3,numericInput("bm_xgb_param_max.delta.step", "Choose Max Delta Step:", min = 0, max = 10, step = 1, value = 0))
                            ),
                            fluidRow(
                                column(3,numericInput("bm_xgb_param_subsample", "Choose Subsample Ratio:", min = 0.1, max = 1, step = 0.05, value = 0.5)),
                                column(3,numericInput("bm_xgb_param_colsample.by.tree", "Choose Colsample By Tree Ratio:", min = 0, max = 1, step = 0.05, value = 1)),
                                column(3,numericInput("bm_xgb_param_alpha", "Choose Alpha:", min = 0, max = 10, step = 1, value = 0)),
                                column(3,numericInput("bm_xgb_param_lambda", "Choose Lambda:", min = 0, max = 10, step = 1, value = 1))
                            ),
                            fluidRow(
                                column(3,numericInput("bm_xgb_param_gamma", "Choose Gamma:", min = 0, max = 10, step = 1, value = 0)),
                                column(3,numericInput("bm_xgb_param_nrounds", "Choose Number of Rounds:", min = 10, max = 2000, step = 1, value = 100)),
                                column(3,numericInput("bm_xgb_param_early.stopping.rounds", "Choose Early Stopping Rounds:", min = 1, max = 40, step = 1, value = 10)),
                                column(3,numericInput("bm_xgb_param_print.every.n", "Print After Every N Trees:", min = 1, max = 50, step = 1, value = 5))
                            ),
                            fluidRow(
                                column(4,actionButton("bm_xgb_compute", "Build Model"))
                            )
                        ),
                        
                        tabsetPanel(
                            id = "bm_xgb_tabs",
                            type = "tabs",
                            tabPanel("Model Output", div(style="height:500px;",fluidRow(verbatimTextOutput("bm_xgb_output", placeholder = FALSE)))),
                            tags$head(tags$style("#bm_xgb_output{color:black; font-size:14px; font-style:italic; overflow-y:scroll; max-height: 450px; background: ghostwhite;}")),
                            tabPanel("Train Lift", div(style="height:500px;",fluidRow(tableOutput("bm_xgb_lift_train")))),
                            tabPanel("Train Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("bm_xgb_stats_train", placeholder = FALSE)))),
                            tabPanel("Validation Lift", div(style="height:500px;",fluidRow(tableOutput("bm_xgb_lift_valid")))),
                            tabPanel("Validation Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("bm_xgb_stats_valid", placeholder = FALSE)))),
                            tabPanel("Test Lift", div(style="height:500px;",fluidRow(tableOutput("bm_xgb_lift_test")))),
                            tabPanel("Test Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("bm_xgb_stats_test", placeholder = FALSE))))
                        ),
                        
                        wellPanel(
                            fluidRow(
                                conditionalPanel(
                                    condition = "input.bm_problem_type == 'classification'",
                                    column(3,radioButtons("bm_xgb_varimp","Choose Variable Importance By:",
                                                          c("Gain" = "gain",
                                                            "Cover" = "cover"))),
                                ),
                                column(3,uiOutput("bm_xgb_varimp_iv_placeholder")),
                                column(3,actionButton("bm_xgb_varimp_plot","Generate Variable Importance Plot"))
                            ),
                            tags$hr(),
                            fluidRow(
                                column(3,sliderInput("bm_xgb_pdp_data_split", "Select Train Ratio:",min = 0.1, max = 1, step = 0.05, value = 0.3)),
                                column(2,radioButtons("bm_xgb_pdp_select_method","Choose PDP Variables:",
                                                      c("Manually" = "manual",
                                                        "From model's feature importance" = "importance"))),
                                conditionalPanel(
                                    condition = "input.bm_xgb_pdp_select_method == 'manual'",
                                    column(3,uiOutput(outputId = "bm_xgb_pdp_vars_manual_placeholder"))
                                ),
                                conditionalPanel(
                                    condition = "input.bm_xgb_pdp_select_method == 'importance'",
                                    column(3,uiOutput(outputId = "bm_xgb_pdp_vars_placeholder"))
                                ),
                                column(2,actionButton("bm_xgb_gen_pdp","Generate PDP")),
                                column(2,uiOutput("bm_xgb_pdp_iv_placeholder"))
                            ),
                            tags$hr(),
                            fluidRow(
                                column(3,downloadButton("bm_xgb_report","Download Model Report")),
                                column(3,downloadButton("bm_xgb_score","Score Model in SAS")),
                                column(3,downloadButton("bm_xgb_test_prob","Download Predictions for Test Data")),
                                conditionalPanel(
                                    condition = "input.bm_xgb_compute > 0",
                                    column(3,downloadButton("bm_xgb_download","Download Model As RData"))
                                )
                            )
                        ),
                        
                        tabsetPanel(
                            id = "xgb_model_interpretation_tabs",
                            type = "tabs",
                            tabPanel("Variable Importance", div(style="height:800px;",fluidRow(plotOutput("bm_xgb_var_imp")))),
                            tabPanel("Partial Dependence Plots", div(style="height:800px;",fluidRow(
                                verbatimTextOutput("bm_xgb_var_summary"),
                                tags$div(style = "padding:10px"),
                                column(6,plotOutput("bm_xgb_pdp_plot", width = "100%")),
                                column(6,plotOutput("bm_xgb_density_plot", width = "100%")))
                            ))
                        ), #End of XGBoost Model
                        
                        # Linear / Logistic Regression Model
                        conditionalPanel(
                            condition="input.bm_problem_type == 'classification'",
                            h2("Logistic Regression Model Build")
                        ),
                        
                        conditionalPanel(
                            condition="input.bm_problem_type == 'regression'",
                            h2("Linear Regression Model Build")
                        ),
                        
                        wellPanel(
                            fluidRow(
                                column(6,uiOutput(outputId = "bm_linear_logistic_select_vars_placeholder"))
                            ),
                            fluidRow(
                                column(4,actionButton("bm_xgb_compute", "Build Model"))
                            )
                        ),
                        
                        tabsetPanel(
                            id = "bm_linear_logistic_tabs",
                            type = "tabs",
                            tabPanel("Model Output", div(style="height:500px;",fluidRow(verbatimTextOutput("bm_linear_logistic_output", placeholder = FALSE)))),
                            tags$head(tags$style("#bm_linear_logistic_output{color:black; font-size:14px; font-style:italic; overflow-y:scroll; max-height: 450px; background: ghostwhite;}")),
                            tabPanel("Train Lift", div(style="height:500px;",fluidRow(tableOutput("bm_linear_logistic_lift_train")))),
                            tabPanel("Train Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("bm_linear_logistic_stats_train", placeholder = FALSE)))),
                            tabPanel("Validation Lift", div(style="height:500px;",fluidRow(tableOutput("bm_linear_logistic_lift_valid")))),
                            tabPanel("Validation Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("bm_linear_logistic_stats_valid", placeholder = FALSE)))),
                            tabPanel("Test Lift", div(style="height:500px;",fluidRow(tableOutput("bm_linear_logistic_lift_test")))),
                            tabPanel("Test Accuracy Metrics", div(style="height:500px;",fluidRow(verbatimTextOutput("bm_linear_logistic_stats_test", placeholder = FALSE))))
                        ) # End of Linear / Logistic Regression Model
                        
                        ),
               
               tabPanel("Use Existing Model"),
               
               tabPanel("Get The Code")
               
               )

))
