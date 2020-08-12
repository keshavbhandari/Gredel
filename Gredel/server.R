#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(dummies)
library(data.table)
library(Information)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(Hmisc)
library(xgboost)
library(pROC)
library(pdp)
library(rmarkdown)

options(shiny.maxRequestSize = 15000*1024^2)

# outlier_in_dst <- function(tb) {
#     nums <- unlist(lapply(tb, is.numeric)) 
#     outlier<-sapply(tb[,nums], function(x) boxplot.stats(x)$out)
#     outlier[lengths(outlier)>0]
#     mins<-sapply(outlier[lengths(outlier)>0],function(x) min(x))
#     maxs<-sapply(outlier[lengths(outlier)>0],function(x) max(x))
#     print(cbind(mins,maxs))
#     return (names(outlier[lengths(outlier)>0]))
# } 

outlier_in_dst <- function(tb) {
    options(warn=-1)
    nums <- unlist(lapply(tb, is.numeric)) 
    outlier_all<-sapply(tb[,nums], function(x) boxplot.stats(x)$out)
    outlier<-outlier_all[lengths(outlier_all)>0]
    outlier_low <- sapply(tb[,names(outlier)], function(x) x[is.element(x,boxplot.stats(x)$out) & x<mean(x,na.rm=TRUE)] )
    outlier_high <- sapply(tb[,names(outlier)], function(x) x[is.element(x,boxplot.stats(x)$out) & x>mean(x,na.rm=TRUE)] ) 
    var_wo_outlier<-sapply(tb[,names(outlier)], function(x) x[!is.element(x,boxplot.stats(x)$out)])
    min_outlier<-sapply(outlier_low,function(x) min(x))
    max_outlier<-sapply(outlier_high,function(x) max(x))
    count_outlier<- sapply(outlier,function(x) sum(table(x)))
    proportion_outlier<-sapply(outlier,function(x) (sum(table(x))/nrow(tb))*100)
    mean_w_outlier<-sapply(tb[,names(outlier)], function(x) mean(x,na.rm=TRUE))
    mean_wo_outlier<-sapply(var_wo_outlier,function(x) mean(x,na.rm=TRUE))
    pct_01 <- sapply(var_wo_outlier, function(x) quantile(x, c(.01),na.rm = TRUE ))
    pct_05 <- sapply(var_wo_outlier, function(x) quantile(x, c(.05),na.rm = TRUE ))
    pct_95 <- sapply(var_wo_outlier, function(x) quantile(x, c(.95),na.rm = TRUE ))
    pct_99 <- sapply(var_wo_outlier, function(x) quantile(x, c(.99),na.rm = TRUE ))
    op_df<-cbind(count_outlier,proportion_outlier,min_outlier,max_outlier,mean_w_outlier,
                 mean_wo_outlier,pct_01, pct_05, pct_95,pct_99)
    options(warn=0)
    return(list(row.names(op_df),op_df))
}



data_split <- function(data, ratio){
    set.seed(123)
    sample <- sample.int(n = nrow(data), size = floor(ratio*nrow(data)), replace = F)
    validation  <- data[-sample,]
    train <- data[sample,]
    train_validation_list <- list(train, validation)
    return(train_validation_list)
}

treat_dep_var <- function(df, dep_var){
    if(length(unique(df[,dep_var]))==2){
        event <- names(which.min(table(df[,dep_var])))
        non_event <- names(which.max(table(df[,dep_var])))
        if(class(df[,dep_var])=="character"){
            df[df[,dep_var]==event,dep_var] <- 1
            df[df[,dep_var]==non_event,dep_var] <- 0
            df[,dep_var] <- as.integer(df[,dep_var])
        }else if(class(df[,dep_var])=="factor"){
            levels(df[,dep_var])[levels(df[,dep_var]) == event] <- 1
            levels(df[,dep_var])[levels(df[,dep_var]) == non_event] <- 0
            df[,dep_var] <- as.integer(as.character(df[,dep_var]))
        }else if(class(df[,dep_var])=="integer"){
            if((0 %in% df[,dep_var] & 1 %in% df[,dep_var])==FALSE){
                validate("Column doesn't contain 0 and 1 values")
            }
        }else if((class(df[,dep_var])=="numeric")){
            if((0.0 %in% df[,dep_var] & 1.0 %in% df[,dep_var])==FALSE){
                validate("Column doesn't contain 0 and 1 values")
            }
        }
        return(df)
    }else{
        validate("Dependent variable has more than 2 levels")
    }
}

treat_Missing <- function(df){
    Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }
    lookup <- as.data.frame(lapply(df[,], function(x) {
        if(class(x)=="factor" | class(x)=="character") Mode(na.omit(x))
        else if(class(x)=="numeric") median(x, na.rm=TRUE)
        else if(class(x)=="integer" & length(unique(x))>2) median(x, na.rm=TRUE)
        else if(class(x)=="integer" & length(unique(x))==2) Mode(na.omit(x))
    }))
    return(lookup)
}

replace_Missing <- function(data, missing_lookup){
    for(i in colnames(data)){
        data[is.na(data[,i]), i] <- missing_lookup[1,i]
    }
    return(data)
}

create_dummies <- function(df){
    options(warn=-1)
    n <- 1
    withProgress(message = 'Implementing One Hot Encoding', value = 0, {
        for (i in 1) {
            incProgress(1/n, detail = paste("For Dataset", i))
            constant_var_logic <- sapply(df, function(x) length(unique(x))==1)
            if('TRUE' %in% constant_var_logic){
                constant_vars <- names(which(constant_var_logic,TRUE))
                df <- df[,setdiff(colnames(df),constant_vars)]
            }
            #Checking if variables have same length as dataframe
            id_var_logic <- sapply(df, function(x) length(unique(x))==nrow(df) & (class(x)=="factor" | class(x)=="character"))
            if('TRUE' %in% id_var_logic){
                id_vars <- names(which(id_var_logic,TRUE))
                #Creating dummies without id_vars
                df <- dummy.data.frame(df[,setdiff(colnames(df),id_vars)], sep="_")
            }else{
                #Creating dummies from data with no IDs
                df <- dummy.data.frame(df, sep="_")
            }
            names(df) <- gsub(" ", "_", names(df))
            options(warn=0)
            return(df)
        }
    })
}

match_cols <- function(df, cols_to_match){
    for (col in cols_to_match) {
        if (!(col %in% names(df))){
            df[,col] <- 0
        }
    }
    return(df)
}

strtable <- function(df, n=4, width=60, n.levels=n, width.levels=width, factor.values=as.character) {
    #n the first n element to show
    #width maximum width in characters for the examples to show
    #n.levels the first n levels of a factor to show.
    #width.levels maximum width in characters for the number of levels to show.
    #factor.values function defining how factor examples should be printed. Possible values are \code{as.character} or \code{as.integer}.
    #stopifnot(is.data.frame(df))
    tab <- data.frame(variable=names(df),
                      class=rep(as.character(NA), ncol(df)),
                      levels=rep(as.character(NA), ncol(df)),
                      examples=rep(as.character(NA), ncol(df)),
                      stringsAsFactors=FALSE)
    collapse.values <- function(col, n, width) {
        result <- NA
        for(j in 1:min(n, length(col))) {
            el <- ifelse(is.numeric(col),
                         paste0(col[1:j], collapse=', '),
                         paste0('"', col[1:j], '"', collapse=', '))
            if(nchar(el) <= width) {
                result <- el
            } else {
                break
            }
        }
        if(length(col) > n) {
            return(paste0(result, ', ...'))
        } else {
            return(result)
        }
    }
    
    for(i in seq_along(df)) {
        if(is.factor(df[,i])) {
            tab[i,]$class <- paste0('Factor w/ ', nlevels(df[,i]), ' levels')
            tab[i,]$levels <- collapse.values(levels(df[,i]), n=n.levels, width=width.levels)
            tab[i,]$examples <- collapse.values(factor.values(df[,i]), n=n, width=width)
        } else {
            tab[i,]$class <- class(df[,i])[1]
            tab[i,]$examples <- collapse.values(df[,i], n=n, width=width)
        }
        
    }
    
    class(tab) <- c('strtable', 'data.frame')
    return(tab)
}

iv_calc <- function(df, dep_var){
    #Placeholder for treating dep_var
    signif_coeffs <- data.frame(Variable= character(), IV=numeric(), stringsAsFactors = F)
    j = 1
    n = length(setdiff(names(df),dep_var))
    withProgress(message = 'Calculating Information Value', value = 0, {
        for (i in names(df)) {
            if(i != dep_var & length(unique(df[,i]))>1){
                # Increment the progress bar, and update the detail text.
                incProgress(1/n, detail = paste("For Variable", j))
                j=j+1
                tmp_df <- data.frame(x_var=df[,i], y=df[,dep_var])
                IV <- create_infotables(data=tmp_df, valid=NULL, y='y', parallel=FALSE)
                signif_coeffs[nrow(signif_coeffs)+1, ] <- c(i, IV$Summary[,2])
            }
        }
    })
    signif_coeffs$IV <- as.numeric(signif_coeffs$IV)
    signif_coeffs <- signif_coeffs[order(-signif_coeffs[,"IV"]),]
    rownames(signif_coeffs) <- NULL
    return(signif_coeffs)
}

var_significance <- function(df, dep_var, method="classification", ratio){
    
    set.seed(123)
    sample <- sample.int(n = nrow(df), size = floor(ratio*nrow(df)), replace = F)
    df  <- df[sample,]
    
    lmp <- function (modelobject) {
        if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
        f <- summary(modelobject)$fstatistic
        p <- pf(f[1],f[2],f[3],lower.tail=F)
        attributes(p) <- NULL
        return(p)
    }
    
    coef <- c()
    p_val <- c()
    i = 1
    n = length(setdiff(colnames(df),dep_var))
    withProgress(message = 'Calculating Significance', value = 0, {
        for (col in setdiff(colnames(df),dep_var)) {
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("For Variable", i))
            i=i+1
            if(method=="classification"){
                model <- glm(df[,dep_var] ~ df[,col], data = df, family = "binomial")
                coef_tbl <- coef(summary(model))
                coef <- c(coef, col)
                p_val <- c(p_val, coef_tbl[2,4])
            }else if(method=="regression"){
                model <- lm(df[,dep_var] ~ df[,col], data = df)
                coef <- c(coef, col)
                p_val <- c(p_val, lmp(model))
            }
        }
    })
    
    coef_summary_tbl <- data.frame(Variable = coef, p_values = p_val)
    coef_summary_tbl$significance_0.01 <- ifelse(coef_summary_tbl$p_values < 0.01,"significant","not significant")
    # coef_summary_tbl$significance_0.05 <- ifelse(coef_summary_tbl$p_values < 0.05,"significant","not significant")
    coef_summary_tbl$Variable <- as.character(coef_summary_tbl$Variable)
    return(coef_summary_tbl)
}

compute_statistical_significance <- function(df, dep_var, method="classification"){
    if(method=="classification"){
        Variable <- c()
        p_value <- c()
        j = 1
        n = length(setdiff(colnames(df),dep_var))
        withProgress(message = 'Calculating Significance', value = 0, {
            for (i in setdiff(colnames(df),dep_var)) {
                incProgress(1/n, detail = paste("For Variable", j))
                j=j+1
                if(length(unique(df[,i]))==2){
                    res.chi_sq <- chisq.test(df[,i],df[,dep_var], simulate.p.value = TRUE)
                    p_val <- res.chi_sq$p.value
                    Variable <- c(Variable,i)
                    p_value <- c(p_value,p_val)
                }else if(length(unique(df[,i]))>2){
                    res.aov <- aov(as.formula(paste0(dep_var," ~ `",i,"`")), data = df)
                    p_val <- summary(res.aov)[[1]][["Pr(>F)"]][1]
                    Variable <- c(Variable,i)
                    p_value <- c(p_value,p_val)
                }
            }
        })
        coef_summary_tbl <- data.frame(Variable = Variable, p_values = p_value)
        coef_summary_tbl$significance_0.01 <- ifelse(coef_summary_tbl$p_values < 0.01,"significant","not significant")
        return(coef_summary_tbl)
    }else if(method=="regression"){
        Variable <- c()
        p_value <- c()
        j = 1
        n = length(setdiff(colnames(df),dep_var))
        withProgress(message = 'Calculating Significance', value = 0, {
            for (i in setdiff(colnames(df),dep_var)) {
                incProgress(1/n, detail = paste("For Variable", j))
                j=j+1
                if(length(unique(df[,i]))==2){
                    res.aov <- aov(as.formula(paste0(dep_var," ~ `",i,"`")), data = df)
                    p_val <- summary(res.aov)[[1]][["Pr(>F)"]][1]
                    Variable <- c(Variable,i)
                    p_value <- c(p_value,p_val)
                }else if(length(unique(df[,i]))>2){
                    res.ttest <- t.test(df[,dep_var], df[,i], data = df)
                    p_val <- res.ttest$p.value
                    Variable <- c(Variable,i)
                    p_value <- c(p_value,p_val)
                }
            }
        })
        coef_summary_tbl <- data.frame(Variable = Variable, p_values = p_value)
        coef_summary_tbl$significance_0.01 <- ifelse(coef_summary_tbl$p_values < 0.01,"significant","not significant")
        return(coef_summary_tbl)
    }
}

var_corr <- function(df, indep_var){
    corr_table <- cor(df[-which(names(df) %in% indep_var)], df[,indep_var])
    corr_df <- data.frame(Variable = rownames(corr_table), Correlation = corr_table[1:nrow(corr_table)])
    corr_df$Correlation <- round(corr_df$Correlation, 4)
    return(corr_df)
}

dvtable <- function(df, dep_var, method="classification"){
    if (method=="classification") {
        tbl_freq <- data.frame(table(df[,dep_var]))
        tbl_freq <- setNames(tbl_freq, c("Level","Frequency"))
        tbl_prop <- data.frame(prop.table(table(df[,dep_var])))
        tbl_prop <- setNames(tbl_prop, c("Level","Proportion"))
        tbl <- merge(tbl_freq, tbl_prop, by = "Level")
        return(tbl)
    }else{
        summary_df <- setNames(data.frame(unclass(summary(df[,dep_var]))),dep_var)
        Unique <- c(length(unique(df[,dep_var])))
        summary_df <- rbind(summary_df, Unique=Unique)
        summary_df <- t(summary_df)
        return(summary_df)
    }
}

var_summary <- function(df,indep_var){
    tbl <- summary(df[,indep_var])
    return(tbl)
}

missing_value_summary <- function(df){
    miss <- sapply(df, function(x) sum(is.na(x)))
    return(miss)
}

density_plot <- function(df,indep_var,dep_var,method="classification"){
    if(method=="classification"){
        ggplot(df, aes(x=df[,indep_var], fill=as.factor(df[,dep_var])))+
            geom_density(alpha=0.4)+labs(title="Density Plot")+
            xlab(indep_var)+
            scale_fill_discrete(name = dep_var)+theme_bw()
    }else if(method=="regression"){
        ggplot(df, aes(x=df[,indep_var]))+
            geom_density(fill="pink")+
            labs(title="Density Plot")+xlab(indep_var)+theme_bw()
    }
}

histogram_plot <- function(df,indep_var,dep_var, bins, method = "classification"){
    if(method=="classification"){
        ggplot(df, aes(x = df[,indep_var])) + 
            geom_histogram(aes(color = as.factor(df[,dep_var]),fill = as.factor(df[,dep_var])), 
                           alpha = 0.4, bins = bins,
                           position = "identity") +
            xlab(indep_var) +
            scale_fill_manual(name = dep_var,values = c("#00AFBB", "#E7B800")) +
            scale_color_manual(name = dep_var,values = c("#00AFBB", "#E7B800"))
    }else if(method=="regression"){
        ggplot(df, aes(x=df[,indep_var]))+
            geom_histogram(color="darkblue", fill="blue",alpha = 0.4, bins = bins,
                           position = "identity") +
            xlab(indep_var)
    }
}

box_plot <- function(df,indep_var,dep_var, sample_rate=0.1, method="classification"){
    s <- sample.int(n = nrow(df), size = floor(sample_rate*nrow(df)), replace = F)
    if(method=="classification"){
        ggplot(df[s,], aes(x = factor(1), y = df[s,indep_var])) +
            geom_boxplot(width = 0.4, fill = "white") +
            geom_jitter(aes(color = as.factor(df[s,dep_var])), 
                        width = 0.1, size = 1) +
            scale_color_manual(name = dep_var, values = c("#00AFBB", "#E7B800")) + 
            labs(x = NULL) +   # Remove x axis label
            labs(y = indep_var)
    }else if(method=="regression"){
        if(class(df[,indep_var])=="integer" | class(df[,indep_var])=="numeric"){
            ggplot(df, aes(x = factor(1), y = df[,indep_var])) +
                geom_boxplot(width = 0.4, fill = "orange") +
                labs(x = NULL) +   # Remove x axis label
                labs(y = indep_var) + theme_bw()
        }
    }else if(method=="regression"){
        if(class(df[,indep_var])=="factor"){
            if(length(unique(df[,indep_var]))>10){
                lf <- names(prop.table(table(df[,indep_var])))[10:length(table(df[,indep_var]))]
                for (variable in lf) {
                    levels(df[,indep_var])[levels(df[,indep_var]) == variable] <- "Other"
                }
            }
            ggplot(df, aes(x = as.factor(df[,indep_var]), y = df[,dep_var], fill = as.factor(df[,indep_var]))) +
                geom_boxplot(outlier.color = "black", outlier.shape = 16, outlier.size = 2) +
                labs(x = NULL) + labs(y = dep_var)+ guides(fill=guide_legend(title=indep_var))
        }
    }
}

bar_plot <- function(df,indep_var,dep_var){
    if(length(unique(df[,indep_var]))>10){
        lf <- names(prop.table(table(df[,indep_var])))[10:length(table(df[,indep_var]))]
        for (variable in lf) {
            levels(df[,indep_var])[levels(df[,indep_var]) == variable] <- "Other"
        }
    }
    ggplot(df, aes(df[,indep_var])) +
        geom_bar(fill = "#0073C2FF") +
        xlab(indep_var) + 
        theme(
            panel.background = element_rect(fill = "transparent") # bg of the panel
            , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
            , panel.grid.major = element_blank() # get rid of major grid
            , panel.grid.minor = element_blank() # get rid of minor grid
            , legend.background = element_rect(fill = "transparent") # get rid of legend bg
            , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
}

stacked_bar_plot <- function(df,indep_var,dep_var){
    if(length(unique(df[,indep_var]))>10){
        lf <- names(prop.table(table(df[,indep_var])))[10:length(table(df[,indep_var]))]
        for (variable in lf) {
            levels(df[,indep_var])[levels(df[,indep_var]) == variable] <- "Other"
        }
    }
    a <- data.frame(table(df[,dep_var],df[,indep_var]))
    a <- setNames(a,c(dep_var,indep_var,"Percentage"))
    a$Percentage <- round(a$Percentage,2)
    a$Percentage2 <- round(a$Percentage/sum(a$Percentage),2)
    a <- setNames(a,c("dep_var", indep_var, "Percentage","Percentage2"))
    
    a2 <- dplyr::group_by(a, a[,indep_var]) %>% dplyr::transmute(dep_var, Percentage = round(Percentage/sum(Percentage),2))
    a2 <- setNames(a2,c("indep_var", dep_var, "Percentage"))
    a2 <- plyr::ddply(a2, plyr::.(indep_var),transform, pos = (ifelse(Percentage<0.5,Percentage*0.5,(1-Percentage)+Percentage*0.5)))
    a2 <- setNames(a2,c(indep_var, dep_var, "Percentage","pos"))
    
    ggplot(data = a2, 
           aes(x = a2[,indep_var], 
               y = Percentage, 
               fill = a2[,dep_var],
               cumulative = TRUE)) +
        geom_col() +
        geom_text(aes(label = paste0(Percentage*100,"%")), 
                  position = position_stack(vjust = 0.5)) + 
        xlab(indep_var) + scale_fill_discrete(name = dep_var) +
        theme_minimal()
}

pie_chart <- function(df,indep_var){
    if(length(unique(df[,indep_var]))>10){
        lf <- names(prop.table(table(df[,indep_var])))[10:length(table(df[,indep_var]))]
        for (variable in lf) {
            levels(df[,indep_var])[levels(df[,indep_var]) == variable] <- "Other"
        }
    }
    tbl <- as.data.frame(table(df[,indep_var]))
    setnames(tbl,names(tbl),c("Variable","Frequency"))
    tbl$Proportion <- tbl$Frequency/sum(tbl$Frequency)
    rownames(tbl) <- NULL
    tbl$Variable <- as.character(tbl$Variable)
    tbl <- tbl[order(tbl$Variable),]
    tbl <- tbl[order(rev(tbl$Variable)),]
    tbl$Variable <- as.factor(tbl$Variable)
    tbl$Proportion <- round(tbl$Proportion,2)
    
    ggplot(data=tbl)+
        geom_bar(aes(x="", y=Proportion, fill=Variable), stat="identity", width = 1)+
        coord_polar("y", start=0)+
        theme_void()+
        geom_text(aes(x=1, y = cumsum(Proportion) - Proportion/2, label=paste0(Proportion*100,'%')))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    if (!interactive()) {
        session$onSessionEnded(function() {
            stopApp()
            q("no")
        })
    }

    ############################################################################
    #------------------------Create Classification Model------------------------
    ############################################################################
    
    all_objects <- reactiveValues(data=NULL, train=NULL, validation=NULL, test=NULL,
                                  raw_data_head=NULL, raw_test_head=NULL, raw_test=NULL,
                                  warning_message=NULL,error_message=NULL,
                                  problem_type=NULL, dep_var=NULL, outlier_variables_summary=NULL, on_hot_encoding=FALSE, missing_lookup=NULL, data_config_train_head=NULL, data_config_validation_head=NULL, data_config_test_head=NULL, data_config_data_shape=NULL, data_config_data_structure=NULL, data_config_train=NULL,data_config_validation=NULL,data_config_test=NULL,
                                  corr_df=NULL, iv_calc_df=NULL, ttest_anova_df=NULL, ss_calc_df=NULL,
                                  vs_train=NULL, vs_validation=NULL, vs_test=NULL,
                                  dv_table=NULL)
    
    # Read Data
    observeEvent(input$bm_file1,{
        tryCatch({
            path <- normalizePath(input$bm_getwd)
            file <- input$bm_file_name
            if(file.exists(paste(path,file,sep = '\\'))==TRUE){
                all_objects$data <- fread(paste(path,file,sep = '\\'),
                                     header = TRUE,
                                     data.table = FALSE,
                                     stringsAsFactors = TRUE,
                                     sep = input$bm_sep,
                                     skip = as.numeric(input$bm_upload_start),
                                     nrows = as.numeric(input$bm_upload_nrows),
                                     na.strings = input$bm_upload_na_strings)
                
                # all_objects$outlier_variables <- outlier_in_dst(all_objects$data)
                
                if(is.null(all_objects$test)==FALSE){
                    if(length(setdiff(names(all_objects$test), names(all_objects$data))) <= 1 & length(setdiff(names(all_objects$data), names(all_objects$test))) <= 1){
                        all_objects$warning_message = NULL
                        all_objects$error_message = NULL
                        all_objects$raw_data_head <- head(all_objects$data,n=20)
                    }else{
                        all_objects$warning_message = "Train and test names do not match"
                        all_objects$error_message = "Train and test names do not match"
                    }
                }else{
                    all_objects$raw_data_head <- head(all_objects$data,n=20)
                    all_objects$warning_message <- NULL
                    all_objects$error_message <- NULL
                }
            }else{
                all_objects$warning_message <- "No such file or directory found"
            }
        },
        warning = function(w){
            all_objects$warning_message <- "No such file or directory found"
        },
        error = function(e){
            all_objects$error_message <- "Error parsing the file"
        })
        return(TRUE)
    })
    
    observeEvent(input$bm_file2,{
        tryCatch({
            path <- normalizePath(input$bm_getwd)
            file <- input$bm_file_name
            if(file.exists(paste(path,file,sep = '\\'))==TRUE){
                all_objects$test <- fread(paste(path,file,sep = '\\'),
                                    header = TRUE,
                                    data.table = FALSE,
                                    stringsAsFactors = TRUE,
                                    sep = input$bm_sep,
                                    skip = as.numeric(input$bm_upload_start),
                                    nrows = as.numeric(input$bm_upload_nrows),
                                    na.strings = input$bm_upload_na_strings)
                
                all_objects$raw_test <- all_objects$test
                
                if(length(setdiff(names(all_objects$test), names(all_objects$data))) <= 1 & length(setdiff(names(all_objects$data), names(all_objects$data))) <= 1){
                    all_objects$warning_message = NULL
                    all_objects$error_message = NULL
                    all_objects$raw_test_head <- head(all_objects$test,n=20)
                }else{
                    all_objects$warning_message = "Train and test names do not match"
                    all_objects$error_message = "Train and test names do not match"
                }
            }else{
                all_objects$warning_message <- "No such file or directory found"
            }
        },
        warning = function(w){
            all_objects$warning_message <- "No such file or directory found"
        },
        error = function(e){
            all_objects$error_message <- "Error parsing the file"
        })
        return(TRUE)
    })
    
    output$bm_train_raw_output <- renderTable({
        input$bm_file1
        input$bm_file2
        isolate({
            validate(need(input$bm_getwd,"Directory not provided"))
            validate(need(input$bm_file_name,"File not provided"))
            validate(need(is.null(all_objects$warning_message)==TRUE,all_objects$warning_message))
            validate(need(is.null(all_objects$error_message)==TRUE,all_objects$error_message))
            all_objects$raw_data_head
        })
    })
    
    output$bm_test_raw_output <- renderTable({
        input$bm_file1
        input$bm_file2
        isolate({
            validate(need(input$bm_getwd,"Directory not provided"))
            validate(need(input$bm_file_name,"File not provided"))
            validate(need(is.null(all_objects$warning_message)==TRUE,all_objects$warning_message))
            validate(need(is.null(all_objects$error_message)==TRUE,all_objects$error_message))
            all_objects$raw_test_head
        })
    })
    
    # Data Configuration
    output$bm_select_dv_placeholder <- renderUI({
        if(!is.null(all_objects$data)){
            choices = names(all_objects$data)
            selectInput("dep_var", "Choose Dependent Variable:",
                        choices = choices, selected = all_objects$dep_var)
        }
    })
    
    output$bm_data_config_outlier_summary <- DT::renderDataTable({
        if(is.null(all_objects$data)==TRUE) return()
        all_objects$outlier_variables_summary <- outlier_in_dst(all_objects$data)
        round(all_objects$outlier_variables_summary[[2]],2)
    })
    outputOptions(output,"bm_data_config_outlier_summary",suspendWhenHidden=FALSE)
    
    output$bm_data_config_outlier_vars_placeholder <- renderUI({
        pickerInput("bm_data_config_select_outlier_vars","Select Outlier Variables",
                    choices=all_objects$outlier_variables_summary[[1]],
                    options = list(`actions-box` = TRUE),multiple = T)
    })
    
    data_preprocess <- observeEvent(input$bm_data_config_confirm,{
        
        all_objects$dep_var <- input$dep_var
        all_objects$problem_type <- input$bm_problem_type
        train_validation = data_split(data = all_objects$data, 
                                      ratio = input$bm_data_config_data_split)
        all_objects$train <- train_validation[[1]]
        all_objects$validation <- train_validation[[2]]
        rm(train_validation)
        invisible(gc())
        
        if(length(unique(all_objects$train[,all_objects$dep_var]))==2 & all_objects$problem_type == "classification"){
            all_objects$train <- treat_dep_var(all_objects$train,all_objects$dep_var)
            all_objects$validation <- treat_dep_var(all_objects$validation,all_objects$dep_var)
            if(all_objects$dep_var %in% names(all_objects$test)){
                all_objects$test <- treat_dep_var(all_objects$test,all_objects$dep_var)
            }
        }
        
        if(input$bm_data_config_missing){
            all_objects$missing_lookup <- treat_Missing(all_objects$train)
            all_objects$train <- replace_Missing(all_objects$train, all_objects$missing_lookup)
            all_objects$validation <- replace_Missing(all_objects$validation, all_objects$missing_lookup)
            if(is.null(all_objects$test)==FALSE){
                all_objects$test <- replace_Missing(all_objects$test, all_objects$missing_lookup)
            }
        }
        
        if(input$bm_data_config_ohe){
            if(all_objects$on_hot_encoding == FALSE){
                all_objects$train <- create_dummies(all_objects$train)
                all_objects$validation <- create_dummies(all_objects$validation)
                all_objects$validation <- match_cols(all_objects$validation, names(all_objects$train))
                if(is.null(all_objects$test)==FALSE){
                    all_objects$test <- create_dummies(all_objects$test)
                    all_objects$test <- match_cols(all_objects$test, names(all_objects$train))
                }
                # all_objects$on_hot_encoding <- TRUE
            }
        }
        
        #Assigning global variables required for temp view
        all_objects$data_config_train_head <- head(all_objects$train,n=30)
        all_objects$data_config_validation_head <- head(all_objects$validation,n=30)
        all_objects$data_config_test_head <- head(all_objects$test,n=30)
        all_objects$data_config_data_structure <- strtable(all_objects$data)
        all_objects$data_config_data_shape <- data.frame("Rows" = nrow(all_objects$data), "Columns" = ncol(all_objects$data))
        
        # Saving Dataframe Copy
        all_objects$data_config_train <- all_objects$train
        all_objects$data_config_validation <- all_objects$validation
        all_objects$data_config_test <- all_objects$test
    })
    
    output$bm_data_config_train_output <- renderTable({
        if(input$bm_data_config_confirm == 0) return()
        if(is.null(all_objects$train)==TRUE) return()
        isolate({
            if(all_objects$problem_type == "classification"){
                validate(need(length(unique(all_objects$train[,all_objects$dep_var]))==2,"Dependent variable is not binary"))   
            }else if(all_objects$problem_type == "regression"){
                validate(need(class(all_objects$train[,all_objects$dep_var]) %in% c("integer","numeric"),"Dependent variable is not numeric"))
                validate(need(length(unique(all_objects$train[,all_objects$dep_var]))>2,"Dependent variable is binary not continuous"))
            }
            
            if(is.null(all_objects$test)==FALSE){
                if(input$dep_var %in% names(all_objects$test)){
                    if(all_objects$problem_type == "classification"){
                        validate(need(length(unique(all_objects$test[,all_objects$dep_var]))==2,"Dependent variable for test dataset is not binary"))   
                    }else if(all_objects$problem_type == "regression"){
                        validate(need(class(all_objects$test[,all_objects$dep_var]) %in% c("integer","numeric"),"Dependent variable for test dataset is not numeric"))
                        validate(need(length(unique(all_objects$test[,all_objects$dep_var]))>2,"Dependent variable for test dataset is binary not continuous"))
                    }
                }
            }
            all_objects$data_config_train_head
        })
    })
    
    output$bm_data_config_validation_output <- renderTable({
        if(input$bm_data_config_confirm == 0) return()
        if(is.null(all_objects$validation)==TRUE) return()
        isolate({
            if(all_objects$problem_type == "classification"){
                validate(need(length(unique(all_objects$validation[,all_objects$dep_var]))==2,"Dependent variable is not binary"))   
            }else if(all_objects$problem_type == "regression"){
                validate(need(class(all_objects$validation[,all_objects$dep_var]) %in% c("integer","numeric"),"Dependent variable is not numeric"))
                validate(need(length(unique(all_objects$validation[,all_objects$dep_var]))>2,"Dependent variable is binary not continuous"))
            }            
            if(is.null(all_objects$test)==FALSE){
                if(input$dep_var %in% names(all_objects$test)){
                    if(all_objects$problem_type == "classification"){
                        validate(need(length(unique(all_objects$test[,all_objects$dep_var]))==2,"Dependent variable for test dataset is not binary"))   
                    }else if(all_objects$problem_type == "regression"){
                        validate(need(class(all_objects$test[,all_objects$dep_var]) %in% c("integer","numeric"),"Dependent variable for test dataset is not numeric"))
                        validate(need(length(unique(all_objects$test[,all_objects$dep_var]))>2,"Dependent variable for test dataset is binary not continuous"))
                    }
                }
            }
            all_objects$data_config_validation_head
        })
    })
    
    output$bm_data_config_test_output <- renderTable({
        if(input$bm_data_config_confirm == 0) return()
        if(is.null(all_objects$test)==TRUE) return()
        isolate({
            if(is.null(all_objects$test)==FALSE){
                if(input$dep_var %in% names(all_objects$test)){
                    if(all_objects$problem_type == "classification"){
                        validate(need(length(unique(all_objects$test[,all_objects$dep_var]))==2,"Dependent variable for test dataset is not binary"))   
                    }else if(all_objects$problem_type == "regression"){
                        validate(need(class(all_objects$test[,all_objects$dep_var]) %in% c("integer","numeric"),"Dependent variable for test dataset is not numeric"))
                        validate(need(length(unique(all_objects$test[,all_objects$dep_var]))>2,"Dependent variable for test dataset is binary not continuous"))
                    }
                }
            }
            all_objects$data_config_test_head
        })
    })
    
    output$bm_data_config_shape <- renderTable({
        if(input$bm_data_config_confirm == 0) return()
        if(is.null(all_objects$data)==TRUE) return()
        isolate({
            all_objects$data_config_data_shape
        })
    })
    
    output$bm_data_config_structure <- DT::renderDataTable({
        if(input$bm_data_config_confirm == 0) return()
        all_objects$data_config_data_structure
    },options = list(pageLength = 25))
    outputOptions(output,"bm_data_config_structure",suspendWhenHidden=FALSE)
    
    observeEvent(input$bm_data_config_refresh,{
        all_objects$data_config_train_head <- head(all_objects$train,n=30)
        all_objects$data_config_validation_head <- head(all_objects$validation,n=30)
        all_objects$data_config_test_head <- head(all_objects$test,n=30)
        all_objects$data_config_data_structure <- strtable(all_objects$data)
        all_objects$data_config_data_shape <- data.frame("Rows" = nrow(all_objects$data), "Columns" = ncol(all_objects$data))
    })
    
    observeEvent(input$bm_data_config_undo,{
        all_objects$data_config_train_head <- NULL
        all_objects$train <- NULL
        all_objects$data_config_validation_head <- NULL
        all_objects$validation <- NULL
        all_objects$data_config_test_head <- head(all_objects$raw_test,n=30)
        all_objects$test <- all_objects$raw_test
    })
    
    #Variable selection
    output$bm_vs_train_output <- renderTable({
        head(all_objects$train,n=30)
    })
    
    output$bm_vs_select_iv_correlation_placeholder <- renderUI({
        selectInput("bm_vs_iv_correlation", "See Variable Correlation:",
                    setdiff(names(all_objects$train[, sapply(all_objects$train, is.numeric)]),all_objects$dep_var), selected = NULL)
    })
    
    observeEvent(input$bm_vs_compute_correlation,{
        all_objects$corr_df <-  var_corr(all_objects$train[, sapply(all_objects$train, is.numeric)],input$bm_vs_iv_correlation)
    })
    
    output$bm_vs_correlation_output <- DT::renderDataTable({
        req(input$bm_vs_compute_correlation)
        all_objects$corr_df <- all_objects$corr_df[all_objects$corr_df$Variable %in% names(all_objects$train),]
        all_objects$corr_df
    },options = list(pageLength = 25))
    
    observeEvent(input$bm_vs_compute_iv,{
        all_objects$iv_calc_df <-  iv_calc(all_objects$train,all_objects$dep_var)
    })
    
    output$bm_vs_iv_output <- DT::renderDataTable({
        all_objects$iv_calc_df
    },options = list(pageLength = 25))
    
    observeEvent(input$bm_vs_compute_ttest_anova,{
        all_objects$ttest_anova_df <-  compute_statistical_significance(all_objects$train,all_objects$dep_var,method = all_objects$problem_type)
    })
    
    output$bm_vs_ttest_anova_output <- DT::renderDataTable({
        all_objects$ttest_anova_df
    },options = list(pageLength = 25))
    
    observeEvent(input$bm_vs_compute_ss,{
            all_objects$ss_calc_df <-  var_significance(all_objects$train, all_objects$dep_var, method=all_objects$problem_type, ratio = input$bm_vs_ss_data_split)
    })
    
    output$bm_vs_ss_output <- DT::renderDataTable({
        all_objects$ss_calc_df
    },options = list(pageLength = 25))
    
    output$bm_vs_select_vars_manual_dropdown <- renderUI({
        pickerInput("bm_vs_select_vars_manual","Independent Variables",
                    choices=setdiff(names(all_objects$train),all_objects$dep_var),
                    selected = setdiff(names(all_objects$train),all_objects$dep_var),
                    options = list(`actions-box` = TRUE),multiple = T)
    })
    
    # Variable Selection Filter
    observeEvent(input$bm_vs_proceed,{
        if(input$bm_vs_method=="iv"){
            if(is.null(all_objects$iv_calc_df)==TRUE){
                all_objects$iv_calc_df <-  iv_calc(all_objects$train,all_objects$dep_var)
            }
            ll <- input$bm_vs_iv_range[1]
            ul <- input$bm_vs_iv_range[2]
            vars <- as.character(all_objects$iv_calc_df[(all_objects$iv_calc_df$IV>=ll & all_objects$iv_calc_df$IV<=ul),1])
            all_objects$iv_calc_df <- all_objects$iv_calc_df[all_objects$iv_calc_df$Variable %in% vars,]
            all_objects$ss_calc_df <- all_objects$ss_calc_df[all_objects$ss_calc_df$Variable %in% vars,]
            vars <- c(vars,all_objects$dep_var)
            all_objects$train <- all_objects$train[,vars]
            all_objects$validation <- all_objects$validation[,vars]
            if(is.null(all_objects$test)==FALSE){
                if(all_objects$dep_var %in% names(all_objects$test)){
                    all_objects$test <- all_objects$test[,vars]
                }else{
                    all_objects$test <- all_objects$test[,setdiff(vars,all_objects$dep_var)]
                }
            }
        }else if(input$bm_vs_method=="ttest_anova"){
            if(is.null(all_objects$ttest_anova_df)==TRUE){
                all_objects$ttest_anova_df <-  compute_statistical_significance(all_objects$train,all_objects$dep_var,method=all_objects$problem_type)
            }
            vars <- as.character(all_objects$ttest_anova_df[(all_objects$ttest_anova_df$p_values<=input$bm_vs_ttest_anova_alpha),1])
            all_objects$ttest_anova_df <- all_objects$ttest_anova_df[all_objects$ttest_anova_df$Variable %in% vars,]
            all_objects$ss_calc_df <- all_objects$ss_calc_df[all_objects$ss_calc_df$Variable %in% vars,]
            vars <- c(vars,all_objects$dep_var)
            all_objects$train <- all_objects$train[,vars]
            all_objects$validation <- all_objects$validation[,vars]
            if(is.null(all_objects$test)==FALSE){
                if(all_objects$dep_var %in% names(all_objects$test)){
                    all_objects$test <- all_objects$test[,vars]
                }else{
                    all_objects$test <- all_objects$test[,setdiff(vars,all_objects$dep_var)]
                }
            }
        }else if(input$bm_vs_method=="regression"){
            if(is.null(all_objects$ss_calc_df)==TRUE){
                all_objects$ss_calc_df <-  var_significance(all_objects$train,all_objects$dep_var,method=all_objects$problem_type,ratio = input$bm_vs_ss_data_split)
            }
            vars <- as.character(all_objects$ss_calc_df[(all_objects$ss_calc_df$p_values<=0.01),1])
            all_objects$iv_calc_df <- all_objects$iv_calc_df[all_objects$iv_calc_df$Variable %in% vars,]
            all_objects$ss_calc_df <- all_objects$ss_calc_df[all_objects$ss_calc_df$Variable %in% vars,]
            vars <- c(vars, all_objects$dep_var)
            all_objects$train <- all_objects$train[,vars]
            all_objects$validation <- all_objects$validation[,vars]
            if(is.null(all_objects$test)==FALSE){
                if(all_objects$dep_var %in% names(all_objects$test)){
                    all_objects$test <- all_objects$test[,vars]
                }else{
                    all_objects$test <- all_objects$test[,setdiff(vars,all_objects$dep_var)]
                }
            }
        }else if(input$bm_vs_method=="manual"){
            vars <- input$bm_vs_select_vars_manual
            all_objects$ss_calc_df <- all_objects$ss_calc_df[all_objects$ss_calc_df$Variable %in% vars,]
            all_objects$iv_calc_df <- all_objects$iv_calc_df[all_objects$iv_calc_df$Variable %in% vars,]
            vars <- c(vars, all_objects$dep_var)
            all_objects$train <- all_objects$train[,vars]
            all_objects$validation <- all_objects$validation[,vars]
            if(is.null(all_objects$test)==FALSE){
                if(all_objects$dep_var %in% names(all_objects$test)){
                    all_objects$test <- all_objects$test[,vars]
                }else{
                    all_objects$test <- all_objects$test[,setdiff(vars,all_objects$dep_var)]
                }
            }
        }
        
        # Saving Dataframe Copy
        all_objects$vs_train <- all_objects$data_config_train
        all_objects$vs_validation <- all_objects$data_config_validation
        all_objects$vs_test <- all_objects$data_config_test
    })
    
    observeEvent(input$bm_vs_undo,{
        all_objects$train <- all_objects$vs_train
        all_objects$validation <- all_objects$vs_validation
        all_objects$test <- all_objects$vs_test
    })
    
    # EDA
    output$bm_eda_select_iv_placeholder <- renderUI({
        selectInput("bm_eda_iv", "Choose Independent Variable To Plot Graph:",
                    setdiff(names(all_objects$train),all_objects$dep_var))
    })
    
    output$bm_eda_dv_info_table <- renderTable({
        req(all_objects$train)
        all_objects$dv_table <- dvtable(all_objects$train, all_objects$dep_var, method = all_objects$problem_type)
        all_objects$dv_table
    })
    
    output$bm_eda_dv_info_plot <- renderPlot({
        ggplot(all_objects$train, aes_string(x=all_objects$dep_var))+
            geom_density(color="darkblue", fill="lightblue")+
            labs(title="Density Plot of Response Variable")+
            xlab(all_objects$dep_var)
    })
    
    output$bm_eda_iv_summary_output <- renderPrint({
        tryCatch({
            var_summary(all_objects$train, input$bm_eda_iv)
        },
        error = function(e){
            validate("")
        }
        )
    })
    
    output$bm_eda_iv_dist_output <- renderPlot({
        
        tryCatch({
            indep_var <- input$bm_eda_iv
            dep_var <- all_objects$dep_var
            bins <- input$bm_eda_cont_hist_bins
            sample_rate <- input$bm_eda_cont_boxplot_sample
            
            if(class(all_objects$train[,indep_var])=="integer" | class(all_objects$train[,indep_var])=="numeric"){
                if(input$bm_eda_cont_plot_type == "density"){
                    #Density Plot
                    density_plot(all_objects$train, indep_var, dep_var, method=all_objects$problem_type)
                }else if(input$bm_eda_cont_plot_type == "histogram"){
                    #Histogram
                    histogram_plot(all_objects$train, indep_var, dep_var, bins, method = all_objects$problem_type)
                }else if(input$bm_eda_cont_plot_type == "boxplot"){
                    #Boxplot
                    box_plot(all_objects$train, indep_var, dep_var, sample_rate, method=all_objects$problem_type)
                }
            }else if(class(all_objects$train[,indep_var]) == "factor"){
                if(input$bm_eda_cat_plot_type == "bar"){
                    bar_plot(all_objects$train, indep_var, dep_var)
                }else if(input$bm_eda_cat_plot_type == "stacked_bar"){
                    stacked_bar_plot(all_objects$train, indep_var, dep_var)
                }else if(input$bm_eda_cat_plot_type == "pie"){
                    pie_chart(all_objects$train, indep_var)
                }
            }
        },
        error = function(e){
            validate("")
        }
        )
    })
    
    output$bm_eda_missing_summary <- renderPrint({
        missing_value_summary(all_objects$train)
    })
    
})
