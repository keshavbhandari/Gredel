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

options(shiny.maxRequestSize = 2000*1024^2)

# Define server logic
shinyServer(function(input, output, session) {
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
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
        return(df)
      }
    })
  }
  
  treat_dep_var_cfn <- function(df, dep_var){
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
  
  dvtable_cfn <- function(df, dep_var){
    tbl_freq <- data.frame(table(df[,dep_var]))
    tbl_freq <- setNames(tbl_freq, c("Level","Frequency"))
    tbl_prop <- data.frame(prop.table(table(df[,dep_var])))
    tbl_prop <- setNames(tbl_prop, c("Level","Proportion"))
    tbl <- merge(tbl_freq, tbl_prop, by = "Level")
    return(tbl)
  }
  
  dvtable_rgn <- function(df, dep_var){
    summary_df <- setNames(data.frame(unclass(summary(df[,dep_var]))),dep_var)
    Unique <- c(length(unique(df[,dep_var])))
    summary_df <- rbind(summary_df, Unique=Unique)
    summary_df <- t(summary_df)
    return(summary_df)
  }
  
  var_summary <- function(df,indep_var){
    tbl <- summary(df[,indep_var])
    return(tbl)
  }
  
  density_plot <- function(df,indep_var,dep_var,method="cfn"){
    if(method=="cfn"){
      ggplot(df, aes(x=df[,indep_var], fill=as.factor(df[,dep_var])))+
        geom_density(alpha=0.4)+labs(title="Density Plot")+
        xlab(indep_var)+
        scale_fill_discrete(name = dep_var)+theme_bw()
    }else if(method=="rgn"){
      ggplot(df, aes(x=df[,indep_var]))+
        geom_density(fill="pink")+
        labs(title="Density Plot")+xlab(indep_var)+theme_bw()
    }
  }
  
  histogram_plot <- function(df,indep_var,dep_var, bins, method = "cfn"){
    if(method=="cfn"){
      ggplot(df, aes(x = df[,indep_var])) + 
        geom_histogram(aes(color = as.factor(df[,dep_var]),fill = as.factor(df[,dep_var])), 
                       alpha = 0.4, bins = bins,
                       position = "identity") +
        xlab(indep_var) +
        scale_fill_manual(name = dep_var,values = c("#00AFBB", "#E7B800")) +
        scale_color_manual(name = dep_var,values = c("#00AFBB", "#E7B800"))
    }else if(method=="rgn"){
      ggplot(df, aes(x=df[,indep_var]))+
        geom_histogram(color="darkblue", fill="blue",alpha = 0.4, bins = bins,
                       position = "identity") +
        xlab(indep_var)
    }
  }
  
  box_plot <- function(df,indep_var,dep_var, sample_rate=0.1, method="cfn"){
    s <- sample.int(n = nrow(df), size = floor(sample_rate*nrow(df)), replace = F)
    if(method=="cfn"){
      ggplot(df[s,], aes(x = factor(1), y = df[s,indep_var])) +
        geom_boxplot(width = 0.4, fill = "white") +
        geom_jitter(aes(color = as.factor(df[s,dep_var])), 
                    width = 0.1, size = 1) +
        scale_color_manual(name = dep_var, values = c("#00AFBB", "#E7B800")) + 
        labs(x = NULL) +   # Remove x axis label
        labs(y = indep_var)
    }else if(method=="rgn_cont"){
      ggplot(df, aes(x = factor(1), y = df[,indep_var])) +
        geom_boxplot(width = 0.4, fill = "orange") +
        labs(x = NULL) +   # Remove x axis label
        labs(y = indep_var) + theme_bw()
    }else if(method=="rgn_cat"){
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
  
  cat_replacement <- function(method,df,indep_var,dep_var){
    if(class(df[,indep_var])=="factor"|class(df[,indep_var])=="character"){
      if(method=="frequency"){
        tbl <- as.data.frame(table(df[,indep_var]))
        var_name <- paste0(indep_var,"_frequency_engineered")
        df[,var_name] <- tbl$Freq[match(unlist(df[,indep_var]),tbl$Var1)]
      }else if(method=="event_rate"){
        tbl <- as.data.frame.matrix(prop.table(table(df[,indep_var], df[,dep_var])))
        tbl$values <- row.names(tbl)
        row.names(tbl) <- NULL
        var_name <- paste0(indep_var,"_event_rate_engineered")
        df[,var_name] <- tbl$`1`[match(unlist(df[,indep_var]), tbl$values)]
      }else if(method=="mean"){
        tbl <- aggregate(df[,dep_var], list(df[,indep_var]), mean)
        tbl <- setNames(tbl,c("variable", "mean"))
        var_name <- paste0(indep_var,"_mean_engineered")
        df[,var_name] <- tbl$`mean`[match(unlist(df[,indep_var]), tbl$variable)]
      }
    }
    return(df)
  }
  
  missing_value_summary <- function(df){
    miss <- sapply(df, function(x) sum(is.na(x)))
    return(miss)
  }
  
  fe_treat_Missing <- function(df, indep_var, method, missing_lookup=NULL, missing_value=NULL){
    
    if(method=="auto"){
      if(class(df[,indep_var])=="factor" | class(df[,indep_var])=="character"){
        df[is.na(df[,indep_var]),indep_var] <- missing_lookup[1,indep_var]
      }else if(class(df[,indep_var])=="numeric" | class(df[,indep_var])=="integer"){
        df[is.na(df[,indep_var]),indep_var] <- missing_lookup[1,indep_var]
      }
    }else if(method=="manual"){
      if(class(df[,indep_var])=="factor"){
        df[,indep_var] <- `levels<-`(addNA(df[,indep_var]), c(levels(df[,indep_var]), missing_value))
      }else if(class(df[,indep_var])=="character"){
        df[is.na(df[,indep_var]),indep_var] <- missing_value
      }else if(class(df[,indep_var])=="numeric" | class(df[,indep_var])=="integer"){
        tryCatch({
          missing_value <- as.numeric(missing_value)
          df[is.na(df[,indep_var]),indep_var] <- missing_value
        },
        warning = function(w){
          #validate("Value is non numeric")
          return(df)
        },
        error = function(e){
          #validate("Value is non numeric")
          return(df)
        })
      }
    }
    return(df)
  }
  
  text_extraction <- function(df, indep_var, method, text_to_find){
    if(method=="contains"){
      var_name <- paste0(indep_var, "_contains_", text_to_find)
      df[,var_name] <- ifelse(grepl(text_to_find, df[,indep_var]), 1, 0)
    }else if(method=="equals"){
      var_name <- paste0(indep_var, "_equals_", text_to_find)
      df[,var_name] <- ifelse(df[,indep_var] == text_to_find, 1, 0)
    }
    return(df)
  }
  
  transformations <- function(df, indep_var, method){
    if(class(df[,indep_var])=="numeric" | class(df[,indep_var])=="integer"){
      if(method=="log"){
        var_name <- paste0(indep_var, "_log")
        df[,var_name] <- log(df[,indep_var])
        df[,var_name][df[,var_name]==Inf | df[,var_name]==-Inf] <- 0
      }else if(method=="exp"){
        var_name <- paste0(indep_var, "_exp")
        df[,var_name] <- exp(df[,indep_var])
      }else if(method=="inv"){
        var_name <- paste0(indep_var, "_inv")
        df[,var_name] <- 1/df[,indep_var]
      }else if(method=="square"){
        var_name <- paste0(indep_var, "_square")
        df[,var_name] <- df[,indep_var]**2
      }
      return(df)
    }else{
      return(df)
    }
  }
  
  delete_var <- function(df, indep_var){
    if(indep_var %in% names(df)){
      df[,indep_var] <- NULL
      return(df)
    }else{
      return(df)
    }
  }
  
  binning <- function(df, indep_var, dep_var, method, bins, cuts){
    if(method=="auto"){
      if(bins>1){
        if(class(df[,indep_var])=="numeric" | class(df[,indep_var])=="integer"){
          var_name <- paste0(indep_var, "_auto_bins")
          df[,var_name] <- as.numeric(cut2(df[,indep_var], g=bins))
        }else if(class(df[,indep_var])=="factor" | class(df[,indep_var])=="character"){
          var_name <- paste0(indep_var, "_auto_bins")
          df[,var_name] <- as.factor(df[,indep_var])
          vars <- names(sort(prop.table(table(df[,var_name])), decreasing = TRUE))[1:(bins-1)]
          vars_to_replace <- setdiff(unique(df[,var_name]),vars)
          levels(df[,var_name])[levels(df[,var_name]) %in% vars_to_replace] <- "Other"
        }
        return(df)
      }else{
        return(df)
      }
    }else if(method=="manual"){
      if(class(df[,indep_var])=="numeric" | class(df[,indep_var])=="integer"){
        tryCatch({
          var_name <- paste0(indep_var, "_manual_binning")
          breaks <- as.numeric(unlist(strsplit(cuts, split=",")))
          df[,var_name] <- as.numeric(df[,indep_var])
          df[,var_name] <- as.numeric(cut(df[,var_name], breaks=breaks))
          return(df)
        },
        warning = function(w){
          return(df)
        },
        error = function(e){
          return(df)
        })
      }else{
        return(df)
      }
    }else if(method=="woe"){
      if(bins>1){
        var_name <- paste0(indep_var, "_woe_binning")
        
        tmp_df <- data.frame(x_var = df[,indep_var], y = df[,dep_var])
        tmp_df <- setNames(tmp_df,c(indep_var,dep_var))
        
        woe_table = list()
        woe_table <- create_infotables(data=tmp_df, valid=NULL, y=dep_var, bins = bins, parallel=FALSE)
        
        tmp_table <- woe_table[["Tables"]][[indep_var]]
        
        df[,var_name] <- df[,indep_var]
        
        if(class(df[,var_name])=="numeric" | class(df[,var_name])=="integer"){
          for(i in 1:nrow(tmp_table)){
            ll <- as.numeric(gsub(".*[[]([^,]+)[,].*", "\\1", tmp_table[i,1]))
            ul <- as.numeric(gsub(".*[,]([^]]+)[]].*", "\\1", tmp_table[i,1]))
            df[,var_name] <- ifelse(between(df[,var_name],ll,ul), tmp_table[i,4], df[,var_name])
          }
        }else if(class(df[,var_name])=="factor" | class(df[,var_name])=="character"){
          df[,var_name] <- as.character(df[,var_name])
          for(i in 1:nrow(tmp_table)){
            df[which(df[,var_name] == tmp_table[i,1]),var_name] <- tmp_table[i,4]
          }
          df[,var_name] <- as.numeric(df[,var_name])
        }
        return(df)
      }else{
        return(df)
      }
    }
  }
  
  iv_calc_cfn <- function(df, dep_var){
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
  
  var_significance <- function(df, dep_var, method="classification"){
    
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
  
  # SQL-style NVL shortcut.
  NVL <- function(x, val) {
    if (is.null(x))
      return(val)
    if (is.vector(x)) {
      x[is.na(x)] <- val
      return(x)
    }
    if (typeof(x) == 'closure')
      return(x)
    stop("typeof(x) == ", typeof(x), " is not supported by NVL")
  }
  
  # Format the evaluation metric string
  format.eval.string <- function(iter, eval_res, eval_err = NULL) {
    if (length(eval_res) == 0)
      validate('no evaluation results')
    enames <- names(eval_res)
    if (is.null(enames))
      validate('evaluation results must have names')
    iter <- sprintf('[%d]\t', iter)
    if (!is.null(eval_err)) {
      if (length(eval_res) != length(eval_err))
        validate('eval_res & eval_err lengths mismatch')
      res <- paste0(sprintf("%s:%f+%f", enames, eval_res, eval_err), collapse = '\t')
    } else {
      res <- paste0(sprintf("%s:%f", enames, eval_res), collapse = '\t')
    }
    return(paste0(iter, res))
  }
  
  #Custom callback evaluation iterations
  cb.print.evaluation <- function(period = 1, showsd = TRUE, method="classification") {
    
    if(method=="classification"){
      output_id <- "cfn_mb_xgb_output"
    }else if(method=="regression"){
      output_id <- "rgn_mb_xgb_output"
    }
    
    callback <- function(env = parent.frame()) {
      if (length(env$bst_evaluation) == 0 ||
          period == 0 ||
          NVL(env$rank, 0) != 0 )
        return()
      
      i <- env$iteration 
      if ((i-1) %% period == 0 || i == env$begin_iteration || i == env$end_iteration) {
        stdev <- if (showsd) env$bst_evaluation_err else NULL
        msg <- format.eval.string(i, env$bst_evaluation, stdev)
        # cat(msg, '\n')
        shinyjs::html(output_id, paste(msg, '\n'), add = TRUE)
      }
    }
    attr(callback, 'call') <- match.call()
    attr(callback, 'name') <- 'cb.print.evaluation'
    callback
  }
  
  #Custom callback early stopping
  cb.early.stop <- function(stopping_rounds, maximize = FALSE, 
                            metric_name = NULL, verbose = TRUE, method="classification") {
    
    if(method=="classification"){
      output_id <- "cfn_mb_xgb_output"
    }else if(method=="regression"){
      output_id <- "rgn_mb_xgb_output"
    }
    
    # state variables
    best_iteration <- -1
    best_ntreelimit <- -1
    best_score <- Inf
    best_msg <- NULL
    metric_idx <- 1
    
    init <- function(env) {
      if (length(env$bst_evaluation) == 0)
        stop("For early stopping, watchlist must have at least one element")
      
      eval_names <- gsub('-', '_', names(env$bst_evaluation))
      if (!is.null(metric_name)) {
        metric_idx <<- which(gsub('-', '_', metric_name) == eval_names)
        if (length(metric_idx) == 0)
          stop("'metric_name' for early stopping is not one of the following:\n",
               paste(eval_names, collapse = ' '), '\n')
      }
      if (is.null(metric_name) &&
          length(env$bst_evaluation) > 1) {
        metric_idx <<- length(eval_names)
        if (verbose)
          # cat('Multiple eval metrics are present. Will use ', 
          #     eval_names[metric_idx], ' for early stopping.\n', sep = '')
          shinyjs::html(output_id, paste('Multiple eval metrics are present. Will use ',
                                         eval_names[metric_idx], ' for early stopping.\n'), add = TRUE)
      }
      
      metric_name <<- eval_names[metric_idx]
      
      # maximize is usually NULL when not set in xgb.train and built-in metrics
      if (is.null(maximize))
        maximize <<- grepl('(_auc|_map|_ndcg)', metric_name)
      
      if (verbose && NVL(env$rank, 0) == 0)
        # cat("Will train until ", metric_name, " hasn't improved in ", 
        #     stopping_rounds, " rounds.\n\n", sep = '')
        shinyjs::html(output_id, paste("Will train until ",
                                       metric_name, " hasn't improved in ",
                                       stopping_rounds, " rounds.\n\n"), add = TRUE)
      
      best_iteration <<- 1
      if (maximize) best_score <<- -Inf
      
      env$stop_condition <- FALSE
      
      if (!is.null(env$bst)) {
        if (!inherits(env$bst, 'xgb.Booster'))
          stop("'bst' in the parent frame must be an 'xgb.Booster'")
        if (!is.null(best_score <- xgb.attr(env$bst$handle, 'best_score'))) {
          best_score <<- as.numeric(best_score)
          best_iteration <<- as.numeric(xgb.attr(env$bst$handle, 'best_iteration')) + 1
          best_msg <<- as.numeric(xgb.attr(env$bst$handle, 'best_msg'))
        } else {
          xgb.attributes(env$bst$handle) <- list(best_iteration = best_iteration - 1,
                                                 best_score = best_score)
        }
      } else if (is.null(env$bst_folds) || is.null(env$basket)) {
        stop("Parent frame has neither 'bst' nor ('bst_folds' and 'basket')")
      }
    }
    
    finalizer <- function(env) {
      if (!is.null(env$bst)) {
        attr_best_score = as.numeric(xgb.attr(env$bst$handle, 'best_score'))
        if (best_score != attr_best_score)
          stop("Inconsistent 'best_score' values between the closure state: ", best_score,
               " and the xgb.attr: ", attr_best_score)
        env$bst$best_iteration = best_iteration
        env$bst$best_ntreelimit = best_ntreelimit
        env$bst$best_score = best_score
      } else {
        env$basket$best_iteration <- best_iteration
        env$basket$best_ntreelimit <- best_ntreelimit
      }
    }
    
    callback <- function(env = parent.frame(), finalize = FALSE) {
      if (best_iteration < 0)
        init(env)
      
      if (finalize)
        return(finalizer(env))
      
      i <- env$iteration
      score = env$bst_evaluation[metric_idx]
      
      if (( maximize && score > best_score) ||
          (!maximize && score < best_score)) {
        
        best_msg <<- format.eval.string(i, env$bst_evaluation, env$bst_evaluation_err)
        best_score <<- score
        best_iteration <<- i
        best_ntreelimit <<- best_iteration * env$num_parallel_tree
        # save the property to attributes, so they will occur in checkpoint
        if (!is.null(env$bst)) {
          xgb.attributes(env$bst) <- list(
            best_iteration = best_iteration - 1, # convert to 0-based index
            best_score = best_score,
            best_msg = best_msg,
            best_ntreelimit = best_ntreelimit)
        }
      } else if (i - best_iteration >= stopping_rounds) {
        env$stop_condition <- TRUE
        env$end_iteration <- i
        if (verbose && NVL(env$rank, 0) == 0)
          # cat("Stopping. Best iteration:\n", best_msg, "\n\n", sep = '')
          shinyjs::html(output_id, paste("Stopping. Best iteration:\n", best_msg, "\n\n"), add = TRUE)
      }
    }
    attr(callback, 'call') <- match.call()
    attr(callback, 'name') <- 'cb.early.stop'
    callback
  }
  
  lift <- function(depvar, predcol, groups=10) {
    if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
    if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
    helper = data.frame(cbind(depvar, predcol))
    helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
      summarise_at(vars(depvar), funs(total = n(),
                                      totalresp=sum(., na.rm = TRUE))) %>%
      mutate(Cumresp = cumsum(totalresp),
             Gain=Cumresp/sum(totalresp)*100,
             Cumlift=Gain/(bucket*(100/groups)))
    return(gaintable)
  }
  
  accuracy_metrics <- function(pred_valid, Y_valid, method="classification"){
    if(method=="classification"){
      prediction <- pred_valid
      Y_valid <- as.factor(Y_valid)
      rocCurve   <- roc(response = Y_valid, predictor = prediction, levels = rev(levels(Y_valid)))
      #Metrics - Fit Statistics
      predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
      Confusion <- table(Predicted = predclass,Actual = Y_valid)
      AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
      Gini <-2*auc(rocCurve)-1
      AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
      AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
      rownames(AUCmetric) <-NULL
      names(AUCmetric) <- c("Metric","Values")
      return(list(AUCmetric = AUCmetric, Confusion = Confusion))
    }else if(method=="regression"){
      mse <- mean((Y_valid - pred_valid)^2)
      rmse <- sqrt(mean((Y_valid - pred_valid)^2))
      mae <- mean(abs(Y_valid - pred_valid))
      mape <- mean(abs((Y_valid-pred_valid)/Y_valid)*100)
      rsq <- cor(Y_valid,pred_valid)^2
      eval_metrics <- data.frame(Metric = c("mse","rmse","mae","mape","r-square"),
                                 Value = c(mse,rmse,mae,mape,rsq))
      return(eval_metrics)
    }
  }
  
  #SAS If Else Statements
  gen_ifelse <- function(tmp_df, leaf_id, new_tree=TRUE){
    var_sign_logic = c()
    id_to_find = leaf_id
    pred <- tmp_df[tmp_df$ID==id_to_find,9]
    booster <- tmp_df[1,1]
    if(nrow(tmp_df)==1){
      statement <- paste("pred",booster," = ",pred,";",sep = "")
      return(statement)
    }else{
      for (j in nrow(tmp_df):1) {
        if(tmp_df[j,4]!="Leaf"){
          if(tmp_df[j,6]==id_to_find){
            var_sign_logic <- c(var_sign_logic, paste0(tmp_df[j,4]," < ",tmp_df[j,5],sep=""))
            id_to_find <- tmp_df[j,3]
          }else{
            if(tmp_df[j,7]==id_to_find){
              var_sign_logic <- c(var_sign_logic, paste0(tmp_df[j,4]," >= ",tmp_df[j,5],sep=""))
              id_to_find <- tmp_df[j,3]
            }else{
              if(tmp_df[j,8]==id_to_find){
                var_sign_logic <- c(var_sign_logic, paste0(tmp_df[j,4],"= .",sep=""))
                id_to_find <- tmp_df[j,3]
              }
            }
          }
        }
      }
      if(new_tree==TRUE){
        statement <- paste("if ",paste(var_sign_logic, collapse = ' and ')," then pred",booster," = ",pred,";",sep = "")
      }else{
        statement <- paste("else if ",paste(var_sign_logic, collapse = ' and ')," then pred",booster," = ",pred,";",sep = "")
      }
      return(statement)
    }
  }
  
  get_leaf_info <- function(xgb_model, output){
    if(xgb_model[["params"]][["booster"]]=="gblinear"){
      dump <- data.frame(xgb.dump(xgb_model, with_stats = TRUE))
      dump <- setNames(dump,"Info")
      return(dump)
    }else{
      dump <- data.frame(xgb.model.dt.tree(model = xgb_model))
      df_leaf <- dump[dump$Feature=="Leaf",]
      leaf_code_list <- list()
      id <- c()
      code <- c()
      gain <- c()
      cover <- c()
      detect_new_tree <- -1
      n = NROW(df_leaf)
      withProgress(message = 'Generating Code', value = 0, {
        for (i in 1:NROW(df_leaf)) {
          incProgress(1/n, detail = paste("For Leaf/Node", i))
          get_leaf_id <- df_leaf[i,3]
          tmp_df <- dump[dump$Tree==df_leaf[i,1],]
          if(df_leaf[i,1] > detect_new_tree){
            tmp <- list(id=get_leaf_id,code=gen_ifelse(tmp_df, get_leaf_id, new_tree = TRUE),gain=df_leaf[i,9],cover=df_leaf[i,10])
            id <- c(id,paste0("'", get_leaf_id))
            code <- c(code,gen_ifelse(tmp_df, get_leaf_id, new_tree = TRUE))
            gain <- c(gain,df_leaf[i,9])
            cover <- c(cover,df_leaf[i,10])
            detect_new_tree <- detect_new_tree + 1
          }else{
            tmp <- list(id=get_leaf_id,code=gen_ifelse(tmp_df, get_leaf_id, new_tree = FALSE),gain=df_leaf[i,9],cover=df_leaf[i,10])
            id <- c(id,paste0("'", get_leaf_id))
            code <- c(code,gen_ifelse(tmp_df, get_leaf_id, new_tree = FALSE))
            gain <- c(gain,df_leaf[i,9])
            cover <- c(cover,df_leaf[i,10])
          }
          leaf_code_list[[get_leaf_id]] <- tmp
        }
      })
      
      leaf_code_df <- data.frame(id=id,code=code,gain=gain,cover=cover)
      
      if(output=="list"){
        return(leaf_code_list)
      }else{
        return(leaf_code_df)
      }
    }
    
  }
  
  container <- function(n_rows, xgb_model, train, validation, imp, dep_var, ratio=1, method="classification", selection="importance"){
    
    sample <- sample.int(n = nrow(train), size = floor(ratio*nrow(train)), replace = F)
    train <- train[sample,]
    rm(sample)
    invisible(gc())
    
    ls_pdp <- list()
    ls_summary <- list()
    ls_density <- list()
    withProgress(message = "Computing Partial Dependence Plots", value = 0, {
      for (i in 1:n_rows) {
        incProgress(1/n_rows, detail = paste("For Variable", i))
        if(selection=="importance"){
          var <- as.character(imp[i,1])
        }else{
          var <- as.character(imp[i])
        }
        pdp_plot <- partial(xgb_model, pred.var = var, train = train[,-which(names(train) %in% c(dep_var))], prob = T, chull = T, parallel = F)
        ls_pdp[[var]] <- pdp_plot
        
        if(is.null(validation)==FALSE){
          summary_df <- setNames(data.frame(unclass(summary(train[,var])),unclass(summary(validation[,var])), check.names = FALSE, stringsAsFactors = FALSE), c("train","validation"))
          Unique <- c(length(unique(train[,var])),length(unique(validation[,var])))
        }else{
          summary_df <- setNames(data.frame(unclass(summary(train[,var])), check.names = FALSE, stringsAsFactors = FALSE), c("train"))
          Unique <- c(length(unique(train[,var])))
        }
        
        summary_df <- rbind(summary_df, Unique=Unique)
        summary_df <- t(summary_df)
        ls_summary[[var]] <- summary_df
        
        if(method=="classification"){
          density_plot <-
            ggplot(train[,c(var,dep_var)], aes_string(x = paste("`",var,"`",sep = ''), fill = as.factor(train[, dep_var]))) +
            geom_density(alpha = 0.4) + labs(title = "Density Plot") +
            xlab(var) +
            scale_fill_discrete(name = dep_var) + theme_bw()
          ls_density[[var]] <- density_plot
        }else if(method=="regression"){
          density_plot <-
            ggplot(train[,c(var,dep_var)], aes_string(x = paste("`",var,"`",sep = ''))) +
            geom_density(color = "darkblue", fill = "lightblue") + 
            labs(title = "Density Plot") +
            xlab(var) + theme_bw()
          ls_density[[var]] <- density_plot
        }
      }
      
      ls_dep_var_info <- list()
      if(method=="classification"){
        ls_dep_var_info[["train_y_info"]] <- dvtable_cfn(train, dep_var)
      }else if(method=="regression"){
        ls_dep_var_info[["train_y_info"]] <- dvtable_rgn(train, dep_var)
      }
      
      if(is.null(validation)==FALSE){
        if(dep_var %in% names(validation)){
          if(method=="classification"){
            ls_dep_var_info[["validation_y_info"]] <- dvtable_cfn(validation, dep_var)
          }else if(method=="regression"){
            ls_dep_var_info[["validation_y_info"]] <- dvtable_rgn(validation, dep_var)
          }
        }else{
          ls_dep_var_info[["validation_y_info"]] <- NULL 
        }
      }
      ls_dep_var_info[["train_y_density_plot"]] <- ggplot(train, aes_string(x=dep_var))+
        geom_density(color="darkblue", fill="lightblue")+
        labs(title="Density Plot of Response Variable")+
        xlab(dep_var)
    })
    ls_container <- list(ls_pdp = ls_pdp, ls_summary = ls_summary, ls_density = ls_density, ls_dep_var_info = ls_dep_var_info)
    return(ls_container)
  }
  
  pdp_plot <- function(ls_container, var, dep_var, method="classification"){
    validate(
      need(ls_container$ls_pdp[[var]], 'Select a valid variable'),
      need(var != '', 'Select a valid variable')
    )
    if(method=="classification"){
      pdp <-  ggplot() + 
        geom_line(data=ls_container$ls_pdp[[var]], aes(ls_container$ls_pdp[[var]][,1], yhat),colour="blue") +
        xlab(var)+
        ylab("Probability")+
        labs(title="Partial Dependence Plot")+
        theme_bw()
    }else if(method=="regression"){
      pdp <- ggplot() + 
        geom_line(data=ls_container$ls_pdp[[var]], aes(ls_container$ls_pdp[[var]][,1], yhat),colour="blue") +
        xlab(var)+
        ylab(dep_var)+
        labs(title="Partial Dependence Plot")+
        theme_bw()
    }
    return(pdp)
  }
  
  ############################################################################
  #------------------------Create Classification Model------------------------
  ############################################################################
  
  dftmp <- reactiveValues(train=NULL, test=NULL, raw_train_head=NULL, train_structure=NULL,
                          train_ohe=FALSE, dep_var=NULL, preprocess_train_head=NULL,
                          missing_lookup=NULL,preprocess_train_structure=NULL,fe_indep_var=NULL, 
                          iv_calc_df=NULL,ss_calc_df=NULL, cfn_corr_df=NULL, train_subset=NULL, 
                          validation_subset=NULL,Y_train_subset=NULL, Y_valid_subset=NULL, 
                          Y_test=NULL, watchlist=NULL,dtrain_subset=NULL, dvalid_subset=NULL,
                          dtest=NULL, xgb_params=NULL, xgb_params_eval_maximize=TRUE, 
                          xgb_model=NULL, train_lift=NULL, validation_lift=NULL, test_lift=NULL,
                          train_accuracy = NULL,valid_accuracy=NULL, test_accuracy=NULL, 
                          pred_train_subset=NULL, pred_valid_subset=NULL, pred_test=NULL, 
                          imp=NULL, container=NULL, warning_message=NULL, error_message=NULL, 
                          ttest_anova_df=NULL, all_continuous=FALSE)
  
  observeEvent(input$cfn_file1,{
    tryCatch({
      path <- normalizePath(input$cfn_getwd)
      file <- input$cfn_file_name
      if(file.exists(paste(path,file,sep = '\\'))==TRUE){
        dftmp$train <- fread(paste(path,file,sep = '\\'),
                             header = TRUE,
                             data.table = FALSE,
                             stringsAsFactors = TRUE,
                             sep = input$cfn_sep,
                             skip = as.numeric(input$cfn_upload_start),
                             nrows = as.numeric(input$cfn_upload_nrows),
                             na.strings = input$cfn_upload_na_strings)
        
        if(is.null(dftmp$test)==FALSE){
          if(length(setdiff(names(dftmp$test), names(dftmp$train))) <= 1 & length(setdiff(names(dftmp$train), names(dftmp$test))) <= 1){
            dftmp$warning_message = NULL
            dftmp$error_message = NULL
            dftmp$raw_train_head <- head(dftmp$train,n=20)
          }else{
            dftmp$warning_message = "Train and test names do not match"
            dftmp$error_message = "Train and test names do not match"
          }
        }else{
          dftmp$raw_train_head <- head(dftmp$train,n=20)
          dftmp$warning_message <- NULL
          dftmp$error_message <- NULL
        }
      }else{
        dftmp$warning_message <- "No such file or directory found"
      }
    },
    warning = function(w){
      dftmp$warning_message <- "No such file or directory found"
    },
    error = function(e){
      dftmp$error_message <- "Error parsing the file"
    })
    return(TRUE)
  })
  
  observeEvent(input$cfn_file2,{
    tryCatch({
      path <- normalizePath(input$cfn_getwd)
      file <- input$cfn_file_name
      if(file.exists(paste(path,file,sep = '\\'))==TRUE){
        dftmp$test <- fread(paste(path,file,sep = '\\'),
                            header = TRUE,
                            data.table = FALSE,
                            stringsAsFactors = TRUE,
                            sep = input$cfn_sep,
                            skip = as.numeric(input$cfn_upload_start),
                            nrows = as.numeric(input$cfn_upload_nrows),
                            na.strings = input$cfn_upload_na_strings)
        
        if(length(setdiff(names(dftmp$test), names(dftmp$train))) <= 1 & length(setdiff(names(dftmp$train), names(dftmp$test))) <= 1){
          dftmp$warning_message = NULL
          dftmp$error_message = NULL
        }else{
          dftmp$warning_message = "Train and test names do not match"
          dftmp$error_message = "Train and test names do not match"
        }
      }else{
        dftmp$warning_message <- "No such file or directory found"
      }
    },
    warning = function(w){
      dftmp$warning_message <- "No such file or directory found"
    },
    error = function(e){
      dftmp$error_message <- "Error parsing the file"
    })
    return(TRUE)
  })
  
  output$raw_output <- renderTable({
    input$cfn_file1
    input$cfn_file2
    isolate({
      validate(need(input$cfn_getwd,"Directory not provided"))
      validate(need(input$cfn_file_name,"File not provided"))
      validate(need(is.null(dftmp$warning_message)==TRUE,dftmp$warning_message))
      validate(need(is.null(dftmp$error_message)==TRUE,dftmp$error_message))
      dftmp$raw_train_head
    })
  })

  #Data Preprocess
  output$cfn_select_dv <- renderUI({
    if(!is.null(dftmp$train)){
      choices = names(dftmp$train)
    }
    selectInput("dep_var", "Choose Dependent Variable:",
                choices = choices, selected = dftmp$dep_var)
  })
  
  data_preprocess <- observeEvent(input$data_preprocess_confirm,{
    
    dftmp$dep_var <- input$dep_var
    
    if(length(unique(dftmp$train[,input$dep_var]))==2){
      dftmp$train <- treat_dep_var_cfn(dftmp$train,input$dep_var)
      if(input$dep_var %in% names(dftmp$test)){
        dftmp$test <- treat_dep_var_cfn(dftmp$test,input$dep_var)
      }
      if(input$cfn_missing){
        dftmp$missing_lookup <- treat_Missing(dftmp$train)
        dftmp$train <- replace_Missing(dftmp$train, dftmp$missing_lookup)
        if(is.null(dftmp$test)==FALSE){
          dftmp$test <- replace_Missing(dftmp$test, dftmp$missing_lookup)
        }
      }
      
      if(input$cfn_ohe){
        if(dftmp$train_ohe == FALSE){
          dftmp$train <- create_dummies(dftmp$train)
          if(is.null(dftmp$test)==FALSE){
            dftmp$test <- create_dummies(dftmp$test)
          }
          dftmp$train_ohe <- TRUE
        }
      }
    }
    
    #Assigning global variables required for temp view
    dftmp$preprocess_train_head <- head(dftmp$train,n=15)
    dftmp$preprocess_train_structure <- strtable(dftmp$train)
  })
  
  output$data_preprocess_output <- renderTable({
    if(input$data_preprocess_confirm == 0) return()
    isolate({
      validate(need(length(unique(dftmp$train[,input$dep_var]))==2,"Dependent variable is not binary"))
      
      if(is.null(dftmp$test)==FALSE){
        if(input$dep_var %in% names(dftmp$test)){
          validate(need(length(unique(dftmp$test[,input$dep_var]))==2,"Dependent variable for test dataset is not binary"))
        }
      }
      dftmp$preprocess_train_head
    })
  })
  
  output$cfn_data_structure_output <- renderDataTable({
    if(input$data_preprocess_confirm == 0) return()
    # dftmp$train_structure
    dftmp$preprocess_train_structure
  },options = list(pageLength = 10))
  
  outputOptions(output,"data_preprocess_output",suspendWhenHidden=FALSE)
  
  observeEvent(input$data_preprocess_refresh,{
    dftmp$preprocess_train_head <- head(dftmp$train,n=15)
    dftmp$preprocess_train_structure <- strtable(dftmp$train)
  })
  
  #EDA
  output$cfn_select_iv <- renderUI({
    selectInput("indep_var", "Choose Independent Variable To Plot Graph:",
                setdiff(names(dftmp$train),dftmp$dep_var))
  })
  
  output$cfn_eda_dv_info_output <- renderTable({
    #if(input$action_2 == 0) return()
    # isolate(dvtable_cfn(dftmp$train, input$dep_var))
    dvtable_cfn(dftmp$train, dftmp$dep_var)
  })
  
  output$cfn_eda_dv_info_plot <- renderPlot({
    ggplot(dftmp$train, aes_string(x=dftmp$dep_var))+
      geom_density(color="darkblue", fill="lightblue")+
      labs(title="Density Plot of Response Variable")+
      xlab(dftmp$dep_var)
  })
  
  output$cfn_eda_iv_summary_output <- renderPrint({
    tryCatch({
      var_summary(dftmp$train, input$indep_var)
    },
    error = function(e){
      validate("")
    }
    )
  })
  
  output$cfn_eda_iv_dist_output <- renderPlot({
    
    tryCatch({
      indep_var <- input$indep_var
      dep_var <- dftmp$dep_var
      bins <- input$bins
      sample_rate <- input$sample
      
      # cfn_eda_plot <- isolate({
      if(class(dftmp$train[,indep_var])=="integer" | class(dftmp$train[,indep_var])=="numeric"){
        if(input$cfn_cont_plot_type == "density"){
          #Density Plot
          density_plot(dftmp$train, indep_var, dep_var, method="cfn")
        }else if(input$cfn_cont_plot_type == "histogram"){
          #Histogram
          histogram_plot(dftmp$train, indep_var, dep_var, bins, method = "cfn")
        }else if(input$cfn_cont_plot_type == "boxplot"){
          #Boxplot
          box_plot(dftmp$train, indep_var, dep_var, sample_rate, method="cfn")
        }
      }else if(class(dftmp$train[,indep_var]) == "factor"){
        if(input$cfn_cat_plot_type == "bar"){
          bar_plot(dftmp$train, indep_var, dep_var)
        }else if(input$cfn_cat_plot_type == "stacked_bar"){
          stacked_bar_plot(dftmp$train, indep_var, dep_var)
        }else if(input$cfn_cat_plot_type == "pie"){
          pie_chart(dftmp$train, indep_var)
        }
      }
    },
    error = function(e){
      validate("")
    }
    )
  })
  
  output$cfn_eda_missing_summary <- renderPrint({
    missing_value_summary(dftmp$train)
  })
  
  #Feature Engineering
  output$cfn_fe_select_iv <- renderUI({
    selectInput("cfn_fe_indep_var", "Choose Independent Variable To Implement Feature Engineering On:",
                setdiff(names(dftmp$train),dftmp$dep_var), selected = dftmp$fe_indep_var)
  })
  
  feature_engineering <- observeEvent(input$cfn_fe_action,{
    dftmp$fe_indep_var <- input$cfn_fe_indep_var
      
    #One Hot Encoding
    if(input$cfn_fe_radio_button == "cfn_fe_ohe"){
      if(dftmp$train_ohe == FALSE){
        dftmp$train <- create_dummies(dftmp$train)
        dftmp$train_ohe <- TRUE
        if(is.null(dftmp$test)==FALSE){
          dftmp$test <- create_dummies(dftmp$test)
        }
      }
    }
    
    #Categorical Replacement
    if(input$cfn_fe_radio_button == "cfn_fe_cat_replacement" & input$cfn_fe_cat_replacement_radio_button == "cfn_fe_cat_freq"){
        dftmp$train <- cat_replacement(method = "frequency", 
                                       df = dftmp$train,indep_var = input$cfn_fe_indep_var, 
                                       dep_var = dftmp$dep_var)
        if(is.null(dftmp$test)==FALSE){
          dftmp$test <- cat_replacement(method = "frequency", 
                                        df = dftmp$test,indep_var = input$cfn_fe_indep_var, 
                                        dep_var = dftmp$dep_var)
        }
    }
    if(input$cfn_fe_radio_button == "cfn_fe_cat_replacement" & input$cfn_fe_cat_replacement_radio_button == "cfn_fe_cat_event_rate"){
        dftmp$train <- cat_replacement(method = "event_rate", 
                                       df = dftmp$train,indep_var = input$cfn_fe_indep_var, 
                                       dep_var = dftmp$dep_var)
        if(is.null(dftmp$test)==FALSE){
          dftmp$test <- cat_replacement(method = "event_rate", 
                                         df = dftmp$test,indep_var = input$cfn_fe_indep_var, 
                                         dep_var = dftmp$dep_var)
        }
    }
    
    #Binning
    if(input$cfn_fe_radio_button == "cfn_fe_bins" & input$cfn_fe_bins_radio_button == "cfn_fe_bin_woe"){
      dftmp$train <- binning(dftmp$train, input$cfn_fe_indep_var, dftmp$dep_var, method = "woe", bins = input$cfn_fe_bin_woe_value_input)
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- binning(dftmp$test, input$cfn_fe_indep_var, dftmp$dep_var, method = "woe", bins = input$cfn_fe_bin_woe_value_input)
      }
    }
    if(input$cfn_fe_radio_button == "cfn_fe_bins" & input$cfn_fe_bins_radio_button == "cfn_fe_bin_auto_cuts"){
      dftmp$train <- binning(dftmp$train, input$cfn_fe_indep_var, dftmp$dep_var, method = "auto", bins = input$cfn_fe_bin_auto_cuts_value_input)
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- binning(dftmp$test, input$cfn_fe_indep_var, dftmp$dep_var, method = "auto", bins = input$cfn_fe_bin_auto_cuts_value_input)
      }
    }
    if(input$cfn_fe_radio_button == "cfn_fe_bins" & input$cfn_fe_bins_radio_button == "cfn_fe_bin_manual_cuts"){
      dftmp$train <- binning(dftmp$train, input$cfn_fe_indep_var, dftmp$dep_var, method = "manual", cuts = input$cfn_fe_bin_manual_cuts_value_input)
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- binning(dftmp$test, input$cfn_fe_indep_var, dftmp$dep_var, method = "manual", cuts = input$cfn_fe_bin_manual_cuts_value_input)
      }
    }
    
    #Missing Value Treatment
    if(input$cfn_fe_radio_button == "cfn_fe_missing" & input$cfn_fe_missing_radio_button == "cfn_fe_central_tendency"){
        dftmp$missing_lookup <- treat_Missing(dftmp$train)
        dftmp$train <- fe_treat_Missing(dftmp$train, input$cfn_fe_indep_var, method = "auto", missing_lookup = dftmp$missing_lookup)
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- fe_treat_Missing(dftmp$test, input$cfn_fe_indep_var, method = "auto", missing_lookup = dftmp$missing_lookup)
      }
    }
    if(input$cfn_fe_radio_button == "cfn_fe_missing" & input$cfn_fe_missing_radio_button == "cfn_fe_missing_value"){
        dftmp$train <- fe_treat_Missing(dftmp$train, input$cfn_fe_indep_var, method = "manual", missing_value = input$cfn_fe_missing_value_input)
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- fe_treat_Missing(dftmp$test, input$cfn_fe_indep_var, method = "manual", missing_value = input$cfn_fe_missing_value_input)
      }
    }
    
    #Text Extraction
    if(input$cfn_fe_radio_button == "cfn_fe_text_extraction" & input$cfn_fe_text_extract_radio_button == "cfn_fe_text_contains"){
        dftmp$train <- text_extraction(dftmp$train, input$cfn_fe_indep_var, method = "contains", text_to_find = input$cfn_fe_text_extraction_value_input)
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- text_extraction(dftmp$test, input$cfn_fe_indep_var, method = "contains", text_to_find = input$cfn_fe_text_extraction_value_input)
      }
    }
    if(input$cfn_fe_radio_button == "cfn_fe_text_extraction" & input$cfn_fe_text_extract_radio_button == "cfn_fe_text_equals"){
        dftmp$train <- text_extraction(dftmp$train, input$cfn_fe_indep_var, method = "equals", text_to_find = input$cfn_fe_text_extraction_value_input)
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- text_extraction(dftmp$test, input$cfn_fe_indep_var, method = "equals", text_to_find = input$cfn_fe_text_extraction_value_input)
      }
    }
    
    #Transformations
    if(input$cfn_fe_radio_button == "cfn_fe_transformations" & input$cfn_fe_transformations_radio_button == "log"){
      dftmp$train <- transformations(dftmp$train, input$cfn_fe_indep_var, method = "log")
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- transformations(dftmp$test, input$cfn_fe_indep_var, method = "log")
      }
    }
    if(input$cfn_fe_radio_button == "cfn_fe_transformations" & input$cfn_fe_transformations_radio_button == "exp"){
      dftmp$train <- transformations(dftmp$train, input$cfn_fe_indep_var, method = "exp")
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- transformations(dftmp$test, input$cfn_fe_indep_var, method = "exp")
      }
    }
    if(input$cfn_fe_radio_button == "cfn_fe_transformations" & input$cfn_fe_transformations_radio_button == "inv"){
      dftmp$train <- transformations(dftmp$train, input$cfn_fe_indep_var, method = "inv")
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- transformations(dftmp$test, input$cfn_fe_indep_var, method = "inv")
      }
    }
    if(input$cfn_fe_radio_button == "cfn_fe_transformations" & input$cfn_fe_transformations_radio_button == "square"){
      dftmp$train <- transformations(dftmp$train, input$cfn_fe_indep_var, method = "square")
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- transformations(dftmp$test, input$cfn_fe_indep_var, method = "square")
      }
    }
    
    #Delete Variable
    if(input$cfn_fe_radio_button == "cfn_fe_remove_var" & input$cfn_fe_remove_var_radio_button == "cfn_fe_remove_var_dropdown"){
      dftmp$train <- delete_var(dftmp$train, input$cfn_fe_indep_var)
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- delete_var(dftmp$test, input$cfn_fe_indep_var)
      }
    }
    if(input$cfn_fe_radio_button == "cfn_fe_remove_var" & input$cfn_fe_remove_var_radio_button == "cfn_fe_remove_var_name"){
      dftmp$train <- delete_var(dftmp$train, input$cfn_fe_remove_var_value_input)
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- delete_var(dftmp$test, input$cfn_fe_remove_var_value_input)
      }
    }
    
  })

  one_hot_encoding <- observeEvent(input$action_3,{
    if(dftmp$train_ohe == FALSE){
      dftmp$train <- create_dummies(dftmp$train)
      dftmp$train_ohe <- TRUE
      if(is.null(dftmp$test)==FALSE){
        dftmp$test <- create_dummies(dftmp$test)
      }
    }
    constant_var_logic <- sapply(dftmp$train, function(x) length(unique(x))==1)
    if('TRUE' %in% constant_var_logic){
      constant_vars <- names(which(constant_var_logic,TRUE))
      dftmp$train <- dftmp$train[,setdiff(colnames(dftmp$train),constant_vars)]
    }
  })
  
  output$cfn_fe_output <- renderTable({
    head(dftmp$train,n=15)
  })
  
  observeEvent(input$action_3,{
    if(length(names(which(sapply(dftmp$train, function(x) is.numeric(x)|is.integer(x)))))==ncol(dftmp$train)){
      dftmp$all_continuous = TRUE
    }
  })
  
  output$cfn_fe_qc <- renderPrint({
    if(dftmp$all_continuous==TRUE){
      print("Status: Ok")
    }
    else{
      validate(need(dftmp$all_continuous==TRUE, "Not all features are numeric. Press proceed button."))
    }
  })
  outputOptions(output,"cfn_fe_qc",suspendWhenHidden=FALSE)
  
  #Variable Selection Output
  output$cfn_vs_output <- renderTable({
    head(dftmp$train,n=20)
  })
  
  output$cfn_vs_select_iv_corr <- renderUI({
    selectInput("cfn_vs_indep_var", "See Variable Correlation:",
                setdiff(names(dftmp$train),dftmp$dep_var), selected = NULL)
  })
  
  output$cfn_vs_select_vars_manual_dropdown <- renderUI({
    pickerInput("select_vars_manual","Independent Variables", 
                choices=setdiff(names(dftmp$train),dftmp$dep_var), 
                selected = setdiff(names(dftmp$train),dftmp$dep_var), 
                options = list(`actions-box` = TRUE),multiple = T)
  })
  
  observeEvent(input$cfn_vs_compute_corr,{
    dftmp$cfn_corr_df <-  var_corr(dftmp$train,input$cfn_vs_indep_var)
  })
  
  output$cfn_vs_corr_output <- renderDataTable({
    req(input$cfn_vs_compute_corr)
    dftmp$cfn_corr_df <- dftmp$cfn_corr_df[dftmp$cfn_corr_df$Variable %in% names(dftmp$train),]
    dftmp$cfn_corr_df
  },options = list(pageLength = 10))
  
  observeEvent(input$cfn_vs_compute_iv,{
    dftmp$iv_calc_df <-  iv_calc_cfn(dftmp$train,dftmp$dep_var)
  })

  output$iv_vs_output <- renderDataTable({
    dftmp$iv_calc_df
  },options = list(pageLength = 10))
  
  observeEvent(input$cfn_vs_compute_ttest_anova,{
    dftmp$ttest_anova_df <-  compute_statistical_significance(dftmp$train,dftmp$dep_var,method = "classification")
  })
  
  output$cfn_ttest_anova_vs_output <- renderDataTable({
    dftmp$ttest_anova_df
  },options = list(pageLength = 10))
  
  observeEvent(input$cfn_vs_compute_ss,{
    if(is.null(dftmp$ss_calc_df)==TRUE){
      dftmp$ss_calc_df <-  var_significance(dftmp$train,dftmp$dep_var,method="classification")
    }
  })
  
  output$ss_vs_output <- renderDataTable({
    # req(input$cfn_vs_compute_ss)
    dftmp$ss_calc_df
  },options = list(pageLength = 10))
  
  #Variable Selection Filter
  observeEvent(input$action_4,{
    if(input$cfn_vs_select_method=="iv"){
      if(is.null(dftmp$iv_calc_df)==TRUE){
        dftmp$iv_calc_df <-  iv_calc_cfn(dftmp$train,dftmp$dep_var)
      }
      ll <- input$iv_range[1]
      ul <- input$iv_range[2]
      vars <- as.character(dftmp$iv_calc_df[(dftmp$iv_calc_df$IV>=ll & dftmp$iv_calc_df$IV<=ul),1])
      dftmp$iv_calc_df <- dftmp$iv_calc_df[dftmp$iv_calc_df$Variable %in% vars,]
      dftmp$ss_calc_df <- dftmp$ss_calc_df[dftmp$ss_calc_df$Variable %in% vars,]
      vars <- c(vars,dftmp$dep_var)
      dftmp$train <- dftmp$train[,vars]
      if(is.null(dftmp$test)==FALSE){
        if(dftmp$dep_var %in% names(dftmp$test)){
          dftmp$test <- dftmp$test[,vars]
        }else{
          dftmp$test <- dftmp$test[,setdiff(vars,dftmp$dep_var)]
        }
      }
    }else if(input$cfn_vs_select_method=="ttest_anova"){
      if(is.null(dftmp$ttest_anova_df)==TRUE){
        dftmp$ttest_anova_df <-  compute_statistical_significance(dftmp$train,dftmp$dep_var,method="classification")
      }
      vars <- as.character(dftmp$ttest_anova_df[(dftmp$ttest_anova_df$p_values<=input$cfn_ttest_anova_alpha),1])
      dftmp$ttest_anova_df <- dftmp$ttest_anova_df[dftmp$ttest_anova_df$Variable %in% vars,]
      dftmp$ss_calc_df <- dftmp$ss_calc_df[dftmp$ss_calc_df$Variable %in% vars,]
      vars <- c(vars,dftmp$dep_var)
      dftmp$train <- dftmp$train[,vars]
      if(is.null(dftmp$test)==FALSE){
        if(dftmp$dep_var %in% names(dftmp$test)){
          dftmp$test <- dftmp$test[,vars]
        }else{
          dftmp$test <- dftmp$test[,setdiff(vars,dftmp$dep_var)]
        }
      }
    }else if(input$cfn_vs_select_method=="logit"){
      if(is.null(dftmp$ss_calc_df)==TRUE){
        dftmp$ss_calc_df <-  var_significance_cfn(dftmp$train,dftmp$dep_var)
      }
      vars <- as.character(dftmp$ss_calc_df[(dftmp$ss_calc_df$p_values<=0.01),1])
      dftmp$iv_calc_df <- dftmp$iv_calc_df[dftmp$iv_calc_df$Variable %in% vars,]
      dftmp$ss_calc_df <- dftmp$ss_calc_df[dftmp$ss_calc_df$Variable %in% vars,]
      vars <- c(vars, dftmp$dep_var)
      dftmp$train <- dftmp$train[,vars]
      if(is.null(dftmp$test)==FALSE){
        if(dftmp$dep_var %in% names(dftmp$test)){
          dftmp$test <- dftmp$test[,vars]
        }else{
          dftmp$test <- dftmp$test[,setdiff(vars,dftmp$dep_var)]
        }
      }
    }else if(input$cfn_vs_select_method=="manual"){
      vars <- input$select_vars_manual
      dftmp$ss_calc_df <- dftmp$ss_calc_df[dftmp$ss_calc_df$Variable %in% vars,]
      dftmp$iv_calc_df <- dftmp$iv_calc_df[dftmp$iv_calc_df$Variable %in% vars,]
      vars <- c(vars, dftmp$dep_var)
      dftmp$train <- dftmp$train[,vars]
      if(is.null(dftmp$test)==FALSE){
        if(dftmp$dep_var %in% names(dftmp$test)){
          dftmp$test <- dftmp$test[,vars]
        }else{
          dftmp$test <- dftmp$test[,setdiff(vars,dftmp$dep_var)]
        }
      }
    }
  })
  
  #XGB Model Build
  observeEvent(input$cfn_mb_xgb_compute, {
      set.seed(123)
      sample <- sample.int(n = nrow(dftmp$train), size = floor(input$cfn_mb_xgb_train_split*nrow(dftmp$train)), replace = F)
      dftmp$validation_subset  <- dftmp$train[-sample,]
      dftmp$train_subset <- dftmp$train[sample,]
      rm(sample)
      invisible(gc())
      
      dftmp$Y_train_subset <- dftmp$train_subset[,dftmp$dep_var]
      dftmp$Y_valid_subset <- dftmp$validation_subset[,dftmp$dep_var]
      if(is.null(dftmp$test)==FALSE){
        if(dftmp$dep_var %in% names(dftmp$test)){
          dftmp$Y_test <- dftmp$test[,dftmp$dep_var] 
        }
      }
      
      dftmp$dtrain_subset  <- xgb.DMatrix(as.matrix(dftmp$train_subset[,-which(names(dftmp$train_subset) %in% c(dftmp$dep_var))]), label = dftmp$Y_train_subset)
      dftmp$dvalid_subset  <- xgb.DMatrix(as.matrix(dftmp$validation_subset[,-which(names(dftmp$validation_subset) %in% c(dftmp$dep_var))]), label = dftmp$Y_valid_subset)
      if(is.null(dftmp$test)==FALSE){
        if(dftmp$dep_var %in% names(dftmp$test)){
          dftmp$dtest  <- xgb.DMatrix(as.matrix(dftmp$test[,-which(names(dftmp$test) %in% c(dftmp$dep_var))]), label = dftmp$Y_test)
        }else{
          dftmp$dtest  <- xgb.DMatrix(as.matrix(dftmp$test))
        }
      }
      
    dftmp$watchlist <- list(train = dftmp$dtrain_subset, valid = dftmp$dvalid_subset)
    
    if(input$cfn_mb_xgb_eval_metric=="auc"){
      eval_metric = "auc"
    }else if(input$cfn_mb_xgb_eval_metric=="logloss"){
      eval_metric = "logloss"
      dftmp$xgb_params_eval_maximize <- FALSE
    }
    
    if(input$cfn_mb_xgb_param_booster=="gbtree"){
      booster = "gbtree"
    }else if(input$cfn_mb_xgb_param_booster=="dart"){
      booster = "dart"
    }
    
    dftmp$xgb_params <- list(objective = "binary:logistic",
                             eval_metric = eval_metric,
                             booster=booster,
                             max_depth = input$cfn_mb_xgb_param_max.depth,
                             eta = input$cfn_mb_xgb_param_eta,
                             gamma = input$cfn_mb_xgb_param_gamma, 
                             subsample = input$cfn_mb_xgb_param_subsample,
                             colsample_bytree = input$cfn_mb_xgb_param_colsample.by.tree, 
                             min_child_weight = input$cfn_mb_xgb_param_min.child.weight,
                             max_delta_step = input$cfn_mb_xgb_param_max.delta.step,
                             alpha = input$cfn_mb_xgb_param_alpha,
                             lambda = input$cfn_mb_xgb_param_lambda)
    
    shinyjs::html("cfn_mb_xgb_output", "")
    
    dftmp$xgb_model <- xgb.train(params=dftmp$xgb_params,
                                 dftmp$dtrain_subset, nrounds = input$cfn_mb_xgb_param_nrounds, dftmp$watchlist,
                                 callbacks = list(cb.print.evaluation(period = input$cfn_mb_xgb_param_print.every.n, method = "classification"), 
                                                  cb.early.stop(stopping_rounds = input$cfn_mb_xgb_param_early.stopping.rounds, maximize = dftmp$xgb_params_eval_maximize, method = "classification")), nthread=6)
    
    dftmp$imp <- xgb.importance(colnames(dftmp$train_subset), model=dftmp$xgb_model)
    
    dftmp$pred_train_subset = predict(dftmp$xgb_model, dftmp$dtrain_subset)
    dftmp$train_lift <- lift(dftmp$train_subset[,dftmp$dep_var], dftmp$pred_train_subset, groups = 10)
    dftmp$pred_valid_subset = predict(dftmp$xgb_model, dftmp$dvalid_subset)
    dftmp$validation_lift <- lift(dftmp$validation_subset[,dftmp$dep_var], dftmp$pred_valid_subset, groups = 10)
    if(is.null(dftmp$test)==FALSE){
      dftmp$pred_test = predict(dftmp$xgb_model, dftmp$dtest)
      if(dftmp$dep_var %in% names(dftmp$test)){
        dftmp$test_lift <- lift(dftmp$test[,dftmp$dep_var], dftmp$pred_test, groups = 10)
      }
    }
    
  })
  
  output$cfn_mb_xgb_lift_train <- renderTable({
    dftmp$train_lift
  })
  
  output$cfn_mb_xgb_lift_valid <- renderTable({
    dftmp$validation_lift
  })
  
  output$cfn_mb_xgb_lift_test <- renderTable({
    validate(need(dftmp$test,"Test dataset not uploaded"))
    validate(need(dftmp$dep_var %in% names(dftmp$test), "Response variable not found"))
    dftmp$test_lift
  })
  
  cfn_mb_xgb_stats <- observeEvent(input$cfn_mb_xgb_compute,{
    dftmp$train_accuracy <- accuracy_metrics(dftmp$pred_train_subset, dftmp$Y_train_subset, method="classification")
    
    dftmp$valid_accuracy <- accuracy_metrics(dftmp$pred_valid_subset, dftmp$Y_valid_subset, method="classification")
    
    if(is.null(dftmp$test)==FALSE){
      if(dftmp$dep_var %in% names(dftmp$test)){
        dftmp$test_accuracy <- accuracy_metrics(dftmp$pred_test, dftmp$Y_test, method="classification")
      }
    }
  })
  
  output$cfn_mb_xgb_stats_train <- renderPrint({
    validate(need(dftmp$train_accuracy[["AUCmetric"]],"Model not built yet"))
    validate(need(dftmp$train_accuracy[["Confusion"]],"Model not built yet"))
    print(dftmp$train_accuracy[["AUCmetric"]])
    cat("\n")
    print(dftmp$train_accuracy[["Confusion"]])
  })
  outputOptions(output,"cfn_mb_xgb_stats_train",suspendWhenHidden=FALSE)
  
  output$cfn_mb_xgb_stats_valid <- renderPrint({
    validate(need(dftmp$valid_accuracy[["AUCmetric"]],"Model not built yet"))
    validate(need(dftmp$valid_accuracy[["Confusion"]],"Model not built yet"))
    print(dftmp$valid_accuracy[["AUCmetric"]])
    cat("\n")
    print(dftmp$valid_accuracy[["Confusion"]])
  })
  outputOptions(output,"cfn_mb_xgb_stats_valid",suspendWhenHidden=FALSE)
  
  output$cfn_mb_xgb_stats_test <- renderPrint({
    validate(need(dftmp$test,"Test dataset not uploaded"))
    validate(need(dftmp$dep_var %in% names(dftmp$test), "Response variable not found"))
    print(dftmp$test_accuracy[["AUCmetric"]])
    cat("\n")
    print(dftmp$test_accuracy[["Confusion"]])
  })
  outputOptions(output,"cfn_mb_xgb_stats_test",suspendWhenHidden=FALSE)
  
  #Model Interpretation
  output$cfn_mb_xgb_varimp_iv <- renderUI({
    if(is.null(dftmp$imp)==FALSE){
      numericInput("cfn_mb_xgb_varimp_top_vars","Select Number of Variables To Display",min = 1,max = nrow(dftmp$imp),step = 1,value = round(nrow(dftmp$imp)/2))
    }else if(is.null(dftmp$imp)==TRUE){
      numericInput("cfn_mb_xgb_varimp_top_vars","Select Number of Variables To Display",min = 1,max = ncol(dftmp$train),step = 1,value = round(ncol(dftmp$train)/2))
    }
  })
  
  output$cfn_mb_xgb_var_imp <- renderPlot({
    if(is.null(dftmp$imp)==TRUE | input$cfn_mb_xgb_varimp_plot<=0){
      return()
    }else{
      gain_or_cover <- switch(input$cfn_mb_xgb_varimp,
                              gain = "Gain",
                              cover = "Cover")
      input$cfn_mb_xgb_varimp_plot
      isolate(
        ggplot(dftmp$imp[1:input$cfn_mb_xgb_varimp_top_vars,], aes(x=reorder(Feature,get(paste0(gain_or_cover))), y=get(paste0(gain_or_cover)),fill=get(paste0(gain_or_cover))))+ 
          geom_bar(stat="identity", position="dodge", show.legend = T)+ coord_flip()+
          ylab(paste0(gain_or_cover))+
          xlab(as.character("Variable Importance"))+
          ggtitle(paste0("Variable Importance by ",paste0(gain_or_cover)))+
          scale_fill_gradient(low="red", high="blue")+labs(fill=paste0(gain_or_cover))+theme_bw()
      )
    }
  }, height = 750)
  
  output$cfn_mb_xgb_pdp_vars <- renderUI({
    if(is.null(dftmp$imp)==FALSE){
      numericInput("cfn_mb_xgb_pdp_top_vars","Select Number of Variables To Display",min = 1,max = nrow(dftmp$imp),step = 1,value = round(nrow(dftmp$imp)/2))
    }else if(is.null(dftmp$imp)==TRUE){
      numericInput("cfn_mb_xgb_pdp_top_vars","Select Number of Variables To Display",min = 1,max = ncol(dftmp$train),step = 1,value = round(ncol(dftmp$train)/2))
    }
  })
  
  output$cfn_mb_xgb_pdp_vars_manual <- renderUI({
    pickerInput("cfn_pdp_vars_manual","Independent Variables",
                choices=setdiff(names(dftmp$train),dftmp$dep_var),
                selected = setdiff(names(dftmp$train),dftmp$dep_var),
                options = list(`actions-box` = TRUE),multiple = T)
  })
  
  output$cfn_mb_xgb_pdp_iv <- renderUI({
    input$cfn_mb_xgb_gen_pdp
    isolate({
      validate(need(is.null(dftmp$imp)==FALSE,""))
      if(input$cfn_pdp_select_method=="manual"){
        validate(need(length(input$cfn_pdp_vars_manual)>0,""))
        if(is.null(dftmp$imp)==FALSE){
          input$cfn_mb_xgb_gen_pdp
          isolate(
            selectInput("cfn_mb_xgb_pdp_dropdown", "Choose Independent Variable To View PDP Plot:",
                        input$cfn_pdp_vars_manual)
          )
        }
      }else if(input$cfn_pdp_select_method=="importance"){
        validate(need(input$cfn_mb_xgb_pdp_top_vars>0,""))
        if(is.null(dftmp$imp)==FALSE){
          input$cfn_mb_xgb_gen_pdp
          isolate(
            selectInput("cfn_mb_xgb_pdp_dropdown", "Choose Independent Variable To View PDP Plot:",
                        dftmp$imp[1:input$cfn_mb_xgb_pdp_top_vars,"Feature"])
          )
        }
      }
      else{
        p("")
      }
    })
  })
  
  #Generate Container
  observeEvent(input$cfn_mb_xgb_gen_pdp,{
    if(is.null(dftmp$xgb_model)==FALSE & 
       is.null(dftmp$imp)==FALSE){
      if(input$cfn_pdp_select_method=="importance"){
        dftmp$container <- container(n_rows = input$cfn_mb_xgb_pdp_top_vars, dftmp$xgb_model, dftmp$train_subset, dftmp$validation_subset, dftmp$imp, dftmp$dep_var, ratio = input$cfn_mb_xgb_pdp_train_split, method = "classification", selection = "importance")
      }else{
        dftmp$container <- container(n_rows = length(input$cfn_pdp_vars_manual), dftmp$xgb_model, dftmp$train_subset, dftmp$validation_subset, input$cfn_pdp_vars_manual, dftmp$dep_var, ratio = input$cfn_mb_xgb_pdp_train_split, method = "classification", selection = "manual")
      }
      dftmp$container$ls_importance[["variable_importance"]] <- dftmp$imp
      dftmp$container$ls_importance[["variable_importance_top_n_vars"]] <- input$cfn_mb_xgb_varimp_top_vars
      if(is.null(dftmp$train_lift)==FALSE &
         is.null(dftmp$train_accuracy)==FALSE){
        dftmp$container$ls_lift[["train_lift"]] <- dftmp$train_lift
        dftmp$container$ls_lift[["validation_lift"]] <- dftmp$validation_lift
        dftmp$container$ls_lift[["test_lift"]] <- dftmp$test_lift
        dftmp$container$ls_accuracy[["train_accuracy"]] <- dftmp$train_accuracy
        dftmp$container$ls_accuracy[["valid_accuracy"]] <- dftmp$valid_accuracy
        dftmp$container$ls_accuracy[["test_accuracy"]] <- dftmp$test_accuracy
        dftmp$container$method <- "classification"
      }
    }
  })
  
  output$cfn_mb_xgb_pdp_plot <- renderPlot({
    validate(need(input$cfn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(dftmp$container)){
      return()
    }else{
      pdp_plot(dftmp$container, input$cfn_mb_xgb_pdp_dropdown, dep_var = dftmp$dep_var, method = "classification")
    }
  })

  output$cfn_mb_xgb_density_plot <- renderPlot({
    validate(need(input$cfn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(dftmp$container)){
      return()
    }else{
      dftmp$container$ls_density[[input$cfn_mb_xgb_pdp_dropdown]]
    }
  })
  
  output$cfn_mb_xgb_var_summary <- renderPrint({
    validate(need(input$cfn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(dftmp$container)){
      return(cat(""))
    }else{
      validate(need(input$cfn_mb_xgb_pdp_dropdown,"Select a valid variable"))
      validate(need(dftmp$container,"Select a valid variable"))
      dftmp$container$ls_summary[[input$cfn_mb_xgb_pdp_dropdown]]
    }
  })
  
  score_code <- reactive({
    leaf_val <- get_leaf_info(xgb_model = dftmp$xgb_model, output = "dataframe")
    return(leaf_val)
  })
  
  #XGBoost R to SAS
  output$cfn_mb_xgb_score <- downloadHandler(
    filename = function() {
      paste("xgb_score_code", ".csv", sep = "")
    },
    content = function(filename) {
      write.csv(score_code(), filename, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  #ML Interpretation Report
  output$cfn_mb_xgb_report <- downloadHandler(
    filename = function() {
      paste('Report', sep = '.', 'docx')
    },
    
    content = function(file) {
      # path <- "C:/Users/kbhandari/Desktop/ML_Dashboard/Format.docx"
      path <- paste(getwd(),"/Format.docx",sep = '')
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      # out <- render('report.Rmd',params = dftmp$container, word_document(reference_docx = "C:/Users/kbhandari/Desktop/ML_Dashboard/Format.docx",toc = TRUE, toc_depth = 2))
      out <- render('report.Rmd',params = dftmp$container, word_document(reference_docx = path,toc = TRUE, toc_depth = 2))
      # out <- render('report.Rmd',params = dftmp$container, word_document())
      
      file.rename(out, file)
    }
  )
  
  #Test Predictions
  getTestPredictions <- reactive({
    if(is.null(dftmp$test)==FALSE){
      df <- data.frame(ID = rownames(dftmp$test), pred = dftmp$pred_test)
      return(df)
    }
  })
  
  #Download Test Predictions
  output$cfn_mb_xgb_test_prob <- downloadHandler(
    
    filename = function() { 
      paste("test_predictions", ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(getTestPredictions(), file, row.names = F)
    })
  
  #Download Model
  cfn_download_model <- reactive({
    return(dftmp$xgb_model)
  })
  
  output$cfn_mb_xgb_download <- downloadHandler(
    filename <- function(){
      paste("xgb_model.RData")
    },
    
    content = function(file) {
      model <- cfn_download_model()
      save(model, file = file)
    }
  )
  
  ############################################################################
  #----------------------------Use Existing Model----------------------------
  ############################################################################
  
  use_cfn_dftmp <- reactiveValues(train=NULL, test=NULL, raw_train_head=NULL, train_structure=NULL,
                          train_ohe=FALSE, dep_var=NULL, preprocess_train_head=NULL,
                          preprocess_train_structure=NULL,fe_indep_var=NULL, iv_calc_df=NULL,
                          ss_calc_df=NULL, cfn_corr_df=NULL, train_subset=NULL,
                          Y_train=NULL, Y_valid=NULL, Y_test=NULL, watchlist=NULL,
                          dtrain=NULL, dvalid=NULL,dtest=NULL, xgb_params=NULL, 
                          xgb_params_eval_maximize=TRUE, xgb_model=NULL, train_lift=NULL, 
                          test_lift=NULL, train_accuracy = NULL, test_accuracy=NULL,
                          pred_train=NULL, pred_test=NULL, imp=NULL,
                          container=NULL, difference=FALSE, missing_lookup=NULL,
                          warning_message=NULL, error_message=NULL, xgb_error=NULL)
  
  observeEvent(input$use_cfn_file1,{
    tryCatch({
      path <- normalizePath(input$use_cfn_getwd)
      file <- input$use_cfn_file_name
      if(file.exists(paste(path,file,sep = '\\'))==TRUE){
        use_cfn_dftmp$train <- fread(paste(path,file,sep = '\\'),
                             header = TRUE,
                             data.table = FALSE,
                             stringsAsFactors = TRUE,
                             sep = input$use_cfn_sep,
                             skip = as.numeric(input$use_cfn_upload_start),
                             nrows = as.numeric(input$use_cfn_upload_nrows),
                             na.strings = input$use_cfn_upload_na_strings)
        
        if(is.null(use_cfn_dftmp$test)==FALSE){
          if(length(setdiff(names(use_cfn_dftmp$test), names(use_cfn_dftmp$train))) <= 1 & length(setdiff(names(use_cfn_dftmp$train), names(use_cfn_dftmp$test))) <= 1){
            use_cfn_dftmp$warning_message = NULL
            use_cfn_dftmp$error_message = NULL
            use_cfn_dftmp$raw_train_head <- head(use_cfn_dftmp$train,n=20)
          }else{
            use_cfn_dftmp$warning_message = "Train and test names do not match"
            use_cfn_dftmp$error_message = "Train and test names do not match"
          }
        }else{
          use_cfn_dftmp$raw_train_head <- head(use_cfn_dftmp$train,n=20)
          use_cfn_dftmp$warning_message <- NULL
          use_cfn_dftmp$error_message <- NULL
        }
      }else{
        use_cfn_dftmp$warning_message <- "No such file or directory found"
      }
    },
    warning = function(w){
      use_cfn_dftmp$warning_message <- "No such file or directory found"
    },
    error = function(e){
      use_cfn_dftmp$error_message <- "Error parsing the file"
    })
    return(TRUE)
  })
  
  observeEvent(input$use_cfn_file2,{
    tryCatch({
      path <- normalizePath(input$use_cfn_getwd)
      file <- input$use_cfn_file_name
      if(file.exists(paste(path,file,sep = '\\'))==TRUE){
        use_cfn_dftmp$test <- fread(paste(path,file,sep = '\\'),
                            header = TRUE,
                            data.table = FALSE,
                            stringsAsFactors = TRUE,
                            sep = input$use_cfn_sep,
                            skip = as.numeric(input$use_cfn_upload_start),
                            nrows = as.numeric(input$use_cfn_upload_nrows),
                            na.strings = input$use_cfn_upload_na_strings)
        
        if(length(setdiff(names(use_cfn_dftmp$test), names(use_cfn_dftmp$train))) <= 1 & length(setdiff(names(use_cfn_dftmp$train), names(use_cfn_dftmp$test))) <= 1){
          use_cfn_dftmp$warning_message = NULL
          use_cfn_dftmp$error_message = NULL
        }else{
          use_cfn_dftmp$warning_message = "Train and test names do not match"
          use_cfn_dftmp$error_message = "Train and test names do not match"
        }
      }else{
        use_cfn_dftmp$warning_message <- "No such file or directory found"
      }
    },
    warning = function(w){
      use_cfn_dftmp$warning_message <- "No such file or directory found"
    },
    error = function(e){
      use_cfn_dftmp$error_message <- "Error parsing the file"
    })
    return(TRUE)
  })
  
  observeEvent(input$use_cfn_file3,{
    tryCatch({
      if ( is.null(input$use_cfn_file3)) return(NULL)
      inFile <- isolate({input$use_cfn_file3})
      file <- inFile$datapath
      # load the file into new environment and get it from there
      e = new.env()
      name <- load(file, envir = e)
      use_cfn_dftmp$xgb_model <- e[[name]]
      use_cfn_dftmp$xgb_error <- NULL
    },
    warning = function(w){
      use_cfn_dftmp$xgb_error <- "Valid model RData file not uploaded"
    },
    error = function(e){
      use_cfn_dftmp$xgb_error <- "Valid model RData file not uploaded"
    })

  })
  
  output$use_cfn_raw_output <- renderTable({
    input$use_cfn_file1
    input$use_cfn_file2
    isolate({
      validate(need(input$use_cfn_getwd,"Directory not provided"))
      validate(need(input$use_cfn_file_name,"File not provided"))
      validate(need(is.null(use_cfn_dftmp$warning_message)==TRUE,use_cfn_dftmp$warning_message))
      validate(need(is.null(use_cfn_dftmp$error_message)==TRUE,use_cfn_dftmp$error_message))
      use_cfn_dftmp$raw_train_head
    })
  })
  
  output$use_cfn_xgb_features <- renderPrint({
    if(is.null(use_cfn_dftmp$xgb_error)==FALSE){
      validate("Valid model RData file not uploaded")
    }else{
      validate(need(is.null(use_cfn_dftmp$xgb_model)==F,"XGBoost model RData file is not uploaded yet"))
      validate(need(is.null(use_cfn_dftmp$xgb_model$feature_names)==F,"Incorrect RData file uploaded"))
      use_cfn_dftmp$xgb_model$feature_names
    }
  })
  
  outputOptions(output,"use_cfn_xgb_features",suspendWhenHidden=FALSE)
  outputOptions(output,"use_cfn_raw_output",suspendWhenHidden=FALSE)
  
  #Data Preprocess
  output$use_cfn_select_dv <- renderUI({
    if(!is.null(use_cfn_dftmp$train)){
      choices = names(use_cfn_dftmp$train)
    }
    selectInput("use_cfn_dep_var", "Choose Dependent Variable:",
                choices = choices, selected = use_cfn_dftmp$dep_var)
  })
  
  observeEvent(input$use_cfn_data_preprocess_confirm,{
    
    use_cfn_dftmp$dep_var <- input$use_cfn_dep_var
    
    if(length(unique(use_cfn_dftmp$train[,input$use_cfn_dep_var]))==2){
      use_cfn_dftmp$train <- treat_dep_var_cfn(use_cfn_dftmp$train,input$use_cfn_dep_var)
      if(input$use_cfn_dep_var %in% names(use_cfn_dftmp$test)){
        use_cfn_dftmp$test <- treat_dep_var_cfn(use_cfn_dftmp$test,input$use_cfn_dep_var)
      }
      
      if(input$use_cfn_missing){
        use_cfn_dftmp$missing_lookup <- treat_Missing(use_cfn_dftmp$train)
        use_cfn_dftmp$train <- replace_Missing(use_cfn_dftmp$train, use_cfn_dftmp$missing_lookup)
        if(is.null(use_cfn_dftmp$test)==FALSE){
          use_cfn_dftmp$test <- replace_Missing(use_cfn_dftmp$test, use_cfn_dftmp$missing_lookup)
        }
      }

      if(input$use_cfn_ohe){
        if(use_cfn_dftmp$train_ohe == FALSE){
          use_cfn_dftmp$train <- create_dummies(use_cfn_dftmp$train)
          if(is.null(use_cfn_dftmp$test)==FALSE){
            use_cfn_dftmp$test <- create_dummies(use_cfn_dftmp$test)
          }
          use_cfn_dftmp$train_ohe <- TRUE
        }
      }
    }
    
    #Assigning global variables required for temp view
    use_cfn_dftmp$preprocess_train_head <- head(use_cfn_dftmp$train,n=15)
    use_cfn_dftmp$preprocess_train_structure <- strtable(use_cfn_dftmp$train)
  })
  
  output$use_cfn_data_preprocess_output <- renderTable({
    if(input$use_cfn_data_preprocess_confirm == 0) return()
    isolate({
      validate(need(length(unique(use_cfn_dftmp$train[,input$use_cfn_dep_var]))==2,"Dependent variable is not binary"))
      
      if(is.null(use_cfn_dftmp$test)==FALSE){
        if(input$use_cfn_dep_var %in% names(use_cfn_dftmp$test)){
          validate(need(length(unique(use_cfn_dftmp$test[,input$use_cfn_dep_var]))==2,"Dependent variable for test dataset is not binary"))
        }
      }
      use_cfn_dftmp$preprocess_train_head
    })
  })
  
  output$use_cfn_data_structure_output <- renderDataTable({
    if(input$use_cfn_data_preprocess_confirm == 0) return()
    # dftmp$train_structure
    use_cfn_dftmp$preprocess_train_structure
  },options = list(pageLength = 10))
  
  outputOptions(output,"use_cfn_data_preprocess_output",suspendWhenHidden=FALSE)
  
  observeEvent(input$use_cfn_data_preprocess_refresh,{
    use_cfn_dftmp$preprocess_train_head <- head(use_cfn_dftmp$train,n=15)
    use_cfn_dftmp$preprocess_train_structure <- strtable(use_cfn_dftmp$train)
  })
  
  #Feature Engineering
  output$use_cfn_fe_select_iv <- renderUI({
    selectInput("use_cfn_fe_indep_var", "Choose Independent Variable To Implement Feature Engineering On:",
                setdiff(names(use_cfn_dftmp$train),use_cfn_dftmp$dep_var), selected = use_cfn_dftmp$fe_indep_var)
  })
  
  use_cfn_feature_engineering <- observeEvent(input$use_cfn_fe_action,{
    use_cfn_dftmp$fe_indep_var <- input$use_cfn_fe_indep_var
    
    #One Hot Encoding
    if(input$use_cfn_fe_radio_button == "cfn_fe_ohe"){
      if(use_cfn_dftmp$train_ohe == FALSE){
        use_cfn_dftmp$train <- create_dummies(use_cfn_dftmp$train)
        use_cfn_dftmp$train_ohe <- TRUE
        if(is.null(use_cfn_dftmp$test)==FALSE){
          use_cfn_dftmp$test <- create_dummies(use_cfn_dftmp$test)
        }
      }
    }
    
    #Categorical Replacement
    if(input$use_cfn_fe_radio_button == "cfn_fe_cat_replacement" & input$use_cfn_fe_cat_replacement_radio_button == "cfn_fe_cat_freq"){
      use_cfn_dftmp$train <- cat_replacement(classification = TRUE, method = "frequency", 
                                     df = use_cfn_dftmp$train,indep_var = input$use_cfn_fe_indep_var, 
                                     dep_var = use_cfn_dftmp$dep_var)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- cat_replacement(classification = TRUE, method = "frequency", 
                                      df = use_cfn_dftmp$test,indep_var = input$use_cfn_fe_indep_var, 
                                      dep_var = use_cfn_dftmp$dep_var)
      }
    }
    if(input$use_cfn_fe_radio_button == "cfn_fe_cat_replacement" & input$use_cfn_fe_cat_replacement_radio_button == "cfn_fe_cat_event_rate"){
      use_cfn_dftmp$train <- cat_replacement(classification = TRUE, method = "event_rate", 
                                     df = use_cfn_dftmp$train,indep_var = input$use_cfn_fe_indep_var, 
                                     dep_var = use_cfn_dftmp$dep_var)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- cat_replacement(classification = TRUE, method = "event_rate", 
                                      df = use_cfn_dftmp$test,indep_var = input$use_cfn_fe_indep_var, 
                                      dep_var = use_cfn_dftmp$dep_var)
      }
    }
    
    #Binning
    if(input$use_cfn_fe_radio_button == "cfn_fe_bins" & input$use_cfn_fe_bins_radio_button == "cfn_fe_bin_woe"){
      use_cfn_dftmp$train <- binning(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, use_cfn_dftmp$dep_var, method = "woe", bins = input$use_cfn_fe_bin_woe_value_input)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- binning(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, use_cfn_dftmp$dep_var, method = "woe", bins = input$use_cfn_fe_bin_woe_value_input)
      }
    }
    if(input$use_cfn_fe_radio_button == "cfn_fe_bins" & input$use_cfn_fe_bins_radio_button == "cfn_fe_bin_auto_cuts"){
      use_cfn_dftmp$train <- binning(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, use_cfn_dftmp$dep_var, method = "auto", bins = input$use_cfn_fe_bin_auto_cuts_value_input)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- binning(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, use_cfn_dftmp$dep_var, method = "auto", bins = input$use_cfn_fe_bin_auto_cuts_value_input)
      }
    }
    if(input$use_cfn_fe_radio_button == "cfn_fe_bins" & input$use_cfn_fe_bins_radio_button == "cfn_fe_bin_manual_cuts"){
      use_cfn_dftmp$train <- binning(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, use_cfn_dftmp$dep_var, method = "manual", cuts = input$use_cfn_fe_bin_manual_cuts_value_input)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- binning(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, use_cfn_dftmp$dep_var, method = "manual", cuts = input$use_cfn_fe_bin_manual_cuts_value_input)
      }
    }
    
    #Missing Value Treatment
    if(input$use_cfn_fe_radio_button == "cfn_fe_missing" & input$use_cfn_fe_missing_radio_button == "cfn_fe_central_tendency"){
      use_cfn_dftmp$missing_lookup <- treat_Missing(use_cfn_dftmp$train)
      use_cfn_dftmp$train <- fe_treat_Missing(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, method = "auto", missing_lookup = use_cfn_dftmp$missing_lookup)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- fe_treat_Missing(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, method = "auto", missing_lookup = use_cfn_dftmp$missing_lookup)
      }
    }
    if(input$use_cfn_fe_radio_button == "cfn_fe_missing" & input$use_cfn_fe_missing_radio_button == "cfn_fe_missing_value"){
      use_cfn_dftmp$train <- fe_treat_Missing(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, method = "manual", missing_value = input$use_cfn_fe_missing_value_input)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- fe_treat_Missing(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, method = "manual", missing_value = input$use_cfn_fe_missing_value_input)
      }
    }
    
    #Text Extraction
    if(input$use_cfn_fe_radio_button == "cfn_fe_text_extraction" & input$use_cfn_fe_text_extract_radio_button == "cfn_fe_text_contains"){
      use_cfn_dftmp$train <- text_extraction(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, method = "contains", text_to_find = input$use_cfn_fe_text_extraction_value_input)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- text_extraction(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, method = "contains", text_to_find = input$use_cfn_fe_text_extraction_value_input)
      }
    }
    if(input$use_cfn_fe_radio_button == "cfn_fe_text_extraction" & input$use_cfn_fe_text_extract_radio_button == "cfn_fe_text_equals"){
      use_cfn_dftmp$train <- text_extraction(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, method = "equals", text_to_find = input$use_cfn_fe_text_extraction_value_input)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- text_extraction(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, method = "equals", text_to_find = input$use_cfn_fe_text_extraction_value_input)
      }
    }
    
    #Transformations
    if(input$use_cfn_fe_radio_button == "cfn_fe_transformations" & input$use_cfn_fe_transformations_radio_button == "log"){
      use_cfn_dftmp$train <- transformations(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, method = "log")
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- transformations(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, method = "log")
      }
    }
    if(input$use_cfn_fe_radio_button == "cfn_fe_transformations" & input$use_cfn_fe_transformations_radio_button == "exp"){
      use_cfn_dftmp$train <- transformations(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, method = "exp")
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- transformations(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, method = "exp")
      }
    }
    if(input$use_cfn_fe_radio_button == "cfn_fe_transformations" & input$use_cfn_fe_transformations_radio_button == "inv"){
      use_cfn_dftmp$train <- transformations(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, method = "inv")
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- transformations(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, method = "inv")
      }
    }
    if(input$use_cfn_fe_radio_button == "cfn_fe_transformations" & input$use_cfn_fe_transformations_radio_button == "square"){
      use_cfn_dftmp$train <- transformations(use_cfn_dftmp$train, input$use_cfn_fe_indep_var, method = "square")
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- transformations(use_cfn_dftmp$test, input$use_cfn_fe_indep_var, method = "square")
      }
    }
    
    #Delete Variable
    if(input$use_cfn_fe_radio_button == "cfn_fe_remove_var" & input$use_cfn_fe_remove_var_radio_button == "cfn_fe_remove_var_dropdown"){
      use_cfn_dftmp$train <- delete_var(use_cfn_dftmp$train, input$use_cfn_fe_indep_var)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- delete_var(use_cfn_dftmp$test, input$use_cfn_fe_indep_var)
      }
    }
    if(input$use_cfn_fe_radio_button == "cfn_fe_remove_var" & input$use_cfn_fe_remove_var_radio_button == "cfn_fe_remove_var_name"){
      use_cfn_dftmp$train <- delete_var(use_cfn_dftmp$train, input$use_cfn_fe_remove_var_value_input)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- delete_var(use_cfn_dftmp$test, input$use_cfn_fe_remove_var_value_input)
      }
    }
  })
  
  use_cfn_one_hot_encoding <- observeEvent(input$use_cfn_action_3,{
    if(use_cfn_dftmp$train_ohe == FALSE){
      use_cfn_dftmp$train <- create_dummies(use_cfn_dftmp$train)
      use_cfn_dftmp$train_ohe <- TRUE
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$test <- create_dummies(use_cfn_dftmp$test)
      }
    }
    constant_var_logic <- sapply(use_cfn_dftmp$train, function(x) length(unique(x))==1)
    if('TRUE' %in% constant_var_logic){
      constant_vars <- names(which(constant_var_logic,TRUE))
      use_cfn_dftmp$train <- use_cfn_dftmp$train[,setdiff(colnames(use_cfn_dftmp$train),constant_vars)]
    }
    
    if(is.null(use_cfn_dftmp$imp)==TRUE){
      use_cfn_dftmp$imp <- xgb.importance(use_cfn_dftmp$xgb_model$feature_names, model=use_cfn_dftmp$xgb_model)
    }
  })
  
  output$use_cfn_fe_output <- renderTable({
    head(use_cfn_dftmp$train,n=15)
  })
  
  #QC Tab
  qc1 <- observeEvent(input$use_cfn_action_1,{
    for (i in use_cfn_dftmp$xgb_model$feature_names) {
      if(i %in% names(use_cfn_dftmp$train) == FALSE){
        use_cfn_dftmp$difference <- TRUE
        break
      }else{
        use_cfn_dftmp$difference <- FALSE
      }
    }
    for (i in setdiff(names(use_cfn_dftmp$train),use_cfn_dftmp$dep_var)) {
      if(i %in% use_cfn_dftmp$xgb_model$feature_names == FALSE){
        use_cfn_dftmp$difference <- TRUE
        break
      }else{
        use_cfn_dftmp$difference <- FALSE
      }
    }
  })
  
  qc2 <- observeEvent(input$use_cfn_fe_action,{
    for (i in use_cfn_dftmp$xgb_model$feature_names) {
      if(i %in% names(use_cfn_dftmp$train) == FALSE){
        use_cfn_dftmp$difference <- TRUE
        break
      }else{
        use_cfn_dftmp$difference <- FALSE
      }
    }
    for (i in setdiff(names(use_cfn_dftmp$train),use_cfn_dftmp$dep_var)) {
      if(i %in% use_cfn_dftmp$xgb_model$feature_names == FALSE){
        use_cfn_dftmp$difference <- TRUE
        break
      }else{
        use_cfn_dftmp$difference <- FALSE
      }
    }
  })
  
  qc3 <- observeEvent(input$use_cfn_action_3,{
    for (i in use_cfn_dftmp$xgb_model$feature_names) {
      if(i %in% names(use_cfn_dftmp$train) == FALSE){
        use_cfn_dftmp$difference <- TRUE
        break
      }else{
        use_cfn_dftmp$difference <- FALSE
      }
    }
    for (i in setdiff(names(use_cfn_dftmp$train),use_cfn_dftmp$dep_var)) {
      if(i %in% use_cfn_dftmp$xgb_model$feature_names == FALSE){
        use_cfn_dftmp$difference <- TRUE
        break
      }else{
        use_cfn_dftmp$difference <- FALSE
      }
    }
  })
  
  output$use_cfn_fe_output_qc <- renderPrint({
    if(use_cfn_dftmp$difference == FALSE){
      print("Feature names match. Status: Ok")
    }
    else{
      validate(need(use_cfn_dftmp$difference == FALSE, "Feature names don't match. Status: Not Ok"))
    }
  })
  outputOptions(output,"use_cfn_fe_output_qc",suspendWhenHidden=FALSE)
  
  #Model Interpretation
  output$use_cfn_mb_xgb_varimp_iv <- renderUI({
      numericInput("use_cfn_mb_xgb_varimp_top_vars","Select Number of Variables To Display",min = 1,max = length(use_cfn_dftmp$xgb_model$feature_names),step = 1,value = round(length(use_cfn_dftmp$xgb_model$feature_names)/2))
  })
  
  output$use_cfn_mb_xgb_pdp_vars <- renderUI({
      numericInput("use_cfn_mb_xgb_pdp_top_vars","Select Number of Variables To Display",min = 1,max = length(use_cfn_dftmp$xgb_model$feature_names),step = 1,value = round(length(use_cfn_dftmp$xgb_model$feature_names)/2))
  })
  
  output$use_cfn_mb_xgb_pdp_vars_manual <- renderUI({
    pickerInput("use_cfn_pdp_vars_manual","Independent Variables",
                choices=setdiff(names(use_cfn_dftmp$train),use_cfn_dftmp$dep_var),
                selected = setdiff(names(use_cfn_dftmp$train),use_cfn_dftmp$dep_var),
                options = list(`actions-box` = TRUE),multiple = T)
  })
  
  output$use_cfn_mb_xgb_pdp_iv <- renderUI({
    input$use_cfn_mb_xgb_gen_pdp
    isolate({
      validate(need(is.null(use_cfn_dftmp$imp)==FALSE,""))
      if(input$use_cfn_pdp_select_method=="manual"){
        validate(need(length(input$use_cfn_pdp_vars_manual)>0,""))
        if(is.null(use_cfn_dftmp$imp)==FALSE){
          input$use_cfn_mb_xgb_gen_pdp
          isolate(
            selectInput("use_cfn_mb_xgb_pdp_dropdown", "Choose Independent Variable To View PDP Plot:",
                        input$use_cfn_pdp_vars_manual)
          )
        }
      }else if(input$use_cfn_pdp_select_method=="importance"){
        validate(need(input$use_cfn_mb_xgb_pdp_top_vars>0,""))
        if(is.null(use_cfn_dftmp$imp)==FALSE){
          input$use_cfn_mb_xgb_gen_pdp
          isolate(
            selectInput("use_cfn_mb_xgb_pdp_dropdown", "Choose Independent Variable To View PDP Plot:",
                        use_cfn_dftmp$imp[1:input$use_cfn_mb_xgb_pdp_top_vars,"Feature"])
          )
        }
      }
      else{
        p("")
      }
    })
  })
  
  output$use_cfn_mb_xgb_var_imp <- renderPlot({
    if(input$use_cfn_mb_xgb_varimp_plot<=0){
      return()
    }else{
      if(is.null(use_cfn_dftmp$imp)==TRUE){
        use_cfn_dftmp$imp <- xgb.importance(use_cfn_dftmp$xgb_model$feature_names, model=use_cfn_dftmp$xgb_model)
      }
        gain_or_cover <- switch(input$use_cfn_mb_xgb_varimp,
                                gain = "Gain",
                                cover = "Cover")
        input$use_cfn_mb_xgb_varimp_plot
        isolate(
          ggplot(use_cfn_dftmp$imp[1:input$use_cfn_mb_xgb_varimp_top_vars,], aes(x=reorder(Feature,get(paste0(gain_or_cover))), y=get(paste0(gain_or_cover)),fill=get(paste0(gain_or_cover))))+ 
            geom_bar(stat="identity", position="dodge", show.legend = T)+ coord_flip()+
            ylab(paste0(gain_or_cover))+
            xlab(as.character("Variable Importance"))+
            ggtitle(paste0("Variable Importance by ",paste0(gain_or_cover)))+
            scale_fill_gradient(low="red", high="blue")+labs(fill=paste0(gain_or_cover))+theme_bw()
        )
    }
  }, height = 750)
  
  #Generate Container
  observeEvent(input$use_cfn_mb_xgb_gen_pdp,{
    #Variable Importance
    if(is.null(use_cfn_dftmp$imp)==TRUE){
      use_cfn_dftmp$imp <- xgb.importance(use_cfn_dftmp$xgb_model$feature_names, model=use_cfn_dftmp$xgb_model)
    }
    
    #Container
    if(input$use_cfn_pdp_select_method=="importance"){
      use_cfn_dftmp$container <- container(n_rows = input$use_cfn_mb_xgb_pdp_top_vars, use_cfn_dftmp$xgb_model, use_cfn_dftmp$train, use_cfn_dftmp$test, use_cfn_dftmp$imp, use_cfn_dftmp$dep_var, ratio = input$use_cfn_mb_xgb_pdp_train_split, method = "classification", selection = "importance")
    }else{
      use_cfn_dftmp$container <- container(n_rows = length(input$use_cfn_pdp_vars_manual), use_cfn_dftmp$xgb_model, use_cfn_dftmp$train, use_cfn_dftmp$test, input$use_cfn_pdp_vars_manual, use_cfn_dftmp$dep_var, ratio = input$use_cfn_mb_xgb_pdp_train_split, method = "classification", selection = "manual")
    }
    
    use_cfn_dftmp$container$ls_importance[["variable_importance"]] <- use_cfn_dftmp$imp
    use_cfn_dftmp$container$ls_importance[["variable_importance_top_n_vars"]] <- input$use_cfn_mb_xgb_varimp_top_vars
    
    #Lift
    if(is.null(use_cfn_dftmp$train_lift)==TRUE){
      use_cfn_dftmp$Y_train <- use_cfn_dftmp$train[,use_cfn_dftmp$dep_var]
      if(is.null(use_cfn_dftmp$test)==FALSE){
        if(use_cfn_dftmp$dep_var %in% names(use_cfn_dftmp$test)){
          use_cfn_dftmp$Y_test <- use_cfn_dftmp$test[,use_cfn_dftmp$dep_var]
        }
      }

      use_cfn_dftmp$dtrain  <- xgb.DMatrix(as.matrix(use_cfn_dftmp$train[,-which(names(use_cfn_dftmp$train) %in% c(use_cfn_dftmp$dep_var))]), label = use_cfn_dftmp$Y_train)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        if(use_cfn_dftmp$dep_var %in% names(use_cfn_dftmp$test)){
          use_cfn_dftmp$dtest  <- xgb.DMatrix(as.matrix(use_cfn_dftmp$test[,-which(names(use_cfn_dftmp$test) %in% c(use_cfn_dftmp$dep_var))]), label = use_cfn_dftmp$Y_test)
        }else{
          use_cfn_dftmp$dtest  <- xgb.DMatrix(as.matrix(use_cfn_dftmp$test))
        }
      }

      use_cfn_dftmp$pred_train = predict(use_cfn_dftmp$xgb_model, use_cfn_dftmp$dtrain)
      use_cfn_dftmp$train_lift <- lift(use_cfn_dftmp$train[,use_cfn_dftmp$dep_var], use_cfn_dftmp$pred_train, groups = 10)
      if(is.null(use_cfn_dftmp$test)==FALSE){
        use_cfn_dftmp$pred_test = predict(use_cfn_dftmp$xgb_model, use_cfn_dftmp$dtest)
        if(use_cfn_dftmp$dep_var %in% names(use_cfn_dftmp$test)){
          use_cfn_dftmp$test_lift <- lift(use_cfn_dftmp$test[,use_cfn_dftmp$dep_var], use_cfn_dftmp$pred_test, groups = 10)
        }
      }
    }
    use_cfn_dftmp$container$ls_lift[["train_lift"]] <- use_cfn_dftmp$train_lift
    # use_cfn_dftmp$container$ls_lift[["test_lift"]] <- use_cfn_dftmp$test_lift
    use_cfn_dftmp$container$ls_lift[["validation_lift"]] <- use_cfn_dftmp$test_lift
    
    #Accuracy
    use_cfn_dftmp$train_accuracy <- accuracy_metrics(use_cfn_dftmp$pred_train, use_cfn_dftmp$Y_train, method="classification")
    if(is.null(use_cfn_dftmp$test)==FALSE){
      if(use_cfn_dftmp$dep_var %in% names(use_cfn_dftmp$test)){
        use_cfn_dftmp$test_accuracy <- accuracy_metrics(use_cfn_dftmp$pred_test, use_cfn_dftmp$Y_test, method="classification")
      }
    }
    use_cfn_dftmp$container$ls_accuracy[["train_accuracy"]] <- use_cfn_dftmp$train_accuracy
    use_cfn_dftmp$container$ls_accuracy[["test_accuracy"]] <- use_cfn_dftmp$test_accuracy
    use_cfn_dftmp$container$method <- "classification"
  })
  
  output$use_cfn_mb_xgb_pdp_plot <- renderPlot({
    validate(need(input$use_cfn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(use_cfn_dftmp$container)){
      return()
    }else{
      pdp_plot(use_cfn_dftmp$container, input$use_cfn_mb_xgb_pdp_dropdown, dep_var = use_cfn_dftmp$dep_var, method = "classification")
    }
  })
  
  output$use_cfn_mb_xgb_density_plot <- renderPlot({
    validate(need(input$use_cfn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(use_cfn_dftmp$container)){
      return()
    }else{
      use_cfn_dftmp$container$ls_density[[input$use_cfn_mb_xgb_pdp_dropdown]]
    }
  })
  
  output$use_cfn_mb_xgb_var_summary <- renderPrint({
    validate(need(input$use_cfn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(use_cfn_dftmp$container)){
      return(cat(""))
    }else{
      validate(need(input$use_cfn_mb_xgb_pdp_dropdown,"Select a valid variable"))
      validate(need(use_cfn_dftmp$container,"Select a valid variable"))
      use_cfn_dftmp$container$ls_summary[[input$use_cfn_mb_xgb_pdp_dropdown]]
    }
  })
  
  #XGBoost R to SAS
  use_cfn_score_code <- reactive({
    leaf_val <- get_leaf_info(xgb_model = use_cfn_dftmp$xgb_model, output = "dataframe")
    return(leaf_val)
  })
  
  #XGBoost R to SAS
  output$use_cfn_mb_xgb_score <- downloadHandler(
    filename = function() {
      paste("xgb_score_code", ".csv", sep = "")
    },
    content = function(filename) {
      write.csv(use_cfn_score_code(), filename, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$use_cfn_mb_xgb_lift_train <- renderTable({
    use_cfn_dftmp$train_lift
  })
  outputOptions(output,"use_cfn_mb_xgb_lift_train",suspendWhenHidden=FALSE)
  
  output$use_cfn_mb_xgb_lift_test <- renderTable({
    validate(need(use_cfn_dftmp$test,"Test dataset not uploaded"))
    validate(need(use_cfn_dftmp$dep_var %in% names(use_cfn_dftmp$test), "Response variable not found"))
    use_cfn_dftmp$test_lift
  })
  outputOptions(output,"use_cfn_mb_xgb_lift_test",suspendWhenHidden=FALSE)
  
  output$use_cfn_mb_xgb_stats_train <- renderPrint({
    validate(need(use_cfn_dftmp$train_accuracy[["AUCmetric"]],"Partial plot button not clicked yet"))
    validate(need(use_cfn_dftmp$train_accuracy[["Confusion"]],"Partial plot button not clicked yet"))
    print(use_cfn_dftmp$train_accuracy[["AUCmetric"]])
    cat("\n")
    print(use_cfn_dftmp$train_accuracy[["Confusion"]])
  })
  outputOptions(output,"use_cfn_mb_xgb_stats_train",suspendWhenHidden=FALSE)
  
  output$use_cfn_mb_xgb_stats_test <- renderPrint({
    validate(need(use_cfn_dftmp$test,"Test dataset not uploaded"))
    validate(need(use_cfn_dftmp$dep_var %in% names(use_cfn_dftmp$test), "Response variable not found"))
    print(use_cfn_dftmp$test_accuracy[["AUCmetric"]])
    cat("\n")
    print(use_cfn_dftmp$test_accuracy[["Confusion"]])
  })
  outputOptions(output,"use_cfn_mb_xgb_stats_test",suspendWhenHidden=FALSE)
  
  #Test Predictions
  use_cfn_getTestPredictions <- reactive({
    if(is.null(use_cfn_dftmp$test)==FALSE){
      if(is.null(use_cfn_dftmp$pred_test)==FALSE){
        df <- data.frame(ID = rownames(use_cfn_dftmp$test), pred = use_cfn_dftmp$pred_test)
        return(df)
      }else{
        use_cfn_dftmp$Y_test <- use_cfn_dftmp$test[,use_cfn_dftmp$dep_var]
        if(use_cfn_dftmp$dep_var %in% names(use_cfn_dftmp$test)){
          use_cfn_dftmp$dtest  <- xgb.DMatrix(as.matrix(use_cfn_dftmp$test[,-which(names(use_cfn_dftmp$test) %in% c(use_cfn_dftmp$dep_var))]), label = use_cfn_dftmp$Y_test)
        }else{
          use_cfn_dftmp$dtest  <- xgb.DMatrix(as.matrix(use_cfn_dftmp$test))
        }
        use_cfn_dftmp$pred_test = predict(use_cfn_dftmp$xgb_model, use_cfn_dftmp$dtest)
        df <- data.frame(ID = rownames(use_cfn_dftmp$test), pred = use_cfn_dftmp$pred_test)
        return(df)
      }
    }
  })
  
  #Download Test Predictions
  output$use_cfn_mb_xgb_test_prob <- downloadHandler(
    
    filename = function() { 
      paste("test_predictions", ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(use_cfn_getTestPredictions(), file, row.names = F)
    })
  
  #ML Interpretation Report
  output$use_cfn_mb_xgb_report <- downloadHandler(
    
    filename = function() {
      paste('Report', sep = '.', 'docx')
    },
    
    content = function(file) {
      path <- paste(getwd(),"/Format.docx",sep = '')
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      out <- render('report.Rmd',params = use_cfn_dftmp$container, word_document(reference_docx = path, toc = TRUE, toc_depth = 2))
      # out <- render('report.Rmd',params = dftmp$container, word_document())
      
      file.rename(out, file)
    }
  )
  
  ############################################################################
  #--------------------------Create Regression Model--------------------------
  ############################################################################
  
  rgn_dftmp <- reactiveValues(train=NULL, test=NULL, raw_train_head=NULL, train_structure=NULL,
                          train_ohe=FALSE, dep_var=NULL, preprocess_train_head=NULL,
                          preprocess_train_structure=NULL,fe_indep_var=NULL, ttest_anova_df=NULL,
                          ss_calc_df=NULL, cfn_corr_df=NULL, train_subset=NULL, validation_subset=NULL,
                          Y_train_subset=NULL, Y_valid_subset=NULL, Y_test=NULL, watchlist=NULL,
                          dtrain_subset=NULL, dvalid_subset=NULL,dtest=NULL, xgb_params=NULL, 
                          xgb_params_eval_maximize=TRUE, xgb_model=NULL, train_lift=NULL, 
                          validation_lift=NULL, test_lift=NULL, missing_lookup=NULL,
                          pred_train_subset=NULL, pred_valid_subset=NULL, pred_test=NULL, imp=NULL,
                          container=NULL, train_accuracy=NULL, valid_accuracy=NULL,test_accuracy=NULL,
                          warning_message=NULL, error_message=NULL)
  
  observeEvent(input$rgn_file1,{
    tryCatch({
      path <- normalizePath(input$rgn_getwd)
      file <- input$rgn_file_name
      if(file.exists(paste(path,file,sep = '\\'))==TRUE){
        rgn_dftmp$train <- fread(paste(path,file,sep = '\\'),
                             header = TRUE,
                             data.table = FALSE,
                             stringsAsFactors = TRUE,
                             sep = input$rgn_sep,
                             skip = as.numeric(input$rgn_upload_start),
                             nrows = as.numeric(input$rgn_upload_nrows),
                             na.strings = input$rgn_upload_na_strings)
        
        if(is.null(rgn_dftmp$test)==FALSE){
          if(length(setdiff(names(rgn_dftmp$test), names(rgn_dftmp$train))) <= 1 & length(setdiff(names(rgn_dftmp$train), names(rgn_dftmp$test))) <= 1){
            rgn_dftmp$warning_message = NULL
            rgn_dftmp$error_message = NULL
            rgn_dftmp$raw_train_head <- head(rgn_dftmp$train,n=20)
          }else{
            rgn_dftmp$warning_message = "Train and test names do not match"
            rgn_dftmp$error_message = "Train and test names do not match"
          }
        }else{
          rgn_dftmp$raw_train_head <- head(rgn_dftmp$train,n=20)
          rgn_dftmp$warning_message <- NULL
          rgn_dftmp$error_message <- NULL
        }
      }else{
        rgn_dftmp$warning_message <- "No such file or directory found"
      }
    },
    warning = function(w){
      rgn_dftmp$warning_message <- "No such file or directory found"
    },
    error = function(e){
      rgn_dftmp$error_message <- "Error parsing the file"
    })
    return(TRUE)
  })
  
  observeEvent(input$rgn_file2,{
    tryCatch({
      path <- normalizePath(input$rgn_getwd)
      file <- input$rgn_file_name
      if(file.exists(paste(path,file,sep = '\\'))==TRUE){
        rgn_dftmp$test <- fread(paste(path,file,sep = '\\'),
                            header = TRUE,
                            data.table = FALSE,
                            stringsAsFactors = TRUE,
                            sep = input$rgn_sep,
                            skip = as.numeric(input$rgn_upload_start),
                            nrows = as.numeric(input$rgn_upload_nrows),
                            na.strings = input$rgn_upload_na_strings)
        
        if(length(setdiff(names(rgn_dftmp$test), names(rgn_dftmp$train))) <= 1 & length(setdiff(names(rgn_dftmp$train), names(rgn_dftmp$test))) <= 1){
          rgn_dftmp$warning_message = NULL
          rgn_dftmp$error_message = NULL
        }else{
          rgn_dftmp$warning_message = "Train and test names do not match"
          rgn_dftmp$error_message = "Train and test names do not match"
        }
      }else{
        rgn_dftmp$warning_message <- "No such file or directory found"
      }
    },
    warning = function(w){
      rgn_dftmp$warning_message <- "No such file or directory found"
    },
    error = function(e){
      rgn_dftmp$error_message <- "Error parsing the file"
    })
    return(TRUE)
  })
  
  output$rgn_raw_output <- renderTable({
    input$rgn_file1
    input$rgn_file2
    isolate({
      validate(need(input$rgn_getwd,"Directory not provided"))
      validate(need(input$rgn_file_name,"File not provided"))
      validate(need(is.null(rgn_dftmp$warning_message)==TRUE,rgn_dftmp$warning_message))
      validate(need(is.null(rgn_dftmp$error_message)==TRUE,rgn_dftmp$error_message))
      rgn_dftmp$raw_train_head
    })
  })
  
  #Data Preprocess
  output$rgn_select_dv <- renderUI({
    if(!is.null(rgn_dftmp$train)){
      choices = names(rgn_dftmp$train)
    }
    selectInput("rgn_dep_var", "Choose Dependent Variable:",
                choices = choices, selected = rgn_dftmp$dep_var)
  })
  
  data_preprocess <- observeEvent(input$rgn_data_preprocess_confirm,{
    
    rgn_dftmp$dep_var <- input$rgn_dep_var
    
    if(class(rgn_dftmp$train[,input$rgn_dep_var]) %in% c("integer","numeric") == TRUE &
       length(unique(rgn_dftmp$train[,input$rgn_dep_var]))>2){
      
      if(input$rgn_missing){
        rgn_dftmp$missing_lookup <- treat_Missing(rgn_dftmp$train)
        rgn_dftmp$train <- replace_Missing(rgn_dftmp$train, rgn_dftmp$missing_lookup)
        if(is.null(rgn_dftmp$test)==FALSE){
          rgn_dftmp$test <- replace_Missing(rgn_dftmp$test, rgn_dftmp$missing_lookup)
        }
      }
      
      if(input$rgn_ohe){
        if(rgn_dftmp$train_ohe == FALSE){
          rgn_dftmp$train <- create_dummies(rgn_dftmp$train)
          if(is.null(rgn_dftmp$test)==FALSE){
            rgn_dftmp$test <- create_dummies(rgn_dftmp$test)
          }
          rgn_dftmp$train_ohe <- TRUE
        }
      }
    }
    
    #Assigning global variables required for temp view
    rgn_dftmp$preprocess_train_head <- head(rgn_dftmp$train,n=15)
    rgn_dftmp$preprocess_train_structure <- strtable(rgn_dftmp$train)
    
  })
  
  output$rgn_data_preprocess_output <- renderTable({
    if(input$rgn_data_preprocess_confirm == 0) return()
    
    input$rgn_data_preprocess_confirm
    
    isolate({
      validate(need(class(rgn_dftmp$train[,input$rgn_dep_var]) %in% c("integer","numeric"),"Dependent variable is not numeric"))
      validate(need(length(unique(rgn_dftmp$train[,input$rgn_dep_var]))>2,"Dependent variable is binary not continuous"))
      
      if(is.null(rgn_dftmp$test)==FALSE){
        if(input$rgn_dep_var %in% names(rgn_dftmp$test)){
          validate(need(class(rgn_dftmp$test[,input$rgn_dep_var]) %in% c("integer","numeric"),"Dependent variable for test dataset is not numeric"))
          validate(need(length(unique(rgn_dftmp$test[,input$rgn_dep_var]))>2,"Dependent variable for test dataset is binary not continuous"))
        }
      }
      rgn_dftmp$preprocess_train_head
    })
  })
  
  output$rgn_data_structure_output <- renderDataTable({
    if(input$rgn_data_preprocess_confirm == 0) return()
    rgn_dftmp$preprocess_train_structure
  },options = list(pageLength = 10))
  
  outputOptions(output,"rgn_data_preprocess_output",suspendWhenHidden=FALSE)
  
  observeEvent(input$rgn_data_preprocess_refresh,{
    rgn_dftmp$preprocess_train_head <- head(rgn_dftmp$train,n=15)
    rgn_dftmp$preprocess_train_structure <- strtable(rgn_dftmp$train)
  })
  
  output$rgn_select_iv <- renderUI({
    selectInput("rgn_indep_var", "Choose Independent Variable To Plot Graph:",
                setdiff(names(rgn_dftmp$train),rgn_dftmp$dep_var))
  })
  
  output$rgn_eda_dv_info_output <- renderTable({
    dvtable_rgn(rgn_dftmp$train, rgn_dftmp$dep_var)
  })
  
  output$rgn_eda_dv_info_plot <- renderPlot({
    ggplot(rgn_dftmp$train, aes_string(x=rgn_dftmp$dep_var))+
      geom_density(color="darkblue", fill="lightblue")+
      labs(title="Density Plot of Response Variable")+
      xlab(rgn_dftmp$dep_var)
  })
  
  output$rgn_eda_iv_summary_output <- renderPrint({
    tryCatch({
      var_summary(rgn_dftmp$train, input$rgn_indep_var)
    },
    error = function(e){
      validate("")
    }
    )
  })
  
  output$rgn_eda_iv_dist_output <- renderPlot({
    
    tryCatch({
      indep_var <- input$rgn_indep_var
      dep_var <- rgn_dftmp$dep_var
      bins <- input$rgn_bins
      sample_rate <- input$rgn_sample
      
      if(class(rgn_dftmp$train[,indep_var])=="integer" | class(rgn_dftmp$train[,indep_var])=="numeric"){
        if(input$rgn_cont_plot_type == "density"){
          #Density Plot
          density_plot(rgn_dftmp$train, indep_var, dep_var, method="rgn")
        }else if(input$rgn_cont_plot_type == "histogram"){
          #Histogram
          histogram_plot(rgn_dftmp$train, indep_var, dep_var, bins, method = "rgn")
        }else if(input$rgn_cont_plot_type == "boxplot"){
          #Boxplot
          box_plot(rgn_dftmp$train, indep_var, dep_var, method="rgn_cont")
        }
      }else if(class(rgn_dftmp$train[,indep_var]) == "factor"){
        if(input$rgn_cat_plot_type == "bar"){
          bar_plot(rgn_dftmp$train, indep_var, dep_var)
        }else if(input$rgn_cat_plot_type == "cat_boxplot"){
          box_plot(rgn_dftmp$train, indep_var, dep_var, method="rgn_cat")
        }else if(input$rgn_cat_plot_type == "pie"){
          pie_chart(rgn_dftmp$train, indep_var)
        }
      }
    },
    error = function(e){
      validate("")
    }
    )
  })
  
  output$rgn_eda_missing_summary <- renderPrint({
    missing_value_summary(rgn_dftmp$train)
  })
  
  #Feature Engineering
  output$rgn_fe_select_iv <- renderUI({
    selectInput("rgn_fe_indep_var", "Choose Independent Variable To Implement Feature Engineering On:",
                setdiff(names(rgn_dftmp$train),rgn_dftmp$dep_var), selected = rgn_dftmp$fe_indep_var)
  })
  
  observeEvent(input$rgn_fe_action,{
    rgn_dftmp$fe_indep_var <- input$rgn_fe_indep_var
    
    #One Hot Encoding
    if(input$rgn_fe_radio_button == "rgn_fe_ohe"){
      if(rgn_dftmp$train_ohe == FALSE){
        rgn_dftmp$train <- create_dummies(rgn_dftmp$train)
        rgn_dftmp$train_ohe <- TRUE
        if(is.null(rgn_dftmp$test)==FALSE){
          rgn_dftmp$test <- create_dummies(rgn_dftmp$test)
        }
      }
    }
    
    #Categorical Replacement
    if(input$rgn_fe_radio_button == "rgn_fe_cat_replacement" & input$rgn_fe_cat_replacement_radio_button == "rgn_fe_cat_freq"){
      rgn_dftmp$train <- cat_replacement(method = "frequency", 
                                     df = rgn_dftmp$train,indep_var = input$rgn_fe_indep_var, 
                                     dep_var = rgn_dftmp$dep_var)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- cat_replacement(method = "frequency", 
                                      df = rgn_dftmp$test,indep_var = input$rgn_fe_indep_var, 
                                      dep_var = rgn_dftmp$dep_var)
      }
    }
    if(input$rgn_fe_radio_button == "rgn_fe_cat_replacement" & input$rgn_fe_cat_replacement_radio_button == "rgn_fe_cat_mean"){
      rgn_dftmp$train <- cat_replacement(method = "mean", 
                                     df = rgn_dftmp$train,indep_var = input$rgn_fe_indep_var, 
                                     dep_var = rgn_dftmp$dep_var)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- cat_replacement(method = "mean", 
                                      df = rgn_dftmp$test,indep_var = input$rgn_fe_indep_var, 
                                      dep_var = rgn_dftmp$dep_var)
      }
    }
    
    #Binning
    if(input$rgn_fe_radio_button == "rgn_fe_bins" & input$rgn_fe_bins_radio_button == "rgn_fe_bin_auto_cuts"){
      rgn_dftmp$train <- binning(rgn_dftmp$train, input$rgn_fe_indep_var, rgn_dftmp$dep_var, method = "auto", bins = input$rgn_fe_bin_auto_cuts_value_input)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- binning(rgn_dftmp$test, input$rgn_fe_indep_var, rgn_dftmp$dep_var, method = "auto", bins = input$rgn_fe_bin_auto_cuts_value_input)
      }
    }
    if(input$rgn_fe_radio_button == "rgn_fe_bins" & input$rgn_fe_bins_radio_button == "rgn_fe_bin_manual_cuts"){
      rgn_dftmp$train <- binning(rgn_dftmp$train, input$rgn_fe_indep_var, rgn_dftmp$dep_var, method = "manual", cuts = input$rgn_fe_bin_manual_cuts_value_input)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- binning(rgn_dftmp$test, input$rgn_fe_indep_var, rgn_dftmp$dep_var, method = "manual", cuts = input$rgn_fe_bin_manual_cuts_value_input)
      }
    }
    
    #Missing Value Treatment
    if(input$rgn_fe_radio_button == "rgn_fe_missing" & input$rgn_fe_missing_radio_button == "rgn_fe_central_tendency"){
      rgn_dftmp$missing_lookup <- treat_Missing(rgn_dftmp$train)
      rgn_dftmp$train <- fe_treat_Missing(rgn_dftmp$train, input$rgn_fe_indep_var, method = "auto", missing_lookup = rgn_dftmp$missing_lookup)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- fe_treat_Missing(rgn_dftmp$test, input$rgn_fe_indep_var, method = "auto", missing_lookup = rgn_dftmp$missing_lookup)
      }
    }
    if(input$rgn_fe_radio_button == "rgn_fe_missing" & input$rgn_fe_missing_radio_button == "rgn_fe_missing_value"){
      rgn_dftmp$train <- fe_treat_Missing(rgn_dftmp$train, input$rgn_fe_indep_var, method = "manual", missing_value = input$rgn_fe_missing_value_input)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- fe_treat_Missing(rgn_dftmp$test, input$rgn_fe_indep_var, method = "manual", missing_value = input$rgn_fe_missing_value_input)
      }
    }
    
    #Text Extraction
    if(input$rgn_fe_radio_button == "rgn_fe_text_extraction" & input$rgn_fe_text_extract_radio_button == "rgn_fe_text_contains"){
      rgn_dftmp$train <- text_extraction(rgn_dftmp$train, input$rgn_fe_indep_var, method = "contains", text_to_find = input$rgn_fe_text_extraction_value_input)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- text_extraction(rgn_dftmp$test, input$rgn_fe_indep_var, method = "contains", text_to_find = input$rgn_fe_text_extraction_value_input)
      }
    }
    if(input$rgn_fe_radio_button == "rgn_fe_text_extraction" & input$rgn_fe_text_extract_radio_button == "rgn_fe_text_equals"){
      rgn_dftmp$train <- text_extraction(rgn_dftmp$train, input$rgn_fe_indep_var, method = "equals", text_to_find = input$rgn_fe_text_extraction_value_input)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- text_extraction(rgn_dftmp$test, input$rgn_fe_indep_var, method = "equals", text_to_find = input$rgn_fe_text_extraction_value_input)
      }
    }
    
    #Transformations
    if(input$rgn_fe_radio_button == "rgn_fe_transformations" & input$rgn_fe_transformations_radio_button == "log"){
      rgn_dftmp$train <- transformations(rgn_dftmp$train, input$rgn_fe_indep_var, method = "log")
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- transformations(rgn_dftmp$test, input$rgn_fe_indep_var, method = "log")
      }
    }
    if(input$rgn_fe_radio_button == "rgn_fe_transformations" & input$rgn_fe_transformations_radio_button == "exp"){
      rgn_dftmp$train <- transformations(rgn_dftmp$train, input$rgn_fe_indep_var, method = "exp")
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- transformations(rgn_dftmp$test, input$rgn_fe_indep_var, method = "exp")
      }
    }
    if(input$rgn_fe_radio_button == "rgn_fe_transformations" & input$rgn_fe_transformations_radio_button == "inv"){
      rgn_dftmp$train <- transformations(rgn_dftmp$train, input$rgn_fe_indep_var, method = "inv")
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- transformations(rgn_dftmp$test, input$rgn_fe_indep_var, method = "inv")
      }
    }
    if(input$rgn_fe_radio_button == "rgn_fe_transformations" & input$rgn_fe_transformations_radio_button == "square"){
      rgn_dftmp$train <- transformations(rgn_dftmp$train, input$rgn_fe_indep_var, method = "square")
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- transformations(rgn_dftmp$test, input$rgn_fe_indep_var, method = "square")
      }
    }
    
    #Delete Variable
    if(input$rgn_fe_radio_button == "rgn_fe_remove_var" & input$rgn_fe_remove_var_radio_button == "rgn_fe_remove_var_dropdown"){
      rgn_dftmp$train <- delete_var(rgn_dftmp$train, input$rgn_fe_indep_var)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- delete_var(rgn_dftmp$test, input$rgn_fe_indep_var)
      }
    }
    if(input$rgn_fe_radio_button == "rgn_fe_remove_var" & input$rgn_fe_remove_var_radio_button == "rgn_fe_remove_var_name"){
      rgn_dftmp$train <- delete_var(rgn_dftmp$train, input$rgn_fe_remove_var_value_input)
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- delete_var(rgn_dftmp$test, input$rgn_fe_remove_var_value_input)
      }
    }
    
  })
  
  observeEvent(input$rgn_action_3,{
    if(rgn_dftmp$train_ohe == FALSE){
      rgn_dftmp$train <- create_dummies(rgn_dftmp$train)
      rgn_dftmp$train_ohe <- TRUE
      if(is.null(rgn_dftmp$test)==FALSE){
        rgn_dftmp$test <- create_dummies(rgn_dftmp$test)
      }
    }
    constant_var_logic <- sapply(rgn_dftmp$train, function(x) length(unique(x))==1)
    if('TRUE' %in% constant_var_logic){
      constant_vars <- names(which(constant_var_logic,TRUE))
      rgn_dftmp$train <- rgn_dftmp$train[,setdiff(colnames(rgn_dftmp$train),constant_vars)]
    }
  })
  
  output$rgn_fe_output <- renderTable({
    head(rgn_dftmp$train,n=15)
  })
  
  #Variable Selection Output
  output$rgn_vs_output <- renderTable({
    head(rgn_dftmp$train,n=20)
  })
  
  output$rgn_vs_select_iv_corr <- renderUI({
    selectInput("rgn_vs_indep_var", "See Variable Correlation:",
                setdiff(names(rgn_dftmp$train),rgn_dftmp$dep_var), selected = NULL)
  })
  
  output$rgn_vs_select_vars_manual_dropdown <- renderUI({
    pickerInput("rgn_select_vars_manual","Independent Variables", 
                choices=setdiff(names(rgn_dftmp$train),rgn_dftmp$dep_var), 
                selected = setdiff(names(rgn_dftmp$train),rgn_dftmp$dep_var), 
                options = list(`actions-box` = TRUE),multiple = T)
  })
  
  observeEvent(input$rgn_vs_compute_corr,{
    rgn_dftmp$rgn_corr_df <-  var_corr(rgn_dftmp$train,input$rgn_vs_indep_var)
  })
  
  output$rgn_vs_corr_output <- renderDataTable({
    req(input$rgn_vs_compute_corr)
    rgn_dftmp$rgn_corr_df <- rgn_dftmp$rgn_corr_df[rgn_dftmp$rgn_corr_df$Variable %in% names(rgn_dftmp$train),]
    rgn_dftmp$rgn_corr_df
  },options = list(pageLength = 10))
  
  observeEvent(input$rgn_vs_compute_ttest_anova,{
    rgn_dftmp$ttest_anova_df <-  compute_statistical_significance(rgn_dftmp$train, rgn_dftmp$dep_var, method = "regression")
  })
  
  output$ttest_anova_vs_output <- renderDataTable({
    rgn_dftmp$ttest_anova_df
  },options = list(pageLength = 10))
  
  observeEvent(input$rgn_vs_compute_ss,{
    if(is.null(rgn_dftmp$ss_calc_df)==TRUE){
      rgn_dftmp$ss_calc_df <-  var_significance(rgn_dftmp$train,rgn_dftmp$dep_var,method = "regression")
    }
  })
  
  output$rgn_ss_vs_output <- renderDataTable({
    # req(input$cfn_vs_compute_ss)
    rgn_dftmp$ss_calc_df
  },options = list(pageLength = 10))
  
  #Variable Selection Filter
  observeEvent(input$rgn_action_4,{
    if(input$rgn_vs_select_method=="ttest_anova"){
      if(is.null(rgn_dftmp$ttest_anova_df)==TRUE){
        rgn_dftmp$ttest_anova_df <-  compute_statistical_significance(rgn_dftmp$train,rgn_dftmp$dep_var,method="regression")
      }
      vars <- as.character(rgn_dftmp$ttest_anova_df[(rgn_dftmp$ttest_anova_df$p_values<=input$ttest_anova_alpha),1])
      rgn_dftmp$ttest_anova_df <- rgn_dftmp$ttest_anova_df[rgn_dftmp$ttest_anova_df$Variable %in% vars,]
      rgn_dftmp$ss_calc_df <- rgn_dftmp$ss_calc_df[rgn_dftmp$ss_calc_df$Variable %in% vars,]
      vars <- c(vars,rgn_dftmp$dep_var)
      rgn_dftmp$train <- rgn_dftmp$train[,vars]
      if(is.null(rgn_dftmp$test)==FALSE){
        if(rgn_dftmp$dep_var %in% names(rgn_dftmp$test)){
          rgn_dftmp$test <- rgn_dftmp$test[,vars]
        }else{
          rgn_dftmp$test <- rgn_dftmp$test[,setdiff(vars,rgn_dftmp$dep_var)]
        }
      }
    }else if(input$rgn_vs_select_method=="linear"){
      if(is.null(rgn_dftmp$ss_calc_df)==TRUE){
        rgn_dftmp$ss_calc_df <-  var_significance(rgn_dftmp$train,rgn_dftmp$dep_var,method = "regression")
      }
      vars <- as.character(rgn_dftmp$ss_calc_df[(rgn_dftmp$ss_calc_df$p_values<=0.01),1])
      rgn_dftmp$ttest_anova_df <- rgn_dftmp$ttest_anova_df[dftmp$ttest_anova_df$Variable %in% vars,]
      rgn_dftmp$ss_calc_df <- rgn_dftmp$ss_calc_df[rgn_dftmp$ss_calc_df$Variable %in% vars,]
      vars <- c(vars, rgn_dftmp$dep_var)
      rgn_dftmp$train <- rgn_dftmp$train[,vars]
      if(is.null(rgn_dftmp$test)==FALSE){
        if(rgn_dftmp$dep_var %in% names(rgn_dftmp$test)){
          rgn_dftmp$test <- rgn_dftmp$test[,vars]
        }else{
          rgn_dftmp$test <- rgn_dftmp$test[,setdiff(vars,rgn_dftmp$dep_var)]
        }
      }
    }else if(input$rgn_vs_select_method=="manual"){
      vars <- input$rgn_select_vars_manual
      rgn_dftmp$ss_calc_df <- rgn_dftmp$ss_calc_df[rgn_dftmp$ss_calc_df$Variable %in% vars,]
      rgn_dftmp$ttest_anova_df <- rgn_dftmp$ttest_anova_df[rgn_dftmp$ttest_anova_df$Variable %in% vars,]
      vars <- c(vars, rgn_dftmp$dep_var)
      rgn_dftmp$train <- rgn_dftmp$train[,vars]
      if(is.null(rgn_dftmp$test)==FALSE){
        if(rgn_dftmp$dep_var %in% names(rgn_dftmp$test)){
          rgn_dftmp$test <- rgn_dftmp$test[,vars]
        }else{
          rgn_dftmp$test <- rgn_dftmp$test[,setdiff(vars,rgn_dftmp$dep_var)]
        }
      }
    }
  })
  
  #XGB Model Build
  observeEvent(input$rgn_mb_xgb_compute, {
    set.seed(123)
    sample <- sample.int(n = nrow(rgn_dftmp$train), size = floor(input$rgn_mb_xgb_train_split*nrow(rgn_dftmp$train)), replace = F)
    rgn_dftmp$validation_subset  <- rgn_dftmp$train[-sample,]
    rgn_dftmp$train_subset <- rgn_dftmp$train[sample,]
    rm(sample)
    invisible(gc())
    
    rgn_dftmp$Y_train_subset <- rgn_dftmp$train_subset[,rgn_dftmp$dep_var]
    rgn_dftmp$Y_valid_subset <- rgn_dftmp$validation_subset[,rgn_dftmp$dep_var]
    if(is.null(rgn_dftmp$test)==FALSE){
      if(rgn_dftmp$dep_var %in% names(rgn_dftmp$test)){
        rgn_dftmp$Y_test <- rgn_dftmp$test[,rgn_dftmp$dep_var] 
      }
    }
    
    rgn_dftmp$dtrain_subset  <- xgb.DMatrix(as.matrix(rgn_dftmp$train_subset[,-which(names(rgn_dftmp$train_subset) %in% c(rgn_dftmp$dep_var))]), label = rgn_dftmp$Y_train_subset)
    rgn_dftmp$dvalid_subset  <- xgb.DMatrix(as.matrix(rgn_dftmp$validation_subset[,-which(names(rgn_dftmp$validation_subset) %in% c(rgn_dftmp$dep_var))]), label = rgn_dftmp$Y_valid_subset)
    if(is.null(rgn_dftmp$test)==FALSE){
      if(rgn_dftmp$dep_var %in% names(rgn_dftmp$test)){
        rgn_dftmp$dtest  <- xgb.DMatrix(as.matrix(rgn_dftmp$test[,-which(names(rgn_dftmp$test) %in% c(rgn_dftmp$dep_var))]), label = rgn_dftmp$Y_test)
      }else{
        rgn_dftmp$dtest  <- xgb.DMatrix(as.matrix(rgn_dftmp$test))
      }
    }
    
    rgn_dftmp$watchlist <- list(train = rgn_dftmp$dtrain_subset, valid = rgn_dftmp$dvalid_subset)
    
    if(input$rgn_mb_xgb_eval_metric=="rmse"){
      eval_metric = "rmse"
      rgn_dftmp$xgb_params_eval_maximize <- FALSE
    }else if(input$rgn_mb_xgb_eval_metric=="mae"){
      eval_metric = "mae"
      rgn_dftmp$xgb_params_eval_maximize <- FALSE
    }
    
    if(input$rgn_mb_xgb_param_booster=="gbtree"){
      booster = "gbtree"
    }else if(input$rgn_mb_xgb_param_booster=="dart"){
      booster = "dart"
    }else if(input$rgn_mb_xgb_param_booster=="gblinear"){
      booster = "gblinear"
    }
    
    rgn_dftmp$xgb_params <- list(objective = "reg:linear",
                             eval_metric = eval_metric,
                             booster=booster,
                             max_depth = input$rgn_mb_xgb_param_max.depth,
                             eta = input$rgn_mb_xgb_param_eta,
                             gamma = input$rgn_mb_xgb_param_gamma, 
                             subsample = input$rgn_mb_xgb_param_subsample,
                             colsample_bytree = input$rgn_mb_xgb_param_colsample.by.tree, 
                             min_child_weight = input$rgn_mb_xgb_param_min.child.weight,
                             max_delta_step = input$rgn_mb_xgb_param_max.delta.step,
                             alpha = input$rgn_mb_xgb_param_alpha,
                             lambda = input$rgn_mb_xgb_param_lambda)
    
    shinyjs::html("rgn_mb_xgb_output", "")
    
    rgn_dftmp$xgb_model <- xgb.train(params=rgn_dftmp$xgb_params,
                                     rgn_dftmp$dtrain_subset, nrounds = input$rgn_mb_xgb_param_nrounds, rgn_dftmp$watchlist,
                                 callbacks = list(cb.print.evaluation(period = input$rgn_mb_xgb_param_print.every.n, method = "regression"), 
                                                  cb.early.stop(stopping_rounds = input$rgn_mb_xgb_param_early.stopping.rounds, maximize = rgn_dftmp$xgb_params_eval_maximize, method = "regression")), nthread=6)
    
    rgn_dftmp$imp <- xgb.importance(colnames(rgn_dftmp$train_subset[,-which(names(rgn_dftmp$train) %in% c(rgn_dftmp$dep_var))]), model=rgn_dftmp$xgb_model)
    
    rgn_dftmp$pred_train_subset = predict(rgn_dftmp$xgb_model, rgn_dftmp$dtrain_subset)
    #Add Eval Stats Here
    rgn_dftmp$pred_valid_subset = predict(rgn_dftmp$xgb_model, rgn_dftmp$dvalid_subset)
    #Add Eval Stats Here
    if(is.null(rgn_dftmp$test)==FALSE){
      rgn_dftmp$pred_test = predict(rgn_dftmp$xgb_model, rgn_dftmp$dtest)
      # if(rgn_dftmp$dep_var %in% names(rgn_dftmp$test)){
      #   #Add Eval Stats Here
      # }
    }
    
  })
  
  rgn_mb_xgb_stats <- observeEvent(input$rgn_mb_xgb_compute,{
    rgn_dftmp$train_accuracy <- accuracy_metrics(rgn_dftmp$pred_train_subset, rgn_dftmp$Y_train_subset, method="regression")
    
    rgn_dftmp$valid_accuracy <- accuracy_metrics(rgn_dftmp$pred_valid_subset, rgn_dftmp$Y_valid_subset, method="regression")
    
    if(is.null(rgn_dftmp$test)==FALSE){
      if(rgn_dftmp$dep_var %in% names(rgn_dftmp$test)){
        rgn_dftmp$test_accuracy <- accuracy_metrics(rgn_dftmp$pred_test, rgn_dftmp$Y_test, method="regression")
      }
    }
  })
  
  output$rgn_mb_xgb_stats_train <- renderPrint({
    validate(need(rgn_dftmp$train_accuracy,"Model not built yet"))
    print(rgn_dftmp$train_accuracy)
  })
  outputOptions(output,"rgn_mb_xgb_stats_train",suspendWhenHidden=FALSE)
  
  output$rgn_mb_xgb_stats_valid <- renderPrint({
    validate(need(rgn_dftmp$valid_accuracy,"Model not built yet"))
    print(rgn_dftmp$valid_accuracy)
  })
  outputOptions(output,"rgn_mb_xgb_stats_valid",suspendWhenHidden=FALSE)
  
  output$rgn_mb_xgb_stats_test <- renderPrint({
    validate(need(rgn_dftmp$test,"Test dataset not uploaded"))
    validate(need(rgn_dftmp$dep_var %in% names(rgn_dftmp$test), "Response variable not found"))
    print(rgn_dftmp$test_accuracy)
  })
  outputOptions(output,"rgn_mb_xgb_stats_test",suspendWhenHidden=FALSE)
  
  #Model Interpretation
  output$rgn_mb_xgb_varimp_iv <- renderUI({
    if(is.null(rgn_dftmp$imp)==FALSE){
      numericInput("rgn_mb_xgb_varimp_top_vars","Select Number of Variables To Display",min = 1,max = nrow(rgn_dftmp$imp),step = 1,value = round(nrow(rgn_dftmp$imp)/2))
    }else if(is.null(rgn_dftmp$imp)==TRUE){
      numericInput("rgn_mb_xgb_varimp_top_vars","Select Number of Variables To Display",min = 1,max = ncol(rgn_dftmp$train),step = 1,value = round(ncol(rgn_dftmp$train)/2))
    }
  })
  
  output$rgn_mb_xgb_var_imp <- renderPlot({
    if(is.null(rgn_dftmp$imp)==TRUE | input$rgn_mb_xgb_varimp_plot<=0){
      return()
    }else{
      input$rgn_mb_xgb_varimp_plot
      isolate(
        if(input$rgn_mb_xgb_param_booster=="gblinear"){
          ggplot(rgn_dftmp$imp[1:input$rgn_mb_xgb_varimp_top_vars,], aes(x=reorder(Feature,Weight), y=Weight, fill=Weight))+ 
            geom_bar(stat="identity", position="dodge", show.legend = T)+ coord_flip()+
            ylab("Weight")+
            xlab(as.character("Variable Importance"))+
            ggtitle(paste0("Variable Importance by ",as.character("Weight")))+
            scale_fill_gradient(low="red", high="blue")+labs(fill="Weight")+theme_bw()
        }else{
          ggplot(rgn_dftmp$imp[1:input$rgn_mb_xgb_varimp_top_vars,], aes(x=reorder(Feature,Gain), y=Gain,fill=Gain))+ 
            geom_bar(stat="identity", position="dodge", show.legend = T)+ coord_flip()+
            ylab("Gain")+
            xlab(as.character("Variable Importance"))+
            ggtitle(paste0("Variable Importance by ",as.character("Gain")))+
            scale_fill_gradient(low="red", high="blue")+labs(fill="Gain")+theme_bw()
        }
      )
    }
  }, height = 750)
  
  output$rgn_mb_xgb_pdp_vars <- renderUI({
    if(is.null(rgn_dftmp$imp)==FALSE){
      numericInput("rgn_mb_xgb_pdp_top_vars","Select Number of Variables To Display",min = 1,max = nrow(rgn_dftmp$imp),step = 1,value = round(nrow(rgn_dftmp$imp)/2))
    }else if(is.null(rgn_dftmp$imp)==TRUE){
      numericInput("rgn_mb_xgb_pdp_top_vars","Select Number of Variables To Display",min = 1,max = ncol(rgn_dftmp$train),step = 1,value = round(ncol(rgn_dftmp$train)/2))
    }
  })
  
  output$rgn_mb_xgb_pdp_vars_manual <- renderUI({
    pickerInput("rgn_pdp_vars_manual","Independent Variables",
                choices=setdiff(names(rgn_dftmp$train),rgn_dftmp$dep_var),
                selected = setdiff(names(rgn_dftmp$train),rgn_dftmp$dep_var),
                options = list(`actions-box` = TRUE),multiple = T)
  })
  
  output$rgn_mb_xgb_pdp_iv <- renderUI({
    input$rgn_mb_xgb_gen_pdp
    isolate({
      validate(need(is.null(rgn_dftmp$imp)==FALSE,""))
      if(input$rgn_pdp_select_method=="manual"){
        validate(need(length(input$rgn_pdp_vars_manual)>0,""))
        if(is.null(rgn_dftmp$imp)==FALSE){
          input$rgn_mb_xgb_gen_pdp
          isolate(
            selectInput("rgn_mb_xgb_pdp_dropdown", "Choose Independent Variable To View PDP Plot:",
                        input$rgn_pdp_vars_manual)
          )
        }
      }else if(input$rgn_pdp_select_method=="importance"){
        validate(need(input$rgn_mb_xgb_pdp_top_vars>0,""))
        if(is.null(rgn_dftmp$imp)==FALSE){
          input$rgn_mb_xgb_gen_pdp
          isolate(
            selectInput("rgn_mb_xgb_pdp_dropdown", "Choose Independent Variable To View PDP Plot:",
                        rgn_dftmp$imp[1:input$rgn_mb_xgb_pdp_top_vars,"Feature"])
          )
        }
      }
      else{
        p("")
      }
    })
  })
  
  #Generate Container
  observeEvent(input$rgn_mb_xgb_gen_pdp,{
    if(is.null(rgn_dftmp$xgb_model)==FALSE & 
       is.null(rgn_dftmp$imp)==FALSE){
      if(input$rgn_pdp_select_method=="importance"){
        rgn_dftmp$container <- container(n_rows = input$rgn_mb_xgb_pdp_top_vars, rgn_dftmp$xgb_model, rgn_dftmp$train_subset, rgn_dftmp$validation_subset, rgn_dftmp$imp, rgn_dftmp$dep_var, ratio = input$rgn_mb_xgb_pdp_train_split, method = "regression", selection = "importance")
      }else{
        rgn_dftmp$container <- container(n_rows = length(input$rgn_pdp_vars_manual), rgn_dftmp$xgb_model, rgn_dftmp$train_subset, rgn_dftmp$validation_subset, input$rgn_pdp_vars_manual, rgn_dftmp$dep_var, ratio = input$rgn_mb_xgb_pdp_train_split, method = "regression", selection = "manual")
      }
      rgn_dftmp$container$ls_importance[["variable_importance"]] <- rgn_dftmp$imp
      rgn_dftmp$container$ls_importance[["variable_importance_top_n_vars"]] <- input$rgn_mb_xgb_varimp_top_vars
      if(is.null(rgn_dftmp$train_accuracy)==FALSE){
        rgn_dftmp$container$method <- "regression"
        rgn_dftmp$container$ls_lift[["train_lift"]] <- rgn_dftmp$train_lift
        rgn_dftmp$container$ls_lift[["validation_lift"]] <- rgn_dftmp$validation_lift
        rgn_dftmp$container$ls_lift[["test_lift"]] <- rgn_dftmp$test_lift
        rgn_dftmp$container$ls_accuracy[["train_accuracy"]] <- rgn_dftmp$train_accuracy
        rgn_dftmp$container$ls_accuracy[["valid_accuracy"]] <- rgn_dftmp$valid_accuracy
        rgn_dftmp$container$ls_accuracy[["test_accuracy"]] <- rgn_dftmp$test_accuracy
      }
    }
  })
  
  output$rgn_mb_xgb_pdp_plot <- renderPlot({
    validate(need(input$rgn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(rgn_dftmp$container)){
      return()
    }else{
      pdp_plot(rgn_dftmp$container, input$rgn_mb_xgb_pdp_dropdown, dep_var = rgn_dftmp$dep_var, method = "regression")
    }
  })
  
  output$rgn_mb_xgb_density_plot <- renderPlot({
    validate(need(input$rgn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(rgn_dftmp$container)){
      return()
    }else{
      rgn_dftmp$container$ls_density[[input$rgn_mb_xgb_pdp_dropdown]]
    }
  })
  
  output$rgn_mb_xgb_var_summary <- renderPrint({
    validate(need(input$rgn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(rgn_dftmp$container)){
      return(cat(""))
    }else{
      validate(need(input$rgn_mb_xgb_pdp_dropdown,"Select a valid variable"))
      validate(need(rgn_dftmp$container,"Select a valid variable"))
      rgn_dftmp$container$ls_summary[[input$rgn_mb_xgb_pdp_dropdown]]
    }
  })
  
  rgn_score_code <- reactive({
    leaf_val <- get_leaf_info(xgb_model = rgn_dftmp$xgb_model, output = "dataframe")
    return(leaf_val)
  })
  
  #XGBoost R to SAS
  output$rgn_mb_xgb_score <- downloadHandler(
    filename = function() {
      paste("xgb_score_code", ".csv", sep = "")
    },
    content = function(filename) {
      write.csv(rgn_score_code(), filename, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  #ML Interpretation Report
  output$rgn_mb_xgb_report <- downloadHandler(
    filename = function() {
      paste('Report', sep = '.', 'docx')
    },
    
    content = function(file) {
      path <- paste(getwd(),"/Format.docx",sep = '')
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      out <- render('report.Rmd',params = rgn_dftmp$container, word_document(reference_docx = path, toc = TRUE, toc_depth = 2))
      # out <- render('report.Rmd',params = dftmp$container, word_document())
      
      file.rename(out, file)
    }
  )
  
  #Test Predictions
  rgn_getTestPredictions <- reactive({
    if(is.null(rgn_dftmp$test)==FALSE){
      df <- data.frame(ID = rownames(rgn_dftmp$test), pred = rgn_dftmp$pred_test)
      return(df)
    }
  })
  
  #Download Test Predictions
  output$rgn_mb_xgb_test_prob <- downloadHandler(
    
    filename = function() { 
      paste("test_predictions", ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(rgn_getTestPredictions(), file, row.names = F)
    })
  
  #Download Model
  rgn_download_model <- reactive({
    return(rgn_dftmp$xgb_model)
  })
  
  output$rgn_mb_xgb_download <- downloadHandler(
    filename <- function(){
      paste("xgb_model.RData")
    },
    
    content = function(file) {
      model <- rgn_download_model()
      save(model, file = file)
    }
  )
  
  
  ############################################################################
  #-----------------------Use Existing Model Regression-----------------------
  ############################################################################
  
  use_rgn_dftmp <- reactiveValues(train=NULL, test=NULL, raw_train_head=NULL, train_structure=NULL,
                                  train_ohe=FALSE, dep_var=NULL, preprocess_train_head=NULL,
                                  preprocess_train_structure=NULL,fe_indep_var=NULL, iv_calc_df=NULL,
                                  ss_calc_df=NULL, cfn_corr_df=NULL, train_subset=NULL,
                                  Y_train=NULL, Y_valid=NULL, Y_test=NULL, watchlist=NULL,
                                  dtrain=NULL, dvalid=NULL,dtest=NULL, xgb_params=NULL, 
                                  xgb_params_eval_maximize=TRUE, xgb_model=NULL, train_lift=NULL, 
                                  test_lift=NULL, train_accuracy = NULL, test_accuracy = NULL,
                                  pred_train=NULL, pred_test=NULL, imp=NULL,
                                  container=NULL, difference=FALSE, missing_lookup=NULL,
                                  warning_message=NULL, error_message=NULL, xgb_error=NULL)
  
  observeEvent(input$use_rgn_file1,{
    tryCatch({
      path <- normalizePath(input$use_rgn_getwd)
      file <- input$use_rgn_file_name
      if(file.exists(paste(path,file,sep = '\\'))==TRUE){
        use_rgn_dftmp$train <- fread(paste(path,file,sep = '\\'),
                                     header = TRUE,
                                     data.table = FALSE,
                                     stringsAsFactors = TRUE,
                                     sep = input$use_rgn_sep,
                                     skip = as.numeric(input$use_rgn_upload_start),
                                     nrows = as.numeric(input$use_rgn_upload_nrows),
                                     na.strings = input$use_rgn_upload_na_strings)
        
        if(is.null(use_rgn_dftmp$test)==FALSE){
          if(length(setdiff(names(use_rgn_dftmp$test), names(use_rgn_dftmp$train))) <= 1 & length(setdiff(names(use_rgn_dftmp$train), names(use_rgn_dftmp$test))) <= 1){
            use_rgn_dftmp$warning_message = NULL
            use_rgn_dftmp$error_message = NULL
            use_rgn_dftmp$raw_train_head <- head(use_rgn_dftmp$train,n=20)
          }else{
            use_rgn_dftmp$warning_message = "Train and test names do not match"
            use_rgn_dftmp$error_message = "Train and test names do not match"
          }
        }else{
          use_rgn_dftmp$raw_train_head <- head(use_rgn_dftmp$train,n=20)
          use_rgn_dftmp$warning_message <- NULL
          use_rgn_dftmp$error_message <- NULL
        }
      }else{
        use_rgn_dftmp$warning_message <- "No such file or directory found"
      }
    },
    warning = function(w){
      use_rgn_dftmp$warning_message <- "No such file or directory found"
    },
    error = function(e){
      use_rgn_dftmp$error_message <- "Error parsing the file"
    })
    return(TRUE)
  })
  
  observeEvent(input$use_rgn_file2,{
    tryCatch({
      path <- normalizePath(input$use_rgn_getwd)
      file <- input$use_rgn_file_name
      if(file.exists(paste(path,file,sep = '\\'))==TRUE){
        use_rgn_dftmp$test <- fread(paste(path,file,sep = '\\'),
                                    header = TRUE,
                                    data.table = FALSE,
                                    stringsAsFactors = TRUE,
                                    sep = input$use_rgn_sep,
                                    skip = as.numeric(input$use_rgn_upload_start),
                                    nrows = as.numeric(input$use_rgn_upload_nrows),
                                    na.strings = input$use_rgn_upload_na_strings)
        
        if(length(setdiff(names(use_rgn_dftmp$test), names(use_rgn_dftmp$train))) <= 1 & length(setdiff(names(use_rgn_dftmp$train), names(use_rgn_dftmp$test))) <= 1){
          use_rgn_dftmp$warning_message = NULL
          use_rgn_dftmp$error_message = NULL
        }else{
          use_rgn_dftmp$warning_message = "Train and test names do not match"
          use_rgn_dftmp$error_message = "Train and test names do not match"
        }
      }else{
        use_rgn_dftmp$warning_message <- "No such file or directory found"
      }
    },
    warning = function(w){
      use_rgn_dftmp$warning_message <- "No such file or directory found"
    },
    error = function(e){
      use_rgn_dftmp$error_message <- "Error parsing the file"
    })
    return(TRUE)
  })
  
  observeEvent(input$use_rgn_file3,{
    tryCatch({
      if ( is.null(input$use_rgn_file3)) return(NULL)
      inFile <- isolate({input$use_rgn_file3})
      file <- inFile$datapath
      # load the file into new environment and get it from there
      e = new.env()
      name <- load(file, envir = e)
      use_rgn_dftmp$xgb_model <- e[[name]]
      use_rgn_dftmp$xgb_error <- NULL
    },
    warning = function(w){
      use_rgn_dftmp$xgb_error <- "Valid model RData file not uploaded"
    },
    error = function(e){
      use_rgn_dftmp$xgb_error <- "Valid model RData file not uploaded"
    })
    
  })
  
  output$use_rgn_raw_output <- renderTable({
    input$use_rgn_file1
    input$use_rgn_file2
    isolate({
      validate(need(input$use_rgn_getwd,"Directory not provided"))
      validate(need(input$use_rgn_file_name,"File not provided"))
      validate(need(is.null(use_rgn_dftmp$warning_message)==TRUE,use_rgn_dftmp$warning_message))
      validate(need(is.null(use_rgn_dftmp$error_message)==TRUE,use_rgn_dftmp$error_message))
      use_rgn_dftmp$raw_train_head
    })
  })
  
  output$use_rgn_xgb_features <- renderPrint({
    if(is.null(use_rgn_dftmp$xgb_error)==FALSE){
      validate("Valid model RData file not uploaded")
    }else{
      validate(need(is.null(use_rgn_dftmp$xgb_model)==F,"XGBoost model RData file is not uploaded yet"))
      validate(need(is.null(use_rgn_dftmp$xgb_model$feature_names)==F,"Incorrect RData file uploaded"))
      use_rgn_dftmp$xgb_model$feature_names
    }
  })
  
  outputOptions(output,"use_rgn_xgb_features",suspendWhenHidden=FALSE)
  outputOptions(output,"use_rgn_raw_output",suspendWhenHidden=FALSE)
  
  #Data Preprocess
  output$use_rgn_select_dv <- renderUI({
    if(!is.null(use_rgn_dftmp$train)){
      choices = names(use_rgn_dftmp$train)
    }
    selectInput("use_rgn_dep_var", "Choose Dependent Variable:",
                choices = choices, selected = use_rgn_dftmp$dep_var)
  })
  
  observeEvent(input$use_rgn_data_preprocess_confirm,{
    
    use_rgn_dftmp$dep_var <- input$use_rgn_dep_var
    
    if(class(use_rgn_dftmp$train[,input$use_rgn_dep_var]) %in% c("integer","numeric") == TRUE &
       length(unique(use_rgn_dftmp$train[,input$use_rgn_dep_var]))>2){
      
      if(input$use_rgn_missing){
        use_rgn_dftmp$missing_lookup <- treat_Missing(use_rgn_dftmp$train)
        use_rgn_dftmp$train <- replace_Missing(use_rgn_dftmp$train, use_rgn_dftmp$missing_lookup)
        if(is.null(use_rgn_dftmp$test)==FALSE){
          use_rgn_dftmp$test <- replace_Missing(use_rgn_dftmp$test, use_rgn_dftmp$missing_lookup)
        }
      }
      
      if(input$use_rgn_ohe){
        if(use_rgn_dftmp$train_ohe == FALSE){
          use_rgn_dftmp$train <- create_dummies(use_rgn_dftmp$train)
          if(is.null(use_rgn_dftmp$test)==FALSE){
            use_rgn_dftmp$test <- create_dummies(use_rgn_dftmp$test)
          }
          use_rgn_dftmp$train_ohe <- TRUE
        }
      }
    }
    
    #Assigning global variables required for temp view
    use_rgn_dftmp$preprocess_train_head <- head(use_rgn_dftmp$train,n=15)
    use_rgn_dftmp$preprocess_train_structure <- strtable(use_rgn_dftmp$train)
    
  })
  
  output$use_rgn_data_preprocess_output <- renderTable({
    if(input$use_rgn_data_preprocess_confirm == 0) return()
    
    input$use_rgn_data_preprocess_confirm
    isolate({
      validate(need(class(use_rgn_dftmp$train[,input$use_rgn_dep_var]) %in% c("integer","numeric"),"Dependent variable is not numeric"))
      validate(need(length(unique(use_rgn_dftmp$train[,input$use_rgn_dep_var]))>2,"Dependent variable is binary not continuous"))
      
      if(is.null(use_rgn_dftmp$test)==FALSE){
        if(input$use_rgn_dep_var %in% names(use_rgn_dftmp$test)){
          validate(need(class(use_rgn_dftmp$test[,input$use_rgn_dep_var]) %in% c("integer","numeric"),"Dependent variable for test dataset is not numeric"))
          validate(need(length(unique(use_rgn_dftmp$test[,input$use_rgn_dep_var]))>2,"Dependent variable for test dataset is binary not continuous"))
        }
      }
      if(is.null(use_rgn_dftmp$preprocess_train_head)==TRUE) {
        return()
        }
      else{
        use_rgn_dftmp$preprocess_train_head
      }
    })
  })
  
  output$use_rgn_data_structure_output <- renderDataTable({
    if(input$use_rgn_data_preprocess_confirm == 0) return()
    use_rgn_dftmp$preprocess_train_structure
  },options = list(pageLength = 10))
  outputOptions(output,"use_rgn_data_preprocess_output",suspendWhenHidden=FALSE)
  
  observeEvent(input$use_rgn_data_preprocess_refresh,{
    use_rgn_dftmp$preprocess_train_head <- head(use_rgn_dftmp$train,n=15)
    use_rgn_dftmp$preprocess_train_structure <- strtable(use_rgn_dftmp$train)
  })
  
  #Feature Engineering
  output$use_rgn_fe_select_iv <- renderUI({
    selectInput("use_rgn_fe_indep_var", "Choose Independent Variable To Implement Feature Engineering On:",
                setdiff(names(use_rgn_dftmp$train),use_rgn_dftmp$dep_var), selected = use_rgn_dftmp$fe_indep_var)
  })
  
  observeEvent(input$use_rgn_fe_action,{
    use_rgn_dftmp$fe_indep_var <- input$use_rgn_fe_indep_var
    
    #One Hot Encoding
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_ohe"){
      if(use_rgn_dftmp$train_ohe == FALSE){
        use_rgn_dftmp$train <- create_dummies(use_rgn_dftmp$train)
        use_rgn_dftmp$train_ohe <- TRUE
        if(is.null(use_rgn_dftmp$test)==FALSE){
          use_rgn_dftmp$test <- create_dummies(use_rgn_dftmp$test)
        }
      }
    }
    
    #Categorical Replacement
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_cat_replacement" & input$use_rgn_fe_cat_replacement_radio_button == "use_rgn_fe_cat_freq"){
      use_rgn_dftmp$train <- cat_replacement(method = "frequency", 
                                         df = use_rgn_dftmp$train,indep_var = input$use_rgn_fe_indep_var, 
                                         dep_var = use_rgn_dftmp$dep_var)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- cat_replacement(method = "frequency", 
                                          df = use_rgn_dftmp$test,indep_var = input$use_rgn_fe_indep_var, 
                                          dep_var = use_rgn_dftmp$dep_var)
      }
    }
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_cat_replacement" & input$use_rgn_fe_cat_replacement_radio_button == "use_rgn_fe_cat_mean"){
      use_rgn_dftmp$train <- cat_replacement(method = "mean", 
                                         df = use_rgn_dftmp$train,indep_var = input$use_rgn_fe_indep_var, 
                                         dep_var = use_rgn_dftmp$dep_var)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- cat_replacement(method = "mean", 
                                          df = use_rgn_dftmp$test,indep_var = input$use_rgn_fe_indep_var, 
                                          dep_var = use_rgn_dftmp$dep_var)
      }
    }
    
    #Binning
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_bins" & input$use_rgn_fe_bins_radio_button == "use_rgn_fe_bin_auto_cuts"){
      use_rgn_dftmp$train <- binning(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, use_rgn_dftmp$dep_var, method = "auto", bins = input$use_rgn_fe_bin_auto_cuts_value_input)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- binning(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, use_rgn_dftmp$dep_var, method = "auto", bins = input$use_rgn_fe_bin_auto_cuts_value_input)
      }
    }
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_bins" & input$use_rgn_fe_bins_radio_button == "use_rgn_fe_bin_manual_cuts"){
      use_rgn_dftmp$train <- binning(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, use_rgn_dftmp$dep_var, method = "manual", cuts = input$use_rgn_fe_bin_manual_cuts_value_input)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- binning(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, use_rgn_dftmp$dep_var, method = "manual", cuts = input$use_rgn_fe_bin_manual_cuts_value_input)
      }
    }
    
    #Missing Value Treatment
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_missing" & input$use_rgn_fe_missing_radio_button == "use_rgn_fe_central_tendency"){
      use_rgn_dftmp$missing_lookup <- treat_Missing(use_rgn_dftmp$train)
      use_rgn_dftmp$train <- fe_treat_Missing(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, method = "auto", missing_lookup = use_rgn_dftmp$missing_lookup)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- fe_treat_Missing(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, method = "auto", missing_lookup = use_rgn_dftmp$missing_lookup)
      }
    }
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_missing" & input$use_rgn_fe_missing_radio_button == "use_rgn_fe_missing_value"){
      use_rgn_dftmp$train <- fe_treat_Missing(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, method = "manual", missing_value = input$use_rgn_fe_missing_value_input)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- fe_treat_Missing(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, method = "manual", missing_value = input$use_rgn_fe_missing_value_input)
      }
    }
    
    #Text Extraction
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_text_extraction" & input$use_rgn_fe_text_extract_radio_button == "use_rgn_fe_text_contains"){
      use_rgn_dftmp$train <- text_extraction(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, method = "contains", text_to_find = input$use_rgn_fe_text_extraction_value_input)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- text_extraction(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, method = "contains", text_to_find = input$use_rgn_fe_text_extraction_value_input)
      }
    }
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_text_extraction" & input$use_rgn_fe_text_extract_radio_button == "use_rgn_fe_text_equals"){
      use_rgn_dftmp$train <- text_extraction(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, method = "equals", text_to_find = input$use_rgn_fe_text_extraction_value_input)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- text_extraction(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, method = "equals", text_to_find = input$use_rgn_fe_text_extraction_value_input)
      }
    }
    
    #Transformations
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_transformations" & input$use_rgn_fe_transformations_radio_button == "log"){
      use_rgn_dftmp$train <- transformations(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, method = "log")
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- transformations(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, method = "log")
      }
    }
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_transformations" & input$use_rgn_fe_transformations_radio_button == "exp"){
      use_rgn_dftmp$train <- transformations(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, method = "exp")
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- transformations(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, method = "exp")
      }
    }
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_transformations" & input$use_rgn_fe_transformations_radio_button == "inv"){
      use_rgn_dftmp$train <- transformations(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, method = "inv")
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- transformations(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, method = "inv")
      }
    }
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_transformations" & input$use_rgn_fe_transformations_radio_button == "square"){
      use_rgn_dftmp$train <- transformations(use_rgn_dftmp$train, input$use_rgn_fe_indep_var, method = "square")
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- transformations(use_rgn_dftmp$test, input$use_rgn_fe_indep_var, method = "square")
      }
    }
    
    #Delete Variable
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_remove_var" & input$use_rgn_fe_remove_var_radio_button == "use_rgn_fe_remove_var_dropdown"){
      use_rgn_dftmp$train <- delete_var(use_rgn_dftmp$train, input$use_rgn_fe_indep_var)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- delete_var(use_rgn_dftmp$test, input$use_rgn_fe_indep_var)
      }
    }
    if(input$use_rgn_fe_radio_button == "use_rgn_fe_remove_var" & input$use_rgn_fe_remove_var_radio_button == "use_rgn_fe_remove_var_name"){
      use_rgn_dftmp$train <- delete_var(use_rgn_dftmp$train, input$use_rgn_fe_remove_var_value_input)
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- delete_var(use_rgn_dftmp$test, input$use_rgn_fe_remove_var_value_input)
      }
    }
    
  })
  
  observeEvent(input$use_rgn_action_3,{
    if(use_rgn_dftmp$train_ohe == FALSE){
      use_rgn_dftmp$train <- create_dummies(use_rgn_dftmp$train)
      use_rgn_dftmp$train_ohe <- TRUE
      if(is.null(use_rgn_dftmp$test)==FALSE){
        use_rgn_dftmp$test <- create_dummies(use_rgn_dftmp$test)
      }
    }
    constant_var_logic <- sapply(use_rgn_dftmp$train, function(x) length(unique(x))==1)
    if('TRUE' %in% constant_var_logic){
      constant_vars <- names(which(constant_var_logic,TRUE))
      use_rgn_dftmp$train <- use_rgn_dftmp$train[,setdiff(colnames(use_rgn_dftmp$train),constant_vars)]
    }
  })
  
  output$use_rgn_fe_output <- renderTable({
    head(use_rgn_dftmp$train,n=15)
  })
  
  #QC Tab
  observeEvent(input$use_rgn_action_1,{
    for (i in use_rgn_dftmp$xgb_model$feature_names) {
      if(i %in% names(use_rgn_dftmp$train) == FALSE){
        use_rgn_dftmp$difference <- TRUE
        break
      }else{
        use_rgn_dftmp$difference <- FALSE
      }
    }
    for (i in setdiff(names(use_rgn_dftmp$train),use_rgn_dftmp$dep_var)) {
      if(i %in% use_rgn_dftmp$xgb_model$feature_names == FALSE){
        use_rgn_dftmp$difference <- TRUE
        break
      }else{
        use_rgn_dftmp$difference <- FALSE
      }
    }
  })
  
  observeEvent(input$use_rgn_fe_action,{
    for (i in use_rgn_dftmp$xgb_model$feature_names) {
      if(i %in% names(use_rgn_dftmp$train) == FALSE){
        use_rgn_dftmp$difference <- TRUE
        break
      }else{
        use_rgn_dftmp$difference <- FALSE
      }
    }
    for (i in setdiff(names(use_rgn_dftmp$train),use_rgn_dftmp$dep_var)) {
      if(i %in% use_rgn_dftmp$xgb_model$feature_names == FALSE){
        use_rgn_dftmp$difference <- TRUE
        break
      }else{
        use_rgn_dftmp$difference <- FALSE
      }
    }
  })
  
  observeEvent(input$use_rgn_action_3,{
    for (i in use_rgn_dftmp$xgb_model$feature_names) {
      if(i %in% names(use_rgn_dftmp$train) == FALSE){
        use_rgn_dftmp$difference <- TRUE
        break
      }else{
        use_rgn_dftmp$difference <- FALSE
      }
    }
    for (i in setdiff(names(use_rgn_dftmp$train),use_rgn_dftmp$dep_var)) {
      if(i %in% use_rgn_dftmp$xgb_model$feature_names == FALSE){
        use_rgn_dftmp$difference <- TRUE
        break
      }else{
        use_rgn_dftmp$difference <- FALSE
      }
    }
  })
  
  output$use_rgn_fe_output_qc <- renderPrint({
    if(use_rgn_dftmp$difference == FALSE){
      print("Feature names match. Status: Ok")
    }
    else{
      validate(need(use_rgn_dftmp$difference == FALSE, "Feature names don't match. Status: Not Ok"))
    }
  })
  outputOptions(output,"use_rgn_fe_output_qc",suspendWhenHidden=FALSE)
  
  #Model Interpretation
  output$use_rgn_mb_xgb_varimp_iv <- renderUI({
    numericInput("use_rgn_mb_xgb_varimp_top_vars","Select Number of Variables To Display",min = 1,max = length(use_rgn_dftmp$xgb_model$feature_names),step = 1,value = round(length(use_rgn_dftmp$xgb_model$feature_names)/2))
  })
  
  output$use_rgn_mb_xgb_pdp_vars <- renderUI({
    numericInput("use_rgn_mb_xgb_pdp_top_vars","Select Number of Variables To Display",min = 1,max = length(use_rgn_dftmp$xgb_model$feature_names),step = 1,value = round(length(use_rgn_dftmp$xgb_model$feature_names)/2))
  })
  
  output$use_rgn_mb_xgb_pdp_vars_manual <- renderUI({
    pickerInput("use_rgn_pdp_vars_manual","Independent Variables",
                choices=setdiff(names(use_rgn_dftmp$train),use_rgn_dftmp$dep_var),
                selected = setdiff(names(use_rgn_dftmp$train),use_rgn_dftmp$dep_var),
                options = list(`actions-box` = TRUE),multiple = T)
  })
  
  output$use_rgn_mb_xgb_pdp_iv <- renderUI({
    input$use_rgn_mb_xgb_gen_pdp
    isolate({
      validate(need(is.null(use_rgn_dftmp$imp)==FALSE,""))
      if(input$use_rgn_pdp_select_method=="manual"){
        validate(need(length(input$use_rgn_pdp_vars_manual)>0,""))
        if(is.null(use_rgn_dftmp$imp)==FALSE){
          input$use_rgn_mb_xgb_gen_pdp
          isolate(
            selectInput("use_rgn_mb_xgb_pdp_dropdown", "Choose Independent Variable To View PDP Plot:",
                        input$use_rgn_pdp_vars_manual)
          )
        }
      }else if(input$use_rgn_pdp_select_method=="importance"){
        validate(need(input$use_rgn_mb_xgb_pdp_top_vars>0,""))
        if(is.null(use_rgn_dftmp$imp)==FALSE){
          input$use_rgn_mb_xgb_gen_pdp
          isolate(
            selectInput("use_rgn_mb_xgb_pdp_dropdown", "Choose Independent Variable To View PDP Plot:",
                        use_rgn_dftmp$imp[1:input$use_rgn_mb_xgb_pdp_top_vars,"Feature"])
          )
        }
      }
      else{
        p("")
      }
    })
  })
  
  output$use_rgn_mb_xgb_var_imp <- renderPlot({
    if(input$use_rgn_mb_xgb_varimp_plot<=0){
      return()
    }else{
      if(is.null(use_rgn_dftmp$imp)==TRUE){
        use_rgn_dftmp$imp <- xgb.importance(use_rgn_dftmp$xgb_model$feature_names, model=use_rgn_dftmp$xgb_model)
      }

      input$use_rgn_mb_xgb_varimp_plot
      isolate(
        ggplot(use_rgn_dftmp$imp[1:input$use_rgn_mb_xgb_varimp_top_vars,], aes(x=reorder(Feature,Weight), y=Weight, fill=Weight))+ 
          geom_bar(stat="identity", position="dodge", show.legend = T)+ coord_flip()+
          ylab("Weight")+
          xlab(as.character("Variable Importance"))+
          ggtitle(paste0("Variable Importance by ",as.character("Weight")))+
          scale_fill_gradient(low="red", high="blue")+labs(fill="Weight")+theme_bw()
      )
    }
  }, height = 750)
  
  #Generate Container
  observeEvent(input$use_rgn_mb_xgb_gen_pdp,{
    #Variable Importance
    if(is.null(use_rgn_dftmp$imp)==TRUE){
      use_rgn_dftmp$imp <- xgb.importance(use_rgn_dftmp$xgb_model$feature_names, model=use_rgn_dftmp$xgb_model)
    }
    
    #Container
    if(input$use_rgn_pdp_select_method=="importance"){
      use_rgn_dftmp$container <- container(n_rows = input$use_rgn_mb_xgb_pdp_top_vars, use_rgn_dftmp$xgb_model, use_rgn_dftmp$train, use_rgn_dftmp$test, use_rgn_dftmp$imp, use_rgn_dftmp$dep_var, ratio = input$use_rgn_mb_xgb_pdp_train_split, method = "regression", selection = "importance")
    }else{
      use_rgn_dftmp$container <- container(n_rows = length(input$use_rgn_pdp_vars_manual), use_rgn_dftmp$xgb_model, use_rgn_dftmp$train, use_rgn_dftmp$test, input$use_rgn_pdp_vars_manual, use_rgn_dftmp$dep_var, ratio = input$use_rgn_mb_xgb_pdp_train_split, method = "regression", selection = "manual")
    }
    
    use_rgn_dftmp$container$ls_importance[["variable_importance"]] <- use_rgn_dftmp$imp
    use_rgn_dftmp$container$ls_importance[["variable_importance_top_n_vars"]] <- input$use_rgn_mb_xgb_varimp_top_vars
    
    use_rgn_dftmp$container$ls_lift[["train_lift"]] <- use_rgn_dftmp$train_lift
    # use_cfn_dftmp$container$ls_lift[["test_lift"]] <- use_cfn_dftmp$test_lift
    use_rgn_dftmp$container$ls_lift[["validation_lift"]] <- use_rgn_dftmp$test_lift
    
    #Accuracy
    if(is.null(use_rgn_dftmp$train_accuracy)==TRUE){
      use_rgn_dftmp$Y_train <- use_rgn_dftmp$train[,use_rgn_dftmp$dep_var]
      use_rgn_dftmp$dtrain  <- xgb.DMatrix(as.matrix(use_rgn_dftmp$train[,-which(names(use_rgn_dftmp$train) %in% c(use_rgn_dftmp$dep_var))]), label = use_rgn_dftmp$Y_train)
      use_rgn_dftmp$pred_train = predict(use_rgn_dftmp$xgb_model, use_rgn_dftmp$dtrain)
      use_rgn_dftmp$train_accuracy <- accuracy_metrics(use_rgn_dftmp$pred_train, use_rgn_dftmp$Y_train, method="regression")
      if(is.null(use_rgn_dftmp$test)==FALSE){
        if(use_rgn_dftmp$dep_var %in% names(use_rgn_dftmp$test)){
          use_rgn_dftmp$Y_test <- use_rgn_dftmp$test[,use_rgn_dftmp$dep_var]
          use_rgn_dftmp$dtest  <- xgb.DMatrix(as.matrix(use_rgn_dftmp$test[,-which(names(use_rgn_dftmp$test) %in% c(use_rgn_dftmp$dep_var))]), label = use_rgn_dftmp$Y_test)
        }else{
          use_rgn_dftmp$dtest  <- xgb.DMatrix(as.matrix(use_rgn_dftmp$test))
        }
        use_rgn_dftmp$pred_test = predict(use_rgn_dftmp$xgb_model, use_rgn_dftmp$dtest)
        if(use_rgn_dftmp$dep_var %in% names(use_rgn_dftmp$test)){
          use_rgn_dftmp$test_accuracy <- accuracy_metrics(use_rgn_dftmp$pred_test, use_rgn_dftmp$Y_test, method="regression")
        }
      }
    }
    
    use_rgn_dftmp$container$ls_accuracy[["train_accuracy"]] <- use_rgn_dftmp$train_accuracy
    use_rgn_dftmp$container$ls_accuracy[["test_accuracy"]] <- use_rgn_dftmp$test_accuracy
    use_rgn_dftmp$container$method <- "regression"
  })
  
  output$use_rgn_mb_xgb_pdp_plot <- renderPlot({
    validate(need(input$use_rgn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(use_rgn_dftmp$container)){
      return()
    }else{
      pdp_plot(use_rgn_dftmp$container, input$use_rgn_mb_xgb_pdp_dropdown, dep_var = use_rgn_dftmp$dep_var, method = "regression")
    }
  })
  
  output$use_rgn_mb_xgb_density_plot <- renderPlot({
    validate(need(input$use_rgn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(use_rgn_dftmp$container)){
      return()
    }else{
      use_rgn_dftmp$container$ls_density[[input$use_rgn_mb_xgb_pdp_dropdown]]
    }
  })
  
  output$use_rgn_mb_xgb_var_summary <- renderPrint({
    validate(need(input$use_rgn_mb_xgb_pdp_dropdown,"Variable not selected"))
    if(is.null(use_rgn_dftmp$container)){
      return(cat(""))
    }else{
      validate(need(input$use_rgn_mb_xgb_pdp_dropdown,"Select a valid variable"))
      validate(need(use_rgn_dftmp$container,"Select a valid variable"))
      use_rgn_dftmp$container$ls_summary[[input$use_rgn_mb_xgb_pdp_dropdown]]
    }
  })
  
  output$use_rgn_mb_xgb_stats_train <- renderPrint({
    validate(need(use_rgn_dftmp$train_accuracy,"Accuracy metrics not compute yet"))
    print(use_rgn_dftmp$train_accuracy)
  })
  outputOptions(output,"use_rgn_mb_xgb_stats_train",suspendWhenHidden=FALSE)
  
  output$use_rgn_mb_xgb_stats_test <- renderPrint({
    validate(need(use_rgn_dftmp$test,"Test dataset not uploaded"))
    validate(need(use_rgn_dftmp$dep_var %in% names(use_rgn_dftmp$test), "Response variable not found"))
    print(use_rgn_dftmp$test_accuracy)
  })
  outputOptions(output,"use_rgn_mb_xgb_stats_test",suspendWhenHidden=FALSE)
  
  
  use_rgn_score_code <- reactive({
    leaf_val <- get_leaf_info(xgb_model = use_rgn_dftmp$xgb_model, output = "dataframe")
    return(leaf_val)
  })
  
  #XGBoost R to SAS
  output$use_rgn_mb_xgb_score <- downloadHandler(
    filename = function() {
      paste("xgb_score_code", ".csv", sep = "")
    },
    content = function(filename) {
      write.csv(use_rgn_score_code(), filename, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  #ML Interpretation Report
  output$use_rgn_mb_xgb_report <- downloadHandler(
    filename = function() {
      paste('Report', sep = '.', 'docx')
    },
    
    content = function(file) {
      path <- paste(getwd(),"/Format.docx",sep = '')
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      out <- render('report.Rmd',params = use_rgn_dftmp$container, word_document(reference_docx = path, toc = TRUE, toc_depth = 2))
      # out <- render('report.Rmd',params = dftmp$container, word_document())
      
      file.rename(out, file)
    }
  )
  
  #Test Predictions
  use_rgn_getTestPredictions <- reactive({
    if(is.null(use_rgn_dftmp$test)==FALSE){
      df <- data.frame(ID = rownames(use_rgn_dftmp$test), pred = use_rgn_dftmp$pred_test)
      return(df)
    }
  })
  
  #Download Test Predictions
  output$use_rgn_mb_xgb_test_prob <- downloadHandler(
    
    filename = function() { 
      paste("test_predictions", ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(use_rgn_getTestPredictions(), file, row.names = F)
    })
  
  #Download Model
  use_rgn_download_model <- reactive({
    return(use_rgn_dftmp$xgb_model)
  })
  
  output$rgn_mb_xgb_download <- downloadHandler(
    filename <- function(){
      paste("xgb_model.RData")
    },
    
    content = function(file) {
      model <- use_rgn_download_model()
      save(model, file = file)
    }
  )
  
})
