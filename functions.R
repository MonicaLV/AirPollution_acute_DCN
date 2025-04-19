mkDescriptiveTable <- function(contVars, catVars, dataFrame) {
  varNames_cat <- c()
  varNames_cont <- c()
  Ns  <- c()
  missings <- c()
  misrates <- c()
  means <- c()
  sds <- c()
  medians <- c()
  p25s <- c()
  p75s <- c()
  iqrs <- c()
  mins <- c()
  maxs <- c()
  ncats <- c()
  percents <- c()
  roundDig <- 3
  for (v in catVars) {
    tableInfo <- table(dataFrame[v], exclude=NULL)
    propInfo <- prop.table(table(dataFrame[v], exclude=NULL))*100
    groups <- length(propInfo)
    for (c in 1:groups) {
      ncat <- ''
      ncat <- tableInfo[c]
      percent <- ''
      percent <- round(propInfo[c], digits=1)
      varName <- paste(v, '_', labels(propInfo[c]), sep='')
      varNames_cat <- c(varNames_cat, varName)
      ncats <- c(ncats, ncat)
      percents <- c(percents, percent)
      
    }
    
    if(isTRUE(labels(propInfo[groups])!='NA')){
      varName <- paste(v, '_', 'NA', sep='')
      varNames_cat <- c(varNames_cat, varName)
      ncats <- c(ncats, 0)
      percents <- c(percents, 0)
    }
  }
  for (v in contVars){
    n <- sum(!is.na(dataFrame[v]))
    missing <- sum(is.na(dataFrame[v]))
    misrate <- prop.table(table(is.na(dataFrame[v])))[2]*100
    mean <- round(mean(dataFrame[v][,1], na.rm=TRUE), digits=1)
    sd <- round(sd(dataFrame[v][,1], na.rm=TRUE), digits=1)
    median <- round(median(dataFrame[v][,1], na.rm=TRUE), digits=1)
    p25 <- round(quantile(dataFrame[v][,1], 0.25, na.rm = T), digits=1)
    p75 <- round(quantile(dataFrame[v][,1], 0.75, na.rm = T), digits=1)
    iqr <- p75 - p25
    min <- round(quantile(dataFrame[v][,1], 0, na.rm = T), digits=1)
    max <- round(quantile(dataFrame[v][,1], 1, na.rm = T), digits=1)
    varNames_cont <- c(varNames_cont, v)
    Ns <- c(Ns, n)
    missings <- c(missings, missing)
    misrates <- c(misrates, misrate)
    means <- c(means, mean)
    sds <- c(sds, sd)
    medians <- c(medians, median)
    p25s <- c(p25s, p25)
    p75s <- c(p75s, p75)
    iqrs <- c(iqrs, iqr)
    mins <- c(mins, min)
    maxs <- c(maxs, max)    
    
  }
  covas_cat <- data.frame(varNames_cat, ncats, percents)
  covars_cont <- data.frame(varNames_cont, Ns, missings, misrates, means, sds, medians, p25s, p75s, iqrs, mins, maxs)
  return(list(covas_cat, covars_cont))
}
mkDescriptiveTable_imp <- function(contVars, catVars, dataFrame) {
  varNames_cat <- c()
  varNames_cont <- c()
  Ns  <- c()
  means <- c()
  sds <- c()
  medians <- c()
  p25s <- c()
  p75s <- c()
  iqrs <- c()
  mins <- c()
  maxs <- c()
  ncats <- c()
  percents <- c()
  roundDig <- 3
  for (v in catVars) {
    tableInfo <- table(dataFrame[v])
    propInfo <- prop.table(table(dataFrame[v]))*100
    groups <- length(propInfo)
    for (c in 1:groups) {
      ncat <- ''
      ncat <- tableInfo[c]
      percent <- ''
      percent <- round(propInfo[c], digits=2)
      varName <- paste(v, '_', labels(propInfo[c]), sep='')
      varNames_cat <- c(varNames_cat, varName)
      ncats <- c(ncats, ncat)
      percents <- c(percents, percent)
      
    }
    
  }
  for (v in contVars){
    n <- sum(!is.na(dataFrame[v]))
    mean <- round(mean(dataFrame[v][,1], na.rm=TRUE), digits=2)
    sd <- round(sd(dataFrame[v][,1], na.rm=TRUE), digits=2)
    median <- round(median(dataFrame[v][,1], na.rm=TRUE), digits=2)
    p25 <- round(quantile(dataFrame[v][,1], 0.25, na.rm = T), digits=2)
    p75 <- round(quantile(dataFrame[v][,1], 0.75, na.rm = T), digits=2)
    iqr <- p75 - p25
    min <- round(quantile(dataFrame[v][,1], 0, na.rm = T), digits=2)
    max <- round(quantile(dataFrame[v][,1], 1, na.rm = T), digits=2)
    varNames_cont <- c(varNames_cont, v)
    Ns <- c(Ns, n)
    means <- c(means, mean)
    sds <- c(sds, sd)
    medians <- c(medians, median)
    p25s <- c(p25s, p25)
    p75s <- c(p75s, p75)
    iqrs <- c(iqrs, iqr)
    mins <- c(mins, min)
    maxs <- c(maxs, max)    
    
  }
  covas_cat <- data.frame(varNames_cat, ncats, percents)
  covars_cont <- data.frame(varNames_cont, Ns,means, sds, medians, p25s, p75s, iqrs, mins, maxs)
  return(list(covas_cat, covars_cont))
}

drawhist   = function(data, fname, xlabel, xaxis = "", binwidth=0.5, width=6, height=6, do.svg=F, xlab) {
  # Compile graph title
  title = paste("Histogram of ", xaxis, " (N=", length(which(!is.na(xlabel))), ")", sep="")
  # Output png or svg name
  if(do.svg){
    svg(fname, width=width, height=height)
  }else{
    png(fname, width=width, height=height, units='in', res=300)}
  # Draw the historgram
  p = ggplot(data, aes(x=xlabel)) +
    scale_x_continuous(breaks = xlab) +
    geom_histogram(binwidth = binwidth, color="black", fill="white") +
    labs(title=title,x=xaxis, y ="Number of Subjects") +
    theme_classic()
  # Save
  print(p)
  dev.off()
}

gam_function <- function(DataFrame, yvars, xvars){
  for (i in seq_along(f_base)){ # for each pollutant
    for (y in yvars){
      f <- paste(y, f_base[i], "+", xvars_child[i])
      fmFull <- gam(as.formula(f), data=DataFrame, weights = DataFrame$weights_avg)
      par(mfrow = c(1,1))
      plot.Gam(fmFull, residuals=F, se=TRUE, col="red", cex=1, terms= paste0("ns(", xvars[i], ", df = 2)"), main = paste0(xvars[i], 'vs', y))
    }
  }
}

get_lme4_info_all_iterx_acute <- function(DataFrame, y, xvars){
  ys <- c()
  xs1 <- c()
  Est1 <- c()
  lower1 <- c()
  upper1 <- c()
  p <- c()
  fdr <- c()
  for (i in seq_along(f_base)){ # for each pollutant
    f <- paste(y,f_base[i], "+",xvars_child[i],"+" ,"(1 |idc)")
    fmFull <- with(DataFrame, lmer(as.formula(f), weights = weights_avg))
    fmFullSummary <- data.frame(summary(pool(fmFull))) 
    ys <- c(ys, y)
    x1 <- xvars[i]
    xs1 <- c(xs1, x1)
    estimate1 <- fmFullSummary[fmFullSummary$term==x1,"estimate"]
    Est1 <- c(Est1, estimate1)
    se1 <- fmFullSummary[fmFullSummary$term==x1,"std.error"]
    lower1 <- c(lower1,  estimate1-1.96*se1)
    upper1 <- c(upper1, estimate1+1.96*se1)
    p <- c(p, fmFullSummary[fmFullSummary$term==x1,"p.value"])
    #qqnorm(resid(fmFull), main = xvars[i])
  }
  fdr <- p.adjust(p, method = 'fdr')
  Est1<-sprintf("%0.3f",Est1)
  lower1<-sprintf("%0.3f",lower1)
  upper1<-sprintf("%0.3f",upper1)
  p<-sprintf("%0.3f",p)
  cis <- paste0(Est1, ' (', lower1, '; ', upper1, ')')
  final_results <- data.frame(ys, xs1, cis,p, fdr)
  return(final_results)
}


get_lme4_info_all_iterx_acute_nochr <- function(DataFrame, y, xvars){
  ys <- c()
  xs1 <- c()
  Est1 <- c()
  lower1 <- c()
  upper1 <- c()
  p <- c()
  fdr <- c()
  for (i in seq_along(f_base)){ # for each pollutant
    f <- paste(y,f_base[i], "+" ,"(1 |idc)")
    fmFull <- with(DataFrame, lmer(as.formula(f), weights = weights_avg))
    fmFullSummary <- data.frame(summary(pool(fmFull))) 
    ys <- c(ys, y)
    x1 <- xvars[i]
    xs1 <- c(xs1, x1)
    estimate1 <- fmFullSummary[fmFullSummary$term==x1,"estimate"]
    Est1 <- c(Est1, estimate1)
    se1 <- fmFullSummary[fmFullSummary$term==x1,"std.error"]
    lower1 <- c(lower1,  estimate1-1.96*se1)
    upper1 <- c(upper1, estimate1+1.96*se1)
    p <- c(p, fmFullSummary[fmFullSummary$term==x1,"p.value"])
    #qqnorm(resid(fmFull), main = xvars[i])
  }
  fdr <- p.adjust(p, method = 'fdr')
  Est1<-sprintf("%0.3f",Est1)
  lower1<-sprintf("%0.3f",lower1)
  upper1<-sprintf("%0.3f",upper1)
  p<-sprintf("%0.3f",p)
  cis <- paste0(Est1, ' (', lower1, '; ', upper1, ')')
  final_results <- data.frame(ys, xs1, cis,p, fdr)
  return(final_results)
}

linear_regression <- function(DataFrame, y, xvars){
  ys<-c()
  xs<-c()
  Est<-c()
  lower<-c()
  upper<-c()
  p<-c()
  for (i in seq_along(f_base)){
    f <- paste(y,f_base[i], "+",xvars_child[i])
    test_model <-with(mids2datlist(DataFrame), lm(as.formula(f), weights = weights_avg))
    betas <- lapply(test_model, coef)
    vars <- lapply(test_model, FUN = function(x){vcovHC(x, type = 'HC1')}) 
    model1 <- summary(pool_mi(betas, vars))
    ys <- c(ys, y)
    x <- xvars_week[i]
    xs <- c(xs, x)
    Est <- c(Est, model1$results[2])
    lower <- c(lower, model1$`(lower`[2])
    upper <- c(upper, model1$`upper)`[2])
    p <- c(p, model1$p[2])
  }
  Est1<-sprintf("%0.3f",Est)
  lower1<-sprintf("%0.3f",lower)
  upper1<-sprintf("%0.3f",upper)
  p<-sprintf("%0.3f",p)
  cis <- paste0(Est1, ' (', lower1, '; ', upper1, ')')
  final_results <- data.frame(ys, xs, cis,p)
  return(final_results)
}


linear_regression_nocr <- function(DataFrame, y, xvars){
  ys<-c()
  xs<-c()
  Est<-c()
  lower<-c()
  upper<-c()
  p<-c()
  for (i in seq_along(f_base)){
    f <- paste(y,f_base[i])
    test_model <-with(mids2datlist(DataFrame), lm(as.formula(f), weights = weights_avg))
    betas <- lapply(test_model, coef)
    vars <- lapply(test_model, FUN = function(x){vcovHC(x, type = 'HC1')}) 
    model1 <- summary(pool_mi(betas, vars))
    ys <- c(ys, y)
    x <- xvars_week[i]
    xs <- c(xs, x)
    Est <- c(Est, model1$results[2])
    lower <- c(lower, model1$`(lower`[2])
    upper <- c(upper, model1$`upper)`[2])
    p <- c(p, model1$p[2])
  }
  Est1<-sprintf("%0.3f",Est)
  lower1<-sprintf("%0.3f",lower)
  upper1<-sprintf("%0.3f",upper)
  p<-sprintf("%0.3f",p)
  cis <- paste0(Est1, ' (', lower1, '; ', upper1, ')')
  final_results <- data.frame(ys, xs, cis,p)
  return(final_results)
}




  