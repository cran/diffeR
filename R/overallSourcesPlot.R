overallSourcesPlot <- function(comp = NULL, ref = NULL, ctmatrix = NULL, analysis = "error", units = NULL, population = NULL, 
                               fontSize = NULL, colorValues = NULL, breaks = waiver(), labels = waiver(), limits = NULL){
  
  ylab <- ifelse(is.null(ctmatrix), "Difference Size (percentage of domain)", 
                 ifelse(is.null(units), "Difference Size (units)", paste0("Difference Size (", units, ")")))
  
  if(!is.null(comp) & !is.null(ref)){
    ctmatrix <- crosstabm(comp, ref, percent = TRUE, population = NULL)
  }
  
  if(!is.null(ctmatrix)){
    if(!is(ctmatrix, "matrix")) stop("ctmatrix must be of class matrix")
    if(!is.null(population)) ctmatrix <- sample2pop(ctmatrix, population)
    resT <- data.frame(Comission = comissionj(ctmatrix), Omission = omissionj(ctmatrix), Category = colnames(ctmatrix), 
                       stringsAsFactors = FALSE)
    if(analysis == "change") colnames(resT)[1:2] <- c("Loss", "Gain")
    rownames(resT) <- NULL
  }
  
  resT2 <- pivot_longer(resT, cols = 1:2, names_to = "differenceType", values_to = "value")
  
  # keep aes_() for passing CRAN checks
  g <- ggplot(data = resT2, 
              aes(x = .data$differenceType, y = .data$value, fill = .data$Category)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Type of Difference", y = ylab, fill = '') + 
    theme_classic() + 
    scale_y_continuous(expand = c(0, 0), breaks = breaks, labels = labels, limits = limits) +
    theme(text = element_text(size = fontSize))
  if(!is.null(colorValues)) g <-  g + scale_fill_manual(values = colorValues)
  return(g)
}
