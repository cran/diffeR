crosstabm <- 
function (comp, ref, percent = FALSE) 
{
    cr1 <- crosstab(comp, ref)
    if (any(is.na(cr1[, 1]))) {
        cr1 <- cr1[-which(is.na(cr1[, 1])), ]
    }
    if (any(is.na(cr1[, 2]))) {
        cr1 <- cr1[-which(is.na(cr1[, 2])), ]
    }
    uniquecr1 <- unique(c(levels(cr1[, 1]), levels(cr1[, 2])))
    dfcr1 <- matrix(0, nrow = length(uniquecr1), ncol = length(uniquecr1))
    colnames(dfcr1) <- uniquecr1
    rownames(dfcr1) <- uniquecr1
    for (i in 1:nrow(cr1)) {
        xi <- which(rownames(dfcr1) == cr1[i,1])
        ji <- which(colnames(dfcr1) == cr1[i,2])
        dfcr1[xi, ji] <- as.numeric(cr1[i, 3])
    }
    if (percent) 
        dfcr1 <- dfcr1/sum(dfcr1) * 100
    return(dfcr1)
}
