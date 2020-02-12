# applyNOISeqEx <- function(counts, conditions, runs, lcontrasts, p.thr=1)
# {
#     factors <- cbind(conditions, runs)
#     
#     res <- lapply(lcontrasts, function(c)
#     {
#         
#         facts <- strsplit(x=c, split="-")[[1]]
#         facts <- gsub(pattern=" ", replacement="", facts)
#         cfactors <- subset(x=factors, conditions %in% facts)
#         ccounts <- counts[, cfactors[,"runs"]]
#         nset <- NOISeq::readData(data=ccounts, factors=cfactors)
#         cres <- NOISeq::noiseq(nset, norm="n", factor="conditions", 
#                     replicates="technical")
#         cres <- cres@results[[1]]
#         cres <- subset(cres, prob <= p.thr)
#     })
#     names(res) <- lcontrasts
#     return(res)
# }
#     
applyEdgeRExample <- function(counts, samples, contrast="Pleura - Broth")
{
    facts <- strsplit(x=contrast, split="-")[[1]]
    facts <- gsub(pattern=" ", replacement="", facts)
    sampIdxList <- lapply(facts, grep, samples)
    trt <- factor(c(rep(facts[1], length(sampIdxList[[1]])),
                    rep(facts[2], length(sampIdxList[[2]]))))
    trt <- trt[c(sampIdxList[[1]], sampIdxList[[2]])]
    design <- model.matrix(~0 + trt)
    colnames(design) <- gsub("trt", "", colnames(design))
    y <- edgeR::DGEList(counts=counts, group=trt)
    y <- edgeR::estimateDisp(y, design)
    fit <- edgeR::glmQLFit(y, design, robust=TRUE)
    con <- limma::makeContrasts(contrasts=contrast, levels=design)
    qlf <- edgeR::glmQLFTest(fit, contrast=con)
    res <- edgeR::topTags(qlf, n=Inf, p.value=0.05)
    return(list("test"=qlf,"results"=res))
}

applyEdgeREx <- function(counts, factors, contrasts=NULL,
                        p.threshold=0.05)
{
    design <- model.matrix(~ 1 + factors)
    colnames(design) <- c(as.character(unique(factors)))
    fit <- applyEdgeRFitEx(counts=counts, factors=factors, design=design)
    resClist <- lapply(contrasts, function(c)
    {
        resC <- applyEdgeRContrastEx(contrast=c, design=design,
                                   fit=fit, p.threshold=p.threshold)
    })
    names(resClist) <- contrasts
    return(resClist)
}


applyEdgeRFitEx <- function(counts, factors, design)
{
    dgel <- edgeR::DGEList(counts=counts, group=factors)
    edisp <- edgeR::estimateDisp(y=dgel, design=design)
    fit <- edgeR::glmQLFit(edisp, design, robust=TRUE)
    return(fit)
}

applyEdgeRContrastEx <- function(contrast, design, fit, p.threshold=1)
{
    contr <- limma::makeContrasts(contrasts=contrast, levels=design)
    qlf <- edgeR::glmQLFTest(fit, contrast=contr)
    res <- edgeR::topTags(qlf, n=Inf, p.value=p.threshold)
    all.gen.res <- res$table
    return(all.gen.res)
}
