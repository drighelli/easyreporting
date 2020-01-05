
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
