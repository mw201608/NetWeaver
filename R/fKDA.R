#
#Network key driver analysis
#
#Author: Minghui Wang
#
getNeigobhors = function(node, net, nLayer = 1, collapse = TRUE, directed = FALSE){
    result = expandToNeighbors(seed = node, net = net, nLayersToExpand = nLayer, return.individual.layer = ! collapse, directed = directed)
    result
}
expandToNeighbors = function(seed, net, nLayersToExpand = 3, return.individual.layer = FALSE, directed = FALSE){
    result = list()
    for(i in 1:nLayersToExpand){
        if(directed){
            net1 = net[net[, 1] %in% seed, , drop = FALSE]
        }else{
            net1 = net[net[, 1] %in% seed | net[, 2] %in% seed, , drop = FALSE]
        }
        if(nrow(net1) == 0) break
        seed1 = union(net1[, 1], net1[, 2])
        if(length(setdiff(seed1, seed)) == 0) break
        result[[i]] = seed1
        seed = seed1
    }
    if(length(result) == 0) return(NULL)
    if(return.individual.layer) return(result)
    result = result[[length(result)]]
    result
}
predictKeyDrivers = function(net, signature = NULL, nLayerToTest = 3, nLayersToExpand = 0, bg.size = NULL, directed = TRUE, reduce.within.nlayer = 2, fdr = 0.05, p.correction.method = 'BH', return.overlap = FALSE){
    #Input:
    #net, data.frame with two columns of genes, representing edges in the network
    #signature, a vector of gene names representing the signature genes
    #nLayerToTest, the maximum distance between a gene under test (i.e., a candidate key driver) and any of the signature members.
    #nLayersToExpand, to expand the seeding signature on the network as the new signature
    #bg.size, background size for enrichment test, default to the total number of genes in the network
    #directed, whether the network is directed
    #reduce.within.nlayer, prioritize global key drivers if predicted key drivers are closedly connected within nlayer
    #fdr, FDR cutoff to call significant key drivers
    #p.correction.method, method for multiple testing correction
    #return.overlap, whether to return the overlapping signature members for each predicted key driver
    #
    #Output: a data.frame of predicted key drivers with statistics
    #
    stopifnot(nLayerToTest >= 1)
    if(is.null(signature) || setequal(signature, union(net[, 1], net[, 2]))) return(hubByConn(net = net, nLayerToTest = nLayerToTest, directed = directed, fdr = fdr, p.correction.method = p.correction.method))
    cat('Call key drivers by testing for signature enrichment...\n')
    expandedSigs = signature
    expandedSigs = expandedSigs[expandedSigs %in% union(net[,1 ], net[, 2])]
    if(length(expandedSigs) == 0) {
        cat("None of the signature gene is present in the network\n")
        return(NULL)
    }
    if(is.null(bg.size)) bg.size = length(union(net[, 1], net[, 2]))
    if(nLayerToTest > nLayersToExpand){
        targets = expandToNeighbors(seed = expandedSigs, net = net, nLayersToExpand = nLayerToTest, return.individual.layer = TRUE, directed = FALSE)
        if(nLayersToExpand > 0) expandedSigs = targets[[min(length(targets), nLayersToExpand)]]
        targets = targets[[length(targets)]]
    }else{
        if(nLayersToExpand > 0) expandedSigs = expandToNeighbors(seed = expandedSigs, net = net, nLayersToExpand = nLayerToTest, return.individual.layer = TRUE, directed = FALSE)
        if(nLayersToExpand == 0) targets = expandedSigs
        if(nLayerToTest > 0) targets = expandedSigs[[min(length(expandedSigs), nLayerToTest)]]
        expandedSigs = expandedSigs[[length(expandedSigs)]]
    }
    if(length(targets) == 0) {
        cat("No candidate key driver found at nLayerToTest =", nLayerToTest, "\n")
        return(NULL)
    }
    cat('Testing', length(targets), 'candidates...\n')
    result = do.call(rbind, lapply(targets, function(x, net, signature, nLayerToTest, M, directed, return.overlap = FALSE){
        neighbors = getNeigobhors(node = x, net = net, nLayer = nLayerToTest, collapse = FALSE, directed = directed)
        if(is.null(neighbors)) return(NULL)
        tab = do.call(rbind, lapply(1:length(neighbors), function(i, neighbors, signature, return.overlap) {
            overlap = neighbors[[i]][neighbors[[i]] %in% signature]
            n1 = length(overlap)
            if(return.overlap == FALSE) return(cbind(BestLayer = i, q = n1, m = length(neighbors[[i]])))
            data.frame(BestLayer = i, q = n1, m = length(neighbors[[i]]), Items = paste(overlap, collapse = ';'), stringsAsFactors = FALSE)
        }, neighbors = neighbors, signature = signature, return.overlap = return.overlap))
        tab = cbind(tab, n = M - tab[, 'm'], k = length(signature))
        tab = cbind(tab, FE = round(tab[, 'q'] * M / tab[, 'm'] / tab[, 'k'], 2))
        tab = cbind(tab, log.P.Value = apply(as.matrix(tab[, ! colnames(tab) %in% c('BestLayer', 'Items'), drop = FALSE]), 1, function(x){
            phyper(max(0, x[1] - 1), x[2], x[3], x[4], lower.tail = FALSE, log.p = TRUE)
        }))
        data.frame(Keydriver = x, tab, stringsAsFactors = FALSE)
    }, net = net, signature = expandedSigs, nLayerToTest = nLayerToTest, M = bg.size, directed = directed, return.overlap = return.overlap))
    result = result[order(result$log.P.Value), ]
    result = result[!duplicated(result$Keydriver), ]
    result$adj.P.Value = p.adjust(exp(result$log.P.Value), method = p.correction.method)
    result = result[result$adj.P.Value <= fdr, ]
    #
    if(nrow(result) == 0) {
        cat('No significant key driver predicted at FDR', fdr, '\n')
        return(NULL)
    }
    cat('Predicted', nrow(result), 'potential key drivers.\n')
    if(nrow(result) == 1){
        if(directed) result$is.root.node = ! result$Keydriver %in% net[, 2]
        result$global.Keydriver = TRUE
        if(return.overlap){
            result$Overlap.Items = result$Items
            result$Items = NULL
        }
        return(result)
    }
    cat('Working to predict global key drivers...\n')
    connMat = sapply(result$Keydriver, function(x, genes,  reduce.within.nlayer){
        ns = getNeigobhors(node = x, net = net, nLayer = reduce.within.nlayer, collapse = TRUE, directed = directed)
        genes %in% ns
    }, genes = result$Keydriver, reduce.within.nlayer = reduce.within.nlayer)
    rownames(connMat) = colnames(connMat)
    diag(connMat) = FALSE
    result$is.signature = result$Keydriver %in% signature
    if(directed){
        result$is.root.node = ! result$Keydriver %in% net[, 2]
        result$global.Keydriver = rowSums(connMat) == 0
    } else {
        result$global.Keydriver = TRUE
        for(i in 1:length(result$Keydriver)){
            j = which(connMat[i, ])
            if(length(j) == 0) next
            if(any(result[j, 'm'] > result[i, 'm'])){
                result$global.Keydriver[i] = FALSE
                next
            }
            k = j[which(result[j, 'm'] == result[i, 'm'])]
            if(length(k) == 0) next
            l = k[which(result[k, 'q'] > result[i, 'q'])]
            if(length(l) > 0) {
                result$global.Keydriver[i] = FALSE
                next
            }
            k = k[which(result[k, 'BestLayer'] < result[i, 'BestLayer'])]
            if(length(k) > 0)  result$global.Keydriver[i] = FALSE
        }
    }
    #
    rownames(result) = NULL
    if(return.overlap){
        result$Overlap.Items = result$Items
        result$Items = NULL
    }
    result
}
hubByConn = function(net, nLayerToTest = 3, directed = FALSE, reduce.within.nlayer = 2, fdr = 0.05, p.correction.method = 'BH', return.overlap = FALSE){
    cat('Perform hub gene analysis...\n')
    targets = union(net[, 1], net[, 2])
    cat('Testing', length(targets), 'candidates...\n')
    result = do.call(rbind, lapply(targets, function(x, net, nLayerToTest, directed, return.overlap = FALSE){
        neighbors = getNeigobhors(node = x, net = net, nLayer = nLayerToTest, collapse = TRUE, directed = directed)
        if(is.null(neighbors)) return(NULL)
        a1 = data.frame(Keydriver = x, n = length(neighbors), stringsAsFactors = FALSE)
        if(return.overlap) a1$Items = paste(neighbors, collapse = ';')
        a1
    }, net = net, nLayerToTest = nLayerToTest, directed = directed, return.overlap = return.overlap))
    result$FE = round(result$n / mean(result$n, na.rm = TRUE), 2)
    result$Z = (result$n - mean(result$n, na.rm = TRUE)) / sd(result$n, na.rm = TRUE)
    result$log.P.Value = pnorm(result$Z, lower.tail = FALSE, log.p = TRUE)
    result = result[order(result$log.P.Value), ]
    result$adj.P.Value = p.adjust(exp(result$log.P.Value), method = p.correction.method)
    result = result[result$adj.P.Value <= fdr, ]
    #
    if(nrow(result) == 0) {
        cat('No significant key driver predicted at FDR', fdr, '\n')
        return(NULL)
    }
    cat('Predicted', nrow(result), 'potential key drivers.\n')
    if(nrow(result) == 1){
        if(directed) result$is.root.node = ! result$Keydriver %in% net[, 2]
        result$global.Keydriver = TRUE
        if(return.overlap){
            result$Overlap.Items = result$Items
            result$Items = NULL
        }
        return(result)
    }
    cat('Working to predict global key drivers...\n')
    connMat = sapply(result$Keydriver, function(x, genes,  reduce.within.nlayer){
        ns = getNeigobhors(node = x, net = net, nLayer = reduce.within.nlayer, collapse = TRUE, directed = directed)
        genes %in% ns
    }, genes = result$Keydriver, reduce.within.nlayer = reduce.within.nlayer)
    rownames(connMat) = colnames(connMat)
    diag(connMat) = FALSE
    if(directed){
        result$is.root.node = ! result$Keydriver %in% net[, 2]
        result$global.Keydriver = rowSums(connMat) == 0
    } else {
        result$global.Keydriver = TRUE
        for(i in 1:length(result$Keydriver)){
            j = which(connMat[i, ])
            if(length(j) == 0) next
            if(any(result[j, 'n'] > result[i, 'n'])){
                result$global.Keydriver[i] = FALSE
                next
            }
        }
    }
    #
    rownames(result) = NULL
    if(return.overlap){
        result$Overlap.Items = result$Items
        result$Items = NULL
    }
    result
}
call_key_drivers = function(net, signature.df = NULL, nLayerToTest = 3, nLayersToExpand = 0, bg.size = NULL, directed = TRUE, reduce.within.nlayer = 2, fdr = 0.05, p.correction.method = 'BH', return.overlap = FALSE){
    #net, data.frame with two columns of genes, representing edges in the network
    #signature.df, data.frame/matrix with two columns of Var and Group, representing the signature genes and their group labels for different signatures
    if(is.null(signature.df)) return(predictKeyDrivers(net = net, signature = NULL, nLayerToTest = nLayerToTest, nLayersToExpand = nLayersToExpand, bg.size = bg.size, directed = directed, reduce.within.nlayer = reduce.within.nlayer, fdr = fdr, p.correction.method = p.correction.method, return.overlap = return.overlap))
    if((is.data.frame(signature.df) || is.matrix(signature.df)) && ncol(signature.df) >=2){
        result = NULL
    }else{
        stop("signature.df must be a data.frame/matrix with two columns")
    }
    for(g1 in unique(signature.df[, 2])){
        cat('Predicting key drivers for signature:', g1, '\n')
        var1 = signature.df[signature.df[, 2] == g1, 1]
        kds = predictKeyDrivers(net = net, signature = var1, nLayerToTest = nLayerToTest, nLayersToExpand = nLayersToExpand, bg.size = bg.size, directed = directed, reduce.within.nlayer = reduce.within.nlayer, fdr = fdr, p.correction.method = p.correction.method, return.overlap = return.overlap)
        if(!is.null(kds)) result = rbind(result, data.frame(Signature = g1, kds, stringsAsFactors = FALSE))
    }
    result
}
