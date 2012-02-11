calibrateProj <-
function(obs, pred, sim, method = c('qqadj','qqmap','bias'), varcode=c('tas','hurs','pr','wss'), return.par = TRUE) {
    obs <- sort(obs)
    pred <- sort(pred)
    sort(sim, index.return=TRUE)$ix -> ix
    sim <- sort(sim)
    varcode <- match.arg(varcode, c('tas','hurs','pr','wss'))
    method <- match.arg(method, c('qqadj','qqmap','bias'))
    return.par <- return.par
    sim-pred -> delta_i  
    delta_i - mean(delta_i) -> delta 
    if(method == 'bias') {
        g = 1
        f = 0
    }
    if(method == 'qqmap') {
        g = 1
        f = 1
    }
    if (method == 'qqadj') {
        if (varcode == 'tas'){    
            g = 1
        } else {
            mean(obs)/mean(pred) -> g 
        }
        if (varcode == 'pr') {
            (quantile(obs, .9, names=FALSE) - quantile(obs, .1, names=FALSE)) / (quantile(pred, .9, names=FALSE) - quantile(pred, .1, names=FALSE)) -> f #Eq.6a
        } else {
            (quantile(obs, .75, names=FALSE) - quantile(obs, .25, names=FALSE)) / (quantile(pred, .75, names=FALSE) - quantile(pred, .25, names=FALSE)) -> f #Eq.6b
        }
    }
    rep(NA,length(obs)) -> prj
    for (k in 1:length(obs)) {
        obs[k] + g*mean(delta_i) + f*delta[k] -> prj[k] 
    }
    if (varcode == 'pr') {
        if (length(which(pred == 0)) > 0) {
            length(which(sim == 0))*length(which(obs == 0)) / length(which(pred == 0)) -> nzp 
            prj[1:nzp] <- 0
        }
    }
    cbind(ix,prj) -> prix
    prix[order(prix[ ,1]),2] -> prj
    if (return.par == TRUE) {
        list('corrvals'=prj, 'g'=g, 'f'=f,'Mean_delta'=mean(delta_i)) -> prj
    } 
    return(prj)
}

