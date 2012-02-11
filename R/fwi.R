fwi <-
function(date,Tm,H,r,W,return.all=FALSE) {
    date <- as.chron(date)
    Tm <- Tm 
    H <- H
    r <- r
    W <- W
    ret <- return.all
    c('date','Tm','H','r','W') -> varnames
    c() -> ind
    for (i in 1:length(varnames)) {
        get(varnames[i]) -> vec
        if (any(is.na(vec))) {
            which(is.na(vec)) -> a
            c(ind,a) -> ind
        }
    }
    if (length(ind) > 0) {
        warning('Some missing values were removed from the time series before computation')
        unique(ind) -> ind
        for (i in 1:length(varnames)) {
            assign(varnames[i], get(varnames[i])[-ind])
        }
    }
    mes <- as.integer(months(date))
    Le <- c(6.5,7.5,9,12.8,13.9,13.9,12.4,10.9,9.4,8.0,7,6.0) 
    dlf <- c(-1.6,-1.6,-1.6,0.9,3.8,5.8,6.4,5.0,2.4,0.4,-1.6,-1.6) 
    f0 <- c(85.0) 
    p0 <- c(6.0) 
    d0 <- c(15.0) 
    ISI <- rep(NA, length(mes))
    BUI <- rep(NA, length(mes))
    FWI <- rep(NA, length(mes))
    if (any(H > 100)) {
        warning('One or more values of humidity above 100% were corrected')
        H[which(H>100)] <- 100
    }
    if (any(H < 0)) {
        warning('Some negative values of humidity were corrected')
        H[which(H<0)] <- 0
    }
    if (any(r < 0)) {
        warning('Some negative values of precipitation were corrected')
        r[which(r<0)] <- 0
    }
    if (any(W < 0)) {
        warning('Some negative values of wind were corrected')
        W[which(W<0)] <- 0
    }
    for (i in 1:length(mes)) {
        m0 = (147.2 * (101 - f0[i])) / (59.5 + f0[i])
        if (r[i] > .5) {
            rA = r[i] - .5
            if (m0 <= 150) {
                mr = m0 + 42.5 * rA * exp(-100/(251 - m0)) * (1 - exp(-6.93/rA))
            } else {
                mr = m0 + 42.5 * rA * exp(-100/(251 - m0)) * (1 - exp(-6.93/rA)) + (0.0015 * (m0 - 150)^2 * (rA^(.5))) 
            }
            if (mr > 250) {
                mr = 250
            } 
                mr -> m0  
        }     
        Ed = 0.942 * H[i]^0.679 + 11 * exp((H[i]-100)/10) + .18 * (21.1 - Tm[i]) * (1 - (1/exp(0.115 * H[i])))
        Ew = 0.618 * H[i]^0.753 + 10 * exp((H[i]-100)/10) + .18 * (21.1 - Tm[i]) * (1 - (1/exp(0.115 * H[i])))
        if(m0 > Ed) {
            k0 = 0.424 * (1 - ((H[i]/100)^1.7)) + 0.0694 * (W[i]^0.5) * (1 - ((H[i]/100)^8))
            kd = k0 * 0.581 * exp(0.0365 * Tm[i])
            m = Ed + (m0 - Ed) * (10^(-kd)) 
        }
        if(m0 < Ed) {
            if(m0 < Ew) {
                k1 = 0.424*(1-((100-H[i])/100)^1.7) + 0.0694 * (W[i]^.5) * (1-((100-H[i])/100)^8)
                kw = k1 * (0.581 * (exp(0.0365 * Tm[i])))
                m = Ew - ((Ew - m0)*10^(-kw))
            }
        }
        if(Ed >= m0 & m0 >= Ew) {
            m <- m0  
        }
        if (m < 0) { 
        	m = 0
    	}
        f = 59.5 * (250 - m) / (147.2 + m)  
        c(f0, f) -> f0
        if (Tm[i] < -1.1) {
            Tm[i] = -1.1
        }
        K = 1.894 * (Tm[i] + 1.1) * (100 - H[i]) * Le[mes[i]] * 1e-06
        if (r[i] > 1.5) {
            re = (0.92 * r[i]) - 1.27
            M0 = 20 + exp(5.6348 - (p0[i]/43.43))
            if (p0[i] <= 33) {
                b = 100 / (0.5 + (0.3 * p0[i]))
            }
            if (p0[i] > 65) {
                b = (6.2 * log(p0[i])) - 17.2
            } 
            if (p0[i] > 33 & p0[i] <= 65) {
                b = 14 - 1.3 * log(p0[i])
            }
            Mr = M0 + ((1000 * re) / (48.77 + b * re))
            pr = 244.72 - 43.43 * log(Mr - 20)
            if (pr < 0) {
                pr = 0
            }
            p = pr + 100 * K
            c(p0, p) -> p0
        } else {
            p = p0[i] + 100 * K
            c(p0, p) -> p0            
        }
        if (Tm[i] < -2.8) {
            Tm[i] = -2.8
        }
        v = .36 * (Tm[i] + 2.8) + dlf[mes[i]]
        if (v < 0) {
            v = 0
        }
        if (r[i] > 2.8) {
            rd = 0.83 * r[i] - 1.27
            q0 = 800 * exp(-d0[i] / 400)
            qr = q0 + 3.937 * rd
            dr = 400 * log(800 / qr)
            if(dr < 0) {
                dr = 0
            }
            d = dr + .5 * v
            c(d0, d) -> d0
        } else { 
            d = d0[i] + .5 * v
            c(d0, d) -> d0
        }
        fW = exp(0.05039 * W[i])
        fF = 91.9 * exp(-0.1386 * m) * (1 + ((m^5.31) / (4.93 * 1e+07)))
        0.208 * fW * fF -> ISI[i]
        if (p <= .4 * d) {
            BUI[i] <- (.8 * p * d) / (p + .4 * d)
        }
        if (p > .4 * d) {
            BUI[i] <- p - (1 - (.8 * d) / (p + .4 * d)) * (.92 + (.0114 * p)^1.7)
        }
        if (is.finite(BUI[i]) == FALSE | BUI[i] < 0) {
            BUI[i] <- 0 
        }
        if (BUI[i] > 80) {
            fD = 1000 / (25 + 108.64 * exp(-.023 * BUI[i]))
        } else {
            fD = (.626 * BUI[i]^.809) + 2
        }
        B = .1 * ISI[i] * fD
    	if (B > 1) {
            Slog = 2.72 * (.434 * log(B))^.647       
            FWI[i] <- exp(Slog)
        } else {
            FWI[i] <- B
        }
    }
    if (ret == FALSE) {
        return(FWI)
    } else {
        DSR = .0272 * FWI^1.77
        cbind.data.frame('FFMC'=f0[-1], 'DMC'=p0[-1], 'DC'=d0[-1], 'ISI'=ISI, 'BUI'=BUI, 'FWI'=FWI, 'DSR'=DSR) -> fds
        return(fds) 
    }
}

