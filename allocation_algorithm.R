

# score function national

allocation_national <- function(recip_matrix, donor_matrix, HLA){

    HLAall = as.numeric(HLA$tx_misa) + as.numeric(HLA$tx_misb) + as.numeric(HLA$tx_misdr)

    changed = c(rep(0 , length(HLAall)))
    allo_score = c(rep(0, length(HLAall)))

    index <- HLAall == 0 & recip_matrix$recip_pra >= 50 & changed == 0
    allo_score[index] <- 60000000
    changed[index] <- 1

    index <-  HLAall == 1 & recip_matrix$recip_pra > 80 & changed  == 0
    allo_score[index] <- 59000000
    changed[index] <- 1



    index <- HLAall == 2 & recip_matrix$recip_pra > 80 & changed == 0
    allo_score[index] <- 58000000
    changed[index] <- 1


    index <- HLAall == 0 & recip_matrix$recip_pra < 50 & changed == 0
    allo_score[index] <- 57000000
    changed[index] <- 1

    index <- (HLA$tx_misdr == 0 & (HLA$tx_misa + HLA$tx_misb == 1) &
                  recip_matrix$recip_pra <= 80 & changed == 0)
    allo_score[index] <-  56000000
    changed[index] <- 1


    index <- (HLA$tx_misdr == 0 & (HLA$tx_misa + HLA$tx_misb) == 2  &
                  recip_matrix$recip_pra <= 80 & changed == 0)
    allo_score[index] <-  55000000
    changed[index] <- 1


    index <- changed == 0
    #allo_score[index] <- 54000000
    allo_score[index] <- 0
    changed[index] <- 1



    index <- recip_matrix$recip_age < 18
    allo_score[index]  = allo_score[index] + 30000

    waiting_time <- as.numeric(donor_matrix$tx_date - recip_matrix$recip_rrtstartdate)/365*12

    #index <- recip_matrix$recip_waittime/30 > 0
    #allo_score[index] = allo_score[index] + recip_matrix$recip_waittime[index]*1

    index <- waiting_time > 0
    allo_score[index] = allo_score[index] + waiting_time[index]*1

    return(allo_score)
}

# length(allocation_national(rawdata,rawdata,HLAmatchCalMat(rawdata,rawdata)))
#
# original_score <- allocation_national(rawdata,rawdata,HLAmatchCalMat(rawdata,rawdata))
# table(original_score < 54000000)
#


# scroe function ped

