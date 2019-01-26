# autoSim.R

nVals = seq(100, 500, by = 100)
distTypes = c("gaussian", "t1", "t5")
#output two MSEs
 for (dist in distTypes){
    for (n in nVals) {
    oFile <- paste("n", n, "dist", dist, ".txt", sep="")
    sysCall <- paste("nohup Rscript runSim.R n=", n, " dist=",
                     shQuote(shQuote(dist)), " seed=", 280, " reps=", 50,
                     " > ", oFile, sep="")
    system(sysCall)
    print(paste("sysCall=", sysCall, sep=""))
  }
 }

