p <- fork()
if (inherits(p, "masterProcess")) {
cat("I'm a child! ", Sys.getpid(), "\n")
exit(,"I was a child")
}
cat("I'm the master\n")
unserialize(readChildren(1.5))


mclapply(1:30, rnorm)
# use the same random numbers for all values
set.seed(1)
mclapply(1:30, rnorm, mc.preschedule=FALSE, mc.set.seed=FALSE)


p <- parallel(1:10)
q <- parallel(1:20)
collect(list(p, q))


p <- parallel(1:10)
collect(p, wait=FALSE, 10) # will retrieve the result (since it's fast)
collect(p, wait=FALSE) # will signal the job as terminating
collect(p, wait=FALSE) # there is no such job

jobs <- lapply(1:10, function(x) parallel(rnorm(x), name=x))
collect(jobs)


multicore:::detectCores(all.tests=TRUE)

fork()

multicore::parallel()
parallel(correction)
