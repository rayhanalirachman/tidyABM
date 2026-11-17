# median real and CPU seconds per benchmark and side, from a pairs.log
x <- readLines(commandArgs(TRUE)[1]); x <- grep("cpu", x, value = TRUE)
d <- data.frame(b = sub(" .*", "", x), v = sub("^[a-z]+ ([AB]) .*", "\\1", x),
                real = as.numeric(sub(".*real ([0-9.]+).*", "\\1", x)), cpu = as.numeric(sub(".*cpu ([0-9.]+).*", "\\1", x)))
print(aggregate(cbind(real, cpu) ~ b + v, d, median))
