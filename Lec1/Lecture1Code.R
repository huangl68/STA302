
y <- c(48, 56, 58, 44, 46, 51)
x <- c(rep("A", 3), rep("B", 3))
dat0 <- data.frame(x=x, y=y)
with(dat0, tapply(y, x, mean))
a <- t(combn(y, 3, simplify=T))
b <- t(apply(a, 1, setdiff, x=y))
dd <- as.data.frame(cbind(a, round(apply(a, 1, mean), 2), b,
round(apply(b, 1, mean), 2), round(apply(a, 1, mean) - apply(b, 1,
mean), 2)))
names(dd)[c(4, 8, 9)] <- c("meanA", "meanB", "diff")
write.table(dd, file="PermTestEG.txt", sep="\t", eol="\n", row.names=F)
hist(dd$diff, breaks=seq(-10, 10, 1), main="Permutation Distribution",
xlab="Difference between sample means", ylab="Frequency")

vector <- sort(dd$diff)
vector




