lambda1 = 4
lambda2 = 12
U = runif(1)
if (U <= 3/4)
{
  X = rexp(1, lambda2)
} else
{
  X = rexp(1, lambda1)
}
expectation = 1/X
print(cat("Estimated expectation of X", expectation, "\n"))
