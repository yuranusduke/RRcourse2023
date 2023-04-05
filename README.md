# RRcourse2023

## Finished Homeworks

### 1. Cleaning code

Date: 2023/03/29

- [x] Python version
- [x] R version

Bug problem:
In R, we should avoid problem in computing variance in 
`wtd.var` function. Default `method` is `unbiased`, we should change argument `method = ML` to compute variance in a biased way since we do it the same in Python, so in order to keep results reproducible in both Python and R, we should change this argument.
