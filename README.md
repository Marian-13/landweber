# TODO

1. Plots?
2. map, reduce (for is slow )
3. Wrap R default functions like `log`, `exp`, `%*%` etc...
4. Styleguide? [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml)
5. Square of vector
6. use(.GlobalEnv, attach = TRUE) ???
7. Nested modules imitation
8. Parametrization, Discretization => IndirectIntegralEquation


# Note
1. In R Programming Language INDEXING starts FROM ONE!
2. R do not support passing by reference.
3. Map returns LIST OF LISTS!!!!!!!!!!!!!!! list + vector = error [Future versions may allow some control of the result type](https://stat.ethz.ch/R-manual/R-devel/library/base/html/funprog.html)
4. [Difference between <- and <<- operators](https://stat.ethz.ch/R-manual/R-devel/library/base/html/assignOps.html)
5. magrittr pipe operator(`%>%`) behaviour:
```
x %>% f(y, .) is equivalent to f(y, x)
x %>% f(y, z = .) is equivalent to f(y, z = x)
x %>% f(y = nrow(.), z = ncol(.)) is equivalent to f(x, y = nrow(x), z = ncol(x))
x %>% {f(y = nrow(.), z = ncol(.))} is equivalent to f(y = nrow(x), z = ncol(x))
```
