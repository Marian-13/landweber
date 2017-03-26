# TODO

#### 1. Impementation tasks
1.1 Plots?
1.2 map, reduce (for is slow )
1.3 Wrap R default functions like `log`, `exp`, `%*%` etc...
1.4 Styleguide? [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml)
1.5 use(.GlobalEnv, attach = TRUE) alternative?
1.6 Nested modules imitation
1.7 Remove unlist
1.8 refactor w_tilde !!!!!!!!!!!!!!!!!!!
1.9 Remove stubs from ExampleSpecificFunctions$f_1, IterativeProcedure$calculate_f_1 !

#### 2. Algorithm issues
2.1 Square of vector
2.2 second integral of u (3.11)
2.3 partial normal derivative of v with respect to x (3.10)

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
