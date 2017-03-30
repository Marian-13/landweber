# TODO

#### 1. Impementation tasks
1. Plots?
2. map, reduce (for is slow )
3. Wrap R default functions like `log`, `exp`, `%*%` etc...
4. Styleguide? [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml)
5. use(.GlobalEnv, attach = TRUE) alternative?
6. Nested modules imitation
7. refactor w_tilde !!!!!!!!!!!!!!!!!!!
8. Remove stubs from ExampleSpecificFunctions$f_1, IterativeProcedure$calculate_f_1 !
9. Write own map which returns matrix to avoid unlist
10. More than 6 params! Long names

#### 2. Algorithm issues
1. Square of vector
2. second integral of u (3.11)
3. partial normal derivative of v with respect to x (3.10)

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
