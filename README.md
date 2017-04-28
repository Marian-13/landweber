# Note
0. Styleguide [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml)
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
6. `use(.GlobalEnv, attach = TRUE)` !!!
6. Currying example:
```
curryied_func <- function(a) {
  function(b) {
    function(c) {
      function(d) {
        a * b * c * d
      }
    }
  }
}

curryied_func(1)(2)(3)(4)

curryied_func(a = 1)(
              b = 2)(
              c = 3)(
              d = 4)

curryied_func_result <- curryied_func(1)
curryied_func_result <- curryied_func_result(2)
curryied_func_result <- curryied_func_result(3)
curryied_func_result <- curryied_func_result(4)
```

7. How to implement smth like following:
```
for (i in row_indices) {
  for (j in 1:(i - 1)) {
    result[i, j] <- first_func(i, j)
  }

  result[i, i] <- second_func(i, i)

  for (j in (i + 1):column_size) {
    result[i, j] <- third_func(i, j)
  }
}
```
