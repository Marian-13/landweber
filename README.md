# TODO

1. Plots?
2. map, reduce (for is slow)
3. Reduce anonymous functions?
4. Wrap R default functions like log, exp etc...
5. Make function names verbs? [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml)
6. Square of vector

# Note
1. magrittr pipe operator(`%>%`) behaviour
```
x %>% f(y, .) is equivalent to f(y, x)
x %>% f(y, z = .) is equivalent to f(y, z = x)
x %>% f(y = nrow(.), z = ncol(.)) is equivalent to f(x, y = nrow(x), z = ncol(x))
x %>% {f(y = nrow(.), z = ncol(.))} is equivalent to f(y = nrow(x), z = ncol(x))
```
