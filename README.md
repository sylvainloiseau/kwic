## kwic : Print Textual Corpora as Key Word in Context (aka concordance)

**Author:** Sylvain Loiseau<br/>
**License:** [BSD_3_clause](https://opensource.org/licenses/BSD-3-Clause)

# Installation

```{r}
devtools::install_github("sylvainloiseau/kwic")
```

# Usage

Basic :

```{r}
data(dickensl)
kwic(dickensl, "the")
```

Controling the output:

```{r}
data(dickensl)
res <- kwic(dickensl, "the")
print(res, left=10, right=10, from=3, to=4, sort.by="left")
```
