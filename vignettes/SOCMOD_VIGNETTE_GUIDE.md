# socmod Vignette Guide

## 🔒 Rule for Vignette Figures

> **Save plots directly in the `vignettes/` directory and include them using relative paths.**

This avoids path issues during development, Quarto rendering, and `pkgdown::build_site()`.

---

## ✅ How to Save and Include a Plot

### Saving (in code chunks):

```r
ggsave("vignettes/example1.png", p, width = 5.75, height = 3)
```

### Including (in .qmd):

Use normal image inclusion syntax, i.e., `![](example1.png)`

---

## 🔍 Why this works

- Paths are relative to the vignette itself
- No `here::here()` needed
- Works inside `pkgdown`'s sandboxed rendering environment
- Ensures the image is copied correctly to `docs/articles/`

---

## 🔁 Alternatives we tried and rejected

- Using `fig.path`: broke image includes during pkgdown build
- Using `here::here()` in `include_graphics()`: fails in subprocess
- Saving to `vignettes/resources/`: fragile and inconsistent

---

## 🧠 Best Practice

If a plot is:
- Specific to one vignette
- Not reused or versioned separately

➡ Save it next to the `.qmd`.

---

Last updated for `socmod` v0.2