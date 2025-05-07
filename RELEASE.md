# ✅ socmod Release Checklist

This checklist walks through everything needed to release new versions, e.g., 0.2 in examples below

---

## 1. Bump Version in DESCRIPTION

- [ ] Open `DESCRIPTION`
- [ ] Change `Version:` e.g. from `0.1` → `0.2`

---

## 2. Update NEWS.md (optional)

Add a new section at the top of `NEWS.md`:

```
# socmod 0.2

- First full vignette-based pipeline working
- Added `initialize_agents()`
- Quarto-compatible documentation with reproducible examples
- Pre-release script `check_dev_ready.R`
```

---

## 3. Commit Your Changes

```bash
git add DESCRIPTION NEWS.md vignettes/ R/ man/ tests/ inst/
git commit -m "v0.2 release: vignette pipeline, init_agents, docs ready"
```

---

## 4. Tag the Release

```bash
git tag -a v0.2 -m "v0.2 release: major documentation and modeling updates"
```

---

## 5. Push to GitHub

```bash
git push origin main
git push origin v0.2
```

---

## 6. Build the Site

In R:

```r
devtools::document()
pkgdown::build_site()
```

Then:

```bash
git add docs/
git commit -m "Update pkgdown site for v0.2"
git push
```

---

🎉 Done! You're ready for a clean public release.