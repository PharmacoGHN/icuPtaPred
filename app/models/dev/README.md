Admin-only development models

Set `ICUPTAPRED_ADMIN_MODE=true` before launching the app to load `.dcf` files from this folder.

Each `.dcf` file should contain one model definition with these fields:

```
drug: Cefepime
model: Example_2026
is_default: FALSE
dose_increment: 1
renal_metric: cg_tbw
renal_formula: Cockcroft-Gault (TBW)
clearance_expr: 4.2 * (renal_value / 100)^0.5
eta_cl_expr: get_sd_from_cv(0.25)
```

If a dev model uses the same `drug` and `model` as a built-in entry, the dev file overrides the built-in definition while admin mode is enabled.