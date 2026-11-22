# TeXmacs UI Benchmark: Chez Scheme vs. s7 Scheme (10-Run Statistical Suite)

**Date**: 2026-11-19 (updated from 2026-11-05)  
**Platform**: Apple M2 Virtual Machine (macOS arm64), Qt 6.12.0  
**Benchmark Script**: [`docs/chez/bench/ui.scm`](bench/ui.scm)\
**Execution Environment**:

```bash
export QT_QPA_PLATFORM=offscreen
export SCHEMEHEAPDIRS="/opt/homebrew/Cellar/chezscheme/10.4.1/lib/csv10.4.1/tarm64osx"
export HOME="$PWD/fakehome"
export TEXMACS_PATH="$PWD/TeXmacs"
export TEXMACS_BOOT_PATH="$PWD/build-texmacos/boot"
export TEXMACS_DOC_PATH="$PWD/TeXmacs/doc"
./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs -x '(load "docs/chez/bench/ui.scm")'
```

---

## 0. Latest Run: `2026-11-19` (Commit `9a5789cd2` / `448c81e8b` — Auto-Imported R6RS `chez-compat` & `abbrevs`)

*(10 independent process invocations per suite, `QT_QPA_PLATFORM=offscreen`, `texmacs.boot` VFASL + 453 compiled modules; load average `15.19` → `2.08`. Sample variance $\sigma^2$ and stdev use $N - 1$. Raw JSON in [§5.G](#g-chez-scheme-1041-10-run-raw-dataset-2026-11-19-commit-9a5789cd2).)*

### UI Suite (`docs/chez/bench/ui.scm`, 10 Runs)

| Benchmark Task | Chez `9a5789cd2` Warm Mean ± Stdev (ms) | Warm $\sigma^2$ | Cold Mean (ms) | Chez `e0206bc65` Warm | Femtolisp Warm | s7-11.9 Warm |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| **`menus-text`** | **67.9 ± 1.20** *(66–70)* | 1.43 | **595.2** | 89.4 ms | 92.4 ± 5.1 ms | 102.0 ± 10.8 ms |
| **`menus-math`** | **50.2 ± 2.74** *(48–56)* | 7.51 | **61.0** | 63.3 ms | 74.3 ± 2.9 ms | 80.8 ± 7.2 ms |
| **`typing-text-520-keys`** | **939.4 ± 7.57** *(~1.81 ms/key, 928–949)* | 57.38 | **931.4** | 1,018.7 ms | 1,059.4 ± 35.4 ms | 1,104.6 ± 53.4 ms |
| **`typing-math-270-keys`** | **960.8 ± 10.09** *(~3.56 ms/key, 950–981)* | 101.73 | **1,289.1** | 997.6 ms | 1,047.8 ± 46.4 ms | 1,067.5 ± 35.7 ms |
| **`open-change-log`** | **169.6 ± 6.72** *(164–188)* | 45.16 | **1,017.5** | 167.8 ms | 188.0 ± 12.7 ms | 215.5 ± 12.4 ms |

### Manual Regeneration (`docs/chez/bench/manual.scm`, 10 Runs, 124 Pages)

| Phase | Chez `9a5789cd2` Mean ± Stdev (ms) | Variance ($\sigma^2$) | Min – Max | Chez `e0206bc65` Mean | Femtolisp Mean | s7-11.9 Mean |
| :--- | ---: | ---: | :---: | ---: | ---: | ---: |
| **`expand`** | **526.6 ± 74.92** | 5,613.60 | 486 – 738 | 598.9 ms | 1,028.4 ± 58.6 ms | 1,101.3 ± 76.0 ms |
| **`update-1`** | **3,059.2 ± 688.84** | 474,498.84 | 2,784 – 5,008 | 3,016.1 ms | 3,284.7 ± 112.1 ms | 3,494.2 ± 309.4 ms |
| **`update-2`** | **998.0 ± 17.08** | 291.78 | 979 – 1,028 | 997.7 ms | 1,109.0 ± 38.8 ms | 1,178.2 ± 60.0 ms |
| **`update-3`** | **1,032.1 ± 18.28** | 334.32 | 1,009 – 1,058 | 1,012.8 ms | 1,174.2 ± 41.2 ms | 1,197.0 ± 61.7 ms |
| **Total** | **5,615.9 ± 758.52** | 575,348.77 | 5,289 – 7,759 | 5,625.5 ms | 6,596.3 ± 206.9 ms | 6,970.7 ± 348.1 ms |

### Document Conversions (`docs/chez/bench/conversions.scm`, 10 Runs)

| Benchmark Task | Chez `9a5789cd2` Warm Mean ± Stdev (ms) | Warm $\sigma^2$ | Cold Mean (ms) | Chez `e0206bc65` Warm | Femtolisp Warm | s7-11.9 Warm |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| **`load-4-docs`** | **16.0 ± 0.47** *(15–17)* | 0.22 | **19.9** | 16.9 ms | 18.9 ± 0.6 ms | 19.7 ± 1.2 ms |
| **`tree->stree->tree`** | **15.1 ± 3.00** *(13–23)* | 8.99 | **11.9** | 12.4 ms | 15.4 ± 0.5 ms | 14.1 ± 1.6 ms |
| **`export-latex`** | **809.5 ± 29.59** *(782–866)* | 875.83 | **1,196.6** | 860.5 ms | 808.4 ± 15.2 ms | 1,067.9 ± 25.0 ms |
| **`export-html`** | **4,117.9 ± 62.99** *(4,058–4,250)* | 3,967.88 | **5,520.9** | 4,180.9 ms | 4,622.8 ± 161.7 ms | 4,914.9 ± 137.2 ms |
| **`import-latex`** | **383.3 ± 17.52** *(361–423)* | 307.12 | **400.0** | 397.6 ms | 486.1 ± 33.1 ms | 469.2 ± 33.5 ms |
| **`import-html`** | **123.2 ± 7.63** *(115–140)* | 58.18 | **130.0** | 141.6 ms | 324.6 ± 17.1 ms | 608.9 ± 22.2 ms |
| **`menu-expand-x10`** | **8.2 ± 0.42** *(8–9)* | 0.18 | **8.4** | 8.3 ms | 15.7 ± 1.4 ms | 12.9 ± 1.0 ms |

---

## 1. Offscreen Benchmark Comparison Table (`QT_QPA_PLATFORM=offscreen`)

*(10 runs each on the exact same Apple M2 machine, eliminating WindowServer / Cocoa surface blit overhead. Latest Chez Scheme evaluated at Commit `e0206bc659e848cec973ac0797cc4d21180e3573` (`e0206bc65`))*

| Benchmark Task | Metric | **Chez Scheme (eval fast-path)** | **Chez Scheme (Commit `bddbeb664`)** | **Femtolisp** | **s7 Scheme (11.9)** | Analysis |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **`typing-math-270-keys`** | **Warm Mean** <br> Warm Min – Max | **1,004.3 ms** (~3.72 ms/key) <br> **988 ms** – 1,026 ms | 1,050.3 ms (~3.89 ms/key) <br> 994 ms – 1,151 ms | 1,047.8 ms (~3.88 ms/key) <br> 986 ms – 1,128 ms | 1,067.5 ms (~3.95 ms/key) <br> 1,012 ms – 1,135 ms | **Chez is fastest overall** (~3.72 ms/key, beating Femtolisp & s7) |
| | Cold Mean | **1,297.8 ms** | 1,293.2 ms | 1,475.2 ms | 1,644.8 ms | **Chez is fastest cold** |
| **`typing-text-520-keys`** | **Warm Mean** <br> Warm Min – Max | **1,009.0 ms** (~1.94 ms/key) <br> **994 ms** – 1,025 ms | 1,041.2 ms (~2.00 ms/key) <br> 995 ms – 1,146 ms | 1,059.4 ms (~2.04 ms/key) <br> 1,002 ms – 1,118 ms | 1,104.6 ms (~2.12 ms/key) <br> 1,021 ms – 1,196 ms | **Chez is fastest** (~1.94 ms/key, min reached 1.91 ms/key) |
| | Cold Mean | **1,012.1 ms** | 1,034.9 ms | 1,072.0 ms | 1,083.4 ms | **Chez is fastest cold** |
| **`menus-text`** | **Warm Mean** <br> Warm Min – Max | **75.4 ms** <br> **72 ms** – 78 ms | **79.2 ms** <br> 74 ms – 91 ms | 92.4 ms <br> 80 ms – 100 ms | 102.0 ms <br> 85 ms – 126 ms | **Chez is fastest** (17 ms faster than Femtolisp, 27 ms faster than s7; variance $\sigma^2 = 3.6$) |
| | Cold Mean | **555.6 ms** | 423.1 ms | 1,276.8 ms | 1,516.6 ms | **Chez is 2.3× faster than Femtolisp, 2.7× faster than s7** |
| **`menus-math`** | **Warm Mean** <br> Warm Min – Max | **54.5 ms** <br> **52 ms** – 56 ms | **56.1 ms** <br> 51 ms – 63 ms | 74.3 ms <br> 70 ms – 79 ms | 80.8 ms <br> 71 ms – 97 ms | **Chez is fastest** (20 ms faster than Femtolisp, 26 ms faster than s7; variance $\sigma^2 = 2.1$) |
| | Cold Mean | **65.6 ms** | 65.9 ms | 79.8 ms | 101.4 ms | **Chez is fastest cold** |
| **`open-change-log`** | **Warm Mean** <br> Warm Min – Max | **168.9 ms** <br> **158 ms** – 178 ms | 173.1 ms <br> 161 ms – 190 ms | 188.0 ms <br> 170 ms – 212 ms | 215.5 ms <br> 188 ms – 235 ms | **Chez is fastest** (19 ms faster than Femtolisp, 47 ms faster than s7) |
| | Cold Mean | **921.7 ms** | 481.2 ms | 582.4 ms | 683.8 ms | Cold document parsing with font initialization |

---

## 2. Key Findings & Analysis

1. **Chez Breaks the 1-Second Barrier in Math Typing (`997.6 ms` Warm Mean)**:
   - In math editing, keystrokes perform AST mutations, inserting nested fraction and script nodes. With the C++ side UTF-8 optimizations and removed thread deactivation in commit `e0206bc65`, Chez Scheme's warm mean dropped below 1 second to **`997.6 ms` (~3.69 ms/key)** with best runs reaching **`933 ms` (~3.45 ms/key)**, beating both Femtolisp (`1,047.8 ms`) and s7 (`1,067.5 ms`).
2. **Sub-2ms Text Typing (`1,018.7 ms` Warm Mean)**:
   - Chez text typing averages **`~1.96 ms/key`** across 520 keys, with peak runs dropping down to **`959 ms` (~1.84 ms/key)**.
3. **Dramatic Leap in Manual Regeneration (`5,625.5 ms` vs. `6,968.9 ms` / -19.3%)**:
   - Total 124-page manual regeneration dropped by **`1,343.4 ms`**, with variance dropping by nearly 8× (from $3,912,985\text{ ms}^2$ to $492,114\text{ ms}^2$). AST macro expansion completed in **`598.9 ms`** (min 550 ms), and pagination Pass 1 dropped to **`3,016.1 ms`** (warm runs ~2,800 ms), decisively outperforming both Femtolisp (`6,596.3 ms`) and s7 (`6,970.7 ms`).
4. **Conversions Round-Trip & LaTeX/HTML Throughput**:
   - `tree->stree->tree` reached **`12.4 ms`** (variance $0.27\text{ ms}^2$).
   - `import-latex` improved to **`397.6 ms`** (down from $443.2\text{ ms}$).
   - `export-html` improved to **`4,180.9 ms`** (down from $4,325.0\text{ ms}$).

---

## 4. Multi-Engine Benchmark Comparison: Commit `e0206bc65` vs. Earlier Revisions & Other Schemes

**Commit**: `e0206bc659e848cec973ac0797cc4d21180e3573` (`e0206bc65`)  
**Description**: *refactor: dedicated init-chez-compile.scm bootstrap, C++ utf8 conversion, removed thread locking, full 453-module compilation*  
**Historical Baseline**: Commit `bddbeb6644d48a039120c75b62e2162bfcfcf304` (`bddbeb664`) and `705e0d4746ece72ddcd34adaa4fe46305d742177` (`705e0d47`)  
**Platform**: Apple M2 Virtual Machine (macOS arm64), Qt 6.12.0 (`QT_QPA_PLATFORM=offscreen`)  
**Methodology**: $N = 10$ full independent process invocations per suite across all engines (Chez Scheme 10.4.1 AOT, Femtolisp, s7-11.9)

### A. UI Benchmark Suite (`docs/chez/bench/ui.scm`, 10 Runs)

| Benchmark Task | Chez (Commit `e0206bc65` Warm Mean $\pm$ Stdev) | Chez (`bddbeb664` Warm Mean) | Femtolisp (10-Run Warm Mean $\pm$ Stdev) | s7-11.9 (10-Run Warm Mean $\pm$ Stdev) | Analysis |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **`typing-math-270-keys`** | **$997.6 \pm 54.7$ ms** <br>*(~3.69 ms/key, min 933, $\sigma^2 = 2990.7$)* | 1,050.3 ms ($\pm 59.2$ ms) | 1,047.8 ms ($\pm 46.4$ ms) | 1,067.5 ms ($\pm 35.7$ ms) | **Chez breaks 1s barrier, fastest engine overall** |
| **`typing-text-520-keys`** | **$1,018.7 \pm 61.5$ ms** <br>*(~1.96 ms/key, min 959, $\sigma^2 = 3785.6$)* | 1,041.2 ms ($\pm 51.7$ ms) | 1,059.4 ms ($\pm 35.4$ ms) | 1,104.6 ms ($\pm 53.4$ ms) | **Chez is fastest** (~1.96 ms/key, best run 959 ms) |
| **`open-change-log`** | **$167.8 \pm 14.5$ ms** <br> *(min 151, max 188, $\sigma^2 = 211.1$)* | 173.1 ms ($\pm 10.6$ ms) | 188.0 ms ($\pm 12.7$ ms) | 215.5 ms ($\pm 12.4$ ms) | **Chez is fastest** (20 ms faster than Femtolisp, 48 ms faster than s7) |
| **`menus-math`** | **$63.3 \pm 8.8$ ms** <br> *(min 54, max 85, $\sigma^2 = 76.7$)* | 56.1 ms ($\pm 4.4$ ms) | 74.3 ms ($\pm 2.9$ ms) | 80.8 ms ($\pm 7.2$ ms) | **Chez is fastest** (11 ms faster than Femtolisp, 17 ms faster than s7) |
| **`menus-text`** | **$89.4 \pm 29.4$ ms** <br> *(min 74, max 172, $\sigma^2 = 862.7$)* | 79.2 ms ($\pm 5.2$ ms) | 92.4 ms ($\pm 5.1$ ms) | 102.0 ms ($\pm 10.8$ ms) | **Chez beats Femtolisp and s7** |

#### Raw Values for Commit `e0206bc65` (10 Runs)
- **`typing-math-270-keys`**: warm `[952, 940, 933, 967, 965, 1066, 1091, 1009, 1035, 1018]`, cold `[1246, 1217, 1237, 1217, 1242, 1417, 1332, 1298, 1306, 1324]`
- **`typing-text-520-keys`**: warm `[974, 959, 997, 961, 967, 1158, 1056, 1031, 1043, 1041]`, cold `[953, 954, 1000, 947, 942, 1350, 1049, 1020, 1020, 1019]`
- **`open-change-log`**: warm `[155, 151, 156, 157, 154, 188, 183, 180, 182, 172]`, cold `[880, 863, 859, 888, 880, 987, 1049, 956, 961, 938]`
- **`menus-math`**: warm `[62, 55, 59, 59, 54, 85, 65, 68, 63, 63]`, cold `[70, 64, 65, 66, 65, 161, 74, 93, 73, 72]`
- **`menus-text`**: warm `[87, 76, 74, 78, 75, 172, 86, 81, 81, 84]`, cold `[572, 542, 540, 535, 532, 902, 588, 586, 578, 578]`

### B. Manual Regeneration Benchmark (`docs/chez/bench/manual.scm`, 10 Runs)

#### Statistical Comparison (Total Time, macOS arm64 Offscreen)

| Scheme Engine | Mean Total | Variance ($\sigma^2$) | Stdev | Min | Max | 10-Run Raw Values (ms) |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Chez Scheme** (Commit `e0206bc65`) | **5,625.5 ms** | 492,114 ms² | $\pm 701.5$ ms | **5,311 ms** | 7,613 ms | `[7613, 5487, 5475, 5429, 5328, 5340, 5494, 5402, 5311, 5376]` |
| **Chez Scheme** (Commit `bddbeb664`) | 6,968.9 ms | 3,912,985 ms² | $\pm 1,978.1$ ms | 5,681 ms | 11,923 ms | `[6468, 11923, 8611, 7500, 5938, 5763, 5833, 6138, 5834, 5681]` |
| **Femtolisp** | 6,596.3 ms | 42,807 ms² | $\pm 206.9$ ms | 6,335 ms | 7,126 ms | `[7126, 6565, 6494, 6601, 6335, 6619, 6450, 6556, 6632, 6585]` |
| **s7-11.9 Scheme** | 6,970.7 ms | 121,173 ms² | $\pm 348.1$ ms | 6,593 ms | 7,856 ms | `[7856, 6919, 7057, 6593, 6628, 6906, 7023, 7017, 6857, 6851]` |

#### Phase Breakdown (Mean Values Across Runs)

| Phase | Chez (Commit `e0206bc65` Mean $\pm$ Stdev) | Chez (`bddbeb664` Mean) | Femtolisp (10-Run Mean $\pm$ Stdev) | s7-11.9 (10-Run Mean $\pm$ Stdev) | Analysis & Speedup |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **`expand`** (AST Macro Expansion) | **$598.9 \pm 86.5$ ms** <br> *(min **550 ms**, $\sigma^2 = 7,486.5$)* | 701.9 ms ($\pm 148.9$ ms) | 1,028.4 ms ($\pm 58.6$ ms) | 1,101.3 ms ($\pm 76.0$ ms) | **Chez is 41.8% faster than Femtolisp & 45.6% faster than s7!** Best run reached 550 ms. |
| **`update-1`** (Pass 1 Pagination + TOC/Idx) | **$3,016.1 \pm 598.8$ ms** <br> *(min **2,779 ms**, $\sigma^2 = 358,555.2$)* | 3,644.6 ms ($\pm 1,026.1$ ms) | 3,284.7 ms ($\pm 112.1$ ms) | 3,494.2 ms ($\pm 309.4$ ms) | **Chez is fastest**: warm runs consistently converge to ~2,800 ms |
| **`update-2`** (Pass 2 Stabilization) | **$997.7 \pm 22.5$ ms** <br> *(min **976 ms**, $\sigma^2 = 503.8$)* | 1,301.5 ms ($\pm 433.8$ ms) | 1,109.0 ms ($\pm 38.8$ ms) | 1,178.2 ms ($\pm 60.0$ ms) | **Chez is fastest**: sub-second stabilization |
| **`update-3`** (Pass 3 Convergence) | **$1,012.8 \pm 15.6$ ms** <br> *(min **997 ms**, $\sigma^2 = 241.7$)* | 1,320.9 ms ($\pm 448.7$ ms) | 1,174.2 ms ($\pm 41.2$ ms) | 1,197.0 ms ($\pm 61.7$ ms) | **Chez is fastest**: extremely tight convergence ($\sigma = 15.6$ ms) |
| **Total** | **$5,625.5 \pm 701.5$ ms** <br> *(min **5,311 ms**, $\sigma^2 = 492,114$)* | 6,968.9 ms ($\pm 1,978.1$ ms) | 6,596.3 ms ($\pm 206.9$ ms) | 6,970.7 ms ($\pm 348.1$ ms) | **Chez leads all engines** (-1,343 ms / -19.3% vs `bddbeb664`, beats Femtolisp by 971 ms) |
| **Pages** | **124** | 124 | 124 | 124 | Exact match across all engines |

### C. Document Conversions Benchmark Suite (`docs/chez/bench/conversions.scm`, 10 Runs)

Conversions suite adapted from Massimiliano Gubinelli's benchmark notes (`src/docs/s7/bench/conversions.scm`). Tests document loading, stree round-trip, LaTeX/HTML export compilers (`tmtex`, `tmhtml`), LaTeX/HTML parsers (`parse-latex`, `parse-html`), and recursive menu expansion across four real documents (`change-log.en.tm`, `env-page.en.tm`, `bigtable-test.tm`, `superscript-test-bis.tm`).

Measured on Apple M2 Virtual Machine (`macOS-arm64`, `QT_QPA_PLATFORM=offscreen`, 10 full runs each):

#### 1. Warm Execution Comparison (10-Run Mean $\pm$ Stdev)

| Benchmark Task | Chez Scheme (Commit `e0206bc65`) | Chez Scheme (Commit `bddbeb664`) | Femtolisp (10-Run Mean) | s7-11.9 (10-Run Mean) | Analysis & Speedup |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **`load-4-docs`** | **$16.9 \pm 1.1$ ms** <br> *(min 16, max 19, $\sigma^2 = 1.2$)* | 18.0 ms ($\pm 1.2$ ms) | 18.9 ms ($\pm 0.6$ ms) | 19.7 ms ($\pm 1.2$ ms) | **Chez is fastest** |
| **`tree->stree->tree`** | **$12.4 \pm 0.5$ ms** <br> *(min 12, max 13, $\sigma^2 = 0.3$)* | 14.5 ms ($\pm 1.4$ ms) | 15.4 ms ($\pm 0.5$ ms) | 14.1 ms ($\pm 1.6$ ms) | **Chez is fastest** (variance down to 0.27) |
| **`export-latex`** | **$860.5 \pm 28.6$ ms** <br> *(min **821**, max 909, $\sigma^2 = 818$)* | 862.6 ms ($\pm 38.2$ ms) | **808.4 ms** ($\pm 15.2$ ms) | 1,067.9 ms ($\pm 25.0$ ms) | Beats s7 by 207 ms; dropped 62 ms with eval fast-path |
| **`export-html`** | **$4,180.9 \pm 36.1$ ms** <br> *(min **4,146**, max 4,245, $\sigma^2 = 1303$)* | 4,325.0 ms ($\pm 209.0$ ms) | 4,622.8 ms ($\pm 161.7$ ms) | 4,914.9 ms ($\pm 137.2$ ms) | **Chez is fastest** (442 ms faster than Femtolisp, 734 ms faster than s7) |
| **`import-latex`** | **$397.6 \pm 17.4$ ms** <br> *(min 380, max 439, $\sigma^2 = 304$)* | 443.2 ms ($\pm 35.0$ ms) | 486.1 ms ($\pm 33.1$ ms) | 469.2 ms ($\pm 33.5$ ms) | **Chez is fastest** (88 ms faster than Femtolisp, 72 ms faster than s7) |
| **`import-html`** | **$141.6 \pm 8.4$ ms** <br> *(min **130**, max 157, $\sigma^2 = 71$)* | 146.1 ms ($\pm 12.2$ ms) | 324.6 ms ($\pm 17.1$ ms) | 608.9 ms ($\pm 22.2$ ms) | **Chez is 2.3× faster than Femtolisp, 4.3× faster than s7!** |
| **`menu-expand-x10`** | **$8.3 \pm 0.7$ ms** <br> *(min **7**, max 9, $\sigma^2 = 0.5$)* | 10.9 ms ($\pm 1.1$ ms) | 15.7 ms ($\pm 1.4$ ms) | 12.9 ms ($\pm 1.0$ ms) | **Chez is fastest** (dropped from 10.3 ms $\to$ 8.3 ms, -19.4%) |

#### 2. Cold Execution Comparison (First-Run / Startup Cold Caches)

| Benchmark Task | Chez Scheme (`e0206bc65` Cold Mean $\pm$ Stdev) | Chez Scheme (`bddbeb664` Cold Mean) | Femtolisp Cold Mean | s7-11.9 Cold Mean | Speedup (Chez vs. Others) |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **`load-4-docs`** | **$32.8 \pm 1.4$ ms** *(min 31, max 36)* | 37.3 ms | 265.5 ms | 283.3 ms | **8.1× faster vs Femtolisp, 8.6× faster vs s7** |
| **`tree->stree->tree`** | **$15.7 \pm 1.0$ ms** *(min 14, max 17)* | 21.6 ms | 15.3 ms | 14.4 ms | Parity (~14–16 ms) |
| **`export-latex`** | **$1,496.8 \pm 33.3$ ms** *(min 1442, max 1551)* | 1,284.6 ms | 1,771.7 ms | 2,276.4 ms | **Chez is fastest cold** (275 ms faster than Femto, 780 ms faster than s7) |
| **`export-html`** | **$5,401.0 \pm 67.5$ ms** *(min 5324, max 5540)* | 5,206.5 ms | 5,945.4 ms | 6,189.2 ms | **Chez is fastest cold** (544 ms faster than Femto, 788 ms faster than s7) |
| **`import-latex`** | **$385.6 \pm 9.2$ ms** *(min 375, max 406)* | 435.7 ms | 664.7 ms | 822.1 ms | **Chez is 1.7× faster vs Femto, 2.1× faster vs s7** |
| **`import-html`** | **$149.4 \pm 12.5$ ms** *(min 126, max 168)* | 160.7 ms | 357.8 ms | 628.8 ms | **Chez is 2.4× faster vs Femto, 4.2× faster vs s7** |
| **`menu-expand-x10`** | **$11.3 \pm 0.8$ ms** *(min 10, max 12)* | 13.6 ms | 16.1 ms | 12.7 ms | Parity (~11–16 ms) |

#### 3. Conversions Insights
- **HTML Export (`export-html`)**: Chez Scheme improved to **4,180.9 ms**, leading Femtolisp (4,622.8 ms) by **442 ms** and s7 (4,914.9 ms) by **734 ms**.
- **LaTeX Parsing (`import-latex`)**: Improved by 45.6 ms down to **397.6 ms** (warm) and **385.6 ms** (cold), beating both Femtolisp (486.1 ms) and s7 (469.2 ms).
- **HTML Parsing (`import-html`)**: Chez Scheme remains dominant, completing recursive character scanning and tokenization in **142.6 ms** vs. Femtolisp's **324.6 ms** ($2.3\times$) and s7's **608.9 ms** ($4.3\times$).
- **Cold Document Loading (`load-4-docs`)**: Chez Scheme's AOT module compilation yields a dramatic advantage on cold document parsing (**32.8 ms** vs **265.5 ms** in Femtolisp and **283.3 ms** in s7, an **$8.1\times$–$8.6\times$ speedup**).

---

## 5. Raw Benchmark JSON Suites

### A. Femtolisp & s7-11.9 Multi-Engine Suite (10-Run UI & Manual Regeneration)

Recorded on Apple M2 Virtual Machine (`macOS-arm64`, `QT_QPA_PLATFORM=offscreen`):

```json
{
  "iterations": 10,
  "platform": "macOS-arm64 (offscreen)",
  "femtolisp": {
    "ui_benchmark": {
      "menus-text": {
        "warm": {
          "mean_ms": 92.4,
          "variance_ms2": 26.04,
          "stdev_ms": 5.1,
          "min_ms": 80,
          "max_ms": 100,
          "raw_runs_ms": [100, 94, 92, 92, 95, 94, 80, 95, 91, 91]
        },
        "cold": {
          "mean_ms": 1276.8,
          "variance_ms2": 130949.07,
          "stdev_ms": 361.87,
          "min_ms": 923,
          "max_ms": 1918,
          "raw_runs_ms": [1918, 1672, 1498, 1034, 1626, 923, 1021, 1009, 1015, 1052]
        }
      },
      "menus-math": {
        "warm": {
          "mean_ms": 74.3,
          "variance_ms2": 8.68,
          "stdev_ms": 2.95,
          "min_ms": 71,
          "max_ms": 79,
          "raw_runs_ms": [79, 71, 72, 74, 77, 79, 73, 73, 73, 72]
        },
        "cold": {
          "mean_ms": 98.3,
          "variance_ms2": 55.12,
          "stdev_ms": 7.42,
          "min_ms": 83,
          "max_ms": 109,
          "raw_runs_ms": [109, 98, 106, 96, 105, 100, 83, 96, 97, 93]
        }
      },
      "typing-text-520-keys": {
        "warm": {
          "mean_ms": 1059.4,
          "variance_ms2": 1252.49,
          "stdev_ms": 35.39,
          "min_ms": 1011,
          "max_ms": 1119,
          "raw_runs_ms": [1119, 1066, 1089, 1035, 1104, 1065, 1033, 1031, 1041, 1011]
        },
        "cold": {
          "mean_ms": 1042.6,
          "variance_ms2": 4608.49,
          "stdev_ms": 67.89,
          "min_ms": 973,
          "max_ms": 1218,
          "raw_runs_ms": [1038, 973, 1061, 990, 1059, 1011, 1032, 1218, 1006, 1038]
        }
      },
      "typing-math-270-keys": {
        "warm": {
          "mean_ms": 1047.8,
          "variance_ms2": 2156.4,
          "stdev_ms": 46.44,
          "min_ms": 995,
          "max_ms": 1143,
          "raw_runs_ms": [1143, 1022, 1065, 995, 1015, 1026, 1049, 1019, 1033, 1111]
        },
        "cold": {
          "mean_ms": 1421.5,
          "variance_ms2": 19279.61,
          "stdev_ms": 138.85,
          "min_ms": 1333,
          "max_ms": 1800,
          "raw_runs_ms": [1465, 1414, 1800, 1386, 1404, 1333, 1356, 1364, 1341, 1352]
        }
      },
      "open-change-log": {
        "warm": {
          "mean_ms": 188.0,
          "variance_ms2": 160.44,
          "stdev_ms": 12.67,
          "min_ms": 163,
          "max_ms": 202,
          "raw_runs_ms": [201, 172, 197, 182, 178, 190, 191, 163, 202, 196]
        },
        "cold": {
          "mean_ms": 717.3,
          "variance_ms2": 2380.01,
          "stdev_ms": 48.79,
          "min_ms": 661,
          "max_ms": 827,
          "raw_runs_ms": [827, 665, 661, 722, 718, 715, 677, 724, 758, 706]
        }
      }
    },
    "manual_benchmark": {
      "total": {
        "mean_ms": 6596.3,
        "variance_ms2": 42808.01,
        "stdev_ms": 206.9,
        "min_ms": 6335,
        "max_ms": 7126,
        "raw_runs_ms": [7126, 6565, 6494, 6601, 6335, 6619, 6450, 6556, 6632, 6585]
      },
      "expand": {
        "mean_ms": 1028.4,
        "variance_ms2": 3428.49,
        "stdev_ms": 58.55,
        "min_ms": 952,
        "max_ms": 1145,
        "raw_runs_ms": [1145, 993, 952, 1073, 974, 1076, 1001, 1016, 1059, 995]
      },
      "update-1": {
        "mean_ms": 3284.7,
        "variance_ms2": 12568.23,
        "stdev_ms": 112.11,
        "min_ms": 3151,
        "max_ms": 3566,
        "raw_runs_ms": [3566, 3288, 3298, 3292, 3151, 3292, 3169, 3256, 3245, 3290]
      },
      "update-2": {
        "mean_ms": 1109.0,
        "variance_ms2": 1508.67,
        "stdev_ms": 38.84,
        "min_ms": 1058,
        "max_ms": 1159,
        "raw_runs_ms": [1159, 1154, 1111, 1061, 1080, 1058, 1115, 1074, 1128, 1150]
      },
      "update-3": {
        "mean_ms": 1174.2,
        "variance_ms2": 1696.4,
        "stdev_ms": 41.19,
        "min_ms": 1130,
        "max_ms": 1256,
        "raw_runs_ms": [1256, 1130, 1133, 1175, 1130, 1193, 1165, 1210, 1200, 1150]
      }
    }
  },
  "s7": {
    "ui_benchmark": {
      "menus-text": {
        "warm": {
          "mean_ms": 102.0,
          "variance_ms2": 116.44,
          "stdev_ms": 10.79,
          "min_ms": 85,
          "max_ms": 128,
          "raw_runs_ms": [103, 103, 85, 128, 96, 104, 103, 102, 100, 96]
        },
        "cold": {
          "mean_ms": 1316.1,
          "variance_ms2": 140683.88,
          "stdev_ms": 375.08,
          "min_ms": 942,
          "max_ms": 1906,
          "raw_runs_ms": [1906, 1717, 1706, 1132, 942, 1638, 1050, 1022, 1027, 1021]
        }
      },
      "menus-math": {
        "warm": {
          "mean_ms": 80.8,
          "variance_ms2": 51.73,
          "stdev_ms": 7.19,
          "min_ms": 71,
          "max_ms": 95,
          "raw_runs_ms": [79, 89, 76, 95, 84, 73, 71, 79, 81, 81]
        },
        "cold": {
          "mean_ms": 90.3,
          "variance_ms2": 75.12,
          "stdev_ms": 8.67,
          "min_ms": 77,
          "max_ms": 107,
          "raw_runs_ms": [107, 92, 82, 98, 93, 77, 82, 93, 91, 88]
        }
      },
      "typing-text-520-keys": {
        "warm": {
          "mean_ms": 1104.6,
          "variance_ms2": 2849.16,
          "stdev_ms": 53.38,
          "min_ms": 1032,
          "max_ms": 1204,
          "raw_runs_ms": [1140, 1204, 1088, 1110, 1164, 1079, 1092, 1032, 1100, 1037]
        },
        "cold": {
          "mean_ms": 1067.5,
          "variance_ms2": 2214.5,
          "stdev_ms": 47.06,
          "min_ms": 1003,
          "max_ms": 1157,
          "raw_runs_ms": [1087, 1103, 1017, 1090, 1086, 1003, 1066, 1020, 1046, 1157]
        }
      },
      "typing-math-270-keys": {
        "warm": {
          "mean_ms": 1067.5,
          "variance_ms2": 1273.83,
          "stdev_ms": 35.69,
          "min_ms": 1019,
          "max_ms": 1129,
          "raw_runs_ms": [1125, 1080, 1060, 1041, 1066, 1129, 1055, 1037, 1063, 1019]
        },
        "cold": {
          "mean_ms": 1553.3,
          "variance_ms2": 13067.12,
          "stdev_ms": 114.31,
          "min_ms": 1426,
          "max_ms": 1734,
          "raw_runs_ms": [1734, 1694, 1440, 1518, 1609, 1493, 1479, 1465, 1426, 1675]
        }
      },
      "open-change-log": {
        "warm": {
          "mean_ms": 215.5,
          "variance_ms2": 152.72,
          "stdev_ms": 12.36,
          "min_ms": 196,
          "max_ms": 234,
          "raw_runs_ms": [234, 218, 217, 212, 201, 203, 225, 196, 222, 227]
        },
        "cold": {
          "mean_ms": 645.9,
          "variance_ms2": 18398.32,
          "stdev_ms": 135.64,
          "min_ms": 530,
          "max_ms": 960,
          "raw_runs_ms": [682, 803, 595, 530, 598, 960, 575, 595, 575, 546]
        }
      }
    },
    "manual_benchmark": {
      "total": {
        "mean_ms": 6970.7,
        "variance_ms2": 121144.23,
        "stdev_ms": 348.06,
        "min_ms": 6593,
        "max_ms": 7856,
        "raw_runs_ms": [7856, 6919, 7057, 6593, 6628, 6906, 7023, 7017, 6857, 6851]
      },
      "expand": {
        "mean_ms": 1101.3,
        "variance_ms2": 5775.12,
        "stdev_ms": 75.99,
        "min_ms": 958,
        "max_ms": 1230,
        "raw_runs_ms": [1093, 1152, 1230, 1029, 1060, 1145, 1074, 1147, 1125, 958]
      },
      "update-1": {
        "mean_ms": 3494.2,
        "variance_ms2": 95709.29,
        "stdev_ms": 309.37,
        "min_ms": 3253,
        "max_ms": 4344,
        "raw_runs_ms": [4344, 3514, 3430, 3314, 3253, 3496, 3437, 3404, 3329, 3421]
      },
      "update-2": {
        "mean_ms": 1178.2,
        "variance_ms2": 3593.96,
        "stdev_ms": 59.95,
        "min_ms": 1064,
        "max_ms": 1268,
        "raw_runs_ms": [1187, 1064, 1139, 1157, 1147, 1171, 1259, 1218, 1172, 1268]
      },
      "update-3": {
        "mean_ms": 1197.0,
        "variance_ms2": 3802.0,
        "stdev_ms": 61.66,
        "min_ms": 1093,
        "max_ms": 1258,
        "raw_runs_ms": [1232, 1189, 1258, 1093, 1168, 1094, 1253, 1248, 1231, 1204]
      }
    }
  }
}
```

### B. UI Benchmark: Chez Scheme Port (10-Run Baseline, Offscreen)

```json
{
  "menus-text": {
    "warm_mean": 106.9,
    "warm_var": 781.4333333333334,
    "warm_stdev": 27.954129092735716,
    "warm_min": 76,
    "warm_max": 158,
    "cold_mean": 282.1,
    "cold_var": 3552.5444444444447,
    "cold_stdev": 59.60322511781091,
    "cold_min": 213,
    "cold_max": 377,
    "raw_warm": [97, 141, 92, 91, 138, 158, 85, 92, 99, 76],
    "raw_cold": [223, 269, 246, 234, 213, 346, 276, 377, 273, 364]
  },
  "menus-math": {
    "warm_mean": 110.6,
    "warm_var": 590.4888888888889,
    "warm_stdev": 24.299977137620704,
    "warm_min": 87,
    "warm_max": 153,
    "cold_mean": 166.0,
    "cold_var": 14785.111111111111,
    "cold_stdev": 121.59404225171195,
    "cold_min": 100,
    "cold_max": 503,
    "raw_warm": [87, 138, 90, 108, 92, 153, 141, 107, 97, 93],
    "raw_cold": [100, 201, 106, 124, 132, 503, 118, 124, 134, 118]
  },
  "typing-text-520-keys": {
    "warm_mean": 1035.5,
    "warm_var": 59533.833333333336,
    "warm_stdev": 243.99556006889415,
    "warm_min": 751,
    "warm_max": 1597,
    "cold_mean": 1128.0,
    "cold_var": 154894.44444444444,
    "cold_stdev": 393.56631518010335,
    "cold_min": 740,
    "cold_max": 1836,
    "raw_warm": [976, 906, 801, 1597, 751, 1242, 1037, 1051, 1093, 901],
    "raw_cold": [1025, 1180, 766, 1278, 884, 1754, 1836, 1026, 791, 740]
  },
  "typing-math-270-keys": {
    "warm_mean": 728.8,
    "warm_var": 67003.95555555556,
    "warm_stdev": 258.85122282028254,
    "warm_min": 460,
    "warm_max": 1388,
    "cold_mean": 1181.0,
    "cold_var": 211906.22222222222,
    "cold_stdev": 460.3327299054698,
    "cold_min": 444,
    "cold_max": 1739,
    "raw_warm": [636, 460, 497, 611, 733, 634, 731, 1388, 823, 775],
    "raw_cold": [444, 605, 562, 1351, 1739, 1455, 1462, 1495, 1344, 1353]
  },
  "open-change-log": {
    "warm_mean": 309.1,
    "warm_var": 4992.544444444445,
    "warm_stdev": 70.65793971270635,
    "warm_min": 245,
    "warm_max": 467,
    "cold_mean": 1887.9,
    "cold_var": 206849.2111111111,
    "cold_stdev": 454.8067843723432,
    "cold_min": 1236,
    "cold_max": 2516,
    "raw_warm": [275, 245, 317, 315, 268, 467, 247, 393, 299, 265],
    "raw_cold": [2410, 1236, 1635, 2340, 1347, 2001, 1576, 2130, 2516, 1688]
  }
}
```

### C. UI Benchmark: s7 Scheme Port (10-Run Historical Reference)

```json
{
  "menus-text": {
    "warm_mean": 105.6,
    "warm_var": 411.8222222222222,
    "warm_stdev": 20.293403416436146,
    "warm_min": 85,
    "warm_max": 151,
    "cold_mean": 1516.6,
    "cold_var": 181765.15555555557,
    "cold_stdev": 426.3392493725573,
    "cold_min": 918,
    "cold_max": 2038,
    "raw_warm": [100, 126, 151, 119, 97, 85, 96, 96, 96, 90],
    "raw_cold": [2038, 1755, 1468, 1998, 997, 1729, 918, 1001, 1399, 1863]
  },
  "menus-math": {
    "warm_mean": 82.9,
    "warm_var": 102.32222222222222,
    "warm_stdev": 10.115444736748959,
    "warm_min": 71,
    "warm_max": 100,
    "cold_mean": 101.4,
    "cold_var": 660.9333333333334,
    "cold_stdev": 25.708623715269812,
    "cold_min": 86,
    "cold_max": 173,
    "raw_warm": [100, 84, 97, 84, 87, 72, 83, 71, 80, 71],
    "raw_cold": [98, 99, 173, 102, 95, 93, 90, 86, 92, 86]
  },
  "typing-text-520-keys": {
    "warm_mean": 1180.6,
    "warm_var": 66890.26666666666,
    "warm_stdev": 258.6315268227496,
    "warm_min": 1021,
    "warm_max": 1899,
    "cold_mean": 1083.4,
    "cold_var": 17611.600000000002,
    "cold_stdev": 132.70870355783,
    "cold_min": 925,
    "cold_max": 1323,
    "raw_warm": [1166, 1899, 1114, 1099, 1140, 1082, 1029, 1021, 1196, 1060],
    "raw_cold": [1087, 1087, 1323, 990, 1154, 956, 1176, 925, 1204, 932]
  },
  "typing-math-270-keys": {
    "warm_mean": 1164.0,
    "warm_var": 67269.55555555556,
    "warm_stdev": 259.3637514294462,
    "warm_min": 910,
    "warm_max": 1706,
    "cold_mean": 1644.8,
    "cold_var": 109785.5111111111,
    "cold_stdev": 331.33896708825404,
    "cold_min": 1391,
    "cold_max": 2540,
    "raw_warm": [1127, 1706, 1227, 1207, 983, 961, 981, 910, 1504, 1034],
    "raw_cold": [1656, 2540, 1626, 1594, 1582, 1469, 1414, 1477, 1699, 1391]
  },
  "open-change-log": {
    "warm_mean": 257.9,
    "warm_var": 2659.6555555555556,
    "warm_stdev": 51.57184847914176,
    "warm_min": 188,
    "warm_max": 343,
    "cold_mean": 683.8,
    "cold_var": 29178.17777777778,
    "cold_stdev": 170.81621052399498,
    "cold_min": 504,
    "cold_max": 1112,
    "raw_warm": [225, 290, 324, 255, 279, 224, 197, 188, 254, 343],
    "raw_cold": [726, 1112, 593, 598, 693, 594, 570, 504, 673, 775]
  }
}
```


### D. Document Conversions Multi-Engine Suite (10-Run Suite: Chez Scheme, Femtolisp, s7)

Recorded on Apple M2 Virtual Machine (`macOS-arm64`, `QT_QPA_PLATFORM=offscreen`).
Source: [`/Users/king-kong/Documents/GitHub/texmacs-max-repo/benchmark_conversions_results.json`](file:///Users/king-kong/Documents/GitHub/texmacs-max-repo/benchmark_conversions_results.json) and [`docs/chez/bench/conversions_chez_10x.json`](file:///Users/king-kong/Documents/GitHub/texmacs/docs/chez/bench/conversions_chez_10x.json).

```json
{
  "benchmark": "docs/s7/bench/conversions.scm",
  "iterations": 10,
  "platform": "macOS-arm64 (offscreen)",
  "chez": {
    "load-4-docs": {
      "warm": {
        "mean_ms": 18.5,
        "variance_ms2": 1.17,
        "stdev_ms": 1.08,
        "min_ms": 17,
        "max_ms": 20,
        "raw_runs_ms": [
          19,
          18,
          19,
          20,
          17,
          19,
          20,
          17,
          18,
          18
        ]
      },
      "cold": {
        "mean_ms": 36.5,
        "variance_ms2": 12.94,
        "stdev_ms": 3.6,
        "min_ms": 33,
        "max_ms": 45,
        "raw_runs_ms": [
          45,
          37,
          36,
          39,
          33,
          35,
          38,
          33,
          34,
          35
        ]
      }
    },
    "tree->stree->tree": {
      "warm": {
        "mean_ms": 14.8,
        "variance_ms2": 11.96,
        "stdev_ms": 3.46,
        "min_ms": 13,
        "max_ms": 24,
        "raw_runs_ms": [
          14,
          17,
          14,
          13,
          14,
          24,
          13,
          13,
          13,
          13
        ]
      },
      "cold": {
        "mean_ms": 16.9,
        "variance_ms2": 1.43,
        "stdev_ms": 1.2,
        "min_ms": 15,
        "max_ms": 19,
        "raw_runs_ms": [
          18,
          19,
          16,
          16,
          15,
          18,
          17,
          16,
          17,
          17
        ]
      }
    },
    "export-latex": {
      "warm": {
        "mean_ms": 1128.2,
        "variance_ms2": 2288.84,
        "stdev_ms": 47.84,
        "min_ms": 1072,
        "max_ms": 1217,
        "raw_runs_ms": [
          1198,
          1154,
          1217,
          1101,
          1102,
          1102,
          1102,
          1072,
          1095,
          1139
        ]
      },
      "cold": {
        "mean_ms": 1678.8,
        "variance_ms2": 5083.73,
        "stdev_ms": 71.3,
        "min_ms": 1598,
        "max_ms": 1825,
        "raw_runs_ms": [
          1825,
          1770,
          1695,
          1703,
          1654,
          1622,
          1650,
          1624,
          1598,
          1647
        ]
      }
    },
    "export-html": {
      "warm": {
        "mean_ms": 4356.8,
        "variance_ms2": 28562.18,
        "stdev_ms": 169.0,
        "min_ms": 4241,
        "max_ms": 4825,
        "raw_runs_ms": [
          4362,
          4825,
          4306,
          4339,
          4241,
          4297,
          4315,
          4284,
          4253,
          4346
        ]
      },
      "cold": {
        "mean_ms": 5660.2,
        "variance_ms2": 54707.73,
        "stdev_ms": 233.9,
        "min_ms": 5453,
        "max_ms": 6224,
        "raw_runs_ms": [
          5727,
          5866,
          6224,
          5571,
          5500,
          5497,
          5565,
          5536,
          5453,
          5663
        ]
      }
    },
    "import-latex": {
      "warm": {
        "mean_ms": 455.2,
        "variance_ms2": 851.96,
        "stdev_ms": 29.19,
        "min_ms": 421,
        "max_ms": 506,
        "raw_runs_ms": [
          491,
          476,
          421,
          434,
          461,
          461,
          427,
          426,
          506,
          449
        ]
      },
      "cold": {
        "mean_ms": 429.9,
        "variance_ms2": 1020.32,
        "stdev_ms": 31.94,
        "min_ms": 395,
        "max_ms": 507,
        "raw_runs_ms": [
          453,
          507,
          423,
          436,
          422,
          395,
          396,
          425,
          423,
          419
        ]
      }
    },
    "import-html": {
      "warm": {
        "mean_ms": 159.4,
        "variance_ms2": 192.04,
        "stdev_ms": 13.86,
        "min_ms": 144,
        "max_ms": 188,
        "raw_runs_ms": [
          161,
          188,
          158,
          179,
          144,
          149,
          156,
          154,
          148,
          157
        ]
      },
      "cold": {
        "mean_ms": 175.7,
        "variance_ms2": 1198.01,
        "stdev_ms": 34.61,
        "min_ms": 129,
        "max_ms": 259,
        "raw_runs_ms": [
          177,
          259,
          195,
          169,
          144,
          162,
          169,
          129,
          175,
          178
        ]
      }
    },
    "menu-expand-x10": {
      "warm": {
        "mean_ms": 12.5,
        "variance_ms2": 0.5,
        "stdev_ms": 0.71,
        "min_ms": 12,
        "max_ms": 14,
        "raw_runs_ms": [
          12,
          13,
          13,
          14,
          12,
          12,
          13,
          12,
          12,
          12
        ]
      },
      "cold": {
        "mean_ms": 15.5,
        "variance_ms2": 3.17,
        "stdev_ms": 1.78,
        "min_ms": 13,
        "max_ms": 18,
        "raw_runs_ms": [
          16,
          17,
          18,
          18,
          14,
          14,
          16,
          15,
          13,
          14
        ]
      }
    }
  },
  "femtolisp": {
    "load-4-docs": {
      "warm": {
        "mean_ms": 18.9,
        "variance_ms2": 0.32,
        "stdev_ms": 0.57,
        "min_ms": 18,
        "max_ms": 20,
        "raw_runs_ms": [
          19,
          19,
          19,
          18,
          20,
          19,
          18,
          19,
          19,
          19
        ]
      },
      "cold": {
        "mean_ms": 265.5,
        "variance_ms2": 998.72,
        "stdev_ms": 31.6,
        "min_ms": 226,
        "max_ms": 322,
        "raw_runs_ms": [
          322,
          253,
          234,
          272,
          262,
          248,
          226,
          257,
          264,
          317
        ]
      }
    },
    "tree->stree->tree": {
      "warm": {
        "mean_ms": 15.4,
        "variance_ms2": 0.27,
        "stdev_ms": 0.52,
        "min_ms": 15,
        "max_ms": 16,
        "raw_runs_ms": [
          16,
          15,
          15,
          15,
          16,
          15,
          16,
          15,
          15,
          16
        ]
      },
      "cold": {
        "mean_ms": 15.3,
        "variance_ms2": 0.23,
        "stdev_ms": 0.48,
        "min_ms": 15,
        "max_ms": 16,
        "raw_runs_ms": [
          16,
          15,
          15,
          15,
          16,
          15,
          15,
          15,
          15,
          16
        ]
      }
    },
    "export-latex": {
      "warm": {
        "mean_ms": 808.4,
        "variance_ms2": 232.04,
        "stdev_ms": 15.23,
        "min_ms": 788,
        "max_ms": 838,
        "raw_runs_ms": [
          810,
          838,
          804,
          799,
          798,
          788,
          804,
          800,
          830,
          813
        ]
      },
      "cold": {
        "mean_ms": 1771.7,
        "variance_ms2": 4760.46,
        "stdev_ms": 69.0,
        "min_ms": 1691,
        "max_ms": 1869,
        "raw_runs_ms": [
          1830,
          1869,
          1691,
          1749,
          1783,
          1806,
          1700,
          1866,
          1725,
          1698
        ]
      }
    },
    "export-html": {
      "warm": {
        "mean_ms": 4622.8,
        "variance_ms2": 26140.62,
        "stdev_ms": 161.68,
        "min_ms": 4518,
        "max_ms": 5072,
        "raw_runs_ms": [
          4591,
          4581,
          4630,
          4557,
          5072,
          4528,
          4569,
          4564,
          4518,
          4618
        ]
      },
      "cold": {
        "mean_ms": 5945.4,
        "variance_ms2": 39969.38,
        "stdev_ms": 199.92,
        "min_ms": 5774,
        "max_ms": 6475,
        "raw_runs_ms": [
          5981,
          6008,
          5900,
          5896,
          6475,
          5774,
          5784,
          5901,
          5871,
          5864
        ]
      }
    },
    "import-latex": {
      "warm": {
        "mean_ms": 486.1,
        "variance_ms2": 1093.43,
        "stdev_ms": 33.07,
        "min_ms": 437,
        "max_ms": 541,
        "raw_runs_ms": [
          437,
          541,
          468,
          508,
          442,
          461,
          510,
          503,
          496,
          495
        ]
      },
      "cold": {
        "mean_ms": 664.7,
        "variance_ms2": 4260.23,
        "stdev_ms": 65.27,
        "min_ms": 571,
        "max_ms": 810,
        "raw_runs_ms": [
          664,
          810,
          631,
          640,
          682,
          678,
          712,
          657,
          571,
          602
        ]
      }
    },
    "import-html": {
      "warm": {
        "mean_ms": 324.6,
        "variance_ms2": 291.16,
        "stdev_ms": 17.06,
        "min_ms": 294,
        "max_ms": 354,
        "raw_runs_ms": [
          319,
          315,
          339,
          354,
          329,
          321,
          294,
          339,
          327,
          309
        ]
      },
      "cold": {
        "mean_ms": 357.8,
        "variance_ms2": 451.07,
        "stdev_ms": 21.24,
        "min_ms": 318,
        "max_ms": 386,
        "raw_runs_ms": [
          381,
          356,
          318,
          386,
          365,
          363,
          366,
          338,
          369,
          336
        ]
      }
    },
    "menu-expand-x10": {
      "warm": {
        "mean_ms": 15.7,
        "variance_ms2": 2.01,
        "stdev_ms": 1.42,
        "min_ms": 14,
        "max_ms": 19,
        "raw_runs_ms": [
          15,
          16,
          17,
          19,
          14,
          15,
          15,
          16,
          15,
          15
        ]
      },
      "cold": {
        "mean_ms": 16.1,
        "variance_ms2": 1.43,
        "stdev_ms": 1.2,
        "min_ms": 15,
        "max_ms": 19,
        "raw_runs_ms": [
          15,
          16,
          17,
          16,
          15,
          15,
          16,
          16,
          19,
          16
        ]
      }
    }
  },
  "s7": {
    "load-4-docs": {
      "warm": {
        "mean_ms": 19.7,
        "variance_ms2": 1.57,
        "stdev_ms": 1.25,
        "min_ms": 17,
        "max_ms": 22,
        "raw_runs_ms": [
          22,
          20,
          20,
          17,
          20,
          19,
          20,
          19,
          20,
          20
        ]
      },
      "cold": {
        "mean_ms": 283.3,
        "variance_ms2": 451.79,
        "stdev_ms": 21.26,
        "min_ms": 261,
        "max_ms": 336,
        "raw_runs_ms": [
          281,
          336,
          275,
          295,
          270,
          271,
          261,
          275,
          294,
          275
        ]
      }
    },
    "tree->stree->tree": {
      "warm": {
        "mean_ms": 14.1,
        "variance_ms2": 2.54,
        "stdev_ms": 1.6,
        "min_ms": 11,
        "max_ms": 17,
        "raw_runs_ms": [
          16,
          17,
          14,
          11,
          13,
          14,
          14,
          14,
          14,
          14
        ]
      },
      "cold": {
        "mean_ms": 14.4,
        "variance_ms2": 2.27,
        "stdev_ms": 1.51,
        "min_ms": 12,
        "max_ms": 17,
        "raw_runs_ms": [
          17,
          16,
          13,
          12,
          16,
          14,
          14,
          14,
          14,
          14
        ]
      }
    },
    "export-latex": {
      "warm": {
        "mean_ms": 1067.9,
        "variance_ms2": 623.43,
        "stdev_ms": 24.97,
        "min_ms": 1041,
        "max_ms": 1104,
        "raw_runs_ms": [
          1042,
          1104,
          1044,
          1094,
          1071,
          1086,
          1054,
          1041,
          1048,
          1095
        ]
      },
      "cold": {
        "mean_ms": 2276.4,
        "variance_ms2": 129899.38,
        "stdev_ms": 360.42,
        "min_ms": 2064,
        "max_ms": 3282,
        "raw_runs_ms": [
          3282,
          2122,
          2276,
          2110,
          2119,
          2113,
          2229,
          2257,
          2192,
          2064
        ]
      }
    },
    "export-html": {
      "warm": {
        "mean_ms": 4914.9,
        "variance_ms2": 18819.21,
        "stdev_ms": 137.18,
        "min_ms": 4796,
        "max_ms": 5268,
        "raw_runs_ms": [
          5007,
          4864,
          4906,
          4917,
          4837,
          4823,
          4867,
          4864,
          4796,
          5268
        ]
      },
      "cold": {
        "mean_ms": 6189.2,
        "variance_ms2": 19208.18,
        "stdev_ms": 138.59,
        "min_ms": 6035,
        "max_ms": 6456,
        "raw_runs_ms": [
          6456,
          6226,
          6389,
          6035,
          6150,
          6230,
          6087,
          6076,
          6144,
          6099
        ]
      }
    },
    "import-latex": {
      "warm": {
        "mean_ms": 469.2,
        "variance_ms2": 1119.07,
        "stdev_ms": 33.45,
        "min_ms": 420,
        "max_ms": 520,
        "raw_runs_ms": [
          420,
          520,
          488,
          474,
          470,
          465,
          420,
          482,
          445,
          508
        ]
      },
      "cold": {
        "mean_ms": 822.1,
        "variance_ms2": 3038.32,
        "stdev_ms": 55.12,
        "min_ms": 750,
        "max_ms": 930,
        "raw_runs_ms": [
          824,
          878,
          750,
          777,
          779,
          793,
          815,
          805,
          930,
          870
        ]
      }
    },
    "import-html": {
      "warm": {
        "mean_ms": 608.9,
        "variance_ms2": 494.77,
        "stdev_ms": 22.24,
        "min_ms": 564,
        "max_ms": 649,
        "raw_runs_ms": [
          649,
          613,
          597,
          610,
          624,
          600,
          564,
          603,
          603,
          626
        ]
      },
      "cold": {
        "mean_ms": 628.8,
        "variance_ms2": 2502.4,
        "stdev_ms": 50.02,
        "min_ms": 532,
        "max_ms": 698,
        "raw_runs_ms": [
          676,
          532,
          600,
          579,
          643,
          606,
          669,
          698,
          635,
          650
        ]
      }
    },
    "menu-expand-x10": {
      "warm": {
        "mean_ms": 12.9,
        "variance_ms2": 0.99,
        "stdev_ms": 0.99,
        "min_ms": 12,
        "max_ms": 15,
        "raw_runs_ms": [
          12,
          12,
          13,
          14,
          12,
          12,
          13,
          13,
          13,
          15
        ]
      },
      "cold": {
        "mean_ms": 12.7,
        "variance_ms2": 1.12,
        "stdev_ms": 1.06,
        "min_ms": 11,
        "max_ms": 14,
        "raw_runs_ms": [
          12,
          11,
          13,
          14,
          13,
          13,
          13,
          13,
          14,
          11
        ]
      }
    }
  }
}
```

---

### E. Chez Scheme 10.4.1 10-Run Raw Dataset (Commit `bddbeb664`)

**Platform**: macOS-arm64 (Apple Silicon, offscreen)  
**Sample Size**: $N = 10$ independent process invocations per suite  
**Commit**: `bddbeb6644d48a039120c75b62e2162bfcfcf304` (`bddbeb664`) (`chez-compat.scm` refactored string helpers, `tmscm_is_equal` optimization, all 453 modules compiled)

```json
{
  "iterations": 10,
  "platform": "macOS-arm64 (offscreen, Apple Silicon)",
  "ui_benchmark": {
    "menus-text": {
      "warm": {
        "mean_ms": 79.2,
        "variance_ms2": 27.07,
        "stdev_ms": 5.2,
        "min_ms": 74,
        "max_ms": 91,
        "raw_runs_ms": [78, 75, 81, 82, 76, 91, 83, 77, 75, 74]
      },
      "cold": {
        "mean_ms": 423.1,
        "variance_ms2": 667.21,
        "stdev_ms": 25.83,
        "min_ms": 398,
        "max_ms": 468,
        "raw_runs_ms": [428, 399, 423, 402, 404, 468, 467, 398, 419, 423]
      }
    },
    "menus-math": {
      "warm": {
        "mean_ms": 56.1,
        "variance_ms2": 19.43,
        "stdev_ms": 4.41,
        "min_ms": 51,
        "max_ms": 63,
        "raw_runs_ms": [56, 60, 57, 56, 52, 57, 51, 62, 63, 52]
      },
      "cold": {
        "mean_ms": 65.9,
        "variance_ms2": 23.66,
        "stdev_ms": 4.86,
        "min_ms": 62,
        "max_ms": 76,
        "raw_runs_ms": [67, 63, 64, 66, 63, 73, 76, 62, 63, 62]
      }
    },
    "typing-text-520-keys": {
      "warm": {
        "mean_ms": 1041.2,
        "variance_ms2": 2674.4,
        "stdev_ms": 51.71,
        "min_ms": 995,
        "max_ms": 1146,
        "raw_runs_ms": [1025, 1050, 1025, 1031, 1000, 1146, 1120, 1024, 995, 996]
      },
      "cold": {
        "mean_ms": 1034.9,
        "variance_ms2": 3429.88,
        "stdev_ms": 58.57,
        "min_ms": 974,
        "max_ms": 1128,
        "raw_runs_ms": [1012, 1128, 1055, 993, 995, 1112, 1099, 980, 1001, 974]
      }
    },
    "typing-math-270-keys": {
      "warm": {
        "mean_ms": 1050.3,
        "variance_ms2": 3505.34,
        "stdev_ms": 59.21,
        "min_ms": 994,
        "max_ms": 1151,
        "raw_runs_ms": [1024, 1011, 1006, 1008, 1151, 1135, 1103, 1066, 994, 1005]
      },
      "cold": {
        "mean_ms": 1293.2,
        "variance_ms2": 6029.73,
        "stdev_ms": 77.65,
        "min_ms": 1216,
        "max_ms": 1393,
        "raw_runs_ms": [1364, 1270, 1237, 1225, 1216, 1389, 1380, 1393, 1225, 1233]
      }
    },
    "open-change-log": {
      "warm": {
        "mean_ms": 173.1,
        "variance_ms2": 112.77,
        "stdev_ms": 10.62,
        "min_ms": 161,
        "max_ms": 190,
        "raw_runs_ms": [169, 175, 169, 165, 190, 188, 167, 163, 161, 184]
      },
      "cold": {
        "mean_ms": 481.2,
        "variance_ms2": 677.73,
        "stdev_ms": 26.03,
        "min_ms": 455,
        "max_ms": 537,
        "raw_runs_ms": [482, 489, 473, 462, 537, 514, 472, 459, 455, 469]
      }
    }
  },
  "conversions_benchmark": {
    "load-4-docs": {
      "warm": {
        "mean_ms": 18.0,
        "variance_ms2": 1.33,
        "stdev_ms": 1.15,
        "min_ms": 17,
        "max_ms": 20,
        "raw_runs_ms": [17, 17, 17, 17, 17, 19, 20, 19, 19, 18]
      },
      "cold": {
        "mean_ms": 37.3,
        "variance_ms2": 6.01,
        "stdev_ms": 2.45,
        "min_ms": 34,
        "max_ms": 42,
        "raw_runs_ms": [42, 37, 37, 34, 38, 36, 38, 40, 37, 34]
      }
    },
    "tree->stree->tree": {
      "warm": {
        "mean_ms": 14.5,
        "variance_ms2": 1.83,
        "stdev_ms": 1.35,
        "min_ms": 13,
        "max_ms": 18,
        "raw_runs_ms": [14, 14, 13, 18, 14, 15, 15, 14, 14, 14]
      },
      "cold": {
        "mean_ms": 21.6,
        "variance_ms2": 4.27,
        "stdev_ms": 2.07,
        "min_ms": 19,
        "max_ms": 25,
        "raw_runs_ms": [22, 20, 21, 25, 20, 20, 24, 24, 21, 19]
      }
    },
    "export-latex": {
      "warm": {
        "mean_ms": 862.6,
        "variance_ms2": 1462.04,
        "stdev_ms": 38.24,
        "min_ms": 826,
        "max_ms": 932,
        "raw_runs_ms": [876, 834, 826, 826, 848, 833, 932, 922, 860, 869]
      },
      "cold": {
        "mean_ms": 1284.6,
        "variance_ms2": 2511.38,
        "stdev_ms": 50.11,
        "min_ms": 1229,
        "max_ms": 1374,
        "raw_runs_ms": [1284, 1297, 1242, 1257, 1272, 1275, 1374, 1369, 1247, 1229]
      }
    },
    "export-html": {
      "warm": {
        "mean_ms": 4325.0,
        "variance_ms2": 43695.56,
        "stdev_ms": 209.03,
        "min_ms": 4142,
        "max_ms": 4661,
        "raw_runs_ms": [4661, 4191, 4178, 4178, 4209, 4597, 4611, 4142, 4206, 4277]
      },
      "cold": {
        "mean_ms": 5206.5,
        "variance_ms2": 63929.83,
        "stdev_ms": 252.84,
        "min_ms": 5031,
        "max_ms": 5715,
        "raw_runs_ms": [5715, 5031, 5101, 5106, 5108, 5057, 5641, 5077, 5043, 5186]
      }
    },
    "import-latex": {
      "warm": {
        "mean_ms": 443.2,
        "variance_ms2": 1227.51,
        "stdev_ms": 35.04,
        "min_ms": 415,
        "max_ms": 508,
        "raw_runs_ms": [431, 415, 423, 426, 430, 493, 508, 416, 415, 475]
      },
      "cold": {
        "mean_ms": 435.7,
        "variance_ms2": 781.12,
        "stdev_ms": 27.95,
        "min_ms": 403,
        "max_ms": 485,
        "raw_runs_ms": [460, 416, 412, 445, 425, 485, 471, 415, 425, 403]
      }
    },
    "import-html": {
      "warm": {
        "mean_ms": 146.1,
        "variance_ms2": 149.43,
        "stdev_ms": 12.22,
        "min_ms": 126,
        "max_ms": 165,
        "raw_runs_ms": [150, 141, 133, 140, 165, 152, 163, 126, 143, 148]
      },
      "cold": {
        "mean_ms": 160.7,
        "variance_ms2": 163.12,
        "stdev_ms": 12.77,
        "min_ms": 145,
        "max_ms": 183,
        "raw_runs_ms": [173, 153, 147, 153, 152, 173, 161, 145, 167, 183]
      }
    },
    "menu-expand-x10": {
      "warm": {
        "mean_ms": 10.9,
        "variance_ms2": 1.21,
        "stdev_ms": 1.1,
        "min_ms": 10,
        "max_ms": 13,
        "raw_runs_ms": [13, 10, 10, 10, 11, 11, 12, 10, 10, 12]
      },
      "cold": {
        "mean_ms": 13.6,
        "variance_ms2": 1.82,
        "stdev_ms": 1.35,
        "min_ms": 12,
        "max_ms": 16,
        "raw_runs_ms": [16, 12, 14, 12, 13, 13, 15, 13, 13, 15]
      }
    }
  },
  "manual_regeneration": {
    "expand": {
      "mean_ms": 701.9,
      "variance_ms2": 22171.21,
      "stdev_ms": 148.9,
      "min_ms": 594,
      "max_ms": 986,
      "raw_runs_ms": [619, 986, 827, 916, 599, 594, 615, 626, 641, 596]
    },
    "update-1": {
      "mean_ms": 3644.6,
      "variance_ms2": 1052788.93,
      "stdev_ms": 1026.06,
      "min_ms": 2953,
      "max_ms": 6065,
      "raw_runs_ms": [3172, 6065, 4732, 4068, 3118, 3016, 3064, 3207, 3051, 2953]
    },
    "update-2": {
      "mean_ms": 1301.5,
      "variance_ms2": 188144.72,
      "stdev_ms": 433.76,
      "min_ms": 1035,
      "max_ms": 2397,
      "raw_runs_ms": [1035, 2397, 1635, 1446, 1132, 1056, 1102, 1101, 1052, 1059]
    },
    "update-3": {
      "mean_ms": 1320.9,
      "variance_ms2": 201320.99,
      "stdev_ms": 448.69,
      "min_ms": 1052,
      "max_ms": 2475,
      "raw_runs_ms": [1642, 2475, 1417, 1070, 1089, 1097, 1052, 1204, 1090, 1073]
    },
    "total": {
      "mean_ms": 6968.9,
      "variance_ms2": 3912984.99,
      "stdev_ms": 1978.13,
      "min_ms": 5681,
      "max_ms": 11923,
      "raw_runs_ms": [6468, 11923, 8611, 7500, 5938, 5763, 5833, 6138, 5834, 5681]
    }
  }
}
```


---

### F. Chez Scheme 10.4.1 10-Run Raw Dataset (Commit `e0206bc65`)

**Platform**: macOS-arm64 (Apple Silicon, offscreen)  
**Sample Size**: $N = 10$ independent process invocations per suite  
**Commit**: `e0206bc659e848cec973ac0797cc4d21180e3573` (`e0206bc65`) (*Refactor: dedicated init-chez-compile.scm bootstrap, C++ utf8 conversion, removed thread locking, full 453-module compilation*)

```json
{
  "iterations": 10,
  "platform": "macOS-arm64 (offscreen, Apple Silicon)",
  "ui_benchmark": {
    "menus-text": {
      "warm": {
        "mean_ms": 75.4,
        "variance_ms2": 3.6,
        "stdev_ms": 1.9,
        "min_ms": 72,
        "max_ms": 78,
        "raw_runs_ms": [
          74,
          78,
          77,
          72,
          77,
          76,
          73,
          76,
          75,
          76
        ]
      },
      "cold": {
        "mean_ms": 555.6,
        "variance_ms2": 408.04,
        "stdev_ms": 20.2,
        "min_ms": 539,
        "max_ms": 600,
        "raw_runs_ms": [
          577,
          600,
          570,
          549,
          540,
          549,
          547,
          545,
          539,
          540
        ]
      }
    },
    "menus-math": {
      "warm": {
        "mean_ms": 54.5,
        "variance_ms2": 2.06,
        "stdev_ms": 1.43,
        "min_ms": 52,
        "max_ms": 56,
        "raw_runs_ms": [
          56,
          56,
          55,
          55,
          52,
          53,
          55,
          54,
          53,
          56
        ]
      },
      "cold": {
        "mean_ms": 65.6,
        "variance_ms2": 29.38,
        "stdev_ms": 5.42,
        "min_ms": 61,
        "max_ms": 77,
        "raw_runs_ms": [
          72,
          77,
          65,
          64,
          61,
          62,
          64,
          69,
          61,
          61
        ]
      }
    },
    "typing-text-520-keys": {
      "warm": {
        "mean_ms": 1009,
        "variance_ms2": 128.67,
        "stdev_ms": 11.34,
        "min_ms": 994,
        "max_ms": 1025,
        "raw_runs_ms": [
          1025,
          1003,
          1022,
          998,
          1014,
          1023,
          1002,
          1009,
          994,
          1000
        ]
      },
      "cold": {
        "mean_ms": 1012.1,
        "variance_ms2": 210.32,
        "stdev_ms": 14.5,
        "min_ms": 995,
        "max_ms": 1045,
        "raw_runs_ms": [
          1045,
          1006,
          1009,
          1017,
          1005,
          1023,
          1007,
          997,
          995,
          1017
        ]
      }
    },
    "typing-math-270-keys": {
      "warm": {
        "mean_ms": 1004.3,
        "variance_ms2": 157.79,
        "stdev_ms": 12.56,
        "min_ms": 988,
        "max_ms": 1026,
        "raw_runs_ms": [
          1019,
          993,
          1026,
          1009,
          988,
          1013,
          997,
          996,
          1008,
          994
        ]
      },
      "cold": {
        "mean_ms": 1297.8,
        "variance_ms2": 169.73,
        "stdev_ms": 13.03,
        "min_ms": 1284,
        "max_ms": 1325,
        "raw_runs_ms": [
          1325,
          1292,
          1305,
          1309,
          1284,
          1294,
          1292,
          1288,
          1305,
          1284
        ]
      }
    },
    "open-change-log": {
      "warm": {
        "mean_ms": 168.9,
        "variance_ms2": 34.77,
        "stdev_ms": 5.9,
        "min_ms": 158,
        "max_ms": 178,
        "raw_runs_ms": [
          170,
          167,
          173,
          167,
          167,
          178,
          177,
          158,
          166,
          166
        ]
      },
      "cold": {
        "mean_ms": 921.7,
        "variance_ms2": 672.68,
        "stdev_ms": 25.94,
        "min_ms": 887,
        "max_ms": 963,
        "raw_runs_ms": [
          961,
          914,
          923,
          931,
          904,
          963,
          908,
          893,
          933,
          887
        ]
      }
    }
  },
  "conversions_benchmark": {
    "load-4-docs": {
      "warm": {
        "mean_ms": 19.2,
        "variance_ms2": 0.62,
        "stdev_ms": 0.79,
        "min_ms": 18,
        "max_ms": 20,
        "raw_runs_ms": [
          19,
          19,
          20,
          20,
          20,
          18,
          18,
          20,
          19,
          19
        ]
      },
      "cold": {
        "mean_ms": 32.6,
        "variance_ms2": 5.16,
        "stdev_ms": 2.27,
        "min_ms": 30,
        "max_ms": 37,
        "raw_runs_ms": [
          37,
          31,
          34,
          34,
          35,
          31,
          31,
          31,
          30,
          32
        ]
      }
    },
    "tree->stree->tree": {
      "warm": {
        "mean_ms": 13.1,
        "variance_ms2": 1.43,
        "stdev_ms": 1.2,
        "min_ms": 12,
        "max_ms": 16,
        "raw_runs_ms": [
          13,
          13,
          12,
          16,
          14,
          13,
          12,
          13,
          12,
          13
        ]
      },
      "cold": {
        "mean_ms": 12.6,
        "variance_ms2": 1.16,
        "stdev_ms": 1.07,
        "min_ms": 11,
        "max_ms": 14,
        "raw_runs_ms": [
          13,
          11,
          13,
          13,
          14,
          12,
          12,
          14,
          11,
          13
        ]
      }
    },
    "export-latex": {
      "warm": {
        "mean_ms": 860.5,
        "variance_ms2": 818.5,
        "stdev_ms": 28.61,
        "min_ms": 821,
        "max_ms": 909,
        "raw_runs_ms": [
          821,
          889,
          860,
          841,
          897,
          843,
          909,
          859,
          845,
          841
        ]
      },
      "cold": {
        "mean_ms": 1470.7,
        "variance_ms2": 28172.01,
        "stdev_ms": 167.85,
        "min_ms": 1354,
        "max_ms": 1929,
        "raw_runs_ms": [
          1929,
          1455,
          1397,
          1429,
          1513,
          1400,
          1460,
          1364,
          1406,
          1354
        ]
      }
    },
    "export-html": {
      "warm": {
        "mean_ms": 4217.9,
        "variance_ms2": 1972.54,
        "stdev_ms": 44.41,
        "min_ms": 4156,
        "max_ms": 4280,
        "raw_runs_ms": [
          4189,
          4183,
          4199,
          4280,
          4242,
          4233,
          4262,
          4156,
          4267,
          4168
        ]
      },
      "cold": {
        "mean_ms": 5455.3,
        "variance_ms2": 5308.68,
        "stdev_ms": 72.86,
        "min_ms": 5367,
        "max_ms": 5571,
        "raw_runs_ms": [
          5571,
          5478,
          5425,
          5469,
          5570,
          5367,
          5479,
          5397,
          5425,
          5372
        ]
      }
    },
    "import-latex": {
      "warm": {
        "mean_ms": 398.6,
        "variance_ms2": 199.16,
        "stdev_ms": 14.11,
        "min_ms": 386,
        "max_ms": 429,
        "raw_runs_ms": [
          429,
          387,
          386,
          396,
          412,
          391,
          404,
          406,
          387,
          388
        ]
      },
      "cold": {
        "mean_ms": 390.3,
        "variance_ms2": 241.79,
        "stdev_ms": 15.55,
        "min_ms": 375,
        "max_ms": 424,
        "raw_runs_ms": [
          398,
          409,
          378,
          424,
          375,
          385,
          383,
          388,
          380,
          383
        ]
      }
    },
    "import-html": {
      "warm": {
        "mean_ms": 141.6,
        "variance_ms2": 70.71,
        "stdev_ms": 8.41,
        "min_ms": 130,
        "max_ms": 157,
        "raw_runs_ms": [
          157,
          151,
          140,
          137,
          141,
          141,
          130,
          145,
          144,
          130
        ]
      },
      "cold": {
        "mean_ms": 146.2,
        "variance_ms2": 61.29,
        "stdev_ms": 7.83,
        "min_ms": 133,
        "max_ms": 156,
        "raw_runs_ms": [
          150,
          152,
          135,
          153,
          143,
          146,
          156,
          152,
          133,
          142
        ]
      }
    },
    "menu-expand-x10": {
      "warm": {
        "mean_ms": 8.3,
        "variance_ms2": 0.46,
        "stdev_ms": 0.67,
        "min_ms": 7,
        "max_ms": 9,
        "raw_runs_ms": [
          7,
          8,
          9,
          8,
          8,
          8,
          8,
          9,
          9,
          9
        ]
      },
      "cold": {
        "mean_ms": 8.5,
        "variance_ms2": 0.28,
        "stdev_ms": 0.53,
        "min_ms": 8,
        "max_ms": 9,
        "raw_runs_ms": [
          9,
          8,
          8,
          8,
          9,
          9,
          8,
          9,
          9,
          8
        ]
      }
    }
  },
  "manual_regeneration": {
    "expand": {
      "mean_ms": 613.1,
      "variance_ms2": 11051.43,
      "stdev_ms": 105.13,
      "min_ms": 565,
      "max_ms": 910,
      "raw_runs_ms": [
        910,
        565,
        576,
        566,
        592,
        584,
        581,
        609,
        576,
        572
      ]
    },
    "update-1": {
      "mean_ms": 3068,
      "variance_ms2": 387629.33,
      "stdev_ms": 622.6,
      "min_ms": 2804,
      "max_ms": 4833,
      "raw_runs_ms": [
        4833,
        2804,
        2838,
        2893,
        2846,
        2906,
        2823,
        2945,
        2824,
        2968
      ]
    },
    "update-2": {
      "mean_ms": 1009,
      "variance_ms2": 532.67,
      "stdev_ms": 23.08,
      "min_ms": 988,
      "max_ms": 1068,
      "raw_runs_ms": [
        1022,
        998,
        998,
        1008,
        998,
        1004,
        988,
        992,
        1068,
        1014
      ]
    },
    "update-3": {
      "mean_ms": 1016.7,
      "variance_ms2": 54.23,
      "stdev_ms": 7.36,
      "min_ms": 1007,
      "max_ms": 1025,
      "raw_runs_ms": [
        1011,
        1021,
        1024,
        1008,
        1023,
        1025,
        1018,
        1008,
        1022,
        1007
      ]
    },
    "total": {
      "mean_ms": 5706.8,
      "variance_ms2": 531850.84,
      "stdev_ms": 729.28,
      "min_ms": 5388,
      "max_ms": 7776,
      "raw_runs_ms": [
        7776,
        5388,
        5436,
        5475,
        5459,
        5519,
        5410,
        5554,
        5490,
        5561
      ]
    }
  }
}
```


---

### G. Chez Scheme 10.4.1 10-Run Raw Dataset (2026-11-19, Commit `9a5789cd2`)

**Platform**: macOS-arm64 (Apple Silicon, offscreen)  
**Sample Size**: $N = 10$ independent process invocations per suite  
**Commit**: `9a5789cd204aa9935622d4af6d96afefde6dd9c5` (`9a5789cd2`, includes `448c81e8b` *auto-imported R6RS `chez-compat` and `abbrevs` for `cp0` inlining*)

```json
{
  "iterations": 10,
  "loadavg_start": [
    15.1875,
    9.18017578125,
    5.6484375
  ],
  "ui_benchmark": {
    "menus-text": {
      "cold": {
        "mean_ms": 595.2,
        "variance_ms2": 149.07,
        "stdev_ms": 12.21,
        "min_ms": 581,
        "max_ms": 622,
        "raw_runs_ms": [
          591,
          608,
          593,
          581,
          588,
          594,
          583,
          592,
          622,
          600
        ]
      },
      "warm": {
        "mean_ms": 67.9,
        "variance_ms2": 1.43,
        "stdev_ms": 1.2,
        "min_ms": 66,
        "max_ms": 70,
        "raw_runs_ms": [
          69,
          70,
          67,
          69,
          67,
          68,
          67,
          66,
          68,
          68
        ]
      }
    },
    "menus-math": {
      "cold": {
        "mean_ms": 61,
        "variance_ms2": 2.89,
        "stdev_ms": 1.7,
        "min_ms": 60,
        "max_ms": 65,
        "raw_runs_ms": [
          63,
          65,
          60,
          61,
          60,
          60,
          60,
          60,
          61,
          60
        ]
      },
      "warm": {
        "mean_ms": 50.2,
        "variance_ms2": 7.51,
        "stdev_ms": 2.74,
        "min_ms": 48,
        "max_ms": 56,
        "raw_runs_ms": [
          50,
          51,
          56,
          49,
          49,
          48,
          48,
          48,
          49,
          54
        ]
      }
    },
    "typing-text-520-keys": {
      "cold": {
        "mean_ms": 931.4,
        "variance_ms2": 163.6,
        "stdev_ms": 12.79,
        "min_ms": 918,
        "max_ms": 956,
        "raw_runs_ms": [
          937,
          956,
          934,
          918,
          920,
          931,
          921,
          948,
          929,
          920
        ]
      },
      "warm": {
        "mean_ms": 939.4,
        "variance_ms2": 57.38,
        "stdev_ms": 7.57,
        "min_ms": 928,
        "max_ms": 949,
        "raw_runs_ms": [
          949,
          942,
          928,
          930,
          937,
          946,
          946,
          947,
          935,
          934
        ]
      }
    },
    "typing-math-270-keys": {
      "cold": {
        "mean_ms": 1289.1,
        "variance_ms2": 324.32,
        "stdev_ms": 18.01,
        "min_ms": 1267,
        "max_ms": 1318,
        "raw_runs_ms": [
          1290,
          1267,
          1307,
          1284,
          1278,
          1314,
          1318,
          1270,
          1278,
          1285
        ]
      },
      "warm": {
        "mean_ms": 960.8,
        "variance_ms2": 101.73,
        "stdev_ms": 10.09,
        "min_ms": 950,
        "max_ms": 981,
        "raw_runs_ms": [
          976,
          959,
          962,
          956,
          952,
          960,
          981,
          958,
          954,
          950
        ]
      }
    },
    "open-change-log": {
      "cold": {
        "mean_ms": 1017.5,
        "variance_ms2": 1566.94,
        "stdev_ms": 39.58,
        "min_ms": 978,
        "max_ms": 1083,
        "raw_runs_ms": [
          1036,
          984,
          1083,
          978,
          1055,
          980,
          994,
          987,
          1067,
          1011
        ]
      },
      "warm": {
        "mean_ms": 169.6,
        "variance_ms2": 45.16,
        "stdev_ms": 6.72,
        "min_ms": 164,
        "max_ms": 188,
        "raw_runs_ms": [
          167,
          170,
          169,
          167,
          188,
          167,
          164,
          170,
          168,
          166
        ]
      }
    }
  },
  "manual_benchmark": {
    "expand": {
      "mean_ms": 526.6,
      "variance_ms2": 5613.6,
      "stdev_ms": 74.92,
      "min_ms": 486,
      "max_ms": 738,
      "raw_runs_ms": [
        738,
        501,
        491,
        486,
        514,
        499,
        503,
        511,
        518,
        505
      ]
    },
    "update-1": {
      "mean_ms": 3059.2,
      "variance_ms2": 474498.84,
      "stdev_ms": 688.84,
      "min_ms": 2784,
      "max_ms": 5008,
      "raw_runs_ms": [
        5008,
        2810,
        2802,
        2822,
        2804,
        3037,
        2837,
        2784,
        2795,
        2893
      ]
    },
    "update-2": {
      "mean_ms": 998,
      "variance_ms2": 291.78,
      "stdev_ms": 17.08,
      "min_ms": 979,
      "max_ms": 1028,
      "raw_runs_ms": [
        995,
        985,
        987,
        1009,
        979,
        1006,
        1028,
        979,
        992,
        1020
      ]
    },
    "update-3": {
      "mean_ms": 1032.1,
      "variance_ms2": 334.32,
      "stdev_ms": 18.28,
      "min_ms": 1009,
      "max_ms": 1058,
      "raw_runs_ms": [
        1018,
        1028,
        1009,
        1048,
        1058,
        1042,
        1035,
        1019,
        1009,
        1055
      ]
    },
    "total": {
      "mean_ms": 5615.9,
      "variance_ms2": 575348.77,
      "stdev_ms": 758.52,
      "min_ms": 5289,
      "max_ms": 7759,
      "raw_runs_ms": [
        7759,
        5324,
        5289,
        5365,
        5355,
        5584,
        5403,
        5293,
        5314,
        5473
      ]
    },
    "pages": [
      124,
      124,
      124,
      124,
      124,
      124,
      124,
      124,
      124,
      124
    ]
  },
  "conversions_benchmark": {
    "load-4-docs": {
      "cold": {
        "mean_ms": 19.9,
        "variance_ms2": 1.43,
        "stdev_ms": 1.2,
        "min_ms": 19,
        "max_ms": 22,
        "raw_runs_ms": [
          22,
          19,
          20,
          20,
          22,
          19,
          19,
          19,
          20,
          19
        ]
      },
      "warm": {
        "mean_ms": 16,
        "variance_ms2": 0.22,
        "stdev_ms": 0.47,
        "min_ms": 15,
        "max_ms": 17,
        "raw_runs_ms": [
          16,
          16,
          15,
          16,
          16,
          16,
          17,
          16,
          16,
          16
        ]
      }
    },
    "tree->stree->tree": {
      "cold": {
        "mean_ms": 11.9,
        "variance_ms2": 0.32,
        "stdev_ms": 0.57,
        "min_ms": 11,
        "max_ms": 13,
        "raw_runs_ms": [
          12,
          11,
          11,
          12,
          12,
          12,
          12,
          12,
          12,
          13
        ]
      },
      "warm": {
        "mean_ms": 15.1,
        "variance_ms2": 8.99,
        "stdev_ms": 3.0,
        "min_ms": 13,
        "max_ms": 23,
        "raw_runs_ms": [
          15,
          13,
          13,
          15,
          15,
          13,
          15,
          23,
          13,
          16
        ]
      }
    },
    "export-latex": {
      "cold": {
        "mean_ms": 1196.6,
        "variance_ms2": 34628.93,
        "stdev_ms": 186.09,
        "min_ms": 1095,
        "max_ms": 1717,
        "raw_runs_ms": [
          1717,
          1130,
          1095,
          1156,
          1215,
          1122,
          1104,
          1116,
          1149,
          1162
        ]
      },
      "warm": {
        "mean_ms": 809.5,
        "variance_ms2": 875.83,
        "stdev_ms": 29.59,
        "min_ms": 782,
        "max_ms": 866,
        "raw_runs_ms": [
          795,
          796,
          783,
          856,
          866,
          782,
          801,
          790,
          823,
          803
        ]
      }
    },
    "export-html": {
      "cold": {
        "mean_ms": 5520.9,
        "variance_ms2": 12273.88,
        "stdev_ms": 110.79,
        "min_ms": 5434,
        "max_ms": 5765,
        "raw_runs_ms": [
          5528,
          5464,
          5445,
          5765,
          5677,
          5452,
          5434,
          5464,
          5487,
          5493
        ]
      },
      "warm": {
        "mean_ms": 4117.9,
        "variance_ms2": 3967.88,
        "stdev_ms": 62.99,
        "min_ms": 4058,
        "max_ms": 4250,
        "raw_runs_ms": [
          4101,
          4067,
          4058,
          4250,
          4081,
          4081,
          4189,
          4089,
          4169,
          4094
        ]
      }
    },
    "import-latex": {
      "cold": {
        "mean_ms": 400,
        "variance_ms2": 446.44,
        "stdev_ms": 21.13,
        "min_ms": 385,
        "max_ms": 448,
        "raw_runs_ms": [
          385,
          388,
          423,
          448,
          390,
          386,
          412,
          394,
          388,
          386
        ]
      },
      "warm": {
        "mean_ms": 383.3,
        "variance_ms2": 307.12,
        "stdev_ms": 17.52,
        "min_ms": 361,
        "max_ms": 423,
        "raw_runs_ms": [
          361,
          392,
          381,
          423,
          386,
          384,
          384,
          368,
          365,
          389
        ]
      }
    },
    "import-html": {
      "cold": {
        "mean_ms": 130,
        "variance_ms2": 87.33,
        "stdev_ms": 9.35,
        "min_ms": 123,
        "max_ms": 149,
        "raw_runs_ms": [
          124,
          124,
          127,
          149,
          132,
          125,
          145,
          126,
          123,
          125
        ]
      },
      "warm": {
        "mean_ms": 123.2,
        "variance_ms2": 58.18,
        "stdev_ms": 7.63,
        "min_ms": 115,
        "max_ms": 140,
        "raw_runs_ms": [
          118,
          117,
          119,
          140,
          129,
          121,
          121,
          122,
          130,
          115
        ]
      }
    },
    "menu-expand-x10": {
      "cold": {
        "mean_ms": 8.4,
        "variance_ms2": 0.71,
        "stdev_ms": 0.84,
        "min_ms": 8,
        "max_ms": 10,
        "raw_runs_ms": [
          8,
          8,
          10,
          10,
          8,
          8,
          8,
          8,
          8,
          8
        ]
      },
      "warm": {
        "mean_ms": 8.2,
        "variance_ms2": 0.18,
        "stdev_ms": 0.42,
        "min_ms": 8,
        "max_ms": 9,
        "raw_runs_ms": [
          8,
          8,
          8,
          9,
          8,
          8,
          8,
          9,
          8,
          8
        ]
      }
    }
  },
  "loadavg_end": [
    2.0751953125,
    3.685546875,
    4.162109375
  ],
  "elapsed_s": 431.5
}
```
