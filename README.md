
## README: EDA - Exploratory Data Analysis in R

### Overview
This project explores country-level economic indicators and network node degree distributions using R. It covers:

1. **Exploratory Data Analysis (EDA)** of country statistics.
2. **Dimensionality Reduction** using **PCA** and **t-SNE** to visualize similarities between countries.
3. **Clustering** using hierarchical methods.
4. **Network Degree Distribution** analysis from a graph dataset.

---

### 📁 Files

| File Name           | Description |
|---------------------|-------------|
| `EDA.R`           | R Markdown file containing all code, plots, and outputs |
| `countrystats.csv`  | Dataset containing economic stats of various countries |
| `network.csv`       | Dataset containing degree (`k`) for each node in a network |

---

### Packages Required

Make sure the following R packages are installed:

```r
install.packages(c("ggplot2", "ggrepel", "Rtsne", "tibble", "pheatmap", "gridExtra"))
```

---

### ▶ How to Run

1. Open `EDA.Rmd` in **RStudio**.
2. Click `Knit` to run the document and generate the HTML output.
3. All plots and answers will be included automatically.

---

### What the Analysis Covers

#### Part 1: Country Stats Analysis
- **t-SNE Visualization** of countries in 2D space.
- Identified 5 countries most similar to **New Zealand**.
- Compared with clustering output (cut from hierarchical dendrogram).
- Plots include heatmaps and t-SNE visualizations.

#### Part 2: Network Degree Distribution
- Histogram of node degrees.
- Line plot of sorted degrees.
- Visual insights about scale-free or sparse network structure.

---

### Notes
- All data was **scaled** before PCA and t-SNE to ensure fair comparisons.
- Both **average** and **complete** linkage methods were tested for clustering.
- Interpretation and comparisons are included in markdown comments within `.Rmd`.

---

## Power&portfoliooptimization: Power Consumption Clustering and Portfolio Optimization

### Overview
This section analyzes household power usage patterns and explores portfolio optimization strategies using R.

1. **Power Consumption Analysis**
   - Visualizes half-hourly electricity usage across households.
   - Performs clustering to identify usage patterns.

2. **Portfolio Optimization**
   - Analyzes 30 investment options (ROI and risk).
   - Creates ROI vs Risk scatterplots and correlation matrix heatmaps.
   - Demonstrates basic portfolio construction principles.

---

### 📁 Files

| File Name             | Description |
|-----------------------|-------------|
| `Power&portfoliooptimization.R`      | R file containing code, analysis, and plots |
| `power.csv`            | Household electricity usage data |
| `invest.csv`           | Investment risk and return data |

---

### Packages Required

```r
install.packages(c("ggplot2", "dplyr", "reshape2"))
```

---

### Notes
- The analysis highlights risk-return trade-offs and diversification challenges based on correlation patterns.
- Clustering of power consumption data helps identify household usage profiles.
