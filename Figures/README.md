# Human_heart_metabolome_atlas

## Overview

This directory contains scripts for UpSet plot, t-SNE plot, chord diagram and bubble plot drew in this article.



### Figue 1 Upset Plot

**Used R packages:**

- **ComplexUpset**: For generating the primary UpSet plot structure
- **ggplot2**: For additional aesthetic customizations and annotations
- **dplyr**: For data manipulation prior to visualization

### Figure 2 t-SNE Plot

**Used R package:**

- **Rtsne**: For implementing the t-SNE algorithm

### Figure 3a t-SNE Plot

**Used R package:**

- **Rtsne**: For implementing the t-SNE algorithm

### Figure 3c Chord Diagram

**Used R package:**

- **circlize**: For creating the chord diagram
- **RColorBrewer**: For color scheme selection
- **dplyr**: For data preparation

**Method of packages installation:**

```R
install.packages("ComplexUpset")
install.packages("ggplot2")
install.packages("Rtsne")
install.packages("circlize")
install.packages("RColorBrewer")
install.packages("viridis")
install.packages("dplyr")
```

### Figure 4 Bubble Plot

Our Bubble Plots are developed in part using Apache ECharts, a powerful open source JavaScript-based visualization library. These charts provide interactivity, allowing users to explore data.

To view and run ECharts related charts, please follow the steps below:

Open HTML file: ECharts charts are generated as independent HTML files, which contain all necessary JavaScript codes. You can open these HTML files directly in the browser. Or, if you are using a local server (such as Python's http.server), you can run this in your terminal:

```bash
python -m http.server 8000
```

Then access it through your browser `http://localhost:8000/Bubble_Plot.html`

**Please note ECharts dependencies**: ECharts is a JavaScript library and usually does not require complex installation steps like Python (such as Conda or pip). It is usually introduced through CDN or directly download .js files. You can place the minimal core file of ECharts in the directory, or use the CDN link in the HTML.