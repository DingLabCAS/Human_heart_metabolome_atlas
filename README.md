# Human_heart_metabolome_atlas

## Visualization Code Overview

Welcome to the Human_heart_metabolome_atlas project’s GitHub repository! 

This repository contains all the open source code used to generate the visualizations and data analysis in this article.

### Figue 1

### Figure 2

### Figure 3

### Figure 4 Bubble Plot

Our Bubble Plots are developed in part using Apache ECharts, a powerful open source JavaScript-based visualization library. These charts provide interactivity, allowing users to explore data.

To view and run ECharts related charts, please follow the steps below:

Open HTML file: ECharts charts are generated as independent HTML files, which contain all necessary JavaScript codes. You can open these HTML files directly in the browser. Or, if you are using a local server (such as Python's http.server), you can run this in your terminal:

```bash
python -m http.server 8000
```

Then access it through your browser `http://localhost:8000/Bubble_Plot.html`

**Please note ECharts dependencies**: ECharts is a JavaScript library and usually does not require complex installation steps like Python (such as Conda or pip). It is usually introduced through CDN or directly download .js files. You can place the minimal core file of ECharts in the directory, or use the CDN link in the HTML.