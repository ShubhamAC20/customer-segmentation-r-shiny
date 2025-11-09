<p align="center">
  <img src="https://img.shields.io/badge/Made%20with-R-blue?style=for-the-badge&logo=r&logoColor=white" alt="Made with R">
  <img src="https://img.shields.io/badge/Framework-Shiny-orange?style=for-the-badge&logo=rstudio&logoColor=white" alt="Shiny">
  <img src="https://img.shields.io/badge/Deployed%20on-Shinyapps.io-brightgreen?style=for-the-badge&logo=cloudflare&logoColor=white" alt="Deployed on Shinyapps.io">
  <img src="https://img.shields.io/badge/Clustering-KMeans-yellow?style=for-the-badge&logo=databricks&logoColor=white" alt="KMeans">
  <img src="https://img.shields.io/badge/License-MIT-lightgrey?style=for-the-badge&logo=open-source-initiative&logoColor=white" alt="MIT License">
</p>

## 🧠 Customer Segmentation Dashboard (R Shiny)

An interactive **R Shiny web application** for performing **RFM-based customer segmentation** using real-world retail transaction data.
The app allows users to upload data, perform clustering, visualize insights, and download labeled customer segments — all within a clean, responsive dashboard.

🔗 **Live App:** [5btii4-shubham-acharya.shinyapps.io/customer-segmentation](https://5btii4-shubham-acharya.shinyapps.io/customer-segmentation/)
📁 **Repo:** Contains R source code (`app.R`), data samples, and deployment details.

---

### ⚙️ Features

* **Dynamic data upload:** Users can import `.csv` retail data directly through the dashboard.
* **Automated preprocessing:** Cleans, filters, and computes RFM metrics (Recency, Frequency, Monetary).
* **Clustering:** Implements **K-Means** with adjustable cluster count (`k`).
* **Visualization suite:**

  * Elbow method for optimal `k`
  * PCA plot for cluster distribution
  * Interactive scatter plots and heatmaps
* **Downloadable output:** Users can export labeled customer segments as CSV.
* **Deployed** on [Shinyapps.io](https://www.shinyapps.io) for public use.

---

### 📊 Tech Stack

| Category      | Tools                                                                   |
| ------------- | ----------------------------------------------------------------------- |
| Language      | R                                                                       |
| Libraries     | dplyr, ggplot2, factoextra, lubridate, tidyr, cluster, shiny, rsconnect |
| Visualization | ggplot2, viridis, PCA, Heatmaps                                         |
| Deployment    | Shinyapps.io                                                            |
| Data Format   | Retail Transactions (InvoiceNo, CustomerID, TotalPrice, Country, etc.)  |

---

### 📈 How It Works

1. **Upload Data** → Retail dataset with basic transaction details.
2. **RFM Calculation** → Computes customer Recency, Frequency, and Monetary metrics.
3. **K-Means Clustering** → Groups customers into segments (e.g., *VIP*, *Loyal*, *Occasional*, *At-Risk*, *Low-Value*).
4. **Visualization** → Displays Elbow curve, PCA projection, and RFM scatterplots.
5. **Download Segments** → Export labeled customer clusters as a CSV file.

---

### 🧩 Folder Structure

```
customer-segmentation-r/
│
├── app.R                     # Main Shiny app script
├── data/                     # Input data (cleaned_retail.csv)
├── plots/                    # Saved visualizations (optional)
├── output/                   # Generated cluster summaries
└── README.md                 # Project documentation
```

---

### 🚀 Deployment

To deploy your own version:

```r
library(rsconnect)
rsconnect::setAccountInfo(name = "YOUR_NAME",
                          token = "YOUR_TOKEN",
                          secret = "YOUR_SECRET")
rsconnect::deployApp(appDir = "path_to_app_folder")
```

---

### 🧾 Example Output

| Segment    | Avg_Recency | Avg_Frequency | Avg_Monetary | Count |
| ---------- | ----------- | ------------- | ------------ | ----- |
| VIP        | 2.1         | 121.75        | 80018.89     | 8     |
| Loyal      | 12.6        | 22.42         | 16539.62     | 209   |
| Occasional | 39.2        | 5.56          | 2442.25      | 1515  |
| At-Risk    | 56.2        | 1.86          | 426.13       | 1635  |
| Low-Value  | 257.3       | 1.45          | 405.30       | 971   |

---

### 🧩 Future Enhancements

* Add **customer lifetime value (CLV)** prediction module.
* Integrate **email targeting simulation** using segment data.
* Add **time-series RFM evolution tracking** to monitor customer behavior over months.

---

### 👨‍💻 Author

**Shubham Acharya**
Data Analyst | R & Power BI Developer
📧 [[YourEmail@example.com](acharyashubham2001@gmail.com)]
🔗 [LinkedIn Profile](https://www.linkedin.com/in/shubhamacharyaanalyst/) | [Portfolio](https://shubhamacharya09.wixsite.com/shubhamacharyadata)
