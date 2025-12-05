# Anomaly Detection in Energy Consumption Data

This project applies statistical modeling and time-series analysis to identify abnormal usage patterns in residential energy consumption. Using standardized features, Principal Component Analysis (PCA), and Hidden Markov Models (HMMs), the system detects anomalies by evaluating temporal state transitions and deviations in normalized log-likelihood. Based on the model’s behaviour and observed deviations across test periods, a numerical threshold is established to determine whether new observations reflect normal or anomalous consumption.

---

## Project Overview

- Processed and transformed a **1.5M+ row time-series energy consumption dataset**, performing data cleaning, filtering, and noise reduction to prepare the data for modeling.
- Trained and compared multiple **Hidden Markov Models (HMMs)** using maximum likelihood estimation.
- Identified an optimal **15-state HMM configuration**, evaluating log-likelihood patterns to define robust thresholds for anomaly detection.
- Applied **data standardization and scaling** to improve robustness against varying ranges, noise levels, outliers, and dimensionality.

---

## Methods & Techniques

### **Data Preprocessing**
- Spline interpolation to replace missing values  
- Standardization (z-score) to address wide feature ranges and many zero values  
- PCA dimensionality reduction (2 components capturing 61% variance)  
- Selection of consistent time window: Fridays, 5 PM–9 PM  

### **Modeling with Hidden Markov Models**
- Gaussian-emission HMMs trained using the Baum–Welch algorithm  
- Trained models across **4–20 states**, evaluated using log-likelihood and BIC  
- Convergence improved by reducing PCA dimensions and fixing random seed (123)

### **Anomaly Detection**
- Compute normalized log-likelihood for each 2009 period  
- Compare deviations against the model’s training likelihood  
- Establish normal behaviour threshold using maximum observed deviation (±0.75)

---

## Technologies Used

- **R**, tidyverse, dplyr, zoo for preprocessing  
- depmixS4 for HMM training  
- ggplot2 for visualization

---

## Lessons Learned

- High-dimensional HMM training is computationally expensive and sensitive to initialization.
- Reducing PCA dimensions improved convergence without losing important variance.
- Optimal state selection required balancing accuracy with runtime feasibility.