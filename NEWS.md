# exametrika 1.6.3

+ Major performance enhancement for GRM (Graded Response Model)
  + Replaced R implementation with high-performance C++ code using Rcpp
  + Implemented analytical gradient computation for significant speed improvements
  + Achieved 5-6x faster convergence compared to numerical differentiation
  + Maintains identical mathematical accuracy to previous implementation
  + Full compatibility with existing GRM() function interface
+ Added `converge` variable to all EM-based functions to indicate algorithm convergence status
  + Functions affected: Biclustering(), LCA(), LRA(), and related methods
  + Returns TRUE if converged within maxiter iterations, FALSE otherwise
  + Displays convergence warning messages when maxiter is reached
+ Enhanced GridSearch() function with convergence handling
  + Automatically excludes non-converged results from optimization
  + Displays warning messages for parameter combinations that failed to converge
  + Returns list of failed settings in output
  + Terminates with error message if all parameter combinations fail to converge
+ Improved numerical stability in Biclustering()
  + Implemented conditional pmax() application to avoid unnecessary log-likelihood inflation
  + Applied numerical correction only when NaN/Inf values are detected
+ High-performance polychoric correlation computation
  + Implemented C++ acceleration for polychoric correlation calculations
  + Achieved significant speed improvements over R-based implementation
+ Improved Array-type plot visualization with enhanced color palette
  + Replaced dull default colors with vibrant, high-contrast color palette
  + Added colorblind-friendly color scheme for better accessibility
  + Binary data (2 categories) now uses black and white for optimal contrast
  + Multi-category data uses enhanced colorblind-accessible palette


# exametrika 1.6.2 

+ Biclustering: Fixed floating-point arithmetic errors causing NaN results

# exametrika 1.6.1 on Aug 26, 2025

+ Biclustering: Fixed improper handling of missing values

# exametrika 1.6.0 on Aug 12, 2025

+ Biculustring.norminal is available!
+ Biclustering.ordinal is available!
+ New function GridSerch() for grid search optimization of model parameters
+ Bugfix: Fix output typos(class/rank)
+ Added duplicate ID validation to dataFormat()
+ Bugfix: Fix ID column detection in dataFormat()
+ Bugfix: Fix stanine division error when unable to split data

# exameterika 1.5.2 on March 29, 2025

+ Bugfix: Fix output typos(TestStatisticsFunction,GRMs)

# exameterika 1.5.1 on March 8, 2025

+ Field analysis for Biclustering is included in the Biclustering() function
+ Class/Rank Reference Vector plot is now available.
+ Bug Fix for polychoric correlations

# exametrika 1.5.0 on March 5, 2025

+ New function GRM is available!

# exametrika 1.4.4 on March 3, 2025

+ In Exametrika 1.4.1, bug fixes were made.
+ In 1.4.2, it became possible to calculate polychoric correlation and polyserial correlation.
+ In 1.4.3, item analysis for polytomous items became available.
+ In 1.4.4, we renamed "ICC" to "IRF," although they refer to the same concept (Item Characteristic Curves and Item Response Functions are interchangeable terms). 
The function will interpret "ICC" input as "IRF" automatically. Additionally, Test Response Function (TRF) output was also made available.

# exametrika 1.4.0 on Feb 25, 2025

+ New function LRA() now supports rated data
+ Hex icon available

# exametrika 1.3.0 on Feb 11, 2025

* Added implementation of latent rank model for ordinal scale data

+ New function LRA() now supports ordinal response data
  + Added visualization methods for ordinal scale analysis:
    + Score frequency with rank thresholds (ScoreFreq)
    + Score-rank probability heatmap (ScoreRank)
    + Item category boundary reference (ICBR)
    + Item category response profile (ICRP)

+ Bug fixes and improvements
+ Standardized terminology: unified the usage of "class" and "rank" throughout the package
+ Various minor bug fixes

# Exametrika 1.2.0 on Jan 30, 2025.

* Improved numerical stability for model estimation
* Bug fixes for log-likelihood calculation 

# Exametrika 1.1.0 on Oct 30, 2024.

* Added support for polytomous response data

# Exametrika 1.0.2 on Aug 17, 2024.

* Bug fix for Issue #12

# Exametrika 1.0.1 on July 31, 2024.

* Bug fix for Item Total Correlation
* Bug fix for Fit indices
* New function called ItemStatistics

# Exametrika 1.0.0 on June 9, 2024.

* Initial Release

# Exametrika 0.11.5 on Mar 30, 2024.

* Bug fix for plot labels for TRP/LRD

# Exametrika 0.11.4 on Jan 27, 2024.

* Bug fix for LCD/LRD for IRM

# Exametrika 0.11.3 on December 15, 2023.

* Bug fix for item information curve

# Exametrika 0.11.2 on October 27, 2023.

* Plot manual update

# Exametrika 0.11.1 on October 27, 2023.

* All model tested by testthat environment

# Exametrika 0.11.0 on October 26, 2023.

* Added BINET model

# Exametrika 0.10.0 on October 18, 2023.

* Added LDB model

# Exametrika 0.9.0 on October 17, 2023.

* Added Structure Learning for LDLRA using PBIL
* Added LDLRA model

# Exametrika 0.8.1 on October 10, 2023.

* Added Structure Learning for BMN with simple GA and PBIL model

# Exametrika 0.8.0 on October 4, 2023.

* Added BNM model

# Exametrika 0.7.0 on October 4, 2023.

* Added IRM model

# Exametrika 0.6.1

* Added Q3 matrix output to the IRT function
