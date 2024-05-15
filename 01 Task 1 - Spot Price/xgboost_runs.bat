@echo off
echo Running R scripts sequentially...

echo "Running CH - Predictive"
cd ".\1 - XGBoost - Prediction - CH - Data Predictive\"
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" "1 - XGBoost - Prediction - CH - Data Predictive.R"

echo "Running CH - Theoretical"
cd ".\1 - XGBoost - Prediction - CH - Data Theoretical\"
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" "1 - XGBoost - Prediction - CH - Data Theoretical.R"

echo "Running DE - Predictive"
cd ".\1 - XGBoost - Prediction - DE - Data Predictive\"
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" "1 - XGBoost - Prediction - DE - Data Predictive.R"

echo "Running DE - Theoretical"
cd ".\1 - XGBoost - Prediction - DE - Data Theoretical\"
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" "1 - XGBoost - Prediction - DE - Data Theoretical.R"

echo All scripts have been executed.
pause