@echo off
echo Running R scripts sequentially...

echo "Running JAO CHDE - Predictive"
cd ".\1 - XGBoost - Prediction - CHDE - Data Predictive\"
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" "1 - XGBoost - Prediction - CHDE - Data Predictive.R"

echo "Running JAO CHDE - Theoretical"
cd ".\1 - XGBoost - Prediction - CHDE - Data Theoretical\"
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" "1 - XGBoost - Prediction - CHDE - Data Theoretical.R"

echo "Running JAO DECH - Predictive"
cd ".\1 - XGBoost - Prediction - DECH - Data Predictive\"
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" "1 - XGBoost - Prediction - DECH - Data Predictive.R"

echo "Running JAO DECH - Theoretical"
cd ".\1 - XGBoost - Prediction - DECH - Data Theoretical\"
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" "1 - XGBoost - Prediction - DECH - Data Theoretical.R"

echo All scripts have been executed.
pause