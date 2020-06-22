###use for Python calling R
###Rong Jin - 5/26/2020

import subprocess

# your local path to Rscript.exe file
command = "C:/Program Files/R/R-4.0.0/bin/Rscript"

# path to your R script
path2script = "main_github.R"

cmd = [command, path2script]

x = subprocess.check_output(cmd, universal_newlines=True)

print("out", x)
