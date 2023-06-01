import nbformat as nbf

# Read your R script
with open('C:\\Users\\user\\PycharmProjects\\RCheatSheet\\Examen2\\Tarea2\\tarea_2_fix.R', 'r') as f:
    r_code = f.read()

# Create a new Jupyter notebook
nb = nbf.v4.new_notebook()

# Add your R code to the notebook
code_cell = nbf.v4.new_code_cell(source=r_code)
nb.cells.append(code_cell)

# Save the notebook
with open('C:\\Users\\user\\PycharmProjects\\RCheatSheet\\PDFs\\tarea_2.ipynb', 'w') as f:
    nbf.write(nb, f)
