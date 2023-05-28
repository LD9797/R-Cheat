input_filepath = "Datos_tarea_2.txt"
output_filepath = "Datos_fixed_tarea_2_fix.csv"

header = "Tiempo;Objetos;Arquitectura;Efectos;Resolucion"

data_1 = []
data_2 = []

with open(input_filepath, 'r') as f:
    good_lines = [line for line in f if "real" in line or ".sc" in line]

    for line in good_lines:
        if "_" in line:
            splitted_line = line.split("_")[1]
            info, resolution = splitted_line.split("-")

            num_objects = ""
            architecture = ""
            efects = ""

            for char in info:
                if char.isdigit():
                    num_objects += char
                else:
                    architecture += char

            efects = num_objects[-3:]
            num_objects = num_objects[:-3]
            resolution = resolution[:-4]

            anti = "XX" if efects[0] == "0" else "AA"
            transp = "XX" if efects[1] == "0" else "TR"
            reflex = "XX" if efects[2] == "0" else "RE"

            efects = f"{anti}-{transp}-{reflex}"

            data_2.append(f"{num_objects};{architecture};{efects};{resolution}")
        else:
            data_1.append(line)

output = header + "\n"
for x, y in zip(data_1, data_2):
    x = x.strip()[5:].replace(",", ".")
    new_line = f"{x};{y}\n"
    output += new_line

with open(output_filepath, 'w') as f:
    f.write(output)
