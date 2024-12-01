import sys

# spaces replaced with backticks `

def count_lines(fname,newfname):
    with open(fname) as file:
        length = [len(line.strip()) for line in file.readlines()]
    with open(newfname,"w") as file:
        file.write(f"{len(length)}\n")
        file.write(f"{max(length)}\n")

def write_file(fname,newfname):
    lines = []
    with open(fname) as file:
        lines = [line.strip() for line in file.readlines()]

    # replace spaces with backticks
    lines = [line.replace(" ","`").replace(",","~").replace(";","~") for line in lines]
    for i in range(len(lines)):
        if lines[i] == "":
            lines[i] = "`"

    with open(newfname,"a") as file:
        for line in lines:
            file.write(f"{line}\n")

if __name__ == "__main__":
    fname = sys.argv[1]
    newfname = f"../fortran/{fname}"
    count_lines(fname,newfname)
    write_file(fname,newfname)