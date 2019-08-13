#! /usr/bin/env python

# Retrieves source code of OPAM packages recursively depending on Lwt into a
# subdirectory ./dependees/, so you can grep through the code.

import os.path
import subprocess

DEPENDEES = "dependees"

def main():
    packages = subprocess.check_output([
        "opam", "list", "--unavailable", "--depends-on=lwt", "--depopts",
        "--recursive", "--short"])

    packages = packages.strip().split("\n")

    subprocess.check_call(["opam", "update"])
    subprocess.check_call(["rm", "-rf", DEPENDEES])

    for package in packages:
        directory = os.path.join(DEPENDEES, package)
        remove_command = ["rm", "-rf", directory]
        source_command = ["opam", "source", "--dir=" + directory]
        subprocess.check_call(remove_command)
        try:
            subprocess.check_call(source_command + ["--dev-repo", package])
        except subprocess.CalledProcessError as e:
            subprocess.check_call(remove_command)
            try:
                subprocess.check_call(source_command + [package])
            except subprocess.CalledProcessError as e:
                pass

if __name__ == "__main__":
    main()
