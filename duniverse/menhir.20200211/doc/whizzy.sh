# Include TEXINPUTS setting from Makefile.local.

# Do not include all of Makefile.local, because both whizzytex and Makefile
# rely on NAME (for different purposes).

if [ -f Makefile.local ]
  then
    echo "Extracting TEXINPUTS setting from Makefile.local..."
    `grep TEXINPUTS Makefile.local`
  fi
