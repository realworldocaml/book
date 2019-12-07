#!/bin/bash -ue
#
# Copyright 2012-2013 OCamlPro
#
# All rights reserved.This file is distributed under the terms of the
# GNU Lesser General Public License version 2.1 with linking
# exception.
#
# TypeRex is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# Lesser GNU General Public License for more details.
#

shopt -s nullglob

ROOT=$(git rev-parse --show-toplevel | tr -d '\r')
OCP_INDENT=$ROOT/_build/install/default/bin/ocp-indent
cd $ROOT/tests

UPDATE=
GIT=
SHOW=
SHOWCMD=
HTML=

usegit() {
    printf "%-12s\t\e[34mgit %s\e[m\n" "" "$*";
    git "$@";
}

is_file_on_git() {
    [ $# -eq 1 ]; f=$1
    git ls-files $f --error-unmatch >/dev/null 2>&1
}

while [ $# -gt 0 ]; do
    case "$1" in
        --update|-u)
            UPDATE=1
            ;;
        --git-update)
            if ! git diff --ignore-cr-at-eol --exit-code -- . >/dev/null; then
                echo -e "\e[1mWarning:\e[m unstaged changes in tests/"
                echo "You may want to do 'git checkout -- tests/' or"\
                     "'git add -u -- tests/' first."
                exit 1
            fi
            UPDATE=1
            GIT="usegit "
            HTML=1
            ;;
        --ocp-indent)
            if [ $# -le 1 ]; then echo "Error: $1 needs an argument"; exit 1; fi
            shift;
            OCP_INDENT=$1
            ;;
        --show)
            SHOW=1
            ;;
        --meld)
            SHOW=1
            SHOWCMD="meld"
            ;;
        --html)
            HTML=1
            ;;
        *)
            cat <<EOF >/dev/stderr
Usage:
  -u --update         update the files according to the current results
  --git-update        update the files and state the changes in git
  --ocp-indent <prg>  use this ocp-indent exe
  --show              show a diff of changed results
  --meld              show progressions/regressions using meld
  --html              generate an html page showing the diff of failing tests
EOF
            exit 1
    esac
    shift
done

TMP=$(mktemp -d /tmp/ocp-indent-test.XXXXX)
trap "rm -rf /tmp/ocp-indent-${TMP#/tmp/ocp-indent-}" EXIT

ocp-indent() {
    [ $# -eq 1 ]
    opts=$(cat $1.opts 2>/dev/null || true)
    "$OCP_INDENT" $opts "$1" >$TMP/$(basename $1) 2>&1 || true
}

ocp-indent-i() {
    [ $# -eq 1 ]
    opts=$(cat $1.opts 2>/dev/null || true)
    "$OCP_INDENT" "-i" $opts "$1" >/dev/null 2>&1 || true
}



reffile() {
    [ $# -eq 1 ]
    if [ -e "$1.ref" ]
    then echo "$1.ref"
    else echo "$1"
    fi
}

PASSING=("")
FAILING=("")
INPLACE=("")
if [ -n "$GIT" ]; then
    PASSING+=($(git ls-files 'passing/*.ml' 'passing/*.ml[iyl]'))
    FAILING+=($(git ls-files 'failing/*.ml' 'failing/*.ml[iyl]'))
    INPLACE+=($(git ls-files 'inplace/*.ml' 'inplace/*.ml[iyl]'))
else
    PASSING+=(passing/*.ml passing/*.ml[iyl])
    FAILING+=(failing/*.ml failing/*.ml[iyl])
    INPLACE+=(inplace/*.ml inplace/*.ml[iyl])
fi
CHANGES=()


for f in ${PASSING[@]}; do
    base=$(basename $f)
    name=${base%.*}
    ocp-indent $f
    if diff --strip-trailing-cr -q "$(reffile "$f")" $TMP/$base >/dev/null; then
        printf "%-12s\t\e[32m[PASSED]\e[m\n" $name
    else
        printf "%-12s\t\e[31m[FAILED]\e[m \e[41m\e[30m[REGRESSION]\e[m\n" $name
        if [ -n "$UPDATE" ]; then
            mkdir -p failing
            $GIT mv -f $f* failing/
            f=failing/${f#passing/}
            mkdir -p failing-output
            cp $TMP/$base failing-output/
            if [ -n "$GIT" ]; then $GIT add failing-output/$base; fi
        fi
        CHANGES+=($f)
    fi
done

for f in ${FAILING[@]}; do
    base=$(basename $f)
    name=${base%.*}
    ocp-indent $f
    if diff --strip-trailing-cr -q $(reffile $f) $TMP/$base >/dev/null; then
        printf "%-12s\t\e[32m[PASSED]\e[m \e[42m\e[30m[PROGRESSION]\e[m\n" $name
        if [ -n "$UPDATE" ]; then
            $GIT mv -f $f* passing/
            $GIT rm -f failing-output/$base
        fi
    elif [ ! -e failing-output/$base ]; then
        printf "%-12s\t\e[33m[FAILED]\e[m \e[43m\e[30m[NEW]\e[m\n" $name
        cp $TMP/$base failing-output/
        if [ -n "$GIT" ]; then $GIT add failing-output/$base; fi
    elif diff --strip-trailing-cr -q $TMP/$base failing-output/$base >/dev/null; then
        printf "%-12s\t\e[33m[FAILED]\e[m\n" $name
        if [ -n "$GIT" ] && ! is_file_on_git failing-output/$base; then
            $GIT add failing-output/$base; fi
    else
        refcount=$(diff --strip-trailing-cr -y --suppress-common-lines \
            $(reffile $f) failing-output/$base \
            |wc -l)
        curcount=$(diff --strip-trailing-cr -y --suppress-common-lines \
            $(reffile $f) $TMP/$base \
            |wc -l)
        progress=$((refcount - curcount))
        printf "%-12s\t\e[33m[FAILED]\e[m \e[%dm\e[30m[CHANGE: %+d]\e[m\n" \
            $name \
            $(if [ $progress -gt 0 ]; then echo 42; \
              elif [ $progress -eq 0 ]; then echo 43; \
              else echo 41; fi) \
            $progress
        if [ -n "$UPDATE" ]; then
            mkdir -p failing-output
            cp $TMP/$base failing-output/
            if [ -n "$GIT" ]; then $GIT add failing-output/$base; fi
        fi
        CHANGES+=($f)
    fi
done

for f in ${INPLACE[@]}; do
    base=$(basename $f)
    name=${base%.*}
    if [ -L $f ]; then
	dest=$(readlink $f)
	ocp-indent-i $f
	if [ -L $f -a $(readlink $f) = $dest ]; then
	    printf "%-12s\t\e[32m[PASSED]\e[m\n" $name
	else
	    printf  "%-12s\t\e[31m[FAILED]\e[m (nothing will be put in CHANGES)\n" $name
	    rm -f $f
	    ln -s $dest $f
	fi
    else
	perm=$(stat -c '%a' $f)
	ocp-indent-i $f
	if [ $(stat -c '%a' $f) = $perm ]; then
	    printf "%-12s\t\e[32m[PASSED]\e[m\n" $name
	else
	    printf  "%-12s\t\e[31m[FAILED]\e[m (nothing will be put in CHANGES)\n" $name
	    chmod $perm $f
	fi
    fi
done

if [ -n "$SHOW" ] && [ ${#CHANGES[@]} -gt 0 ]; then
    if [ -z "$SHOWCMD" ]; then
        for f in ${CHANGES[@]}; do
            echo
            printf "\e[1m=== Showing differences in %s ===\e[m\n" $f
            # Custom less buggy version of colordiff -y
            diff --strip-trailing-cr -W 130 -ty  $(reffile $f) $TMP/$(basename $f) \
                | awk '/^.{64}[^ ].*/ { printf "[31m%s[m\n",$0; next } 1' \
                || true
        done
    else
        echo
        echo "Meld view:"
        echo "[reference] [new result] [registered]"
        echo "You can update reference and registered status from meld"
        cmd=(meld)
        for f in ${CHANGES[@]}; do
            cur=failing-output/$(basename $f)
            if ! [ -e $cur ]; then cur=; fi
            cmd+=(--diff $(reffile $f) $TMP/$(basename $f) $cur)
        done
        ${cmd[*]}
    fi
elif [ -n "$SHOW" ]; then
    echo
    echo "No changes to show. To check the current failures use for example:"
    echo "  meld tests/failing tests/failing-output"
fi

diff2html() {
    f1=$1; shift
    f2=$1; shift
    [ $# -eq 0 ]

    echo "<div>"
    echo "<h2>Differences in $(basename $f1)</h2>"
    echo "<table>"
    echo "<tr><th><th>Expected<th>Ocp-indent output</tr>"

    {
        line=0
        XIFS="$IFS"
        IFS=
        while read -r l1; do
            read -r l2 <&3 || true
            class="correct"
            if [ "$l1" != "$l2" ]; then
                class="different"
                l1=$(sed 's/ /·/g' <<<"$l1")
                l2=$(sed 's/ /·/g' <<<"$l2")
            fi
            echo -n '<tr>'
            echo -n '<td class="linenum">'$line'</td>'
            echo -n '<td class="'$class'"><pre>'"$l1"'</pre></td>'
            echo -n '<td class="'$class'"><pre>'"$l2"'</pre></td>'
            echo    '</tr>'
            : $((line++))
        done
        while read -r l2 <&3; do
            l2=$(sed 's/ /·/g' <<<"$l2")
            echo -n '<tr>'
            echo -n '<td class="linenum">'$line'</td>'
            echo -n '<td class="different"><pre></pre></td>'
            echo -n '<td class="different"><pre>'"$l2"'</pre></td>'
            echo    '</tr>'
            : $((line++))
        done
        IFS="$XIFS"
    } <$f1 3<$f2

    echo "</table>"
    echo "</div>"
}

if [ -n "$HTML" ]; then
    VERSION=$($OCP_INDENT --version | awk '{ print $NF; exit }')
    if COMMITS_SINCE=$(git log --oneline $VERSION.. 2>/dev/null); then
        VERSION="$VERSION+$((1+$(wc -l <<<"$COMMITS_SINCE")))"
    fi
    VERSION_STRING="$VERSION ($(date +%F))"
    echo
    echo -n "Generating summary of failures tests/failing.html..."
    cat <<EOF > failing.html
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
 "http://www.w3.org/TR/REC-html40/loose.dtd">
<html>
<head>
    <title>Failing tests, ocp-indent version $VERSION_STRING</title>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <style>
      TABLE { border-collapse: collapse; border-spacing: 0px; margin: auto; }
      H2 { margin-top: 5ex; text-align: center; padding: 1ex;
           background-color: orange; }
      TR,TD,PRE { padding: 0; margin: 0; }
      PRE { font-family: mono; }
      TD.linenum { vertical-align: top; font-family: mono;
                   padding-right:2px; text-align: right }
      TD.correct { background-color: #EEE; border: 1px solid white; }
      TD.different { background-color: orange; border: 1px solid white; }
    </style>
</head>
<body>
<h1>Failing tests, ocp-indent version $VERSION_STRING</h1>
EOF
    complete_success="1"
    for f in $(git ls-files 'failing/*.ml'); do
        complete_success=
        diff2html "$(reffile $f)" "failing-output/${f#failing/}" \
            >>failing.html
        echo -n "."
    done
    if [ -n "$complete_success" ]; then
        echo "<p>All tests pass: no currently known bugs.</p>" >>failing.html
    fi
    cat <<EOF >>failing.html
</body>
</html>
EOF

    echo " done"
    if [ -n "$GIT" ]; then $GIT add failing.html; fi
fi

exit ${#CHANGES[@]}
