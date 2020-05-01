#!/usr/bin/env bash

set -e

git remote set-url origin git@github.com:aantron/bisect_ppx.git
git config user.name "Anton Bachin"
git config user.email "antonbachin@yahoo.com"
echo $DEPLOY_KEY | base64 --decode > ~/.ssh/binaries
chmod 400 ~/.ssh/binaries
echo >> ~/.ssh/config
echo "Host github.com" >> ~/.ssh/config
echo "  IdentityFile ~/.ssh/binaries" >> ~/.ssh/config
echo "  StrictHostKeyChecking no" >> ~/.ssh/config

set +e
set -x

case $TRAVIS_OS_NAME in
    "linux") OS=linux;;
    "osx") OS=macos;;
    *) echo Unsupported system $TRAVIS_OS_NAME; exit 1;;
esac

try_to_commit() {
    git config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
    git fetch --unshallow origin
    git checkout -b binaries origin/binaries || git checkout -b binaries
    if ! git merge-base --is-ancestor $TRAVIS_COMMIT binaries
    then
        git reset --hard $TRAVIS_COMMIT
    fi
    mkdir -p bin/$OS
    cp test/bucklescript/node_modules/bisect_ppx/ppx bin/$OS/
    cp test/bucklescript/node_modules/.bin/bisect-ppx-report bin/$OS/
    strip bin/$OS/ppx
    strip bin/$OS/bisect-ppx-report
    git add bin/
    echo "Binaries for '$OS'" > commit-message
    if [ `ls bin | wc -l` != 2 ]
    then
        echo >> commit-message
        echo "[skip ci]" >> commit-message
    fi
    git commit -F commit-message
    git push --force-with-lease -u origin binaries
    RESULT=$?
    git checkout $TRAVIS_BRANCH
    git branch -D binaries
    return $RESULT
}

if ! try_to_commit
then
    if ! try_to_commit
    then
        try_to_commit
    fi
fi
