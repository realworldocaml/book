echo travis_fold:start:prepare
ANDROID_REPOSITORY=git://github.com/whitequark/opam-cross-android
export OCAMLFINDFLAGS

case "$OCAML_VERSION" in
4.01.0) ppa=avsm/ppa ;;
4.02.3) ppa=avsm/ppa ;;
*) ppa=avsm/ppa; use_opam=true ;;
esac

install_on_linux () {
  echo "yes" | sudo add-apt-repository ppa:$ppa
  sudo apt-get update -qq
  if test $use_opam; then
      sudo apt-get install -qq opam
      opam init --compiler=$OCAML_COMPILER
  else
      sudo apt-get install -qq ocaml ocaml-native-compilers opam
      opam init
  fi
  eval `opam env`
}

install_on_osx () {
  curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
  sudo hdiutil attach XQuartz-2.7.6.dmg
  sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
  brew update
  brew reinstall ocaml
  brew install libffi opam
  opam init --compiler=$OCAML_COMPILER
  eval `opam env` 
}

install_android_toolchain () {
  sudo apt-get update -qq
  sudo apt-get install -qq gcc-multilib
  install_on_linux
  opam remote add android $ANDROID_REPOSITORY
  ARCH=arm SUBARCH=armv7 SYSTEM=linux_eabi \
    CCARCH=arm TOOLCHAIN=arm-linux-androideabi-4.9 \
    TRIPLE=arm-linux-androideabi LEVEL=24 \
    STLVER=4.9 STLARCH=armeabi \
    opam install conf-android
  opam install ocaml-android
  opam install integers-android
  OCAMLFINDFLAGS='-toolchain android'
}

export OPAMYES=1

echo travis_fold:end:prepare
echo travis_fold:start:install
echo $TRAVIS_OS_NAME
case $ANDROID in
  true) install_android_toolchain ;;
  *) case $TRAVIS_OS_NAME in
       osx) install_on_osx ;;
       linux) install_on_linux ;;
     esac
esac
echo travis_fold:end:install

echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

echo travis_fold:start:install-coverage
# Optional dependencies for coverage testing
if test $COVERAGE -a $TRAVIS_OS_NAME != osx ; then
    opam install bisect_ppx ocveralls
fi
echo travis_fold:end:install-coverage


export OPAMVERBOSE=1
echo travis_fold:start:build
opam pin add -n .
opam upgrade 
if test $ANDROID; then
	opam install --yes ctypes
else
	opam install --build-test --yes ctypes
fi
echo travis_fold:end:build

# TODO Check that the inverted stubs package builds with this release
#opam pin add -n ctypes-inverted-stubs-example https://github.com/yallop/ocaml-ctypes-inverted-stubs-example.git 
#if test ! $ANDROID && test ! $COVERAGE && opam install --show-actions ctypes-inverted-stubs-example; then
#    opam install --build-test --yes ctypes-inverted-stubs-example
#else
#    echo "Pinning the inverted stubs example failed, probably due to OCaml version incompatibility"
#fi

