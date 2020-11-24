#!/bin/sh

CLASSPATH='.:json.jar:junit-4.8.2.jar'

javac -classpath $CLASSPATH com/mylife/test/*.java
javac -classpath $CLASSPATH AtdjTest.java
javadoc -quiet -Xdoclint:none -classpath $CLASSPATH -d doc/test \
        -public com.mylife.test -quiet \
    | grep -v "Creating destination directory"
java  -classpath $CLASSPATH AtdjTest | grep -v -E '^(Time:|JUnit version)' > java.trace
