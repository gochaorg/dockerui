#!/bin/bash

JAVA=/Library/Java/JavaVirtualMachines/liberica-jdk-17-full.jdk/Contents/Home/bin/java
CP="target/dependencies/*"
MAIN=xyz.cofe.lima.ui.Main

$JAVA -classpath "$CP" $MAIN