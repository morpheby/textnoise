#!/usr/bin/env sh

java  -Xms1G -XX:NewSize=512M -XX:MaxNewSize=2G -Xmx8G -jar target/textnoise-1.0-SNAPSHOT.jar
