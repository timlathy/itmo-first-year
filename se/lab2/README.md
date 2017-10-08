# Lab 2

## Prerequisites

* Docker
* Pokemon.jar (should be copied to *libs/*)

Set up the environment by running
```bash
docker pull java:8-jdk
docker build -t kotlin-dev .
```

## Development

Use `./docker-repl` to run a REPL with
`ru.ifmo.se.pokemon` available in the classpath.

## Release

Use `./docker-run` to build a JAR and run it -- handy for quick demos.

Once the build is finished, you can find the standalone JAR at
*./build/libs/src-all.jar*.
