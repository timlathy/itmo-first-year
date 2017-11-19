import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

buildscript {
  repositories {
    jcenter()
    mavenCentral()
 }
 dependencies {
   //classpath(kotlin("gradle-plugin"))
   //classpath("com.github.jengelman.gradle.plugins:shadow:2.0.1")
 }
}

plugins {
  application
  kotlin("jvm") version "1.1.51"
  id("com.github.johnrengelman.shadow") version "2.0.1"
}

application {
  mainClassName = "ru.ifmo.se.lab3.MainKt"
}

dependencies {
  compile(kotlin("stdlib"))
  compile("com.github.michaelbull:kotlin-result:1.0.0")
  classpath("com.google.code.gson:2.8.2")
}

repositories {
  mavenCentral()
  maven { url = uri("https://jitpack.io") }
}

val shadowJar: ShadowJar by tasks
shadowJar.apply {
  manifest.attributes.apply {
    put("Main-Class", "ru.ifmo.se.lab3.MainKt")
  }
  
  baseName = project.name + "-all"
}
