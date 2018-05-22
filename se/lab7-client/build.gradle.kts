import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

buildscript {
  repositories {
    jcenter()
    mavenCentral()
  }
  dependencies {
    classpath("com.github.jengelman.gradle.plugins:shadow:2.0.1")
    classpath(kotlinModule("gradle-plugin", "1.2.41"))
  }
}

plugins {
  application
  kotlin("jvm") version "1.2.41"
  id("com.github.johnrengelman.shadow") version "2.0.1"
}

repositories {
  jcenter()
  mavenCentral()
}

dependencies {
  compile(kotlin("stdlib"))
  compile("org.jetbrains.kotlin:kotlin-stdlib")
  compile("org.jetbrains.kotlin:kotlin-reflect")
  compile("com.fasterxml.jackson.module:jackson-module-kotlin:2.9.4")
  compile("com.fasterxml.jackson.module:jackson-module-jsonSchema:2.9.4")
  compile("com.fasterxml.jackson.datatype:jackson-datatype-jsr310:2.9.4")
  compile("no.tornado:tornadofx:1.7.16")
  compile("no.tornado:tornadofx-controlsfx:0.1")
  testCompile("org.junit.jupiter:junit-jupiter-api:5.1.0")
  testCompile("org.mockito:mockito-core:2.15.0")
}

tasks {
  withType<KotlinCompile> {
    kotlinOptions {
      jvmTarget = "1.8"
    }
  }
}

val mainClass = "ru.ifmo.se.lab7.client.ApplicationKt"

application {
  mainClassName = mainClass
}

val shadowJar: ShadowJar by tasks
shadowJar.apply {
  baseName = project.name + "-all"
  manifest.attributes.put("Main-Class", mainClass)
}
