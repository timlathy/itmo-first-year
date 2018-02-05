import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

buildscript {
  repositories {
    jcenter()
    mavenCentral()
  }
  dependencies {
    //classpath(kotlin("gradle-plugin"))
    classpath("com.github.jengelman.gradle.plugins:shadow:2.0.1")
  }
}

plugins {
  val kotlinv = "1.2.21"
  application
  kotlin("jvm") version kotlinv
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
  compile("org.jline:jline:3.6.0")
}

tasks {
  withType<KotlinCompile> {
    kotlinOptions {
      jvmTarget = "1.8"
    }
  }
}

application {
  mainClassName = "ru.ifmo.se.lab5.ApplicationKt"
}

val shadowJar: ShadowJar by tasks
shadowJar.apply {
  manifest.attributes.apply {
    put("Main-Class", "ru.ifmo.se.lab5.ApplicationKt")
  }
  
  baseName = project.name + "-all"
}
