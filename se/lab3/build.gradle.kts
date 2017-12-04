import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

buildscript {
  repositories {
    jcenter()
    mavenCentral()
    maven("https://repo.spring.io/milestone")
  }
  dependencies {
    classpath("org.springframework.boot:spring-boot-gradle-plugin:2.0.0.M7")
    classpath("org.jetbrains.kotlin:kotlin-noarg:1.2.0")
    //classpath(kotlin("gradle-plugin"))
    classpath("com.github.jengelman.gradle.plugins:shadow:2.0.1")
  }
}

apply {
  plugin("org.springframework.boot")
  plugin("kotlin-jpa")
}

plugins {
  val kotlinv = "1.2.0"
  application
  kotlin("jvm") version kotlinv
  id("org.jetbrains.kotlin.plugin.spring") version kotlinv
  id("com.github.johnrengelman.shadow") version "2.0.1"
  id("io.spring.dependency-management") version "1.0.3.RELEASE"
}

repositories {
  mavenCentral()
  maven("http://repo.spring.io/milestone")
}

dependencies {
  compile(kotlin("stdlib"))
  compile("org.springframework.boot:spring-boot-starter-web")
  compile("org.springframework.boot:spring-boot-starter-data-jpa")
  compile("com.h2database:h2")
  compile("org.jetbrains.kotlin:kotlin-stdlib")
  compile("org.jetbrains.kotlin:kotlin-reflect")
  compile("com.fasterxml.jackson.module:jackson-module-kotlin")
}

tasks {
  withType<KotlinCompile> {
    kotlinOptions {
      jvmTarget = "1.8"
    }
  }
}

application {
  mainClassName = "ru.ifmo.se.lab3.ApplicationKt"
}

val shadowJar: ShadowJar by tasks
shadowJar.apply {
  manifest.attributes.apply {
    put("Main-Class", "ru.ifmo.se.lab3.ApplicationKt")
  }
  
  baseName = project.name + "-all"
}
