import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

buildscript {
  var kotlin_version: String by extra
  kotlin_version = "1.2.50"

  repositories {
    mavenCentral()
  }
  dependencies {
    classpath(kotlinModule("gradle-plugin", kotlin_version))
  }
}

apply {
  plugin("kotlin")
}

plugins {
  application
}

val kotlin_version: String by extra

repositories {
  mavenCentral()
  maven(url = "https://jitpack.io")
}

dependencies {
  compile(kotlinModule("stdlib-jdk8", kotlin_version))
  compile("com.github.thymelous:pearl:1bb97d66ef")
  compile("com.sparkjava:spark-core:2.7.2")
  compile("com.github.perwendel:spark-kotlin:master-SNAPSHOT")
  compile("com.fasterxml.jackson.module:jackson-module-kotlin:2.9.5")
  compile("com.fasterxml.jackson.datatype:jackson-datatype-jsr310:2.9.5")
  compile("org.slf4j:slf4j-simple:1.7.25")
  compile("org.mindrot:jbcrypt:0.4")
  testCompile("org.junit.jupiter:junit-jupiter-api:5.2.0")
  testCompile("org.junit.vintage:junit-vintage-engine:5.2.0")
  testCompile("com.despegar:spark-test:1.1.8")
}

application {
  mainClassName = "ru.ifmo.se.lab8.AppKt"
}

configure<JavaPluginConvention> {
  sourceCompatibility = JavaVersion.VERSION_1_8
}
tasks.withType<KotlinCompile> {
  kotlinOptions.jvmTarget = "1.8"
}
