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
  compile("org.hibernate.validator:hibernate-validator:6.0.7.Final")
  compile("org.hibernate.validator:hibernate-validator-annotation-processor:6.0.7.Final")
  compile("javax.validation:validation-api:2.0.1.Final")
  compile("org.glassfish:javax.el:3.0.1-b08")
  compile("log4j:log4j:1.2.17")
  compile("org.mindrot:jbcrypt:0.4")
  compile("com.github.lgooddatepicker:LGoodDatePicker:10.3.1")
  compile("com.alexandriasoftware.swing:jsplitbutton:1.3.1")
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

application {
  mainClassName = "ru.ifmo.se.lab7.server.ApplicationKt"
}

val shadowJar: ShadowJar by tasks
shadowJar.apply {
  manifest.attributes.apply {
    put("Main-Class", "ru.ifmo.se.lab7.server.ApplicationKt")
  }
  
  baseName = project.name + "-all"
}
