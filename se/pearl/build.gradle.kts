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
}

dependencies {
  compile(kotlinModule("stdlib-jdk8", kotlin_version))
  compile("org.jetbrains.kotlin:kotlin-reflect:$kotlin_version")
  compile("org.jetbrains.kotlin:kotlin-test:$kotlin_version")
  compile("org.jetbrains.kotlin:kotlin-test-annotations-common:$kotlin_version")
  compile("org.jetbrains.kotlin:kotlin-test-junit5:$kotlin_version")
  compile("org.postgresql:postgresql:42.2.2")
  testCompile("org.mockito:mockito-core:2.15.0")
}

configure<JavaPluginConvention> {
  sourceCompatibility = JavaVersion.VERSION_1_8
}
tasks.withType<KotlinCompile> {
  kotlinOptions.jvmTarget = "1.8"
}
