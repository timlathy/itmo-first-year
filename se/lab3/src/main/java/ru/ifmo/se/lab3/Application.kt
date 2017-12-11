package ru.ifmo.se.lab3

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.boot.CommandLineRunner
import org.springframework.context.annotation.Bean

import java.time.*

@SpringBootApplication
class Application

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}
