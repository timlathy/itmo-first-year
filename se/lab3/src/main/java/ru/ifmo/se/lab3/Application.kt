package ru.ifmo.se.lab3

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

import org.springframework.boot.CommandLineRunner
import org.springframework.context.annotation.Bean

@SpringBootApplication
class Application {
  @Bean
  fun init(repo: CharacterRepository) = CommandLineRunner {
    repo.save(Character("N"))
  }
}

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}
