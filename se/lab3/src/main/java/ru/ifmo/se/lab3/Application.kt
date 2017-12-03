package ru.ifmo.se.lab3

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

import org.springframework.boot.CommandLineRunner
import org.springframework.context.annotation.Bean

@SpringBootApplication
class Application {
  @Bean
  fun init(characters: CharacterRepository,
           shoutedRemarks: ShoutedRemarkRepository) = CommandLineRunner {
    val ne = characters.save(Character("Ne"))
    val ko = characters.save(Character("Ko"))

    shoutedRemarks.save(ShoutedRemark("Hey", ne))
  }
}

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}
