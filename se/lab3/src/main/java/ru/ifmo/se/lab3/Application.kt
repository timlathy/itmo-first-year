package ru.ifmo.se.lab3

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

import org.springframework.boot.CommandLineRunner
import org.springframework.context.annotation.Bean

@SpringBootApplication
class Application {
  @Bean
  fun init(people: PersonRepository,
           actions: ActionRepository) = CommandLineRunner {
    val ne = people.save(Person("Ne"))
    val ko = people.save(Person("Ko"))

    actions.save(LocationChangeAction(ne, "new location", TransportationMeans.BY_FOOT))
  }
}

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}
