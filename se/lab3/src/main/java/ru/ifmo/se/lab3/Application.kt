package ru.ifmo.se.lab3

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

import org.springframework.boot.CommandLineRunner
import org.springframework.context.annotation.Bean

@SpringBootApplication
class Application {
  @Bean
  fun init(people: PersonRepository,
           bankAccounts: BankAccountRepository,
           actions: ActionRepository) = CommandLineRunner {
    val ne = people.save(Person(name = "Ne",
      bankAccount = bankAccounts.save(BankAccount(10))))
    val ko = people.save(Person(name = "Ko",
      bankAccount = bankAccounts.save(BankAccount(30))))

    actions.save(LocationChangeAction(ne, "new location", TransportationMeans.BY_FOOT))
  }
}

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}
