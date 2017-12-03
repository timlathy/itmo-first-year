package ru.ifmo.se.lab3

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.boot.CommandLineRunner
import org.springframework.context.annotation.Bean

import java.time.*

@SpringBootApplication
class Application {
  @Bean
  fun init(people: PersonRepository,
           bankAccounts: BankAccountRepository,
           actions: ActionRepository,
           conversations: RecordedConversationRepository) = CommandLineRunner {
    val ne = people.save(Person(name = "Незнайка",
      bankAccount = bankAccounts.save(BankAccount(10))))
    val ko = people.save(Person(name = "Козлик",
      bankAccount = bankAccounts.save(BankAccount(30))))

    actions.save(
      LocationChangeAction(ne, "new location", TransportationMeans.BY_FOOT))

    conversations.save(
      RecordedConversation(
        participants = setOf(ne, ko),
        recognizedContent = "придется ... одноразовое питание ... лучше всего питаться вечером перед сном ... если проешь свои денежки днем или утром к вечеру ... проголодаешься и ночью не сможешь заснуть",
        date = LocalDateTime.of(2028, Month.JUNE, 13, 19, 49)))
  }
}

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}
