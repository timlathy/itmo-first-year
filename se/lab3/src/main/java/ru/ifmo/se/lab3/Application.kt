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
           transactions: BankTransactionService,
           actions: ActionRepository,
           conversations: RecordedConversationRepository) = CommandLineRunner {
    val ne = people.save(Person(name = "Незнайка",
      bankAccount = bankAccounts.save(BankAccount(10))))
    val ko = people.save(Person(name = "Козлик",
      bankAccount = bankAccounts.save(BankAccount(30))))

    val richPeople = instantiateRichPeople(people, bankAccounts)   

    actions.save(
      LocationChangeAction(ne, LocalDateTime.of(2028, Month.JUNE, 12, 8, 17), "Downtown", TransportationMeans.BY_FOOT))
    actions.save( 
      LocationChangeAction(ko, LocalDateTime.of(2028, Month.JUNE, 12, 8, 18), "Downtown", TransportationMeans.BY_FOOT))

    // First encounter
    conversations.save(
      RecordedConversation(
        participants = setOf(ne, ko, richPeople[0]),
        recognizedContent = "позвольте ... пожалуйста ... спасибо спасибо",
        date = LocalDateTime.of(2028, Month.JUNE, 12, 9, 32)))
    conversations.save(
      RecordedConversation(
        participants = setOf(ne, ko, richPeople[0]),
        recognizedContent = "какие тяжелые ... помочь ... вот сюда ... отлично",
        date = LocalDateTime.of(2028, Month.JUNE, 12, 9, 49)))
    transactions.transitFunds(amount = 12, drawee = richPeople[0], drawer = ne,
      date = LocalDateTime.of(2028, Month.JUNE, 12, 9, 51))

    conversations.save(
      RecordedConversation(
        participants = setOf(ne, ko),
        recognizedContent = "придется ... одноразовое питание ... лучше всего питаться вечером перед сном ... если проешь свои денежки днем или утром к вечеру ... проголодаешься и ночью не сможешь заснуть",
        date = LocalDateTime.of(2028, Month.JUNE, 13, 19, 49)))
  }

  fun instantiateRichPeople(people: PersonRepository,
                            bankAccounts: BankAccountRepository): ArrayList<Person> = arrayListOf(
    people.save(Person(name = "Богач Билли", bankAccount = bankAccounts.save(BankAccount(8_192)))),
    people.save(Person(name = "Скряга Сэм", bankAccount = bankAccounts.save(BankAccount(16_384)))),
    people.save(Person(name = "Толстосум Том", bankAccount = bankAccounts.save(BankAccount(32_768))))
  )
}

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}
