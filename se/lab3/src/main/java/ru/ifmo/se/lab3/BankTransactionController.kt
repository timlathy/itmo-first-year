package ru.ifmo.se.lab3

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RestController

@RestController
class BankTransactionController(private val repo: BankTransactionRepository,
                                private val peopleRepo: PersonRepository) {
  @GetMapping("/transactions/drawee/{personName}")
  fun findByDrawee(@PathVariable personName: String) =
    repo.findByDrawee(peopleRepo.findByName(personName))

  @GetMapping("/transactions/drawer/{personName}")
  fun findByDrawer(@PathVariable personName: String) =
    repo.findByDrawer(peopleRepo.findByName(personName))
}
