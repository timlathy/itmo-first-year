package ru.ifmo.se.lab3

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RestController

@RestController
class BankTransactionController(private val repo: BankTransactionRepository,
                                private val accountRepo: BankAccountRepository) {
  @GetMapping("/transactions/drawee/{personName}")
  fun findByDrawee(@PathVariable personName: String) =
    repo.findByDraweeAccountOwnerName(personName)
  
  @GetMapping("/transactions/drawer/{personName}")
  fun findByDrawer(@PathVariable personName: String) =
    repo.findByDrawerAccountOwnerName(personName)
}
