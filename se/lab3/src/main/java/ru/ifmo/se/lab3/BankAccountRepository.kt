package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository

interface BankAccountRepository : CrudRepository<BankAccount, Long> {
  fun findByOwner(owner: Person): BankAccount
  fun findByOwnerName(name: String): BankAccount
}
