package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository

interface BankAccountRepository : CrudRepository<BankAccount, Long> {
  fun findByName(name: String): BankAccount
  fun findByOwner(owner: Person): BankAccount
  fun findByOwnerName(ownerName: String): BankAccount
}
