package ru.ifmo.se.lab3.repository

import org.springframework.data.repository.CrudRepository

import ru.ifmo.se.lab3.domain.BankAccount
import ru.ifmo.se.lab3.domain.Person

interface BankAccountRepository : CrudRepository<BankAccount, Long> {
  fun findByName(name: String): BankAccount
  fun findByOwner(owner: Person): BankAccount
  fun findByOwnerName(ownerName: String): BankAccount
}
