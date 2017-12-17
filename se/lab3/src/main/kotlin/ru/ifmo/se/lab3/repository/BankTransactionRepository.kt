package ru.ifmo.se.lab3.repository

import org.springframework.data.repository.CrudRepository
import org.springframework.data.jpa.repository.Query

import ru.ifmo.se.lab3.domain.BankTransaction

interface BankTransactionRepository : CrudRepository<BankTransaction, Long> {
  fun findByDraweeAccountOwnerName(name: String): Iterable<BankTransaction>
  fun findByDrawerAccountOwnerName(name: String): Iterable<BankTransaction>
}
